library(shiny)
library(DBI)
library(RPostgres)
library(DT)
library(rhandsontable)
library(visNetwork)

# -----------------------------------------------------------------------------
# Connection and database helpers
# -----------------------------------------------------------------------------

db_get_conn <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("mml_pep_db"),
    host = Sys.getenv("mml_pep_ip"),
    user = Sys.getenv("mml_pep_user"),
    password = Sys.getenv("mml_pep_user_pw")
  )
}

with_db <- function(operation) {
  con <- db_get_conn()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  operation(con)
}

db_query <- function(sql, params = NULL) {
  with_db(function(con) {
    if (is.null(params)) {
      DBI::dbGetQuery(con, sql)
    } else {
      DBI::dbGetQuery(con, sql, params = params)
    }
  })
}

download_table_csv <- function(schema_name, table_name, row_mode, file) {
  if (
    length(schema_name) != 1L ||
      is.na(schema_name) ||
      !nzchar(schema_name) ||
      length(table_name) != 1L ||
      is.na(table_name) ||
      !nzchar(table_name)
  ) {
    stop("Select a schema and a specific table.", call. = FALSE)
  }

  if (!row_mode %in% c("all", "sample")) {
    stop("Choose all rows or a sample.", call. = FALSE)
  }

  with_db(function(con) {
    table_id <- as.character(DBI::dbQuoteIdentifier(
      con,
      DBI::Id(schema = schema_name, table = table_name)
    ))

    if (row_mode == "all") {
      sql <- paste("SELECT * FROM", table_id)
    } else {
      stats <- DBI::dbGetQuery(
        con,
        "
        SELECT c.reltuples::double precision AS estimated_rows
        FROM pg_class c
        JOIN pg_namespace n ON n.oid = c.relnamespace
        WHERE n.nspname = $1
          AND c.relname = $2
          AND c.relkind IN ('r', 'p')
        ",
        params = list(schema_name, table_name)
      )

      estimate <- if (nrow(stats)) {
        as.numeric(stats$estimated_rows[[1]])
      } else {
        NA_real_
      }

      if (is.finite(estimate) && estimate >= 0 && estimate <= 100000) {
        sql <- paste("SELECT * FROM", table_id, "ORDER BY random() LIMIT 500")
      } else {
        percent <- if (is.finite(estimate) && estimate > 0) {
          min(100, max(0.1, 200000 / estimate))
        } else {
          1
        }

        sql <- paste0(
          "SELECT * FROM ",
          table_id,
          " TABLESAMPLE SYSTEM (",
          format(percent, digits = 8, scientific = FALSE, trim = TRUE),
          ") LIMIT 500"
        )
      }
    }

    result <- DBI::dbSendQuery(con, sql)
    on.exit(DBI::dbClearResult(result), add = TRUE)

    column_names <- DBI::dbColumnInfo(result)$name
    first_chunk <- TRUE

    repeat {
      chunk <- DBI::dbFetch(result, n = 1000L)
      if (!nrow(chunk)) {
        break
      }

      # Convert database array/list columns to text so they can be represented in CSV.
      chunk[] <- lapply(chunk, function(column) {
        if (is.list(column)) {
          vapply(
            column,
            function(value) paste(capture.output(dput(value)), collapse = ""),
            character(1)
          )
        } else {
          column
        }
      })

      write.table(
        chunk,
        file = file,
        sep = ",",
        row.names = FALSE,
        col.names = first_chunk,
        append = !first_chunk,
        na = "",
        qmethod = "double",
        fileEncoding = "UTF-8"
      )

      first_chunk <- FALSE
    }

    if (first_chunk) {
      empty <- as.data.frame(
        setNames(
          replicate(length(column_names), character(0), simplify = FALSE),
          column_names
        ),
        check.names = FALSE
      )

      write.table(
        empty,
        file = file,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        na = "",
        qmethod = "double",
        fileEncoding = "UTF-8"
      )
    }
  })
}

db_fetch_one <- function(sql, params = NULL) {
  result <- db_query(sql, params)

  if (nrow(result) == 0L) {
    return(NULL)
  }

  if (nrow(result) != 1L) {
    stop(
      "Multiple metadata records matched. Resolve duplicates first.",
      call. = FALSE
    )
  }

  result
}

db_exec_one <- function(sql, params) {
  with_db(function(con) {
    DBI::dbWithTransaction(con, {
      affected <- DBI::dbExecute(con, sql, params = params)

      if (affected != 1L) {
        stop(
          paste(
            "Expected exactly one updated record.",
            "The record may have changed or become inaccessible.",
            "Reload before saving."
          ),
          call. = FALSE
        )
      }
    })

    invisible(TRUE)
  })
}

# -----------------------------------------------------------------------------
# Shared utility helpers
# -----------------------------------------------------------------------------

display_text <- function(x) {
  result <- as.character(x)
  result[is.na(result)] <- ""
  result
}

same_metadata_value <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

normalize_deprecated <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(!is.na(x) & x != 0)
  }

  x <- tolower(trimws(as.character(x)))
  ifelse(
    x %in% c("true", "t", "1", "y", "yes"),
    TRUE,
    ifelse(
      x %in% c("false", "f", "0", "n", "no"),
      FALSE,
      NA
    )
  )
}

filter_deprecated <- function(data, show_deprecated = FALSE) {
  if (isTRUE(show_deprecated) || !"deprecated" %in% names(data)) {
    return(data)
  }

  deprecated <- normalize_deprecated(data$deprecated)
  data[is.na(deprecated) | !deprecated, , drop = FALSE]
}

coverage_text <- function(done, total) {
  if (total == 0L) {
    return("N/A")
  }
  paste0(round(100 * done / total), "% (", done, "/", total, ")")
}

# -----------------------------------------------------------------------------
# Read-only browser queries
# -----------------------------------------------------------------------------

SQL_TABLES <- "
SELECT
  schema_name,
  table_name,
  table_description,
  deprecated
FROM database_meta.data4app_tables
ORDER BY schema_name, table_name, id
"

SQL_FIELDS <- "
SELECT
  f.schema_name,
  f.table_name,
  f.field_name,
  f.field_alias,
  f.field_description,
  f.allowed_values,
  f.null_values,
  f.missing_values,
  f.example_values,
  f.how_populated,
  f.units,
  f.deprecated,
  f.data_type,
  f.character_maximum_length,
  f.numeric_precision,
  f.is_nullable,
  f.column_default,
  f.ordinal_position
FROM database_meta.data4app_fields f
WHERE f.schema_name = $1
  AND ($2 = '' OR f.table_name = $2)
ORDER BY f.table_name, f.ordinal_position, f.field_name
"

SQL_FK <- "
SELECT
  schema_name,
  table_name_1,
  field_name_1,
  table_name_2,
  field_name_2,
  join_type_1to2,
  deprecated
FROM database_meta.data4app_fk
WHERE schema_name = $1
  AND ($2 = '' OR table_name_1 = $2 OR table_name_2 = $2)
ORDER BY table_name_1, table_name_2, field_name_1, id
"

dt_widget <- function(data) {
  long_columns <- c(
    "table_description",
    "field_description",
    "how_populated",
    "allowed_values",
    "null_values",
    "missing_values",
    "example_values",
    "column_default"
  )

  long_indexes <- which(names(data) %in% long_columns) - 1L
  column_definitions <- list()

  if (length(long_indexes)) {
    column_definitions <- list(
      list(width = "350px", targets = long_indexes)
    )
  }

  DT::datatable(
    data,
    rownames = FALSE,
    escape = TRUE,
    filter = "top",
    options = list(
      pageLength = 15,
      lengthMenu = c(15, 25, 50, 100),
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = column_definitions
    )
  )
}

# -----------------------------------------------------------------------------
# Lookup and validation helpers
# -----------------------------------------------------------------------------

# SQL_HOW_POPULATED_LOOKUP <- "
# SELECT
#   how_populated_lku::text AS how_populated_lku,
#   how_populated
# FROM database_meta.lku_how_populated
# ORDER BY how_populated
# "

# validate_how_lookup <- function(data) {
#   required <- c("how_populated_lku", "how_populated")

#   if (!all(required %in% names(data))) {
#     stop(
#       "Lookup must contain how_populated_lku and how_populated.",
#       call. = FALSE
#     )
#   }

# data$how_populated_lku <- as.character(data$how_populated_lku)
#   data$how_populated <- as.character(data$how_populated)

#   if (
#     anyNA(data$how_populated_lku) ||
#       anyDuplicated(data$how_populated_lku) ||
#       anyNA(data$how_populated) ||
#       any(!nzchar(trimws(data$how_populated))) ||
#       anyDuplicated(data$how_populated)
#   ) {
#     stop(
#       paste(
#         "Lookup codes and descriptions must be unique and non-missing.",
#         "Descriptions must not be blank."
#       ),
#       call. = FALSE
#     )
#   }

#   data
# }

# load_how_populated_lookup <- function() {
#   validate_how_lookup(db_query(SQL_HOW_POPULATED_LOOKUP))
# }

# -----------------------------------------------------------------------------
# Table-description editing
# -----------------------------------------------------------------------------

TABLE_KEYS <- c("id", "schema_name", "table_name")

load_table_metadata <- function(schema_name) {
  data <- db_query(
    "
    SELECT
      id::text AS id,
      schema_name,
      table_name,
      table_description
    FROM database_meta.tbl_table_descriptions
    WHERE schema_name = $1
    ORDER BY table_name, id
    ",
    list(schema_name)
  )

  if (anyNA(data$id) || anyDuplicated(data$id)) {
    stop("Table metadata IDs must be unique and non-missing.", call. = FALSE)
  }

  data
}

make_table_grid <- function(original) {
  grid <- original
  grid[] <- lapply(grid, display_text)
  grid
}

decode_table_grid <- function(grid, original) {
  expected <- make_table_grid(original)

  if (
    !is.data.frame(grid) ||
      !identical(names(grid), names(expected)) ||
      nrow(grid) != nrow(expected)
  ) {
    stop(
      "Rows or columns changed unexpectedly. Discard edits and reload.",
      call. = FALSE
    )
  }

  grid[] <- lapply(grid, display_text)

  for (column in TABLE_KEYS) {
    if (!identical(grid[[column]], expected[[column]])) {
      stop(
        "A read-only column changed: ",
        column,
        ". Discard edits and reload.",
        call. = FALSE
      )
    }
  }

  edited <- original
  description <- grid$table_description

  description[
    is.na(original$table_description) & description == ""
  ] <- NA_character_

  edited$table_description <- description
  edited
}

changed_table_rows <- function(original, edited) {
  which(
    !same_metadata_value(
      original$table_description,
      edited$table_description
    )
  )
}

table_editor_hotable <- function(grid) {
  hot <- rhandsontable::rhandsontable(
    grid,
    rowHeaders = TRUE,
    height = 650,
    useTypes = TRUE,
    contextMenu = FALSE,
    minSpareRows = 0,
    allowInsertRow = FALSE,
    allowRemoveRow = FALSE,
    allowInsertColumn = FALSE,
    allowRemoveColumn = FALSE,
    manualRowMove = FALSE,
    manualColumnMove = FALSE,
    columnSorting = FALSE,
    colWidths = c(80, 160, 240, 650),
    rowHeights = 60,
    wordWrap = TRUE,
    autoRowSize = TRUE,
    manualColumnResize = TRUE,
    manualRowResize = TRUE,
    stretchH = "none"
  )

  for (column in names(grid)) {
    hot <- rhandsontable::hot_col(
      hot,
      column,
      type = "text",
      wordWrap = TRUE,
      readOnly = column != "table_description"
    )
  }

  hot
}

save_table_metadata <- function(original, edited) {
  required <- c(TABLE_KEYS, "table_description")

  if (
    !is.data.frame(original) ||
      !is.data.frame(edited) ||
      !all(required %in% names(original)) ||
      !identical(names(original), names(edited)) ||
      nrow(original) != nrow(edited) ||
      !identical(original[TABLE_KEYS], edited[TABLE_KEYS]) ||
      anyNA(original$id) ||
      anyDuplicated(original$id) ||
      !is.character(edited$table_description)
  ) {
    stop("Invalid editing dataset. Discard edits and reload.", call. = FALSE)
  }

  rows <- changed_table_rows(original, edited)

  if (!length(rows)) {
    return(0L)
  }

  with_db(function(con) {
    DBI::dbWithTransaction(con, {
      for (row in rows) {
        affected <- DBI::dbExecute(
          con,
          "
          UPDATE database_meta.tbl_table_descriptions
          SET table_description = $1
          WHERE id = $2
            AND schema_name IS NOT DISTINCT FROM $3
            AND table_name IS NOT DISTINCT FROM $4
            AND table_description IS NOT DISTINCT FROM $5
          ",
          params = list(
            edited$table_description[[row]],
            original$id[[row]],
            original$schema_name[[row]],
            original$table_name[[row]],
            original$table_description[[row]]
          )
        )

        if (affected != 1L) {
          stop(
            "Record ",
            original$id[[row]],
            " changed, was removed, or is not writable. ",
            "No descriptions were saved. Reload before retrying.",
            call. = FALSE
          )
        }
      }
    })

    length(rows)
  })
}

# -----------------------------------------------------------------------------
# Field editor configuration
# -----------------------------------------------------------------------------

FIELD_KEYS <- c("id", "schema_name", "table_name", "field_name")

FIELD_TEXT <- c(
  "field_description",
  "field_alias",
  "allowed_values",
  "null_values",
  "missing_values",
  "example_values",
  "how_populated",
  "units"
)

FIELD_EDITABLE <- FIELD_TEXT
FIELD_GRID_EDITABLE <- FIELD_TEXT

SQL_FIELD_EDIT <- "
SELECT
  f.id::text AS id,
  f.schema_name,
  f.table_name,
  f.field_name,
  f.field_alias,
  f.field_description,
  f.allowed_values,
  f.null_values,
  f.missing_values,
  f.example_values,
  f.how_populated,
  f.units
FROM database_meta.tbl_field_descriptions f
WHERE f.schema_name = $1
  AND ($2 = '' OR f.table_name = $2)
ORDER BY f.table_name, f.field_name
"

load_field_metadata <- function(schema_name, table_name = "") {
  data <- db_query(SQL_FIELD_EDIT, list(schema_name, table_name))

  if (anyNA(data$id) || anyDuplicated(data$id)) {
    stop(
      "Field IDs must be unique. Check metadata.",
      call. = FALSE
    )
  }

  if (!all(vapply(data[FIELD_TEXT], is.character, logical(1)))) {
    stop(
      "The editable text columns must be character/text columns.",
      call. = FALSE
    )
  }

  data
}

changed_field_rows <- function(original, edited) {
  changed <- rep(FALSE, nrow(original))

  for (column in FIELD_EDITABLE) {
    changed <- changed |
      !same_metadata_value(original[[column]], edited[[column]])
  }

  which(changed)
}

make_field_grid <- function(original) {
  # codes <- original$how_populated_lku

  # if (any(!is.na(codes) & !codes %in% lookup$how_populated_lku)) {
  #   stop(
  #     "Some field records reference lookup codes that do not exist.",
  #     call. = FALSE
  #   )
  # }

  grid <- original
  # grid$how_populated <- lookup$how_populated[match(
  #   codes,
  #   lookup$how_populated_lku
  # )]
  # grid$how_populated_lku <- NULL
  grid[] <- lapply(grid, display_text)
  grid
}

decode_field_grid <- function(grid, original) {
  expected <- make_field_grid(original)

  if (
    !is.data.frame(grid) ||
      !identical(names(grid), names(expected)) ||
      nrow(grid) != nrow(expected)
  ) {
    stop(
      "Rows or columns changed unexpectedly. Reload the editor.",
      call. = FALSE
    )
  }

  grid[] <- lapply(grid, display_text)

  readonly <- setdiff(names(expected), FIELD_GRID_EDITABLE)
  for (column in readonly) {
    if (!identical(grid[[column]], expected[[column]])) {
      stop(
        "A read-only column changed: ",
        column,
        ". Reload the editor.",
        call. = FALSE
      )
    }
  }

  # labels <- grid$how_populated
  # if (any(nzchar(labels) & !labels %in% lookup$how_populated)) {
  #   stop("Choose how_populated values from the dropdown.", call. = FALSE)
  # }

  edited <- original

  for (column in FIELD_TEXT) {
    value <- grid[[column]]
    value[is.na(original[[column]]) & value == ""] <- NA_character_
    edited[[column]] <- value
  }

  # edited$how_populated_lku <- lookup$how_populated_lku[match(
  #   labels,
  #   lookup$how_populated
  # )]
  # edited$how_populated <- lookup$how_populated[match(
  #   labels,
  #   lookup$how_populated
  # )]

  edited
}

field_editor_hotable <- function(grid) {
  widths <- stats::setNames(rep(160, ncol(grid)), names(grid))

  preferred_widths <- c(
    id = 70,
    schema_name = 140,
    table_name = 190,
    field_name = 200,
    field_description = 320,
    field_alias = 220,
    allowed_values = 280,
    null_values = 280,
    missing_values = 280,
    example_values = 280,
    how_populated = 150,
    units = 75
  )

  matched <- intersect(names(widths), names(preferred_widths))
  widths[matched] <- preferred_widths[matched]

  hot <- rhandsontable::rhandsontable(
    grid,
    rowHeaders = TRUE,
    height = 650,
    useTypes = TRUE,
    contextMenu = FALSE,
    minSpareRows = 0,
    allowInsertRow = FALSE,
    allowRemoveRow = FALSE,
    allowInsertColumn = FALSE,
    allowRemoveColumn = FALSE,
    manualRowMove = FALSE,
    manualColumnMove = FALSE,
    columnSorting = FALSE,
    colWidths = unname(widths),
    rowHeights = 40,
    wordWrap = TRUE,
    autoRowSize = TRUE,
    manualColumnResize = TRUE,
    manualRowResize = TRUE,
    stretchH = "none"
  )

  for (column in names(grid)) {
    hot <- rhandsontable::hot_col(
      hot,
      column,
      type = "text",
      wordWrap = TRUE,
      readOnly = !column %in% FIELD_GRID_EDITABLE
    )
  }

  hot

  # rhandsontable::hot_col(
  #   hot,
  #   "how_populated",
  #   type = "dropdown",
  #   source = c("", lookup$how_populated),
  #   strict = TRUE,
  #   allowInvalid = FALSE,
  #   wordWrap = TRUE,
  #   readOnly = FALSE
  # )
}

save_field_metadata <- function(original, edited) {
  if (
    !is.data.frame(original) ||
      !is.data.frame(edited) ||
      !identical(names(original), names(edited)) ||
      nrow(original) != nrow(edited) ||
      !identical(original[FIELD_KEYS], edited[FIELD_KEYS]) ||
      anyNA(original$id) ||
      anyDuplicated(original$id)
  ) {
    stop("Invalid editing dataset. Reload the editor.", call. = FALSE)
  }

  rows <- changed_field_rows(original, edited)
  if (!length(rows)) {
    return(0L)
  }

  with_db(function(con) {
    quote_column <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

    DBI::dbWithTransaction(con, {
      # lookup <- validate_how_lookup(DBI::dbGetQuery(
      #   con,
      #   SQL_HOW_POPULATED_LOOKUP
      # ))
      # codes <- edited$how_populated_lku[rows]

      # if (any(!is.na(codes) & !codes %in% lookup$how_populated_lku)) {
      #   stop(
      #     "A lookup code is no longer valid. Reload the editor.",
      #     call. = FALSE
      #   )
      # }

      compare_columns <- c(FIELD_KEYS, FIELD_EDITABLE)

      for (row in rows) {
        columns <- FIELD_EDITABLE[
          vapply(
            FIELD_EDITABLE,
            function(column) {
              !same_metadata_value(
                original[[column]][row],
                edited[[column]][row]
              )
            },
            logical(1)
          )
        ]

        set_clause <- paste(
          paste0(quote_column(columns), " = $", seq_along(columns)),
          collapse = ", "
        )

        where_clause <- paste(
          paste0(
            quote_column(compare_columns),
            " IS NOT DISTINCT FROM $",
            length(columns) + seq_along(compare_columns)
          ),
          collapse = " AND "
        )

        params <- c(
          lapply(columns, function(column) edited[[column]][row]),
          lapply(compare_columns, function(column) original[[column]][row])
        )

        affected <- DBI::dbExecute(
          con,
          paste0(
            "UPDATE database_meta.tbl_field_descriptions SET ",
            set_clause,
            " WHERE ",
            where_clause
          ),
          params = params
        )

        if (affected != 1L) {
          stop(
            "Record ",
            original$id[row],
            " changed, was removed, or is not writable. Reload before retrying.",
            call. = FALSE
          )
        }
      }
    })

    length(rows)
  })
}

# -----------------------------------------------------------------------------
# Database diagram helpers
# -----------------------------------------------------------------------------

SQL_DIAGRAM_TABLES <- "
SELECT
  id,
  schema_name,
  table_name,
  table_description,
  deprecated
FROM database_meta.data4app_tables
WHERE schema_name = $1
  AND ($2 = '' OR table_name = $2)
ORDER BY table_name, id
"

SQL_DIAGRAM_RELATIONSHIPS <- "
SELECT
  id,
  schema_name,
  table_name_1,
  field_name_1,
  table_name_2,
  field_name_2,
  join_type_1to2,
  deprecated
FROM database_meta.data4app_fk
WHERE schema_name = $1
  AND ($2 = '' OR table_name_1 = $2 OR table_name_2 = $2)
ORDER BY table_name_1, table_name_2, field_name_1, id
"

load_diagram_tables <- function(schema_name, table_name = "") {
  db_query(SQL_DIAGRAM_TABLES, list(schema_name, table_name))
}

load_diagram_relationships <- function(schema_name, table_name = "") {
  db_query(SQL_DIAGRAM_RELATIONSHIPS, list(schema_name, table_name))
}

normalize_join_direction <- function(join_type) {
  toupper(trimws(as.character(join_type)))
}

join_arrow <- function(join_type) {
  jt <- normalize_join_direction(join_type)

  if (jt == "LEFT") {
    return(list(from = "table_name_1", to = "table_name_2", label = "LEFT"))
  }

  if (jt == "RIGHT") {
    return(list(from = "table_name_2", to = "table_name_1", label = "RIGHT"))
  }

  list(from = "table_name_1", to = "table_name_2", label = "INNER")
}

build_diagram_node_label <- function(table_name, relationships_for_view) {
  fields <- unique(c(
    relationships_for_view$field_name_1[
      relationships_for_view$table_name_1 == table_name
    ],
    relationships_for_view$field_name_2[
      relationships_for_view$table_name_2 == table_name
    ]
  ))

  fields <- as.character(fields)
  fields <- fields[!is.na(fields) & nzchar(trimws(fields))]

  heading <- paste0("<b>", htmltools::htmlEscape(table_name), "</b>")

  if (!length(fields)) {
    return(heading)
  }

  paste(
    c(heading, "", htmltools::htmlEscape(sort(unique(fields)))),
    collapse = "\n"
  )
}

build_database_diagram <- function(tables, relationships, selected_table = "") {
  if (nrow(tables) == 0L) {
    return(
      visNetwork::visNetwork(
        data.frame(id = character(0), label = character(0)),
        data.frame(from = character(0), to = character(0))
      )
    )
  }

  nodes <- data.frame(
    id = paste0(tables$schema_name, ".", tables$table_name),
    label = vapply(
      tables$table_name,
      function(tbl) build_diagram_node_label(tbl, relationships),
      character(1)
    ),
    title = ifelse(
      is.na(tables$table_description) | !nzchar(tables$table_description),
      paste0("<b>", tables$table_name, "</b>"),
      paste0("<b>", tables$table_name, "</b><br/>", tables$table_description)
    ),
    group = tables$schema_name,
    shape = "box",
    stringsAsFactors = FALSE
  )

  nodes$color <- ifelse(
    startsWith(as.character(tables$table_name), "lku_") %in% TRUE,
    "#99b7d4", # Lookup tables: blue
    "#6ca762" # Other visible tables: green
  )
  nodes$borderWidth <- 1

  selected_node_id <- if (nzchar(selected_table)) {
    paste0(unique(tables$schema_name)[1], ".", selected_table)
  } else {
    ""
  }

  if (nzchar(selected_node_id) && selected_node_id %in% nodes$id) {
    nodes$color[nodes$id == selected_node_id] <- "#ca7830"
    nodes$borderWidth[nodes$id == selected_node_id] <- 3
  }

  related_ids <- if (nzchar(selected_node_id) && nrow(relationships) > 0L) {
    unique(c(
      selected_node_id,
      paste0(relationships$schema_name, ".", relationships$table_name_1),
      paste0(relationships$schema_name, ".", relationships$table_name_2)
    ))
  } else {
    nodes$id
  }

  nodes$opacity <- ifelse(nodes$id %in% related_ids, 1, 0.25)

  edges <- if (nrow(relationships) > 0L) {
    do.call(
      rbind,
      lapply(seq_len(nrow(relationships)), function(i) {
        rel <- relationships[i, , drop = FALSE]
        arrow <- join_arrow(rel$join_type_1to2)

        from_tbl <- rel[[arrow$from]][1]
        to_tbl <- rel[[arrow$to]][1]

        data.frame(
          from = paste0(rel$schema_name[[1]], ".", from_tbl),
          to = paste0(rel$schema_name[[1]], ".", to_tbl),
          title = paste0(
            "<b>",
            rel$table_name_1[[1]],
            ".",
            rel$field_name_1[[1]],
            "</b><br/>",
            "→ ",
            "<b>",
            rel$table_name_2[[1]],
            ".",
            rel$field_name_2[[1]],
            "</b><br/>",
            "Join type: ",
            rel$join_type_1to2[[1]]
          ),
          arrows = "to",
          stringsAsFactors = FALSE
        )
      })
    )
  } else {
    data.frame(from = character(0), to = character(0))
  }

  visNetwork::visNetwork(nodes, edges) |>
    visNetwork::visNodes(
      shape = "box",
      margin = list(top = 12, right = 16, bottom = 12, left = 16),
      font = list(
        face = "Arial",
        size = 14,
        align = "left",
        multi = "html",
        bold = list(face = "Arial", size = 18)
      ),
      labelHighlightBold = FALSE
    ) |>
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)),
      color = list(color = "#6c757d", highlight = "#2c7fb8"),
      smooth = FALSE,
      width = 1.5
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1)
    ) |>
    visNetwork::visPhysics(
      stabilization = TRUE,
      barnesHut = list(
        gravitationalConstant = -4000,
        springLength = 220,
        springConstant = 0.02,
        damping = 0.15
      )
    ) |>
    visNetwork::visLayout(randomSeed = 42)
}

# -----------------------------------------------------------------------------
# Metadata dashboard queries and loaders
# -----------------------------------------------------------------------------

SQL_DASH_TABLES <- "
SELECT
  schema_name,
  table_name,
  table_description,
  deprecated
FROM database_meta.data4app_tables
ORDER BY schema_name, table_name
"

SQL_DASH_FIELDS <- "
SELECT
  schema_name,
  table_name,
  field_name,
  field_description,
  how_populated,
  deprecated
FROM database_meta.data4app_fields
ORDER BY schema_name, table_name, field_name
"

SQL_DASH_RELATIONSHIPS <- "
SELECT
  schema_name,
  table_name_1,
  field_name_1,
  table_name_2,
  field_name_2,
  join_type_1to2,
  deprecated
FROM database_meta.data4app_fk
ORDER BY schema_name, table_name_1, table_name_2, field_name_1, field_name_2
"

load_dashboard_tables <- function() {
  db_query(SQL_DASH_TABLES, NULL)
}

load_dashboard_fields <- function() {
  db_query(SQL_DASH_FIELDS, NULL)
}

load_dashboard_relationships <- function() {
  db_query(SQL_DASH_RELATIONSHIPS, NULL)
}
