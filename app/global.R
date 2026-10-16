library(shiny)
library(DBI)
library(RPostgres)
library(DT)
library(rhandsontable)

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
# Read-only browser queries
# -----------------------------------------------------------------------------

SQL_TABLES <- "
SELECT
  id,
  schema_name,
  table_name,
  table_description,
  deprecated
FROM database_meta.data4app_tables
ORDER BY schema_name, table_name, id
"

SQL_FIELDS <- "
SELECT
  f.id,
  f.schema_name,
  f.table_name,
  f.field_name,
  f.field_description,
  f.field_alias,
  f.field_format,
  f.null_values,
  f.missing_values,
  f.allowed_values,
  f.how_populated_lku,
  l.how_populated,
  f.units,
  f.deprecated,
  f.data_type,
  f.character_maximum_length,
  f.numeric_precision,
  f.is_nullable,
  f.column_default,
  f.ordinal_position
FROM database_meta.data4app_fields f
LEFT JOIN database_meta.lku_how_populated l
  ON l.how_populated_lku = f.how_populated_lku
WHERE f.schema_name = $1
  AND ($2 = '' OR f.table_name = $2)
ORDER BY f.table_name, f.ordinal_position, f.field_name, f.id
"

SQL_FK <- "
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

SQL_HOW_POPULATED_LOOKUP <- "
SELECT
  how_populated_lku::text AS how_populated_lku,
  how_populated
FROM database_meta.lku_how_populated
ORDER BY how_populated
"

validate_how_lookup <- function(data) {
  required <- c("how_populated_lku", "how_populated")

  if (!all(required %in% names(data))) {
    stop(
      "Lookup must contain how_populated_lku and how_populated.",
      call. = FALSE
    )
  }

  data$how_populated_lku <- as.character(data$how_populated_lku)
  data$how_populated <- as.character(data$how_populated)

  if (
    anyNA(data$how_populated_lku) ||
      anyDuplicated(data$how_populated_lku) ||
      anyNA(data$how_populated) ||
      any(!nzchar(trimws(data$how_populated))) ||
      anyDuplicated(data$how_populated)
  ) {
    stop(
      paste(
        "Lookup codes and descriptions must be unique and non-missing.",
        "Descriptions must not be blank."
      ),
      call. = FALSE
    )
  }

  data
}

load_how_populated_lookup <- function() {
  validate_how_lookup(db_query(SQL_HOW_POPULATED_LOOKUP))
}

# -----------------------------------------------------------------------------
# Table-description editing
# -----------------------------------------------------------------------------

load_table_description <- function(schema_name, table_name) {
  db_fetch_one(
    "
    SELECT
      id::text AS id,
      schema_name,
      table_name,
      table_description
    FROM database_meta.tbl_table_descriptions
    WHERE schema_name = $1
      AND table_name = $2
    ",
    list(schema_name, table_name)
  )
}

save_table_description <- function(original, description) {
  if (
    !is.data.frame(original) ||
      nrow(original) != 1L ||
      !is.character(description) ||
      length(description) != 1L
  ) {
    stop("Invalid loaded record or description.", call. = FALSE)
  }

  db_exec_one(
    "
    UPDATE database_meta.tbl_table_descriptions
    SET table_description = $1
    WHERE id = $2
      AND schema_name IS NOT DISTINCT FROM $3
      AND table_name IS NOT DISTINCT FROM $4
      AND table_description IS NOT DISTINCT FROM $5
    ",
    list(
      description,
      original$id[[1]],
      original$schema_name[[1]],
      original$table_name[[1]],
      original$table_description[[1]]
    )
  )
}

# -----------------------------------------------------------------------------
# Field editor configuration
# -----------------------------------------------------------------------------

FIELD_KEYS <- c(
  "id",
  "schema_name",
  "table_name",
  "field_name"
)

FIELD_TEXT <- c(
  "field_description",
  "field_alias",
  "field_format",
  "null_values",
  "missing_values",
  "allowed_values",
  "units"
)

FIELD_EDITABLE <- c(FIELD_TEXT, "how_populated_lku")
FIELD_GRID_EDITABLE <- c(FIELD_TEXT, "how_populated")

SQL_FIELD_EDIT <- "
SELECT
  f.id::text AS id,
  f.schema_name,
  f.table_name,
  f.field_name,
  f.field_description,
  f.field_alias,
  f.field_format,
  f.null_values,
  f.missing_values,
  f.allowed_values,
  f.how_populated_lku::text AS how_populated_lku,
  l.how_populated,
  f.units
FROM database_meta.tbl_field_descriptions f
LEFT JOIN database_meta.lku_how_populated l
  ON l.how_populated_lku = f.how_populated_lku
WHERE f.schema_name = $1
  AND ($2 = '' OR f.table_name = $2)
ORDER BY f.table_name, f.field_name, f.id
"

load_field_metadata <- function(schema_name, table_name = "") {
  data <- db_query(SQL_FIELD_EDIT, list(schema_name, table_name))

  if (anyNA(data$id) || anyDuplicated(data$id)) {
    stop(
      "Field IDs must be unique. Check metadata and lookup keys.",
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

display_text <- function(x) {
  result <- as.character(x)
  result[is.na(result)] <- ""
  result
}

same_metadata_value <- function(x, y) {
  (is.na(x) & is.na(y)) |
    (!is.na(x) & !is.na(y) & x == y)
}

changed_field_rows <- function(original, edited) {
  changed <- rep(FALSE, nrow(original))

  for (column in FIELD_EDITABLE) {
    changed <- changed |
      !same_metadata_value(original[[column]], edited[[column]])
  }

  which(changed)
}

make_field_grid <- function(original, lookup) {
  codes <- original$how_populated_lku

  if (
    any(
      !is.na(codes) &
        !codes %in% lookup$how_populated_lku
    )
  ) {
    stop(
      "Some field records reference lookup codes that do not exist.",
      call. = FALSE
    )
  }

  grid <- original
  grid$how_populated <- lookup$how_populated[
    match(codes, lookup$how_populated_lku)
  ]

  grid$how_populated_lku <- NULL
  grid[] <- lapply(grid, display_text)
  grid
}

decode_field_grid <- function(grid, original, lookup) {
  expected <- make_field_grid(original, lookup)

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

  labels <- grid$how_populated

  if (
    any(
      nzchar(labels) &
        !labels %in% lookup$how_populated
    )
  ) {
    stop(
      "Choose how_populated values from the dropdown.",
      call. = FALSE
    )
  }

  edited <- original

  for (column in FIELD_TEXT) {
    value <- grid[[column]]
    value[is.na(original[[column]]) & value == ""] <- NA_character_
    edited[[column]] <- value
  }

  edited$how_populated_lku <- lookup$how_populated_lku[
    match(labels, lookup$how_populated)
  ]
  edited$how_populated <- lookup$how_populated[
    match(labels, lookup$how_populated)
  ]

  edited
}

field_editor_hotable <- function(grid, lookup) {
  widths <- stats::setNames(rep(160, ncol(grid)), names(grid))

  preferred_widths <- c(
    id = 70,
    schema_name = 140,
    table_name = 190,
    field_name = 200,
    field_description = 320,
    field_alias = 220,
    field_format = 220,
    null_values = 280,
    missing_values = 280,
    allowed_values = 280,
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

  rhandsontable::hot_col(
    hot,
    "how_populated",
    type = "dropdown",
    source = c("", lookup$how_populated),
    strict = TRUE,
    allowInvalid = FALSE,
    wordWrap = TRUE,
    readOnly = FALSE
  )
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
    quote_column <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    DBI::dbWithTransaction(con, {
      lookup <- validate_how_lookup(
        DBI::dbGetQuery(con, SQL_HOW_POPULATED_LOOKUP)
      )

      codes <- edited$how_populated_lku[rows]

      if (
        any(
          !is.na(codes) &
            !codes %in% lookup$how_populated_lku
        )
      ) {
        stop(
          "A lookup code is no longer valid. Reload the editor.",
          call. = FALSE
        )
      }

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
            " changed, was removed, or is not writable. ",
            "Reload before retrying.",
            call. = FALSE
          )
        }
      }
    })

    length(rows)
  })
}

# -----------------------------------------------------------------------------
# Browser display helpers
# -----------------------------------------------------------------------------

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

dt_widget <- function(data) {
  long_columns <- c(
    "table_description",
    "field_description",
    "null_values",
    "missing_values",
    "allowed_values",
    "column_default"
  )

  long_indexes <- which(names(data) %in% long_columns) - 1L

  column_definitions <- list()

  if (length(long_indexes)) {
    column_definitions <- list(
      list(
        width = "350px",
        targets = long_indexes
      )
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
