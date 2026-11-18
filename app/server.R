function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Shared session state and error reporting
  # ---------------------------------------------------------------------------

  metadata_saved <- reactiveVal(0L)

  field_sheet <- reactiveVal(NULL)
  sheet_number <- 0L

  notify_error <- function(prefix, error) {
    message(prefix, ": ", conditionMessage(error))
    showNotification(
      paste(prefix, "Check the R console/application log for details."),
      type = "error",
      duration = NULL
    )
  }

  # ---------------------------------------------------------------------------
  # Catalog and sidebar selections
  # ---------------------------------------------------------------------------

  catalog <- reactive({
    input$refresh
    metadata_saved()
    db_query(SQL_TABLES)
  })

  observe({
    schemas <- sort(unique(catalog()$schema_name))
    schemas <- schemas[!is.na(schemas) & nzchar(schemas)]

    current <- isolate(input$schema)
    selected <- if (length(current) == 1L && current %in% schemas) {
      current
    } else if (length(schemas)) {
      schemas[[1]]
    } else {
      character(0)
    }

    updateSelectInput(session, "schema", choices = schemas, selected = selected)
  })

  observe({
    req(input$schema)

    data <- catalog()
    tables <- sort(unique(data$table_name[data$schema_name %in% input$schema]))
    tables <- tables[!is.na(tables) & nzchar(tables)]

    current <- isolate(input$table)
    selected <- if (length(current) == 1L && current %in% c("", tables)) {
      current
    } else {
      ""
    }

    updateSelectInput(
      session,
      "table",
      choices = c("All tables" = "", stats::setNames(tables, tables)),
      selected = selected
    )
  })

  selection <- reactive({
    req(input$schema)
    req(length(input$table) == 1L)

    data <- catalog()
    req(input$schema %in% data$schema_name)

    tables <- data$table_name[data$schema_name %in% input$schema]
    req(input$table == "" || input$table %in% tables)

    list(schema = input$schema, table = input$table)
  })

  # ---------------------------------------------------------------------------
  # Read-only browser tabs
  # ---------------------------------------------------------------------------

  filtered_tables <- reactive({
    selected <- selection()
    data <- catalog()

    data <- data[data$schema_name %in% selected$schema, , drop = FALSE]
    if (nzchar(selected$table)) {
      data <- data[data$table_name %in% selected$table, , drop = FALSE]
    }

    filter_deprecated(data, input$show_deprecated)
  })

  details <- reactive({
    input$refresh
    metadata_saved()

    selected <- selection()
    params <- list(selected$schema, selected$table)

    list(
      fields = db_query(SQL_FIELDS, params),
      relationships = db_query(SQL_FK, params),
      retrieved_at = Sys.time()
    )
  })

  output$tables <- DT::renderDT({
    dt_widget(filtered_tables())
  })

  output$fields <- DT::renderDT({
    dt_widget(filter_deprecated(details()$fields, input$show_deprecated))
  })

  output$relationships <- DT::renderDT({
    dt_widget(filter_deprecated(details()$relationships, input$show_deprecated))
  })

  output$status <- renderText({
    selected <- selection()

    paste0(
      "Schema: ",
      selected$schema,
      " | Table: ",
      if (nzchar(selected$table)) selected$table else "(all)",
      " | Details retrieved: ",
      format(details()$retrieved_at, "%Y-%m-%d %H:%M:%S %Z")
    )
  })

  # ---------------------------------------------------------------------------
  # Schema-wide table-description editor
  # ---------------------------------------------------------------------------

  table_sheet <- reactiveVal(NULL)
  table_sheet_number <- 0L

  start_table_sheet <- function(original, schema_name) {
    grid <- make_table_grid(original)
    previous <- isolate(table_sheet())

    if (!is.null(previous)) {
      output[[previous$id]] <- NULL
    }

    table_sheet_number <<- table_sheet_number + 1L
    widget_id <- paste0("table_sheet_", table_sheet_number)

    output[[widget_id]] <- rhandsontable::renderRHandsontable({
      table_editor_hotable(grid)
    })

    table_sheet(list(
      id = widget_id,
      original = original,
      schema_name = schema_name
    ))
  }

  clear_table_sheet <- function() {
    current <- isolate(table_sheet())

    if (!is.null(current)) {
      output[[current$id]] <- NULL
    }

    table_sheet(NULL)
  }

  output$table_grid_container <- renderUI({
    sheet <- table_sheet()

    if (is.null(sheet)) {
      return(helpText("No table descriptions loaded."))
    }

    rhandsontable::rHandsontableOutput(sheet$id, height = "650px")
  })

  current_table_data <- reactive({
    sheet <- table_sheet()

    if (is.null(sheet)) {
      return(NULL)
    }

    payload <- input[[sheet$id]]

    if (is.null(payload)) {
      return(sheet$original)
    }

    decode_table_grid(
      rhandsontable::hot_to_r(payload),
      sheet$original
    )
  })

  output$table_grid_target <- renderText({
    sheet <- table_sheet()

    if (is.null(sheet)) {
      return("No table descriptions loaded.")
    }

    paste0(
      "Loaded schema: ",
      sheet$schema_name,
      " — ",
      nrow(sheet$original),
      " record(s)"
    )
  })

  output$table_grid_changes <- renderText({
    sheet <- table_sheet()

    if (is.null(sheet)) {
      return("0 records with unsaved changes.")
    }

    tryCatch(
      {
        count <- length(changed_table_rows(
          sheet$original,
          current_table_data()
        ))

        paste(count, "record(s) with unsaved changes.")
      },
      error = function(e) {
        paste("Cannot save yet:", conditionMessage(e))
      }
    )
  })

  observeEvent(input$load_table_grid, {
    req(input$schema)

    tryCatch(
      {
        current <- table_sheet()

        if (
          !is.null(current) &&
            length(changed_table_rows(
              current$original,
              current_table_data()
            )) >
              0L
        ) {
          showNotification(
            "Save or discard your edits before loading another schema.",
            type = "warning"
          )
          return()
        }

        original <- load_table_metadata(input$schema)

        if (nrow(original) == 0L) {
          showNotification(
            "No existing table-description records matched this schema.",
            type = "warning"
          )
          return()
        }

        start_table_sheet(original, input$schema)
      },
      error = function(e) {
        notify_error(
          "Table load failed. Discard invalid edits first if necessary.",
          e
        )
      }
    )
  })

  observeEvent(input$reset_table_grid, {
    sheet <- table_sheet()
    req(!is.null(sheet))
    start_table_sheet(sheet$original, sheet$schema_name)

    showNotification(
      "Unsaved table-description edits discarded.",
      type = "message"
    )
  })

  observeEvent(input$save_table_grid, {
    sheet <- table_sheet()

    if (is.null(sheet)) {
      showNotification(
        "Load table descriptions first.",
        type = "warning"
      )
      return()
    }

    tryCatch(
      {
        count <- save_table_metadata(
          sheet$original,
          current_table_data()
        )

        if (count == 0L) {
          showNotification("No changes to save.")
          return()
        }

        clear_table_sheet()
        metadata_saved(metadata_saved() + 1L)

        showNotification(
          paste(
            count,
            "table description(s) saved. Load records to continue."
          ),
          type = "message"
        )
      },
      error = function(e) {
        notify_error(
          "Batch save failed; edits remain in the spreadsheet.",
          e
        )
      }
    )
  })
  # ---------------------------------------------------------------------------
  # Spreadsheet lifecycle
  # ---------------------------------------------------------------------------

  start_field_sheet <- function(original, target) {
    grid <- make_field_grid(original)
    previous <- isolate(field_sheet())

    if (!is.null(previous)) {
      output[[previous$id]] <- NULL
    }

    sheet_number <<- sheet_number + 1L
    widget_id <- paste0("field_sheet_", sheet_number)

    output[[widget_id]] <- rhandsontable::renderRHandsontable({
      field_editor_hotable(grid)
    })

    field_sheet(list(
      id = widget_id,
      original = original,
      target = target
    ))
  }

  clear_field_sheet <- function() {
    current <- isolate(field_sheet())

    if (!is.null(current)) {
      output[[current$id]] <- NULL
    }

    field_sheet(NULL)
  }

  output$field_grid_container <- renderUI({
    sheet <- field_sheet()

    if (is.null(sheet)) {
      return(helpText("No field records loaded."))
    }

    rhandsontable::rHandsontableOutput(sheet$id, height = "650px")
  })

  current_field_data <- reactive({
    sheet <- field_sheet()
    if (is.null(sheet)) {
      return(NULL)
    }

    payload <- input[[sheet$id]]
    if (is.null(payload)) {
      return(sheet$original)
    }

    grid <- rhandsontable::hot_to_r(payload)
    decode_field_grid(grid, sheet$original)
  })

  output$field_grid_target <- renderText({
    sheet <- field_sheet()

    if (is.null(sheet)) {
      return("No field records loaded.")
    }

    paste0("Loaded: ", sheet$target, " — ", nrow(sheet$original), " record(s)")
  })

  output$field_grid_changes <- renderText({
    sheet <- field_sheet()

    if (is.null(sheet)) {
      return("0 records with unsaved changes.")
    }

    tryCatch(
      {
        count <- length(changed_field_rows(
          sheet$original,
          current_field_data()
        ))
        paste(count, "record(s) with unsaved changes.")
      },
      error = function(e) {
        paste("Cannot save yet:", conditionMessage(e))
      }
    )
  })

  observeEvent(input$load_field_grid, {
    selected <- selection()

    tryCatch(
      {
        current <- field_sheet()

        if (
          !is.null(current) &&
            length(changed_field_rows(current$original, current_field_data())) >
              0L
        ) {
          showNotification(
            "Save or discard your edits before loading another dataset.",
            type = "warning"
          )
          return()
        }

        original <- load_field_metadata(selected$schema, selected$table)
        # lookup <- load_how_populated_lookup()

        if (nrow(original) == 0L) {
          showNotification(
            "No existing field-metadata records matched.",
            type = "warning"
          )
          return()
        }

        target <- paste0(
          selected$schema,
          ".",
          if (nzchar(selected$table)) selected$table else "(all tables)"
        )

        start_field_sheet(original, target)
      },
      error = function(e) {
        notify_error(
          "Field load failed. Discard invalid edits first if necessary.",
          e
        )
      }
    )
  })

  observeEvent(input$reset_field_grid, {
    sheet <- field_sheet()
    req(!is.null(sheet))

    start_field_sheet(sheet$original, sheet$target)
    showNotification("Unsaved edits discarded.", type = "message")
  })

  observeEvent(input$save_field_grid, {
    sheet <- field_sheet()

    if (is.null(sheet)) {
      showNotification("Load field records first.", type = "warning")
      return()
    }

    tryCatch(
      {
        edited <- current_field_data()
        count <- save_field_metadata(sheet$original, edited)

        if (count == 0L) {
          showNotification("No changes to save.")
          return()
        }

        clear_field_sheet()
        metadata_saved(metadata_saved() + 1L)

        showNotification(
          paste(count, "field record(s) saved. Load records to continue."),
          type = "message"
        )
      },
      error = function(e) {
        notify_error("Batch save failed; edits remain in the spreadsheet.", e)
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Database diagram
  # ---------------------------------------------------------------------------

  diagram_data <- reactive({
    selected <- selection()
    scope <- input$diagram_scope %||% "selected"

    tables <- load_diagram_tables(selected$schema, "")
    relationships <- load_diagram_relationships(selected$schema, "")

    if (!isTRUE(input$diagram_show_lookup)) {
      is_lookup <- startsWith(
        as.character(tables$table_name),
        "lku_"
      ) %in%
        TRUE

      tables <- tables[!is_lookup, , drop = FALSE]
    }

    visible_tables <- tables$table_name

    relationships <- relationships[
      relationships$table_name_1 %in%
        visible_tables &
        relationships$table_name_2 %in% visible_tables,
      ,
      drop = FALSE
    ]

    if (identical(scope, "selected")) {
      validate(need(
        nzchar(selected$table),
        "Select a table in the sidebar or choose Whole schema."
      ))

      validate(need(
        selected$table %in% visible_tables,
        paste(
          "The selected table is hidden.",
          "Enable Show lookup tables or select another table."
        )
      ))

      hop1 <- unique(c(
        selected$table,
        relationships$table_name_1[
          relationships$table_name_2 %in% selected$table
        ],
        relationships$table_name_2[
          relationships$table_name_1 %in% selected$table
        ]
      ))

      hop2 <- unique(c(
        hop1,
        relationships$table_name_1[
          relationships$table_name_2 %in% hop1
        ],
        relationships$table_name_2[
          relationships$table_name_1 %in% hop1
        ]
      ))

      tables <- tables[
        tables$table_name %in% hop2,
        ,
        drop = FALSE
      ]

      relationships <- relationships[
        relationships$table_name_1 %in%
          tables$table_name &
          relationships$table_name_2 %in% tables$table_name,
        ,
        drop = FALSE
      ]
    }

    list(
      tables = tables,
      relationships = relationships,
      selected_table = if (identical(scope, "selected")) {
        selected$table
      } else {
        ""
      }
    )
  })

  output$database_diagram <- visNetwork::renderVisNetwork({
    data <- diagram_data()

    build_database_diagram(
      tables = data$tables,
      relationships = data$relationships,
      selected_table = data$selected_table
    )
  })

  # ---------------------------------------------------------------------------
  # Metadata dashboard
  # ---------------------------------------------------------------------------

  dash_last_refresh <- reactiveVal(NULL)
  dash_cache <- reactiveVal(NULL)

  refresh_dashboard_data <- function() {
    tables <- load_dashboard_tables()
    fields <- load_dashboard_fields()
    rels <- load_dashboard_relationships()

    tables <- filter_deprecated(tables, input$dash_include_deprecated)
    fields <- filter_deprecated(fields, input$dash_include_deprecated)
    rels <- filter_deprecated(rels, input$dash_include_deprecated)

    dash_cache(list(tables = tables, fields = fields, rels = rels))
    dash_last_refresh(Sys.time())
  }

  observeEvent(
    input$dash_refresh,
    {
      refresh_dashboard_data()
    },
    ignoreInit = TRUE
  )

  observe({
    req(is.null(dash_cache()))
    refresh_dashboard_data()
  })

  dash_data <- reactive({
    d <- dash_cache()
    req(d)
    d
  })

  dash_attention_data <- reactive({
    d <- dash_data()
    issue <- input$dash_issue_filter %||% "Missing field description"

    if (identical(issue, "Missing table description")) {
      idx <- !has_text(d$tables$table_description)
      x <- data.frame(
        Schema = as.character(d$tables$schema_name[idx]),
        Table = as.character(d$tables$table_name[idx]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else if (identical(issue, "Missing field description")) {
      idx <- !has_text(d$fields$field_description)
      x <- data.frame(
        Schema = as.character(d$fields$schema_name[idx]),
        Table = as.character(d$fields$table_name[idx]),
        Field = as.character(d$fields$field_name[idx]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else if (identical(issue, "Missing how_populated")) {
      idx <- !has_text(d$fields$how_populated)
      x <- data.frame(
        Schema = as.character(d$fields$schema_name[idx]),
        Table = as.character(d$fields$table_name[idx]),
        Field = as.character(d$fields$field_name[idx]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      idx <- !has_text(d$rels$join_type_1to2)
      x <- data.frame(
        Schema = as.character(d$rels$schema_name[idx]),
        Table1 = as.character(d$rels$table_name_1[idx]),
        Field1 = as.character(d$rels$field_name_1[idx]),
        Table2 = as.character(d$rels$table_name_2[idx]),
        Field2 = as.character(d$rels$field_name_2[idx]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    rownames(x) <- NULL
    x
  })

  output$dash_last_refresh <- renderText({
    ts <- dash_last_refresh()
    if (is.null(ts)) {
      return("Not refreshed yet")
    }
    format(ts, "%Y-%m-%d %H:%M:%S")
  })

  output$dash_active_tables <- renderText({
    nrow(dash_data()$tables)
  })

  output$dash_tables_documented <- renderText({
    t <- dash_data()$tables
    coverage_text(sum(has_text(t$table_description)), nrow(t))
  })

  output$dash_fields_documented <- renderText({
    f <- dash_data()$fields
    coverage_text(sum(has_text(f$field_description)), nrow(f))
  })

  output$dash_rels_review <- renderText({
    r <- dash_data()$rels
    sum(!has_text(r$join_type_1to2))
  })

  output$dash_schema_summary <- renderTable(
    {
      d <- dash_data()

      schemas <- sort(unique(c(
        d$tables$schema_name,
        d$fields$schema_name,
        d$rels$schema_name
      )))
      schemas <- schemas[!is.na(schemas) & nzchar(schemas)]

      out <- lapply(schemas, function(s) {
        t <- d$tables[d$tables$schema_name == s, , drop = FALSE]
        f <- d$fields[d$fields$schema_name == s, , drop = FALSE]
        r <- d$rels[d$rels$schema_name == s, , drop = FALSE]

        data.frame(
          Schema = s,
          ActiveTables = nrow(t),
          TableDescriptions = coverage_text(
            sum(has_text(t$table_description)),
            nrow(t)
          ),
          ActiveFields = nrow(f),
          FieldDescriptions = coverage_text(
            sum(has_text(f$field_description)),
            nrow(f)
          ),
          ActiveForeignKeys = nrow(r),
          stringsAsFactors = FALSE
        )
      })

      if (!length(out)) {
        return(data.frame(
          Schema = character(0),
          ActiveTables = integer(0),
          TableDescriptions = character(0),
          ActiveFields = integer(0),
          FieldDescriptions = character(0),
          ActiveForeignKeys = integer(0),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, out)
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "xs"
  )

  output$dash_attention_count <- renderText({
    total <- nrow(dash_attention_data())
    shown <- min(25L, total)
    paste0(
      "Showing ",
      format(shown, big.mark = ","),
      " of ",
      format(total, big.mark = ",")
    )
  })

  output$dash_attention_table <- DT::renderDT(
    {
      x <- dash_attention_data()
      issue <- input$dash_issue_filter %||% "Missing field description"

      ord <- if (identical(issue, "Missing table description")) {
        list(list(0, "asc"), list(1, "asc"))
      } else if (
        issue %in% c("Missing field description", "Missing how_populated")
      ) {
        list(list(0, "asc"), list(1, "asc"), list(2, "asc"))
      } else {
        list(
          list(0, "asc"),
          list(1, "asc"),
          list(2, "asc"),
          list(3, "asc"),
          list(4, "asc")
        )
      }

      DT::datatable(
        x,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        class = "stripe hover compact",
        options = list(
          pageLength = 15,
          lengthMenu = c(15, 25, 50, 100),
          order = ord,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    },
    server = TRUE
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      selected <- selection()
      safe_name <- function(x) {
        gsub("[^[:alnum:]_.-]", "_", x)
      }

      paste0(
        safe_name(selected$schema),
        "_",
        safe_name(if (nzchar(selected$table)) selected$table else "all_tables"),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      selected <- selection()

      if (!nzchar(selected$table)) {
        stop("Select a specific table in the sidebar before downloading.")
      }

      download_table_csv(
        selected$schema,
        selected$table,
        input$download_rows,
        file
      )
    }
  )
}
