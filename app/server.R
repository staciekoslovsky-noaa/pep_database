function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Shared session state and error reporting
  # ---------------------------------------------------------------------------

  metadata_saved <- reactiveVal(0L)
  editing_table <- reactiveVal(NULL)
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

  clear_table_editor <- function() {
    editing_table(NULL)
    updateTextAreaInput(session, "description", value = "")
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
  # Table-description editor
  # ---------------------------------------------------------------------------

  output$edit_target <- renderText({
    record <- editing_table()

    if (is.null(record)) {
      return("No record loaded.")
    }

    paste0(
      record$schema_name[[1]],
      ".",
      record$table_name[[1]],
      " (metadata ID: ",
      record$id[[1]],
      ")"
    )
  })

  observeEvent(input$load_description, {
    selected <- selection()

    if (!nzchar(selected$table)) {
      showNotification(
        "Select a specific table, not All tables.",
        type = "warning"
      )
      return()
    }

    clear_table_editor()

    tryCatch(
      {
        record <- load_table_description(selected$schema, selected$table)

        if (is.null(record)) {
          showNotification(
            "No existing description record was found.",
            type = "warning"
          )
          return()
        }

        editing_table(record)
        updateTextAreaInput(
          session,
          "description",
          value = display_text(record$table_description[[1]])
        )
      },
      error = function(e) {
        notify_error("Description load failed.", e)
      }
    )
  })

  observeEvent(input$cancel_edit, {
    clear_table_editor()
  })

  observeEvent(input$save_description, {
    original <- editing_table()

    if (is.null(original)) {
      showNotification("Load a table description first.", type = "warning")
      return()
    }

    req(!is.null(input$description))
    description <- input$description

    if (description == "" && is.na(original$table_description[[1]])) {
      description <- NA_character_
    }

    if (same_metadata_value(description, original$table_description[[1]])) {
      showNotification("No changes to save.")
      return()
    }

    tryCatch(
      {
        save_table_description(original, description)
        clear_table_editor()
        metadata_saved(metadata_saved() + 1L)
        showNotification("Table description saved.", type = "message")
      },
      error = function(e) {
        notify_error("Save failed; your text remains in the form.", e)
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Spreadsheet lifecycle
  # ---------------------------------------------------------------------------

  start_field_sheet <- function(original, lookup, target) {
    grid <- make_field_grid(original, lookup)
    previous <- isolate(field_sheet())

    if (!is.null(previous)) {
      output[[previous$id]] <- NULL
    }

    sheet_number <<- sheet_number + 1L
    widget_id <- paste0("field_sheet_", sheet_number)

    output[[widget_id]] <- rhandsontable::renderRHandsontable({
      field_editor_hotable(grid, lookup)
    })

    field_sheet(list(
      id = widget_id,
      original = original,
      lookup = lookup,
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
    decode_field_grid(grid, sheet$original, sheet$lookup)
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
        lookup <- load_how_populated_lookup()

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

        start_field_sheet(original, lookup, target)
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

    start_field_sheet(sheet$original, sheet$lookup, sheet$target)
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

  diagram_selection <- reactiveVal(NULL)

  observeEvent(input$diagram_scope, {
    diagram_selection(NULL)
  })

  diagram_data <- reactive({
    req(input$schema)
    req(length(input$table) == 1L)

    scope <- input$diagram_scope %||% "selected"

    all_tables <- load_diagram_tables(input$schema, "")
    all_relationships <- load_diagram_relationships(input$schema, "")

    if (scope == "schema") {
      return(list(
        tables = all_tables,
        relationships = all_relationships
      ))
    }

    selected_table <- input$table
    req(nzchar(selected_table))

    # 2-hop expansion from selected table
    hop1 <- unique(c(
      selected_table,
      all_relationships$table_name_1[
        all_relationships$table_name_2 == selected_table
      ],
      all_relationships$table_name_2[
        all_relationships$table_name_1 == selected_table
      ]
    ))

    hop2 <- unique(c(
      hop1,
      all_relationships$table_name_1[all_relationships$table_name_2 %in% hop1],
      all_relationships$table_name_2[all_relationships$table_name_1 %in% hop1]
    ))

    tables <- all_tables[all_tables$table_name %in% hop2, , drop = FALSE]
    rels <- all_relationships[
      all_relationships$table_name_1 %in%
        hop2 &
        all_relationships$table_name_2 %in% hop2,
      ,
      drop = FALSE
    ]

    list(tables = tables, relationships = rels)
  })

  output$database_diagram <- visNetwork::renderVisNetwork({
    data <- diagram_data()

    build_database_diagram(
      data$tables,
      data$relationships,
      selected_table = if (identical(input$diagram_scope, "selected")) {
        input$table
      } else {
        ""
      }
    )
  })

  output$diagram_selection_title <- renderText({
    selected <- diagram_selection()
    if (is.null(selected)) {
      return("No diagram node selected.")
    }
    paste0(selected$schema_name, ".", selected$table_name)
  })

  output$diagram_selection_details <- renderText({
    selected <- diagram_selection()
    if (is.null(selected)) {
      return("Click a table in the diagram to see details here.")
    }

    paste(
      "Table:",
      selected$table_name,
      "\nDescription:",
      selected$table_description,
      "\nDeprecated:",
      selected$deprecated
    )
  })

  output$diagram_relationships_panel <- renderUI({
    selected <- diagram_selection()
    if (is.null(selected)) {
      return(helpText("Select a table node to see join relationships."))
    }

    rels <- load_diagram_relationships(input$schema, "")
    related <- rels[
      rels$table_name_1 == selected$table_name |
        rels$table_name_2 == selected$table_name,
      ,
      drop = FALSE
    ]

    if (nrow(related) == 0L) {
      return(helpText("No join relationships found for this table."))
    }

    tags$ul(lapply(seq_len(nrow(related)), function(i) {
      rel <- related[i, , drop = FALSE]
      tags$li(HTML(paste0(
        "<b>",
        rel$table_name_1[[1]],
        ".",
        rel$field_name_1[[1]],
        "</b> → ",
        "<b>",
        rel$table_name_2[[1]],
        ".",
        rel$field_name_2[[1]],
        "</b>",
        "<br/>Join type: ",
        rel$join_type_1to2[[1]]
      )))
    }))
  })

  observeEvent(input$database_diagram_selected, {
    sel <- input$database_diagram_selected

    if (is.null(sel)) {
      diagram_selection(NULL)
      return()
    }

    selected_table_id <- sel[1]
    selected_tables <- load_diagram_tables(input$schema, "")

    match_row <- selected_tables[
      paste0(selected_tables$schema_name, ".", selected_tables$table_name) ==
        selected_table_id,
      ,
      drop = FALSE
    ]

    if (nrow(match_row) == 1L) {
      diagram_selection(match_row)
    }
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
      idx <- !has_text(d$fields$how_populated_lku)
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
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          order = ord,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    },
    server = TRUE
  )
}
