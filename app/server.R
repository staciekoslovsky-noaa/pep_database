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
      paste(
        prefix,
        "Check the R console/application log for details."
      ),
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

    updateSelectInput(
      session,
      "schema",
      choices = schemas,
      selected = selected
    )
  })

  observe({
    req(input$schema)

    data <- catalog()
    tables <- sort(unique(
      data$table_name[data$schema_name %in% input$schema]
    ))
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
      choices = c(
        "All tables" = "",
        stats::setNames(tables, tables)
      ),
      selected = selected
    )
  })

  selection <- reactive({
    req(input$schema)
    req(length(input$table) == 1L)

    data <- catalog()
    req(input$schema %in% data$schema_name)

    tables <- data$table_name[
      data$schema_name %in% input$schema
    ]

    req(input$table == "" || input$table %in% tables)

    list(schema = input$schema, table = input$table)
  })

  # ---------------------------------------------------------------------------
  # Read-only browser tabs
  # ---------------------------------------------------------------------------

  filtered_tables <- reactive({
    selected <- selection()
    data <- catalog()

    data <- data[
      data$schema_name %in% selected$schema,
      ,
      drop = FALSE
    ]

    if (nzchar(selected$table)) {
      data <- data[
        data$table_name %in% selected$table,
        ,
        drop = FALSE
      ]
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
    dt_widget(filter_deprecated(
      details()$fields,
      input$show_deprecated
    ))
  })

  output$relationships <- DT::renderDT({
    dt_widget(filter_deprecated(
      details()$relationships,
      input$show_deprecated
    ))
  })

  output$status <- renderText({
    selected <- selection()

    paste0(
      "Schema: ",
      selected$schema,
      " | Table: ",
      if (nzchar(selected$table)) selected$table else "(all)",
      " | Details retrieved: ",
      format(
        details()$retrieved_at,
        "%Y-%m-%d %H:%M:%S %Z"
      )
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
        record <- load_table_description(
          selected$schema,
          selected$table
        )

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
      showNotification(
        "Load a table description first.",
        type = "warning"
      )
      return()
    }

    req(!is.null(input$description))
    description <- input$description

    if (
      description == "" &&
        is.na(original$table_description[[1]])
    ) {
      description <- NA_character_
    }

    if (
      same_metadata_value(
        description,
        original$table_description[[1]]
      )
    ) {
      showNotification("No changes to save.")
      return()
    }

    tryCatch(
      {
        save_table_description(original, description)
        clear_table_editor()
        metadata_saved(metadata_saved() + 1L)

        showNotification(
          "Table description saved.",
          type = "message"
        )
      },
      error = function(e) {
        notify_error(
          "Save failed; your text remains in the form.",
          e
        )
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

    # This renderer captures a snapshot. Cell edits do not re-render it.
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

    rhandsontable::rHandsontableOutput(
      sheet$id,
      height = "650px"
    )
  })

  # Use the latest payload from the current spreadsheet.
  current_field_data <- reactive({
    sheet <- field_sheet()

    if (is.null(sheet)) {
      return(NULL)
    }

    payload <- input[[sheet$id]]

    # Before the widget sends its first payload, nothing has changed.
    if (is.null(payload)) {
      return(sheet$original)
    }

    grid <- rhandsontable::hot_to_r(payload)

    decode_field_grid(
      grid,
      sheet$original,
      sheet$lookup
    )
  })

  output$field_grid_target <- renderText({
    sheet <- field_sheet()

    if (is.null(sheet)) {
      return("No field records loaded.")
    }

    paste0(
      "Loaded: ",
      sheet$target,
      " — ",
      nrow(sheet$original),
      " record(s)"
    )
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

  # ---------------------------------------------------------------------------
  # Load and discard
  # ---------------------------------------------------------------------------

  observeEvent(input$load_field_grid, {
    selected <- selection()

    tryCatch(
      {
        current <- field_sheet()

        if (
          !is.null(current) &&
            length(changed_field_rows(
              current$original,
              current_field_data()
            )) >
              0L
        ) {
          showNotification(
            "Save or discard your edits before loading another dataset.",
            type = "warning"
          )
          return()
        }

        original <- load_field_metadata(
          selected$schema,
          selected$table
        )

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
          if (nzchar(selected$table)) {
            selected$table
          } else {
            "(all tables)"
          }
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

    start_field_sheet(
      sheet$original,
      sheet$lookup,
      sheet$target
    )

    showNotification(
      "Unsaved edits discarded.",
      type = "message"
    )
  })

  # ---------------------------------------------------------------------------
  # Save all changed rows
  # ---------------------------------------------------------------------------

  observeEvent(input$save_field_grid, {
    sheet <- field_sheet()

    if (is.null(sheet)) {
      showNotification(
        "Load field records first.",
        type = "warning"
      )
      return()
    }

    tryCatch(
      {
        edited <- current_field_data()

        count <- save_field_metadata(
          sheet$original,
          edited
        )

        if (count == 0L) {
          showNotification("No changes to save.")
          return()
        }

        clear_field_sheet()
        metadata_saved(metadata_saved() + 1L)

        showNotification(
          paste(
            count,
            "field record(s) saved. Load records to continue."
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

    if (scope == "schema") {
      tables <- load_diagram_tables(input$schema, "")
      rels <- load_diagram_relationships(input$schema, "")
      return(list(tables = tables, relationships = rels))
    }

    selected_table <- input$table
    req(nzchar(selected_table))

    all_tables <- load_diagram_tables(input$schema, "")
    rels <- load_diagram_relationships(input$schema, "")

    # 2-hop expansion from the selected table.
    hop1 <- unique(c(
      selected_table,
      rels$table_name_1[rels$table_name_2 == selected_table],
      rels$table_name_2[rels$table_name_1 == selected_table]
    ))

    hop2 <- unique(c(
      hop1,
      rels$table_name_1[rels$table_name_2 %in% hop1],
      rels$table_name_2[rels$table_name_1 %in% hop1]
    ))

    tables <- all_tables[all_tables$table_name %in% hop2, , drop = FALSE]

    rels <- rels[
      rels$table_name_1 %in% hop2 & rels$table_name_2 %in% hop2,
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

    tags$ul(
      lapply(seq_len(nrow(related)), function(i) {
        rel <- related[i, , drop = FALSE]

        tags$li(
          HTML(paste0(
            "<b>",
            rel$table_name_1[[1]],
            ".",
            rel$field_name_1[[1]],
            "</b> ",
            "→ ",
            "<b>",
            rel$table_name_2[[1]],
            ".",
            rel$field_name_2[[1]],
            "</b>",
            "<br/>Join type: ",
            rel$join_type_1to2[[1]]
          ))
        )
      })
    )
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
}
