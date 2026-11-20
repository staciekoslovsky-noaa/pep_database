fluidPage(
  titlePanel("PEP Database Dictionary"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        "schema",
        "Schema",
        choices = character(0)
      ),

      selectInput(
        "table",
        "Table",
        choices = c("All tables" = ""),
        selected = "",
        selectize = FALSE
      ),

      checkboxInput(
        "show_deprecated",
        "Show deprecated rows in browser tabs",
        value = FALSE
      ),

      actionButton(
        "refresh",
        "Refresh from database"
      ),

      hr(),

      helpText(
        "Sidebar selections control the browser tabs. ",
        "Editors keep their loaded selection until you load again."
      )
    ),

    mainPanel(
      width = 10,
      textOutput("status"),
      br(),

      tabsetPanel(
        tabPanel(
          "Metadata Dashboard",
          br(),

          fluidRow(
            column(
              3,
              checkboxInput(
                "dash_include_deprecated",
                "Include deprecated",
                value = FALSE
              )
            ),
            column(
              3,
              actionButton("dash_refresh", "Refresh")
            ),
            column(
              6,
              strong("Last refresh: "),
              textOutput("dash_last_refresh", inline = TRUE)
            )
          ),

          hr(),

          fluidRow(
            column(
              3,
              wellPanel(
                h4("Active tables"),
                h3(textOutput("dash_active_tables"))
              )
            ),
            column(
              3,
              wellPanel(
                h4("Tables documented"),
                h3(textOutput("dash_tables_documented"))
              )
            ),
            column(
              3,
              wellPanel(
                h4("Fields documented"),
                h3(textOutput("dash_fields_documented"))
              )
            ),
            column(
              3,
              wellPanel(
                h4("Relationships to review"),
                h3(textOutput("dash_rels_review"))
              )
            )
          ),

          hr(),

          h4("Schema comparison"),
          tableOutput("dash_schema_summary"),

          hr(),

          h4("Needs attention"),
          selectInput(
            "dash_issue_filter",
            "Filter",
            choices = c(
              "Missing table description",
              "Missing field description",
              "Missing how_populated",
              "Relationship missing join_type"
            ),
            selected = "Missing field description"
          ),
          textOutput("dash_attention_count"),
          DT::DTOutput("dash_attention_table")
        ),
        tabPanel(
          "Tables",
          br(),
          DT::DTOutput("tables")
        ),

        tabPanel(
          "Fields",
          br(),
          DT::DTOutput("fields")
        ),

        tabPanel(
          "Relationships",
          br(),
          DT::DTOutput("relationships")
        ),

        tabPanel(
          "Database Diagram",
          br(),

          fluidRow(
            column(
              4,
              radioButtons(
                "diagram_scope",
                "Diagram view",
                choices = c(
                  "Selected table" = "selected",
                  "Whole schema" = "schema"
                ),
                selected = "selected",
                inline = TRUE
              )
            ),
            column(
              8,
              helpText(
                "Selected table shows the active table and its neighbors. ",
                br(),
                "Whole schema shows all tables and relationships in the selected schema."
              )
            )
          ),

          checkboxInput(
            "diagram_show_lookup",
            "Show lookup tables",
            value = TRUE
          ),

          # Full-width diagram
          fluidRow(
            column(
              12,
              visNetwork::visNetworkOutput("database_diagram", height = "760px")
            )
          )
        ),

        tabPanel(
          "Edit Table Descriptions",
          br(),

          helpText(
            "Load all existing table-description records for the selected schema. ",
            "The sidebar Table selection is ignored. ",
            "Changing the sidebar does not replace loaded edits."
          ),

          actionButton(
            "load_table_grid",
            "Load schema descriptions"
          ),

          actionButton(
            "save_table_grid",
            "Save all changes",
            class = "btn-primary"
          ),

          actionButton(
            "reset_table_grid",
            "Discard unsaved changes"
          ),

          br(),
          br(),

          textOutput("table_grid_target"),
          textOutput("table_grid_changes"),

          helpText(
            "Edit descriptions or paste a rectangular range of text. ",
            "Record identifiers are read-only. ",
            "Finish the last cell edit with Enter before saving."
          ),

          helpText(
            "Clearing a populated description saves an empty string. ",
            "Untouched NULL descriptions remain NULL. ",
            "Discard restores the loaded snapshot, not a fresh database read."
          ),

          uiOutput("table_grid_container")
        ),

        tabPanel(
          "Edit Fields",
          br(),

          helpText(
            "Load existing field records for the selected table, ",
            "or all tables in the selected schema."
          ),

          actionButton(
            "load_field_grid",
            "Load field records"
          ),

          actionButton(
            "save_field_grid",
            "Save all changes",
            class = "btn-primary"
          ),

          actionButton(
            "reset_field_grid",
            "Discard unsaved changes"
          ),

          br(),
          br(),

          textOutput("field_grid_target"),
          textOutput("field_grid_changes"),

          helpText(
            "Edit cells or paste a rectangular range of text. ",
            "Finish the last cell edit with Enter before saving."
          ),

          # helpText(
          #   "how_populated shows lookup descriptions; ",
          #   "the matching code is saved automatically. ",
          #   "A blank choice saves NULL. ",
          #   "deprecated and record identifiers are read-only."
          # ),

          helpText(
            "Clearing populated text saves an empty string. ",
            "Untouched NULL text remains NULL. ",
            "Discard restores the loaded snapshot, not a fresh DB read."
          ),

          uiOutput("field_grid_container")
        ),
        tabPanel(
          "Download Data",
          br(),
          helpText(
            "Choose a schema and table using the sidebar. ",
            "Select all rows or a sample of up to 5000 rows. ",
            "Large-table samples are approximate; downloading all rows may take longer."
          ),
          radioButtons(
            "download_rows",
            "Rows to download",
            choices = c(
              "Sample (up to 5000)" = "sample",
              "All rows" = "all"
            ),
            selected = "sample"
          ),
          downloadButton("download_csv", "Download CSV")
        )
      )
    )
  )
)
