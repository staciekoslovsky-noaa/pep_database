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
        choices = c("All tables" = "")
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
                "Whole schema shows all tables and relationships in the selected schema."
              )
            )
          ),

          fluidRow(
            column(
              9,
              visNetwork::visNetworkOutput("database_diagram", height = "720px")
            ),
            column(
              3,
              wellPanel(
                h4(textOutput("diagram_selection_title")),
                verbatimTextOutput("diagram_selection_details"),
                hr(),
                h4("Join relationships"),
                uiOutput("diagram_relationships_panel")
              )
            )
          )
        ),

        tabPanel(
          "Edit Table Description",
          br(),

          helpText(
            "Select a specific table, then load its description. ",
            "Loading again replaces any unsaved form text."
          ),

          actionButton(
            "load_description",
            "Load selected table"
          ),

          hr(),

          strong("Editing:"),
          textOutput("edit_target"),

          br(),

          textAreaInput(
            "description",
            "Table description",
            value = "",
            rows = 6,
            width = "100%"
          ),

          actionButton(
            "save_description",
            "Save to database",
            class = "btn-primary"
          ),

          actionButton(
            "cancel_edit",
            "Clear form"
          )
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

          helpText(
            "how_populated shows lookup descriptions; ",
            "the matching code is saved automatically. ",
            "A blank choice saves NULL. ",
            "deprecated and record identifiers are read-only."
          ),

          helpText(
            "Clearing populated text saves an empty string. ",
            "Untouched NULL text remains NULL. ",
            "Discard restores the loaded snapshot, not a fresh DB read."
          ),

          uiOutput("field_grid_container")
        )
      )
    )
  )
)
