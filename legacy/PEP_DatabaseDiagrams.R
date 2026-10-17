# PEP Database Diagrams
# S. Koslovsky

# Variables ------------------------------------------------------
schema_of_interest <- 
  'surv_jobss_kamera'
  #'surv_jobss_detections'
  #'administrative'
  #'acoustics'

schema_db <-
  'surv_jobss'

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("dm")

# Run code -------------------------------------------------------

# Update data in DB 
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

# Get tables, foreign keys, and table descriptions from database

foreign_keys <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM database_meta.tbl_foreign_keys") %>%
  filter(schema_erd == schema_of_interest) %>%
  filter(include_in_erd == 'Y')

table_descriptions <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM database_meta.tbl_table_descriptions") %>%
  filter(schema_erd == schema_of_interest) %>%
  filter(include_in_erd == 'Y')

tables <- dm::dm_from_con(con, schema = schema_db) %>% 
  # dm::dm_select_tbl(tables_2diagram$tablename)
  dm::dm_select_tbl(table_descriptions$table_name)

RPostgreSQL::dbDisconnect(con)

# Clean up geo_images_meta tables for better appearance in diagram
if ("geo_images_meta" %in% names(tables) == TRUE) {
  tables <- tables %>%
    dm::dm_select(geo_images_meta, !(starts_with("evt"))) %>%
    dm::dm_select(geo_images_meta, !(starts_with("uv"))) %>%
    dm::dm_select(geo_images_meta, !(starts_with("rgb"))) %>%
    dm::dm_select(geo_images_meta, !(starts_with("ir"))) %>%
    dm::dm_select(geo_images_meta, !(starts_with("ins"))) 
}

# Assign foreign keys to tables dm
for (i in 1:nrow(foreign_keys)) {
  tables <- tables %>%
    dm::dm_add_fk(table = !!foreign_keys$table_name_2[i], columns = !!foreign_keys$field_name_2[i],
                  ref_table = !!foreign_keys$table_name_1[i], ref_columns = !!foreign_keys$field_name_1[i]) 
}

# Assign table descriptions to tables dm
for (i in 1:nrow(table_descriptions)) {
  desc <- rlang::set_names(table_descriptions$table_name[i],
                           table_descriptions$table_description[i])
  tables <- tables %>%
    dm::dm_set_table_description(!!desc)
}

# Create dm for drawing diagram
tables_drawn <- tables %>% 
  dm::dm_set_colors(orange = starts_with("lku"), 
                    darkblue = starts_with("geo"), 
                    darkgreen = starts_with("tbl")) %>%
  dm::dm_draw(view_type = "all",
              rankdir = "TB",
              graph_name = paste0("Database Diagram: ", schema_of_interest, " schema"),
              font_size = c(header = 18L, table_description = 12L, column = 15L))

# Print diagram
tables_drawn
