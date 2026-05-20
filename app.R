if (file.exists(".Renviron")) readRenviron(".Renviron")

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(shinymanager)

# Source global configurations and load libraries
source("global.R")

# Source page modules
if (file.exists("pages/momentum_ui.R")) source("pages/momentum_ui.R")
if (file.exists("pages/momentum_server.R")) source("pages/momentum_server.R")

# Check if the app is currently running on shinyapps.io
is_deployed <- nzchar(Sys.getenv("SHINYAPPS_APP_NAME"))

APP_PASSWORD <- Sys.getenv("APP_PASSWORD")
DATA_PATH <- Sys.getenv("DATA_PATH")

master_data = read_csv(DATA_PATH, show_col_types = FALSE)

# ==============================================================================
# 1. SHINYMANAGER CREDENTIALS
# ==============================================================================
# Define the single-user credentials dataframe using the token from your .Renviron
credentials <- data.frame(
  user = "admin",                    # The username required on the web interface
  password = APP_PASSWORD,           # Securely pulled from your environment file
  stringsAsFactors = FALSE
)

# ==============================================================================
# 2. MAIN DASHBOARD UI
# ==============================================================================
main_ui <- page_navbar(
  fillable = "Momentum Radar",
  title = "Book Momentum",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty" # A soft, elegant bookish color scheme
  ),
  
  # Page 1 Module
  nav_panel(
    title = "Momentum Radar",
    value = "Momentum Radar",
    page_momentum_ui("momentum_page")
  ),
  
  # Placeholder for Page 2
  nav_panel(
    title = "Inventory Scout",
    card(
      card_header("Future Module"), 
      "Inventory analysis page coming soon. This card can be maximized to take up the full screen."
    )
  )
)

ui <- secure_app(main_ui)

server <- function(input, output, session) {
  
  # Secure the server side using the environment-injected credentials
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Call Page Modules
  callModule(page_momentum_server, "momentum_page", master_data = master_data)
}

shinyApp(ui, server)