# ==============================================================================
#   
#   Standalone Module Test Runner: Page 1 (Momentum)
# 
# Run this file directly in RStudio to test this page without launching the main app
# 
# ==============================================================================
  
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)

# Set path relative to the project root

if (basename(getwd()) == "pages") setwd("..")

# 1. Source files individually

source("pages/page_momentum_ui.R")
source("pages/page_momentum_server.R")

# 2. Re-create dummy environment if data folder is missing

if(!dir.exists("data")) dir.create("data")

# 3. Build minimalist standalone wrapper app

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(class = "p-3 bg-secondary text-white text-center mb-3",
      h3("Page 1 (Momentum Module) - Standalone Mode")),
  page_momentum_ui("standalone_test_id")
)

server <- function(input, output, session) {
  callModule(page_momentum_server, "standalone_test_id")
}

# 4. Run the local app

shinyApp(ui, server)