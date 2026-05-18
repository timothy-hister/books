# Standalone Test Harness for Momentum Page (Bypasses security and outer layout)
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(reactable)
library(stringr)

# Direct source
source("pages/momentum_ui.R")
source("pages/momentum_server.R")

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  h2("Standalone Page Testing Environment", class = "mt-3 mb-1 text-center text-secondary"),
  p("Simulates production environment without login wrapper.", class = "text-muted text-center mb-4"),
  page_momentum_ui("standalone_test")
)

server <- function(input, output, session) {
  # Call Page Module directly
  callModule(page_momentum_server, "standalone_test")
}

shinyApp(ui, server)