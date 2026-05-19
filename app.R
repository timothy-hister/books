library(shiny)
library(bslib)
library(dplyr)
library(readr)

debug = F

# Source global configurations and load libraries
source("global.R")

# Source page modules
if (file.exists("pages/momentum_ui.R")) source("pages/momentum_ui.R")
if (file.exists("pages/momentum_server.R")) source("pages/momentum_server.R")

# Check if the app is currently running on shinyapps.io
is_deployed <- nzchar(Sys.getenv("SHINYAPPS_APP_NAME"))

APP_PASSWORD <- Sys.getenv("APP_PASSWORD")
DATA_PATH <- Sys.getenv("DATA_PATH")

master_data = read_csv(DATA_PATH) |>
  group_by(date_pulled) %>%
  mutate(scraped_rank = row_number()) %>%
  ungroup() %>%
  mutate(
    safe_isbn = ifelse(is.na(isbn), "noisbn", isbn),
    uid = paste(safe_isbn, title, author, sep = "_") %>% 
      str_to_lower() %>% 
      str_replace_all("[^a-z0-9]", "_") %>%
      str_trunc(80, ellipsis = "")
  ) %>%
  select(-safe_isbn)

# 1. MAIN UI
main_ui <- page_navbar(
  title = "Canadian Book Momentum",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty" # A soft, elegant bookish color scheme
  ),
  
  # Page 1 Module
  nav_panel(
    title = "Momentum Radar",
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

# 2. LOGIN UI
login_ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(
    style = "max-width: 400px; margin: 100px auto; padding: 20px;",
    card(
      card_header(class = "bg-primary text-white text-center", "Secure Access"),
      card_body(
        passwordInput("password_entry", "Enter Access Key:", placeholder = "Password"),
        actionButton("login_btn", "Log In", class = "btn-primary w-100"),
        uiOutput("login_error")
      )
    )
  )
)

# 3. ROUTED UI
ui <- function(request) {
  if (is_deployed) {
    # Force authentication on the cloud
    uiOutput("auth_router")
  } else {
    # Render main dashboard directly if running locally
    main_ui
  }
}

# 4. SERVER
server <- function(input, output, session) {
  
  # Session-level authentication state
  authenticated <- reactiveVal(FALSE)
  
  # Auth screen routing
  output$auth_router <- renderUI({
    if (authenticated()) {
      main_ui
    } else {
      login_ui
    }
  })
  
  # Handle login action
  observeEvent(input$login_btn, {
    if (input$password_entry == APP_PASSWORD) {
      authenticated(TRUE)
    } else {
      output$login_error <- renderUI({
        div(class = "text-danger mt-2 text-center", "Incorrect Password. Please try again.")
      })
    }
  })
  
  # Call Page Modules
  callModule(page_momentum_server, "momentum_page", master_data = master_data)
}

shinyApp(ui, server)