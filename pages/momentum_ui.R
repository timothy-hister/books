page_momentum_ui <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  master_data <- args$master_data
  if (is.null(master_data) || nrow(master_data) == 0) {
    target_val <- Sys.Date()
    max_slider <- 14
    default_days <- 7
  } else {
    available_dates <- sort(unique(data$date_pulled))
    target_val <- max(available_dates)
    num_days <- length(available_dates)
    default_days <- min(7, num_days - 1)
    max_slider <- max(1, min(14, num_days - 1)) # Ensure max is at least 1
  }
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      dateInput(ns("target_date"), "Target Date:", value = target_val),
      sliderInput(ns("lookback_days"), "Days to Look Back:", min = 1, max = max_slider, value = default_days, step = 1),
      sliderInput(
        ns("rank_bracket"), 
        "Rank Bracket:", 
        min = 1, 
        max = 200,      # Or max(available_ranks) dynamically
        value = c(50, 200), 
        step = 5
      ),
      
      hr(),
      
      # Dynamic select dropdown populated on server initialization
      selectizeInput(
        ns("genre_filter"), 
        "Select Genres:", 
        choices = sort(unique(master_data$genre)), 
        multiple = TRUE,
        options = list(placeholder = "All Genres")
      ),
      
      # Filter flags
      checkboxInput(ns("canadian_only"), "🍁 Canadian Authors Only", value = FALSE),
      checkboxInput(ns("juvenile_only"), "👶 Children / Juvenile Only", value = FALSE)
    ),
    
    # Adaptive layout wraps
    layout_column_wrap(
      width = 1,
      
      # Interactive ggiraph plotting canvas
      card(
        full_screen = TRUE, # bslib full-screen utility
        card_header("The Momentum Radar"),
        card_body(
          girafeOutput(ns("radar_plot"), width = "100%", height = "400px")
        )
      ),
      
      # Elegant reactable scouting ledger
      card(
        full_screen = TRUE,
        card_header("Scouting Ledger"),
        card_body(
          reactableOutput(ns("ledger_table"))
        )
      )
    )
  )
}