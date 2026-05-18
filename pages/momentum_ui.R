page_momentum_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      
      # Endogenous Dates (User-customizable baseline comparison periods!)
      dateInput(ns("baseline_date"), "Baseline Date:", value = Sys.Date() - 3),
      dateInput(ns("comparison_date"), "Comparison Date:", value = Sys.Date()),
      
      hr(),
      
      # Dynamic select dropdown populated on server initialization
      selectizeInput(
        ns("genre_filter"), 
        "Select Genres:", 
        choices = NULL, 
        multiple = TRUE,
        options = list(placeholder = "All Genres")
      ),
      
      # Filter flags
      checkboxInput(ns("canadian_only"), "Canadian Authors Only", value = FALSE),
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