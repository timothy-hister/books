page_momentum_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    fillable = F,
    fill = F,
    sidebar = sidebar(
      title = "Filters",
      
      selectInput(
        ns("target_date"), 
        label = with_help("Target Date:", "The main snapshot date you want to analyze. Only dates with active, verified data scrapes are available."), 
        choices = NULL # Fully populated by the server on boot
      ),
      
      sliderInput(
        ns("lookback_days"), 
        label = with_help("Days to Look Back:", "The historical window to calculate data trends. A longer window yields smoother trends."), 
        min = 1, max = 14, value = 7, step = 1 # Static safe limits; server caps this dynamically based on data availability
      ),
      
      sliderInput(
        ns("rank_bracket"), 
        label = with_help("Rank Bracket:", "Filters books based on their final position. Default (50-200) focuses on the 'mid-list' sweet spot, hiding obvious mega-bestsellers and long-tail noise."), 
        min = 1, max = 200, value = c(50, 200), step = 5 # Standard mid-list baseline bracket
      ),
      
      hr(),
      
      selectizeInput(
        ns("genre_filter"), 
        label = with_help("Select Genres:", "Narrow down the view to specific retail categories. Leave empty to evaluate all genres simultaneously."), 
        choices = NULL, # Completely decoupled from master_data; server handles item list initialization
        multiple = TRUE,
        options = list(placeholder = "All Genres")
      ),
      
      checkboxInput(
        ns("canadian_only"), 
        label = with_help("🍁 Canadian Authors Only", "Self-explanatory"), 
        value = FALSE
      ),
      
      checkboxInput(
        ns("juvenile_only"), 
        label = with_help("👶 Children / Juvenile Only", "Currently uses targeted text matching to isolate children's, juvenile, and Young Adult (YA) titles; we can improve this."), 
        value = FALSE
      )
    ),
    
    # Interactive ggiraph plotting canvas
    card(
      full_screen = TRUE, 
      card_header("The Momentum Radar"),
      card_body(
        fillable = F,
        shinycssloaders::withSpinner(
          girafeOutput(ns("radar_plot"), width = "100%", height = "350px"), # Locked height for visual breathing room
          color = "#2ecc71",
          type = 6
        )
      )
    ),
    
    # Elegant reactable scouting ledger
    card(
      full_screen = TRUE,
      card_header("Scouting Ledger"),
      card_body(
        shinycssloaders::withSpinner(
          reactableOutput(ns("ledger_table")),
          color = "#2ecc71",
          type = 6
        )
      )
    )
  )
}