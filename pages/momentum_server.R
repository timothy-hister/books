page_momentum_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive Reader for our scraped master CSV
  master_data <- reactive({
    req(file.exists("data/book_data_master.csv"))
    read_csv("data/book_data_master.csv", show_col_types = FALSE)
  })
  
  # Dynamically populate genres dropdown on setup
  observe({
    data <- master_data()
    req(nrow(data) > 0)
    
    unique_genres <- data %>% 
      filter(!is.na(genre)) %>% 
      pull(genre) %>% 
      unique() %>% 
      sort()
    
    updateSelectizeInput(
      session, 
      "genre_filter", 
      choices = unique_genres, 
      server = TRUE
    )
  })
  
  # CORE TRANSFORMATION: Align baseline vs comparison, calculate deltas on the fly
  momentum_data <- reactive({
    data <- master_data()
    req(nrow(data) > 0)
    
    baseline <- input$baseline_date
    comparison <- input$comparison_date
    
    # 1. Calculate ranks dynamically for both selected dates based on row order
    ranked_baseline <- data %>%
      filter(date_pulled == baseline) %>%
      mutate(baseline_rank = row_number()) %>%
      select(title, author, genre, isbn, is_canadian, baseline_rank)
    
    ranked_comparison <- data %>%
      filter(date_pulled == comparison) %>%
      mutate(comparison_rank = row_number()) %>%
      select(title, author, genre, isbn, is_canadian, comparison_rank)
    
    # 2. Join sets to trace individual journeys
    joined <- ranked_comparison %>%
      left_join(ranked_baseline, by = c("title", "author", "genre", "isbn", "is_canadian")) %>%
      # Handle brand new entries gracefully
      mutate(
        baseline_rank = ifelse(is.na(baseline_rank), 999, baseline_rank),
        # Delta: Positive means the rank improved (e.g., moved from rank 100 -> rank 20)
        rank_delta = baseline_rank - comparison_rank,
        is_new_entry = (baseline_rank == 999)
      )
    
    # 3. Apply user-selected filters
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      joined <- joined %>% filter(genre %in% input$genre_filter)
    }
    
    if (input$canadian_only) {
      joined <- joined %>% filter(is_canadian == TRUE)
    }
    
    if (input$juvenile_only) {
      joined <- joined %>% filter(str_detect(tolower(genre), "juvenile|children|ya|young adult"))
    }
    
    # SILENT INTERN DEBUGGER: Export state locally (and silently!) for live debugging
    saveRDS(joined, "dev/dev_reactives/momentum_reactive.rds")
    
    joined
  })
  
  # RENDER INTERACTIVE RADAR PLOT (ggiraph)
  output$radar_plot <- renderGirafe({
    df <- momentum_data()
    req(nrow(df) > 0)
    
    # Highlight the Top 5 Momentum Risers (Hidden Gems!)
    top_gems <- df %>%
      filter(!is_new_entry) %>%
      arrange(desc(rank_delta)) %>%
      slice_head(n = 5) %>%
      pull(title)
    
    plot_df <- df %>%
      mutate(
        highlight = ifelse(title %in% top_gems, "Top Riser 💍", "Standard"),
        tooltip_text = paste0(
          "<b>", title, "</b><br/>",
          "Author: ", author, "<br/>",
          "Current Rank: #", comparison_rank, "<br/>",
          "Rank Shift: ", ifelse(is_new_entry, "New Entry! ✨", paste0("+", rank_delta, " spots"))
        )
      )
    
    gg <- ggplot(plot_df, aes(x = comparison_rank, y = rank_delta)) +
      geom_point_interactive(
        aes(
          tooltip = tooltip_text,
          data_id = title,
          color = highlight,
          size = highlight
        ),
        alpha = 0.85
      ) +
      scale_x_reverse(limits = c(max(plot_df$comparison_rank), 1)) + # Put #1 bestseller on the right
      scale_color_manual(values = c("Standard" = "#808f9d", "Top Riser 💍" = "#d95f02")) +
      scale_size_manual(values = c("Standard" = 2.5, "Top Riser 💍" = 5)) +
      labs(
        title = "Rank Position vs Rank Momentum",
        x = "Comparison Date Rank (Position #1 is on the far right)",
        y = "Momentum (Spots climbed since baseline)"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14)
      )
    
    girafe(
      ggobj = gg,
      width_svg = 7,
      height_svg = 4,
      options = list(
        opts_hover(css = "fill:#2ecc71;stroke:#27ae60;cursor:pointer;"),
        opts_selection(type = "single"),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
  
  # RENDER HIGH-END SORTABLE LEDGER (reactable)
  output$ledger_table <- renderReactable({
    df <- momentum_data()
    req(nrow(df) > 0)
    
    table_df <- df %>%
      select(comparison_rank, baseline_rank, rank_delta, title, author, genre, is_canadian) %>%
      mutate(
        status = case_when(
          is_canadian & rank_delta > 15 ~ "🇨🇦 🔥",
          is_canadian ~ "🇨🇦",
          rank_delta > 15 ~ "🔥",
          baseline_rank == 999 ~ "✨",
          TRUE ~ ""
        )
      ) %>%
      arrange(comparison_rank)
    
    reactable(
      table_df,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      columns = list(
        comparison_rank = colDef(name = "Current Rank", width = 110, align = "center"),
        baseline_rank = colDef(name = "Prior Rank", width = 110, align = "center", cell = function(value) {
          if (value == 999) "—" else value
        }),
        rank_delta = colDef(name = "Delta", width = 100, align = "center", cell = function(value) {
          if (value > 0) paste0("+", value) else value
        }, style = function(value) {
          color <- if (value > 0) "#27ae60" else if (value < 0) "#c0392b" else "#7f8c8d"
          list(color = color, fontWeight = "bold")
        }),
        title = colDef(name = "Book Title", minWidth = 200),
        author = colDef(name = "Author", minWidth = 150),
        genre = colDef(name = "Genre", minWidth = 120),
        status = colDef(name = "Status", width = 90, align = "center"),
        is_canadian = colDef(show = FALSE)
      )
    )
  })
}