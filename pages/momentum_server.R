page_momentum_server <- function(input, output, session, ...) {
  ns <- session$ns
  args <- list(...)
  master_data <- args$master_data
  
  # CORE TRANSFORMATION: Linear Regression Velocity Engine
  momentum_data <- reactive({
    req(input$target_date, input$lookback_days)
    data <- master_data
    req(nrow(data) > 0)
    
    # 1. PRE-FILTERING: Apply all data filters at the top
    filtered_data <- data %>%
      filter(date_pulled >= (input$target_date - input$lookback_days) & date_pulled <= input$target_date)
    
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      filtered_data <- filtered_data %>% filter(genre %in% input$genre_filter)
    }
    
    if (input$canadian_only) {
      filtered_data <- filtered_data %>% filter(is_canadian == TRUE)
    }
    
    if (input$juvenile_only) {
      filtered_data <- filtered_data %>% 
        filter(str_detect(tolower(genre), "juvenile|children|ya|young adult"))
    }
    
    # 2. CALCULATE VELOCITY: Run the regression on the filtered set
    # We use scraped_rank (pre-calculated globally) as the y-axis
    # days_from_start as the x-axis
    velocity_data <- filtered_data %>%
      group_by(uid) %>%
      mutate(days_from_start = as.numeric(date_pulled - min(date_pulled))) %>%
      filter(n() >= 2) %>% # Need at least 2 days of data for a slope
      summarise(
        title = first(title),
        author = first(author),
        genre = first(genre),
        is_canadian = first(is_canadian),
        # Current rank is the rank on the target_date
        current_rank = last(scraped_rank[date_pulled == input$target_date]),
        # Multiply slope by -1 because lower rank number = better position
        velocity = -lm(scraped_rank ~ days_from_start)$coefficients["days_from_start"],
        .groups = "drop"
      ) %>%
      filter(!is.na(current_rank)) %>%
      mutate(is_new_entry = FALSE) # New entries with only 1 day were filtered out by n() >= 2
    
    # 3. DEBUG EXPORT
    if (exists("debug") && isTRUE(debug)) {
      saveRDS(velocity_data, "dev/dev_reactives/momentum_reactive.rds")
    }
    
    velocity_data
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