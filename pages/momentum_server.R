page_momentum_server <- function(input, output, session, ...) {
  ns <- session$ns
  args <- list(...)
  master_data <- args$master_data
  
  # =========================================================================
  # GUARD A: INITIAL SESSION BOOT (Runs once on launch to hydrate the UI)
  # =========================================================================
  observe({
    data <- master_data
    req(nrow(data) > 0)
    
    # Sort decreasing so the newest scrape is comfortably sitting at the top of the list
    available_dates <- sort(unique(data$date_pulled), decreasing = TRUE)
    all_genres <- sort(unique(data$genre))
    
    # SWAPPED: Update select dropdown choices instead of calendar values
    updateSelectInput(
      session, 
      "target_date", 
      choices = as.character(available_dates), 
      selected = as.character(max(available_dates))
    )
    
    # Populate the search choices cleanly on the server side
    updateSelectizeInput(session, "genre_filter", choices = all_genres, server = TRUE)
  })
  
  # =========================================================================
  # GUARD B: CONTEXT CHAPERONE (Fires strictly when target_date shifts)
  # =========================================================================
  observeEvent(input$target_date, {
    data <- master_data
    req(nrow(data) > 0, input$target_date)
    
    # CAST TO DATE: Convert character dropdown string to true R Date object
    target_date_val <- as.Date(input$target_date)
    
    # 1. Calibrate Rank Bracket Max based on current daily capacity
    total_books_today <- nrow(data[data$date_pulled == target_date_val, ])
    slider_max_rank <- if (total_books_today > 0) total_books_today else 200
    
    updateSliderInput(session, "rank_bracket", max = slider_max_rank)
    
    # 2. Calibrate Lookback Runway based on available backward timeline
    available_dates <- sort(unique(data$date_pulled))
    historical_window <- available_dates[available_dates <= target_date_val]
    safe_max_lookback <- min(14, max(1, length(historical_window) - 1))
    
    updateSliderInput(session, "lookback_days", max = safe_max_lookback)
  })
  
  # =========================================================================
  # GUARD C: JUVENILE GENRE FILTER SYNC (Fires when Checkbox changes)
  # =========================================================================
  observeEvent(input$juvenile_only, {
    data <- master_data
    req(nrow(data) > 0)
    
    # Extract the absolute master list of unique categories
    all_genres <- sort(unique(data$genre))
    
    if (input$juvenile_only) {
      # Isolate only categories matching our text pattern
      juvenile_genres <- all_genres[stringr::str_detect(
        tolower(all_genres), 
        "juvenile|children|ya|young adult"
      )]
      
      # Constrain the dropdown choices strictly to the matching subset
      updateSelectizeInput(
        session, 
        "genre_filter", 
        choices = juvenile_genres, 
        server = TRUE
      )
    } else {
      # Reset cleanly back to the comprehensive list of categories
      updateSelectizeInput(
        session, 
        "genre_filter", 
        choices = all_genres, 
        server = TRUE
      )
    }
  }, ignoreInit = TRUE) # Essential: Prevents this from fighting Guard A during initial application boot
  
  # =========================================================================
  # CORE TRANSFORMATION: Linear Regression Velocity Engine
  # =========================================================================
  momentum_data <- reactive({
    req(input$target_date, input$lookback_days, input$rank_bracket)
    data <- master_data
    req(nrow(data) > 0)
    
    # CAST TO DATE: Safeguard text dropdown inputs for downstream date math
    target_date_val <- as.Date(input$target_date)
    
    # 1. PRE-FILTERING: Apply all data filters at the top using target_date_val
    filtered_data <- data %>%
      filter(date_pulled >= (target_date_val - input$lookback_days) & date_pulled <= target_date_val)
    
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
    
    filtered_data <- filtered_data %>% 
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
    
    # 2. CALCULATE VELOCITY: Run the regression on the filtered set
    velocity_data <- filtered_data %>%
      group_by(uid) %>%
      mutate(days_from_start = as.numeric(date_pulled - min(date_pulled))) %>%
      filter(n() >= 2) %>% 
      summarise(
        title = first(title),
        author = first(author),
        genre = first(genre),
        is_canadian = first(is_canadian),
        # FIXED: Evaluates current rank using target_date_val
        current_rank = last(scraped_rank[date_pulled == target_date_val]),
        velocity = round(-lm(scraped_rank ~ days_from_start)$coefficients["days_from_start"], 1),
        .groups = "drop"
      ) %>%
      filter(!is.na(current_rank)) %>%
      filter(current_rank >= input$rank_bracket[1] & current_rank <= input$rank_bracket[2]) %>%
      mutate(is_new_entry = FALSE)
    
    velocity_data
  })  
  
  # =========================================================================
  # RENDER INTERACTIVE RADAR PLOT (ggiraph)
  # =========================================================================
  output$radar_plot <- renderGirafe({
    df <- momentum_data()
    
    # THE ANTI-FREAK-OUT SHIELD: Graceful user warning instead of an empty crash
    validate(
      need(nrow(df) > 0, "No titles match your current filter combination. Try expanding your Rank Bracket or adjusting your selected genres!")
    )
    
    top_accelerators <- df %>%
      arrange(desc(velocity)) %>%
      slice_head(n = 5) %>%
      pull(uid)
    
    plot_df <- df %>%
      mutate(
        highlight = ifelse(uid %in% top_accelerators & velocity > 0, "Top Riser \u26a1", "Standard"),
        tooltip_text = paste0(
          "<b>", title, "</b><br/>",
          "Author: ", author, "<br/>",
          "Current Rank: #", current_rank, "<br/>",
          "Velocity Index: ", ifelse(velocity > 0, paste0("+", velocity), velocity), " ranks/day"
        )
      )
    
    gg <- ggplot(plot_df, aes(x = current_rank, y = velocity)) +
      geom_point_interactive(
        aes(
          tooltip = tooltip_text,
          data_id = uid, 
          color = highlight,
          size = highlight
        ),
        alpha = 0.85
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#bdc3c7") + 
      scale_x_reverse(limits = c(max(plot_df$current_rank), 1)) + 
      scale_color_manual(values = c("Standard" = "#808f9d", "Top Riser \u26a1" = "#d95f02")) +
      scale_size_manual(values = c("Standard" = 2.5, "Top Riser \u26a1" = 5)) +
      labs(
        title = "Current Position vs Steady Trend Velocity",
        x = "Target Date Rank (Position #1 is on the far right)",
        y = "Velocity (Average ranks climbed per day)"
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
  
  # =========================================================================
  # RENDER HIGH-END SORTABLE LEDGER (reactable)
  # =========================================================================
  output$ledger_table <- renderReactable({
    df <- momentum_data()
    
    # THE ANTI-FREAK-OUT SHIELD: Mirror safety pass for the table ledger
    validate(
      need(nrow(df) > 0, "No data available for the current selection.")
    )
    
    table_df <- df %>%
      select(current_rank, velocity, title, author, genre, is_canadian) %>%
      mutate(
        status = case_when(
          is_canadian & velocity > 2 ~ "\ud83c\udf41 \ud83d\udd25",
          is_canadian ~ "\ud83c\udf41",
          velocity > 2 ~ "\ud83d\udd25",
          TRUE ~ ""
        )
      ) %>%
      arrange(desc(velocity)) # Forces high-velocity breakouts directly to the top
    
    reactable(
      table_df,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      defaultSorted = list(velocity = "desc"), # Forces sorting wedge UI styling on load
      columns = list(
        current_rank = colDef(name = "Current Rank", width = 110, align = "center"),
        velocity = colDef(
          name = "Velocity (Ranks/Day)", 
          width = 160, 
          align = "center", 
          cell = function(value) {
            if (value > 0) paste0("+", value) else value
          }, 
          style = function(value) {
            color <- if (value > 0) "#27ae60" else if (value < 0) "#c0392b" else "#7f8c8d"
            list(color = color, fontWeight = "bold")
          }
        ),
        title = colDef(name = "Book Title", minWidth = 200),
        author = colDef(name = "Author", minWidth = 150),
        genre = colDef(name = "Genre", minWidth = 120),
        status = colDef(name = "Status", width = 90, align = "center"),
        is_canadian = colDef(show = FALSE)
      )
    )
  })
}