library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

scrape_bookmanager <- function(url = "https://bookmanager.com/browse") {
  # 1. Start a headless browser session
  b <- ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(2.5) 
  
  doc <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value %>%
    read_html()
  b$parent$close()
  
  books <- doc %>% html_nodes("div.summary")
  if (length(books) == 0) return(tibble())
  
  data <- books %>% map_df(~{
    # --- 1. TITLE CLEANING ---
    # Grab the main title and the subtitle separately to prevent squashing
    main_title <- .x %>% html_node(".summary-title a") %>% html_text(trim = TRUE)
    full_title_block <- .x %>% html_node(".summary-title") %>% html_text(trim = TRUE)
    
    # If "A Novel" is stuck to the end, ensure a space exists
    clean_title <- if (!is.na(main_title) && main_title != full_title_block) {
      subtitle <- str_replace(full_title_block, fixed(main_title), "")
      paste(main_title, str_trim(subtitle), sep = ": ")
    } else {
      full_title_block
    }
    # Final scrub of newlines and extra spaces
    clean_title <- clean_title %>% str_replace_all("\\n", " ") %>% str_squish()
    
    # --- 2. AUTHOR & PATRIOTISM ---
    raw_author <- .x %>% html_node("span[style*='line-height: 1.2em']") %>% html_text(trim = TRUE)
    
    # Flag the Maple Leaf nonsense
    is_canadian <- str_detect(raw_author, "(?i)maple leaf|flag of canada")
    
    # Scrub the patriotic text and the "by" prefix
    clean_author <- raw_author %>%
      str_replace("(?i)^by\\s+", "") %>%
      str_replace_all("(?i)maple leaf from the flag of canada", "") %>%
      str_replace_all(",\\s*$", "") %>% 
      str_squish()
    
    # --- 3. ISBN & UID ---
    raw_isbn <- .x %>% html_text() %>% str_extract("978[0-9xX]{7,10}")
    safe_isbn <- ifelse(is.na(raw_isbn), "noisbn", raw_isbn)
    
    tibble(
      title = clean_title,
      author = clean_author,
      is_canadian = is_canadian,
      rank_raw = .x %>% html_node("span[style*='rgb(181, 44, 44)']") %>% html_text(trim = TRUE),
      genre = .x %>% html_node(".c") %>% html_text(trim = TRUE),
      isbn = raw_isbn,
      date_pulled = Sys.Date()
    ) %>%
      mutate(uid = paste(safe_isbn, title, author, sep = "_") %>% 
               str_to_lower() %>% 
               str_replace_all("[^a-z0-9]", "_") %>%
               str_trunc(80, ellipsis = ""))
  })
  
  if ("rank_raw" %in% names(data)) {
    data <- data %>%
      mutate(rank = as.integer(str_extract(rank_raw, "\\d+"))) %>%
      select(-rank_raw)
  }
  
  return(data)
}