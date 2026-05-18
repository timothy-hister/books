library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

scrape_bookmanager <- function(url = "https://bookmanager.com/browse") {
  Sys.sleep(1)
  
  # 1. Set the timeout option globally for chromote
  options(chromote.timeout = 30)
  
  # 2. Set the command-line arguments via an environment variable BEFORE chromote starts
  Sys.setenv(
    CHROMOTE_CHROME_ARGS = "--headless --disable-gpu --no-sandbox --disable-dev-shm-usage"
  )
  
  # 3. Start the session normally
  b <- ChromoteSession$new()
  
  b$Page$navigate(url)
  Sys.sleep(2.5)
  
  html_string <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  b$close()
  
  doc = read_html(html_string)
  books <- doc %>% html_nodes("div.summary")
  if (length(books) == 0) return(tibble())
  
  data <- books %>% map_df(~{
    # --- 1. TITLE CLEANING ---
    main_title <- .x %>% html_node(".summary-title a") %>% html_text(trim = TRUE)
    full_title_block <- .x %>% html_node(".summary-title") %>% html_text(trim = TRUE)
    
    clean_title <- if (!is.na(main_title) && main_title != full_title_block) {
      subtitle <- str_replace(full_title_block, fixed(main_title), "")
      paste(main_title, str_trim(subtitle), sep = ": ")
    } else {
      full_title_block
    }
    clean_title <- clean_title %>% str_replace_all("\\n", " ") %>% str_squish()
    
    # --- 2. AUTHOR & PATRIOTISM ---
    raw_author <- .x %>% html_node("span[style*='line-height: 1.2em']") %>% html_text(trim = TRUE)
    is_canadian <- str_detect(raw_author, "(?i)maple leaf|flag of canada")
    
    clean_author <- raw_author %>%
      str_replace("(?i)^by\\s+", "") %>%
      str_replace_all("(?i)maple leaf from the flag of canada", "") %>%
      str_replace_all(",\\s*$", "") %>% 
      str_squish()
    
    # --- 3. ISBN ---
    raw_isbn <- .x %>% html_text() %>% str_extract("978[0-9xX]{7,10}")
    
    # Only keep the pure, raw scraped data fields
    tibble(
      title = clean_title,
      author = clean_author,
      is_canadian = is_canadian,
      genre = .x %>% html_node(".c") %>% html_text(trim = TRUE),
      isbn = raw_isbn,
      date_pulled = Sys.Date()
    )
  })
  
  return(data)
}