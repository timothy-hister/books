library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

scrape_bookmanager <- function(url) {
  # 1. Start a headless browser session
  b <- ChromoteSession$new()
  
  # 2. Navigate to the URL
  b$Page$navigate(url)
  
  # 3. Wait for the page to load the JS
  Sys.sleep(2) 
  
  # 4. Pull the rendered HTML from the browser
  doc <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value %>%
    read_html()
  
  # 5. Close the browser session
  b$parent$close()
  
  # Get all book containers
  books <- doc %>% html_nodes("div.summary")
  
  if (length(books) == 0) return(tibble())
  
  data <- books %>% map_df(~{
    # Extract raw text nodes
    title_text  <- .x %>% html_node(".summary-title a") %>% html_text(trim = TRUE)
    author_text <- .x %>% html_node("span[style*='line-height: 1.2em'] a") %>% html_text(trim = TRUE)
    rank_raw    <- .x %>% html_node("span[style*='rgb(181, 44, 44)']") %>% html_text(trim = TRUE)
    genre_text  <- .x %>% html_node(".c") %>% html_text(trim = TRUE)
    
    # BRUTE FORCE ISBN: Look through ALL text in this container for a 13-digit number starting with 978
    # We include [x0-9] to catch those masked 'xxx' digits too
    isbn_text   <- .x %>% html_text() %>% str_extract("978[0-9xX]{7,10}")
    
    tibble(
      title = title_text,
      author = author_text,
      rank_raw = rank_raw,
      genre = genre_text,
      isbn = isbn_text,
      date_pulled = Sys.Date()
    ) %>%
      # Create the Composite Key (uid)
      mutate(uid = paste(isbn, title, author, sep = "_") %>% 
               str_to_lower() %>% 
               str_replace_all("[^a-z0-9]", "_"))
  })
  
  # Clean up the rank
  if ("rank_raw" %in% names(data)) {
    data <- data %>%
      mutate(rank = as.integer(str_extract(rank_raw, "\\d+"))) %>%
      select(-rank_raw)
  }
  
  return(data)
}