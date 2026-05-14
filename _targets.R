# Inside _targets.R
library(targets)
library(dplyr)
library(readr)

# Set target options:
tar_option_set(
  packages = c("dplyr", "rvest", "readr", "purrr", "stringr", "chromote")
)

# Source your function:
source("R/functions.R")

list(
  tar_target(
    book_urls,
    {
      offsets <- seq(0, 1300, by = 26)
      paste0("https://bookmanager.com/browse/filter/o/", offsets, "/l/26")
    }
  ),
  
  tar_target(
    raw_data,
    {
      data <- map_df(book_urls, scrape_bookmanager)
      
      # HARD BREAK: If we got nothing at all
      stopifnot(nrow(data) > 0)
      
      # HARD BREAK: If more than 20% of ISBNs are missing (Total Scraper Failure)
      isbn_missing_pct <- mean(is.na(data$isbn))
      if(isbn_missing_pct > 0.20) stop("Scraper Failure: Too many missing ISBNs")
      
      data
    }
  ),
  
  tar_target(
    save_snapshot,
    {
      file_path <- "data/book_data_master.csv"
      
      if (file.exists(file_path)) {
        old_data <- read_csv(file_path)
        # Combine and keep only unique day/isbn combos
        final_data <- bind_rows(old_data, raw_data) %>%
          distinct(date_pulled, uid, .keep_all = TRUE)
      } else {
        final_data <- raw_data
      }
      
      write_csv(final_data, file_path)
      file_path # Return the path so targets tracks the file
    },
    format = "file"
  )
)