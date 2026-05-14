library(targets)

# Source your functions
source("R/functions.R")

# Set target options (standard practice)
tar_option_set(packages = c("dplyr", "rvest", "readr"))

# The Pipeline
list(
  tar_target(raw_data, get_book_data()),
  tar_target(save_snapshot, write_csv(raw_data, "book_data_master.csv", append = TRUE))
)