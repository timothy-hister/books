library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(reactable)
library(stringr)

# Ensure local developer debugging directory exists
dir.create("dev/dev_reactives", recursive = TRUE, showWarnings = FALSE)

# Source page modules
if (file.exists("pages/page_momentum_ui.R")) source("pages/page_momentum_ui.R")
if (file.exists("pages/page_momentum_server.R")) source("pages/page_momentum_server.R")