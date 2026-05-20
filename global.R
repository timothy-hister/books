library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(reactable)
library(stringr)
library(bsicons)
library(shinycssloaders)

# Global UI Tooltip Helper
with_help <- function(label_text, help_text) {
  tags$span(
    label_text,
    " ",
    bslib::tooltip(
      bsicons::bs_icon("info-circle", class = "text-muted", style = "font-size: 0.9em; cursor: pointer;"),
      help_text,
      placement = "top"
    )
  )
}