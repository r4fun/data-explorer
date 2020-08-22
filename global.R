library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(waiter)
library(reactable)
library(dplyr)
library(shinycssloaders)
library(openxlsx)
library(sever)

utils <- list.files("R", full.names = TRUE)
invisible(lapply(utils, source))

disconnected <- sever_default(
  title = "😭",
  subtitle = p("Your session ended!", br("This can happen if you stepped away for awhile or if the application crashed.")),
  button = "Reconnect",
  button_class = "info"
)

config <- list(
  data_choices = list.files("data")
)
