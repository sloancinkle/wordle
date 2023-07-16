# # Uncomment this chunk to install necessary packages.
# install.packages(c("shiny", "shinyjs", "htmltools", "ggplot2",
#                    "fitdistrplus", "gtools", "stringr", "plyr", "purrr"))

# UI & server
library(shiny)
library(shinyjs)
library(htmltools)

# Graphing
library(ggplot2)
library(fitdistrplus)

# Words & hints
library(gtools)
library(stringr)

# Algorithms 
library(plyr)
library(purrr)


source("algorithms.R")
source("mastermind/logic.R")
source("mastermind/ui.R")
source("mastermind/server.R")


shinyApp(ui, server)
