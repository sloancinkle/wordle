# # Uncomment this chunk to install necessary packages.
# install.packages(c("rstudioapi", "shiny", "shinyjs", "htmltools", "ggplot2",
#                    "fitdistrplus", "gtools", "stringr", "plyr", "purrr"))

# Accessing data
library(rstudioapi)

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


setwd(dirname(getActiveDocumentContext()$path))


source("algorithms.R")
source("wordle/logic.R")
source("wordle/ui.R")
source("wordle/server.R")


shinyApp(ui, server)
