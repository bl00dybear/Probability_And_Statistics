library(shiny)
library(bslib)
library(ggplot2)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)