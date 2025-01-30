library(shiny)
library(bslib)

source("utils.R")
source("ui.R")
source("server/server.R")

shinyApp(ui = ui, server = server)
