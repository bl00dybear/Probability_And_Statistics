# app.R
source("utils.R")
source("ui.R")
source("server/server.R")


shinyApp(ui = ui, server = server)