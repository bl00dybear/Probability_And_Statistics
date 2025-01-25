# app.R
source("utils.R")
source("ui.R")
source("server/server.R")

# run on specific port
shinyApp(ui = ui, server = server, options = list(port = 1234))