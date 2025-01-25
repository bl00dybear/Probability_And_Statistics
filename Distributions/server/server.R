
# here we make the server functions imports
source("server/poisson.R")

# main server function
server <- function(input, output, session) {

  # render pois server
  pois_server(input, output, session)

}