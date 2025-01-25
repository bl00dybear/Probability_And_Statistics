
# here we make the server functions imports
source("server/poisson.R")
source("server/binom.R")

# main server function
server <- function(input, output, session) {

  # render pois server
  pois_server(input, output, session)

  # render binom server
  binom_server(input, output, session)

}