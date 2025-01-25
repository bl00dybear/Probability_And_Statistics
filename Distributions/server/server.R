# here we make the server functions imports
source("server/poisson.R")
source("server/std_normal.R")

# main server function
server <- function(input, output, session) {
  # render servers
  std_normal_server(input, output, session)
  pois_server(input, output, session)
}