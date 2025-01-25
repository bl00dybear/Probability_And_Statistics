# here we make the server functions imports
source("server/poisson.R")
source("server/std_normal.R")
source("server/normal.R")
source("server/exponential.R")

# main server function
server <- function(input, output, session) {
  # render servers
  std_normal_server(input, output, session)
  normal_server(input, output, session)
  exponential_server(input, output, session)
  pois_server(input, output, session)
}