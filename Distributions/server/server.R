source("server/std_normal.R")
source("server/normal.R")
source("server/exponential.R")
source("server/binom.R")
source("server/poisson.R")

server <- function(input, output, session) {
  std_normal_server(input, output, session)
  normal_server(input, output, session)
  exponential_server(input, output, session)
  binom_server(input, output, session)
  pois_server(input, output, session)
}
