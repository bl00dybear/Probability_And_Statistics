NORMALA_STANDARD <- "NORMALA_STANDARD"
NORMALA <- "NORMALA"
EXPONENTIALA <- "EXPONENTIALA"
BINOMIALA <- "BINOMIALA"
POISSON <- "POISSON"

get_output_distribution <- function(distribution) {
  switch(
    distribution,
    NORMALA_STANDARD = plotOutput("std_normal_plot"),
    NORMALA = plotOutput("normal_plot"),
    EXPONENTIALA = plotOutput("exponential_plot"),
    BINOMIALA = plotOutput("binom_plot"),
    POISSON = plotOutput("pois_plot")
  )
}