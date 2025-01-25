library(ggplot2)
library(dplyr)
library(shiny)

create_pois_slider <- function() {
  tagList(
    sliderInput("pois_lambda", "λ:", min = 0.1, max = 10, value = 1, step = 0.1),
    sliderInput("pois_n", "n:", min = 1, max = 100, value = 1, step = 1),
    sliderInput("pois_xline", "x:", min = 25, max = 1000, value = 25, step = 1)
  )
}

create_pois_plot_X <- function(pois_lambda, pois_n, pois_xline) {
  renderPlot({
    lambda <- pois_lambda()
    xline <- pois_xline()

    # Generate a set of k values (from 0 to 100)
    X <- 0:25

    # Calculate the CDF for each k value
    cdf_values <- ppois(X, lambda)

    # Create the plot (without points, only lines)
    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoarea lui X",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție X ~ Pois(λ)",
      las = 1               # Rotate Y-axis labels for readability
    )

    # Draw horizontal lines for the CDF values
    for (i in 1:(length(X) - 1)) {
      lines(
        c(X[i], X[i+1]),        # X-coordinates (start and end)
        c(cdf_values[i], cdf_values[i]),      # Y-coordinates (same CDF value for each line)
        col = "#56B4E9",                      # Line color
        lwd = 2                               # Line width
      )
    }
  })
}



create_pois_plot_X_transformed <- function(pois_lambda, pois_n, pois_xline) {
  renderPlot({
    lambda <- pois_lambda()
    xline <- pois_xline()

    # Generate a set of k values (from 0 to 100)
    X <- 0:20
    X_transformed <- 3 * X - 2

    # Calculate the CDF for each k value
    cdf_values <- ppois(X, lambda)

    # Create the plot (without points, only lines)
    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoare lui 3X - 2",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție 3X - 2 ~ Pois(λ)",
      las = 1               # Rotate Y-axis labels for readability
    )

    # Draw horizontal lines for the CDF values
    for (i in 1:(length(X_transformed) - 1)) {
      lines(
        c(X_transformed[i], X_transformed[i+1]),        # X-coordinates (start and end)
        c(cdf_values[i], cdf_values[i]),      # Y-coordinates (same CDF value for each line)
        col = "#56B4E9",                      # Line color
        lwd = 2                               # Line width
      )
    }
  })
}

create_pois_plot_X2 <- function(pois_lambda, pois_n, pois_xline) {
  renderPlot({
   lambda <- pois_lambda()
   xline <- pois_xline()

    # Generate data for X^2
    X <- 0:25
    X_squared <- X^2

    # Compute the probability mass function
    cdf_values <- ppois(X, lambda)

    # Create the plot
    plot(
      x = NULL, y = NULL,              # Empty plot (no data points displayed)
      xlim = c(0, xline),               # X-axis limits
      ylim = c(0, 1),
      xlab = "Valoarea lui X^2",       # X-axis label
      ylab = "Probabilitat Cumulativă", # Y-axis label
      main = "Funcția de Repartiție X^2 ~ Pois(λ)",
      las = 1                          # Rotate Y-axis labels for readability
    )

    # Draw horizontal lines for the CDF values
    for (i in 1:(length(X_squared) - 1)) {
      lines(
        c(X_squared[i], X_squared[i+1]),  # X-coordinates (start and end)
        c(cdf_values[i], cdf_values[i]),  # Y-coordinates (same CDF value for each line)
        col = "#56B4E9",                   # Line color
        lwd = 2                            # Line width
      )
    }
  })
}

create_pois_plot_X_sum <- function(pois_lambda, pois_n, pois_xline) {
  renderPlot({
    lambda <- pois_lambda()
    xline <- pois_xline()
    n <- pois_n()

    # Generate data for x1 + x2 + ... + xn
    X <- 0:25
    lambda <- lambda * n


    # Compute the probability mass function
    cdf_values <- ppois(X, lambda)

    # Create the plot
    plot(
      x = NULL, y = NULL,              # Empty plot (no data points displayed)
      xlim = c(0, xline),               # X-axis limits
      ylim = c(0, 1),
      xlab = "Valoarea lui Sum(Xi)",       # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție Sum(Xi) ~ Pois(λ)",
      las = 1                          # Rotate Y-axis labels for readability
    )

    # Draw horizontal lines for the CDF values
    for (i in 1:(length(X) - 1)) {
      lines(
        c(X[i], X[i+1]),  # X-coordinates (start and end)
        c(cdf_values[i], cdf_values[i]),  # Y-coordinates (same CDF value for each line)
        col = "#56B4E9",                   # Line color
        lwd = 2                            # Line width
      )
    }
  })
}





pois_server <- function (input, output, session) {
  # get reactive values for pois
  pois_lambda <- reactive(input$pois_lambda)
  pois_n <- reactive(input$pois_n)
  pois_xline <- reactive(input$pois_xline)

  # render multiple pois plots
  output$pois_plot_X <- create_pois_plot_X(pois_lambda, pois_n, pois_xline)
  output$pois_plot_X_transformed <- create_pois_plot_X_transformed(pois_lambda, pois_n, pois_xline)
  output$pois_plot_X2 <- create_pois_plot_X2(pois_lambda, pois_n, pois_xline)
  output$pois_plot_X_sum <- create_pois_plot_X_sum(pois_lambda, pois_n, pois_xline)
}