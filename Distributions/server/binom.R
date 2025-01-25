
create_binom_slider <- function() {
  tagList(
    sliderInput("binom_n", "n:", min = 1, max = 100, value = 1, step = 1),
    sliderInput("binom_p", "p:", min = 0.01, max = 1, value = 0.5, step = 0.01),
    sliderInput("binom_r", "r:", min = 1, max = 50, value = 10, step = 1),
    sliderInput("binom_xline", "x:", min = 25, max = 5000, value = 25, step = 1)
  )
}

create_binom_plot_X <- function(binom_n, binom_p, binom_r, binom_xline) {
  renderPlot({
    p <- binom_p()
    r <- binom_r()
    xline <- binom_xline()

    # Generate a set of k values (from 0 to 100)
    X <- 0:50

    # Calculate the CDF for each k value
    cdf_values <- pbinom(X, r, p)

    # Create the plot (without points, only lines)
    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoarea lui X",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție X ~ Bin(n, p)",
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

create_binom_plot_X_transformed <- function(binom_n, binom_p, binom_r, binom_xline) {
  renderPlot({
    p <- binom_p()
    r <- binom_r()
    xline <- binom_xline()

    # Generate a set of k values (from 0 to 100)
    X <- 0:100
    X_transformed <- 5 * X - 4

    cdf_values <- pbinom(X, r, p)

    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoarea lui X",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție 5X - 4 ~ Bin(n, p)",
      las = 1               # Rotate Y-axis labels for readability
    )

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

create_binom_plot_X3 <- function(binom_n, binom_p, binom_r, binom_xline) {
  renderPlot({
    p <- binom_p()
    r <- binom_r()
    xline <- binom_xline()

    # Generate a set of k values (from 0 to 100)
    X <- 0:50
    X_cubed <- X^3

    cdf_values <- pbinom(X, r, p)

    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoarea lui X",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție X^3 ~ Bin(n, p)",
      las = 1               # Rotate Y-axis labels for readability
    )

    for (i in 1:(length(X_cubed) - 1)) {
      lines(
        c(X_cubed[i], X_cubed[i+1]),        # X-coordinates (start and end)
        c(cdf_values[i], cdf_values[i]),      # Y-coordinates (same CDF value for each line)
        col = "#56B4E9",                      # Line color
        lwd = 2                               # Line width
      )
    }

  })
}

create_binom_plot_X_sum <- function(binom_n, binom_p, binom_r, binom_xline) {
  renderPlot({
    n <- binom_n()
    p <- binom_p()
    r <- binom_r()
    xline <- binom_xline()

    r <- r * n

    # Generate a set of k values (from 0 to 100)
    X <- 0:50

    cdf_values <- pbinom(X, r, p)

    plot(
      x = NULL, y = NULL,  # Empty plot (no data points displayed)
      xlim = c(0, xline),     # X-axis limits
      ylim = c(0, 1),      # Y-axis limits
      xlab = "Valoarea lui X",    # X-axis label
      ylab = "Probabilitate Cumulativă", # Y-axis label
      main = "Funcția de Repartiție Sum(Xi) ~ Bin(n, p)",
      las = 1               # Rotate Y-axis labels for readability
    )

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

binom_server <- function(input, output, session) {
  binom_n <- reactive(input$binom_n)
  binom_p <- reactive(input$binom_p)
  binom_r <- reactive(input$binom_r)
  binom_xline <- reactive(input$binom_xline)

  output$binom_plot_X <- create_binom_plot_X(binom_n, binom_p, binom_r, binom_xline)
  output$binom_plot_X_transformed <- create_binom_plot_X_transformed(binom_n, binom_p, binom_r, binom_xline)
  output$binom_plot_X3 <- create_binom_plot_X3(binom_n, binom_p, binom_r, binom_xline)
  output$binom_plot_X_sum <- create_binom_plot_X_sum(binom_n, binom_p, binom_r, binom_xline)
}

