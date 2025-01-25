
create_pois_slider <- function() {
  tagList(
    sliderInput("pois_lambda", "λ:", min = 0.1, max = 10, value = 1, step = 0.1),
    sliderInput("pois_n", "n:", min = 1, max = 10, value = 1, step = 1)
  )
}

create_pois_plot_X <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    X <- rpois(1000, lambda)

    # Plot the distribution
    hist(X, main = "X ~ Pois(λ)", breaks = 30, col = 'lightblue', xlab = "")
  })
}

create_pois_plot_X_transformed <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    X <- rpois(1000, lambda)
    X_transformed <- 3 * X - 2

    # Plot the distribution
    hist(X_transformed, main = "3X - 2 ~ Pois(λ)", breaks = 30, col = 'lightgreen', xlab = "")
  })
}

create_pois_plot_X2 <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    X <- rpois(1000, lambda)
    X_squared <- X^2

    # Plot the distribution
    hist(X_squared, main = "X^2 ~ Pois(λ)", breaks = 30, col = 'lightcoral', xlab = "")
  })
}

create_pois_plot_X_sum <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()
    n <- pois_n()

    X_sum <- replicate(1000, sum(rpois(n, lambda)))

    # Plot the distribution
    hist(X_sum, main = paste("Sum of", n, "X_i ~ Pois(λ)"), breaks = 30, col = 'lightyellow', xlab = "")
  })
}


pois_server <- function (input, output, session) {
  # get reactive values for pois
  pois_lambda <- reactive(input$pois_lambda)
  pois_n <- reactive(input$pois_n)

  # render multiple pois plots
  output$pois_plot_X <- create_pois_plot_X(pois_lambda, pois_n)
  output$pois_plot_X_transformed <- create_pois_plot_X_transformed(pois_lambda, pois_n)
  output$pois_plot_X2 <- create_pois_plot_X2(pois_lambda, pois_n)
  output$pois_plot_X_sum <- create_pois_plot_X_sum(pois_lambda, pois_n)
}