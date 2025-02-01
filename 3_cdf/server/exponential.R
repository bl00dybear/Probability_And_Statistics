create_exponential_slider <- function() {
  tagList(
    sliderInput(
      inputId = "exponential_n",
      label = "Selectează valoarea n:",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    sliderInput(
      inputId = "exponential_lambda",
      label = "Rata (\u03bb):",
      min = 0.1,
      max = 10,
      value = 1,
      step = 0.1
    ),
    radioButtons(
      inputId = "exponential_var",
      label = "Selectează variabila aleatoare:",
      choices = list(
        "X ~ Exp(\u03bb)" = "var1",
        "2 + 5X ~ Exp(\u03bb)" = "var2",
        "X^2 ~ Exp(\u03bb)" = "var3",
        "Suma cumulativă" = "var4"
      ),
      selected = "var1"
    )
  )
}

exponential_server <- function(input, output, session) {
  output$exponential_plot <- renderPlot({
    var <- input$exponential_var
    
    if (var == "var1") {
      x <- seq(0, 10, length.out = 500)
      cdf_values <- pexp(x, rate = input$exponential_lambda)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#339999",
        xlab = "X",
        ylab = "F(X)",
        main = "Funcția de repartiție pentru X ~ Exp(\u03bb)"
      )
    } else if (var == "var2") {
      x <- seq(0, 10, length.out = 500)
      transformed_x <- 2 + 5 * x
      cdf_values <- pexp(transformed_x, rate = input$exponential_lambda)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#FF6666",
        xlab = "2 + 5X",
        ylab = "F(2 + 5X)",
        main = "Funcția de repartiție pentru 2 + 5X ~ Exp(\u03bb)"
      )
    } else if (var == "var3") {
      x <- seq(0, 10, length.out = 500)
      transformed_x <- x^2
      cdf_values <- pexp(transformed_x, rate = input$exponential_lambda)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#3399FF",
        xlab = "X^2",
        ylab = "F(X^2)",
        main = "Funcția de repartiție pentru X^2 ~ Exp(\u03bb)"
      )
    } else if (var == "var4") {
      n <- input$exponential_n
      lambda <- input$exponential_lambda
      theta <- 1 / lambda

      x_values <- seq(0, qgamma(0.999, shape = n, scale = theta), length.out = 100)

      cdf_values <- pgamma(x_values, shape = n, scale = theta)

      plot(x_values, cdf_values, type = "l", col = "blue", lwd = 2,
           main = "Funcția de repartiție cumulativă (CDF) - Gamma",
           xlab = "x", ylab = "F_X(x)")
    }
  })
}