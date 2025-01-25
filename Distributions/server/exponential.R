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
      label = "Rata (\u03BB):",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    )
  )
}

create_exponential_var1 <- function(input) {
  renderPlot({
    validate(
      need(is.numeric(input$exponential_lambda) && input$exponential_lambda > 0, "\u03BB trebuie să fie numeric și pozitiv.")
    )
    x <- seq(0, 10, length.out = 500)
    cdf_values <- pexp(x, rate = input$exponential_lambda)
    
    plot(
      x, cdf_values,
      type = "l",
      lwd = 4,
      col = "#339999",
      xlab = "X",
      ylab = "F(X)",
      main = "Funcția de distribuție cumulativă (CDF)
Transformarea: var1 = X"
    )
  })
}

create_exponential_var2 <- function(input) {
  renderPlot({
    validate(
      need(is.numeric(input$exponential_lambda) && input$exponential_lambda > 0, "\u03BB trebuie să fie numeric și pozitiv.")
    )
    x <- seq(0, 10, length.out = 500)
    transformed_x <- 2 + 5 * x
    cdf_values <- pexp(transformed_x, rate = input$exponential_lambda)
    
    plot(
      x, cdf_values,
      type = "l",
      lwd = 4,
      col = "#FF6666",
      xlab = "X",
      ylab = "F(X)",
      main = "Funcția de distribuție cumulativă (CDF)
Transformarea: var2 = 2 + 5X"
    )
  })
}

create_exponential_var3 <- function(input) {
  renderPlot({
    validate(
      need(is.numeric(input$exponential_lambda) && input$exponential_lambda > 0, "\u03BB trebuie să fie numeric și pozitiv.")
    )
    x <- seq(0, 10, length.out = 500)
    transformed_x <- x^2
    cdf_values <- pexp(transformed_x, rate = input$exponential_lambda)
    
    plot(
      x, cdf_values,
      type = "l",
      lwd = 4,
      col = "#3399FF",
      xlab = "X",
      ylab = "F(X)",
      main = "Funcția de distribuție cumulativă (CDF)
Transformarea: var3 = X^2"
    )
  })
}

create_exponential_var4 <- function(input) {
  renderPlot({
    validate(
      need(is.numeric(input$exponential_lambda) && input$exponential_lambda > 0, "\u03BB trebuie să fie numeric și pozitiv."),
      need(is.numeric(input$exponential_n) && input$exponential_n > 0, "n trebuie să fie numeric și pozitiv.")
    )
    n <- input$exponential_n
    X <- rexp(n, rate = input$exponential_lambda)
    S_n <- cumsum(X)
    
    plot(
      1:n, S_n,
      type = "o",
      lwd = 2,
      col = "#FF9900",
      xlab = "n",
      ylab = "\u2211 X_i",
      main = "Suma cumulativă a variabilelor aleatoare X_i"
    )
  })
}

create_exponential_var5 <- function(input) {
  renderPlot({
    validate(
      need(is.numeric(input$exponential_lambda) && input$exponential_lambda > 0, "\u03BB trebuie să fie numeric și pozitiv."),
      need(is.numeric(input$exponential_n) && input$exponential_n > 0, "n trebuie să fie numeric și pozitiv.")
    )
    n <- input$exponential_n
    X <- rexp(n, rate = input$exponential_lambda)
    S_n2 <- cumsum(X^2)
    
    plot(
      1:n, S_n2,
      type = "o",
      lwd = 2,
      col = "#33CC33",
      xlab = "n",
      ylab = "\u2211 X_i^2",
      main = "Suma cumulativă a pătratelor variabilelor X_i"
    )
  })
}

exponential_server <- function(input, output, session) {
  output$exponential_var1 <- create_exponential_var1(input)
  output$exponential_var2 <- create_exponential_var2(input)
  output$exponential_var3 <- create_exponential_var3(input)
  output$exponential_var4 <- create_exponential_var4(input)
  output$exponential_var5 <- create_exponential_var5(input)
}
