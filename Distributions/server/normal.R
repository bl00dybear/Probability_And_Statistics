create_normal_slider <- function() {
  tagList(
    sliderInput(
      inputId = "normal_n",
      label = "Selectează valoarea n:",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    sliderInput(
      inputId = "normal_mu",
      label = "Media (\u03BC):",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    sliderInput(
      inputId = "normal_sigma",
      label = "Deviatia standard (\u03C3):",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    )
  )
}

create_normal_var1 <- function(input) {
  renderPlot({
    x <- seq(-10, 10, length.out = 500)
    cdf_values <- pnorm(x, mean = input$normal_mu, sd = input$normal_sigma)
    
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

create_normal_var2 <- function(input) {
  renderPlot({
    x <- seq(-10, 10, length.out = 500)
    transformed_x <- 3 - 2 * x
    cdf_values <- pnorm(transformed_x, mean = input$normal_mu, sd = input$normal_sigma)
    
    plot(
      x, cdf_values,
      type = "l",
      lwd = 4,
      col = "#FF6666",
      xlab = "X",
      ylab = "F(X)",
      main = "Funcția de distribuție cumulativă (CDF)
Transformarea: var2 = 3 - 2X"
    )
  })
}

create_normal_var3 <- function(input) {
  renderPlot({
    x <- seq(-10, 10, length.out = 500)
    transformed_x <- x^2
    cdf_values <- pnorm(transformed_x, mean = input$normal_mu, sd = input$normal_sigma)
    
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

create_normal_var4 <- function(input) {
  renderPlot({
    n <- input$normal_n
    X <- rnorm(n, mean = input$normal_mu, sd = input$normal_sigma)
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

create_normal_var5 <- function(input) {
  renderPlot({
    n <- input$normal_n
    X <- rnorm(n, mean = input$normal_mu, sd = input$normal_sigma)
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

normal_server <- function(input, output, session) {
  output$normal_var1 <- create_normal_var1(input)
  output$normal_var2 <- create_normal_var2(input)
  output$normal_var3 <- create_normal_var3(input)
  output$normal_var4 <- create_normal_var4(input)
  output$normal_var5 <- create_normal_var5(input)
}
