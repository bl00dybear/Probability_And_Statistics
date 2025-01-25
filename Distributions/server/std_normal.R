create_std_normal_slider <- function() {
  sliderInput(
    inputId = "std_normal_n",
    label = "Selectează valoarea n:",
    min = 1,
    max = 100,
    value = 10,
    step = 1
  )
}

create_std_normal_var1 <- function() {
  renderPlot({
    x <- seq(-4, 4, length.out = 500)
    cdf_values <- pnorm(x, mean = 0, sd = 1)
    
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

create_std_normal_var2 <- function() {
  renderPlot({
    x <- seq(-4, 4, length.out = 500)
    transformed_x <- 3 - 2 * x
    cdf_values <- pnorm(transformed_x, mean = 0, sd = 1)
    
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

create_std_normal_var3 <- function() {
  renderPlot({
    x <- seq(-4, 4, length.out = 500)
    transformed_x <- x^2
    cdf_values <- pnorm(transformed_x, mean = 0, sd = 1)
    
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


create_std_normal_var4 <- function(input) {
  renderPlot({
    n <- input$std_normal_n
    X <- rnorm(n, mean = 0, sd = 1)
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

create_std_normal_var5 <- function(input) {
  renderPlot({
    n <- input$std_normal_n
    X <- rnorm(n, mean = 0, sd = 1)
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

std_normal_server <- function(input, output, session) {
  output$std_normal_var1 <- create_std_normal_var1()
  output$std_normal_var2 <- create_std_normal_var2()
  output$std_normal_var3 <- create_std_normal_var3()
  output$std_normal_var4 <- create_std_normal_var4(input)
  output$std_normal_var5 <- create_std_normal_var5(input)
}
