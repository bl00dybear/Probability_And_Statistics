create_std_normal_slider <- function() {
  tagList(
    sliderInput(
      inputId = "std_normal_n",
      label = "Selectează valoarea n:",
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    radioButtons(
      inputId = "std_normal_var",
      label = "Selectează variabila aleatoare:",
      choices = list(
        "X ~ N(0, 1)" = "var1",
        "3 - 2X ~ N(0, 1)" = "var2",
        "X^2 ~ N(0, 1)" = "var3",
        "Suma cumulativă" = "var4",
        "Suma pătratelor" = "var5"
      ),
      selected = "var1"
    )
  )
}

std_normal_server <- function(input, output, session) {
  output$std_normal_plot <- renderPlot({
    var <- input$std_normal_var
    
    if (var == "var1") {
      x <- seq(-4, 4, length.out = 500)
      cdf_values <- pnorm(x, mean = 0, sd = 1)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#339999",
        xlab = "X",
        ylab = "F(X)",
        main = "Funcția de repartiție pentru X ~ N(0, 1)"
      )
    } else if (var == "var2") {
      x <- seq(-4, 4, length.out = 500)
      transformed_x <- 3 - 2 * x
      cdf_values <- pnorm(transformed_x, mean = 0, sd = 1)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#FF6666",
        xlab = "3 - 2X",
        ylab = "F(3 - 2X)",
        main = "Funcția de repartiție pentru 3 - 2X ~ N(0, 1)"
      )
    } else if (var == "var3") {
      x <- seq(-4, 4, length.out = 500)
      transformed_x <- x^2
      cdf_values <- pnorm(transformed_x, mean = 0, sd = 1)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#3399FF",
        xlab = "X^2",
        ylab = "F(X^2)",
        main = "Funcția de repartiție pentru X^2 ~ N(0, 1)"
      )
    } else if (var == "var4") {
      x <- seq(-4, 4, length.out = 500)
      cdf_values <- pnorm(x, mean = 0, sd = n)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#FF9900",
        xlab = "n",
        ylab = "\u2211 X_i",
        main = "Suma cumulativă a variabilelor aleatoare X_i"
      )
    } else if (var == "var5") {
      x <- seq(-4, 4, length.out = 500)
      cdf_values <- pnorm(x^2, mean = 0, sd = n)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#33CC33",
        xlab = "n",
        ylab = "\u2211 X_i^2",
        main = "Suma cumulativă a pătratelor variabilelor X_i"
      )
    }
  })
}
