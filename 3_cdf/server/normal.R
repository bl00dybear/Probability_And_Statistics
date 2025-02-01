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
    ),
    radioButtons(
      inputId = "normal_var",
      label = "Selectează variabila aleatoare:",
      choices = list(
        "X ~ N(\u03BC, \u03C3^2)" = "var1",
        "3 - 2X ~ N(\u03BC, \u03C3^2)" = "var2",
        "X^2 ~ N(\u03BC, \u03C3^2)" = "var3",
        "Suma cumulativă" = "var4",
        "Suma pătratelor" = "var5"
      ),
      selected = "var1"
    )
  )
}

normal_server <- function(input, output, session) {
  output$normal_plot <- renderPlot({
    var <- input$normal_var
    
    if (var == "var1") {
      x <- seq(-10, 10, length.out = 500)
      cdf_values <- pnorm(x, mean = input$normal_mu, sd = input$normal_sigma)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#339999",
        xlab = "X",
        ylab = "F(X)",
        main = "Funcția de repartiție pentru X ~ N(\u03BC, \u03C3^2)"
      )
    } else if (var == "var2") {
      x <- seq(-10, 10, length.out = 500)
      transformed_x <- 3 - 2 * x
      cdf_values <- pnorm(transformed_x, mean = input$normal_mu, sd = input$normal_sigma)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#FF6666",
        xlab = "3 - 2X",
        ylab = "F(3 - 2X)",
        main = "Funcția de repartiție pentru 3 - 2X ~ N(\u03BC, \u03C3^2)"
      )
    } else if (var == "var3") {
      x <- seq(-10, 10, length.out = 500)
      transformed_x <- x^2
      cdf_values <- pnorm(transformed_x, mean = input$normal_mu, sd = input$normal_sigma)
      plot(
        x, cdf_values,
        type = "l",
        lwd = 4,
        col = "#3399FF",
        xlab = "X^2",
        ylab = "F(X^2)",
        main = "Funcția de repartiție pentru X^2 ~ N(\u03BC, \u03C3^2)"
      )
    } else if (var == "var4") {
      n <- input$normal_n
      mu <- input$normal_mu   # Media individuală
      sigma <- input$normal_sigma  # Deviația standard individuală

      # Parametrii lui X = sum(X_i)
      mu_X <- n * mu
      sigma_X <- sqrt(n * sigma^2)

      # Generăm valori pentru x
      x_values <- seq(mu_X - 4*sigma_X, mu_X + 4*sigma_X, length.out = 100)

      # Calculăm CDF-ul
      cdf_values <- pnorm(x_values, mean = mu_X, sd = sigma_X)

      # Afișăm graficul CDF
      plot(x_values, cdf_values, type = "l", col = "blue", lwd = 2,
           main = "Funcția de repartiție cumulativă (CDF)",
           xlab = "x", ylab = "F_X(x)")
    } else if (var == "var5") {
      n <- input$normal_n
      mu <- input$normal_mu
      sigma <- input$normal_sigma

      lambda <- n * (mu / sigma)^2

      x_values <- seq(0, qchisq(0.999, df = n, ncp = lambda), length.out = 100)

      cdf_values <- pchisq(x_values, df = n, ncp = lambda)

      plot(x_values, cdf_values, type = "l", col = "blue", lwd = 2,
           main = "Funcția de repartiție cumulativă (CDF) - Chi-pătrat non-central",
           xlab = "x", ylab = "F_X(x)")
    }
  })
}