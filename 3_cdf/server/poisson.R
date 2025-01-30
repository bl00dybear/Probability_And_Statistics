create_pois_slider <- function() {
  tagList(
    sliderInput("pois_lambda", "λ:", min = 0.1, max = 10, value = 1, step = 0.1),
    sliderInput("pois_n", "n:", min = 1, max = 100, value = 1, step = 1),
    sliderInput("pois_x", "x:", min = 25, max = 1000, value = 25, step = 1),
    radioButtons(
      inputId = "pois_var",
      label = "Selectează variabila aleatoare:",
      choices = list(
        "X ~ Pois(λ)" = "var1",
        "3X - 2 ~ Pois(λ)" = "var2",
        "X^2 ~ Pois(λ)" = "var3",
        "Suma cumulativă" = "var4"
      ),
      selected = "var1"
    )
  )
}

pois_server <- function(input, output, session) {
  output$pois_plot <- renderPlot({
    var <- input$pois_var
    
    if (var == "var1") {
      lambda <- input$pois_lambda
      x <- input$pois_x
      X <- 0:25
      cdf_values <- ppois(X, lambda)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "X",
        ylab = "F(X)",
        main = "Funcția de repartiție pentru X ~ Pois(λ)",
        las = 1
      )
      for (i in 1:(length(X) - 1)) {
        lines(
          c(X[i], X[i + 1]),
          c(cdf_values[i], cdf_values[i]),
          col = "#339999",
          lwd = 4
        )
      }
    } else if (var == "var2") {
      lambda <- input$pois_lambda
      x <- input$pois_x
      X <- 0:20
      X_transformed <- 3 * X - 2
      cdf_values <- ppois(X, lambda)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "3X - 2",
        ylab = "F(3X - 2)",
        main = "Funcția de repartiție pentru 3X - 2 ~ Pois(λ)",
        las = 1
      )
      for (i in 1:(length(X_transformed) - 1)) {
        lines(
          c(X_transformed[i], X_transformed[i + 1]),
          c(cdf_values[i], cdf_values[i]),
          col = "#FF6666",
          lwd = 4
        )
      }
    } else if (var == "var3") {
      lambda <- input$pois_lambda
      x <- input$pois_x
      X <- 0:25
      X_squared <- X^2
      cdf_values <- ppois(X, lambda)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "X^2",
        ylab = "F(X^2)",
        main = "Funcția de repartiție pentru X^2 ~ Pois(λ)",
        las = 1
      )
      for (i in 1:(length(X_squared) - 1)) {
        lines(
          c(X_squared[i], X_squared[i + 1]),
          c(cdf_values[i], cdf_values[i]),
          col = "#3399FF",
          lwd = 4
        )
      }
    } else if (var == "var4") {
      lambda <- input$pois_lambda
      x <- input$pois_x
      n <- input$pois_n
      X <- 0:25
      lambda <- lambda * n
      cdf_values <- ppois(X, lambda)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "Sum(Xi)",
        ylab = "F(Sum(Xi))",
        main = "Funcția de repartiție pentru Sum(Xi) ~ Pois(λ)",
        las = 1
      )
      for (i in 1:(length(X) - 1)) {
        lines(
          c(X[i], X[i + 1]),
          c(cdf_values[i], cdf_values[i]),
          col = "#FF9900",
          lwd = 4
        )
      }
    }
  })
}