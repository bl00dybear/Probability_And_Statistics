create_binom_slider <- function() {
  tagList(
    sliderInput("binom_n", "n:", min = 1, max = 100, value = 1, step = 1),
    sliderInput("binom_p", "p:", min = 0.01, max = 1, value = 0.5, step = 0.01),
    sliderInput("binom_r", "r:", min = 1, max = 50, value = 10, step = 1),
    sliderInput("binom_x", "x:", min = 25, max = 5000, value = 25, step = 1),
    radioButtons(
      inputId = "binom_var",
      label = "Selectează variabila aleatoare:",
      choices = list(
        "X ~ Bin(n, p)" = "var1",
        "5X - 4 ~ Bin(n, p)" = "var2",
        "X^3 ~ Bin(n, p)" = "var3",
        "Suma cumulativă" = "var4"
      ),
      selected = "var1"
    )
  )
}

binom_server <- function(input, output, session) {
  output$binom_plot <- renderPlot({
    var <- input$binom_var
    
    if (var == "var1") {
      p <- input$binom_p
      r <- input$binom_r
      x <- input$binom_x
      X <- 0:50
      cdf_values <- pbinom(X, r, p)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "X",
        ylab = "F(X)",
        main = "Funcția de repartiție pentru X ~ Bin(n, p)",
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
      p <- input$binom_p
      r <- input$binom_r
      x <- input$binom_x
      X <- 0:100
      X_transformed <- 5 * X - 4
      cdf_values <- pbinom(X, r, p)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "5X - 4",
        ylab = "F(5X - 4)",
        main = "Funcția de repartiție pentru 5X - 4 ~ Bin(n, p)",
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
      p <- input$binom_p
      r <- input$binom_r
      x <- input$binom_x
      X <- 0:50
      X_cubed <- X^3
      cdf_values <- pbinom(X, r, p)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "X^3",
        ylab = "F(X^3)",
        main = "Funcția de repartiție pentru X^3 ~ Bin(n, p)",
        las = 1
      )
      for (i in 1:(length(X_cubed) - 1)) {
        lines(
          c(X_cubed[i], X_cubed[i + 1]),
          c(cdf_values[i], cdf_values[i]),
          col = "#3399FF",
          lwd = 4
        )
      }
    } else if (var == "var4") {
      n <- input$binom_n
      p <- input$binom_p
      r <- input$binom_r
      x <- input$binom_x
      r <- r * n
      X <- 0:50
      cdf_values <- pbinom(X, r, p)
      plot(
        x = NULL, y = NULL,
        xlim = c(0, x),
        ylim = c(0, 1),
        xlab = "Sum(Xi)",
        ylab = "F(Sum(Xi))",
        main = "Funcția de repartiție pentru Sum(Xi) ~ Bin(n, p)",
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