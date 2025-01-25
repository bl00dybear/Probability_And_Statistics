create_std_normal_slider <- function() {
  # Slider pentru a selecta valoarea la care se calculeaza CDF
  sliderInput(
    inputId = "std_normal_x",
    label = "Selectează valoarea X:",
    min = -4,
    max = 4,
    value = 0,
    step = 0.1
  )
}

std_normal_server <- function(input, output, session) {
  # Calculul și afișarea CDF
  output$std_normal_cdf_plot <- renderPlot({
    x <- seq(-4, 4, length.out = 500)
    cdf_values <- pnorm(x, mean = 0, sd = 1) # CDF pentru normală standard
    
    # Plotare
    plot(
      x, cdf_values,
      type = "l",
      lwd = 2,
      col = "blue",
      xlab = "X",
      ylab = "F(X)",
      main = "Funcția de distribuție cumulativă (CDF)\nNormala Standard"
    )
    
    # Afișare punct pentru valoarea selectată
    points(input$std_normal_x, pnorm(input$std_normal_x), col = "red", pch = 19)
    abline(v = input$std_normal_x, col = "red", lty = 2)
    abline(h = pnorm(input$std_normal_x), col = "red", lty = 2)
  })
}
