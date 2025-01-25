library(ggplot2)
library(dplyr)

create_pois_slider <- function() {
  tagList(
    sliderInput("pois_lambda", "λ:", min = 0.1, max = 10, value = 1, step = 0.1),
    sliderInput("pois_n", "n:", min = 1, max = 10, value = 1, step = 1)
  )
}

create_pois_plot_X <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    # Generate data and apply transformation
    X <- rpois(1000, lambda)

    # Compute the empirical CDF
    df <- data.frame(Value = sort(X), CDF = ecdf(X)(sort(X)))
    df_steps <- df %>%
      mutate(Value_next = lead(Value, default = Value[n()]),  # Next value
             CDF_next = CDF) %>%                            # Maintain same CDF for horizontal line
      filter(!is.na(Value_next))                            # Remove last row

    ggplot(df_steps, aes(x = Value, y = CDF)) +
      geom_segment(aes(xend = Value_next, yend = CDF), color = "#56B4E9", size = 1.2) +
      geom_point(data = df, aes(x = Value, y = CDF), color = "#E69F00", size = 1) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Funcția de Repartiție Cumulativă: 3X - 2 ~ Pois(λ)",
        x = "Valoare",
        y = "Probabilitate Cumulativă (CDF)"
      ) +
      theme(
        text = element_text(color = "#000"),
        plot.background = element_rect(fill = "#FFF"),
        panel.background = element_rect(fill = "#FFF"),
        axis.text = element_text(color = "#000"),
        axis.title = element_text(color = "#000"),
        panel.border = element_blank(),
        aspect.ratio = 0.5
      )
  })
}

create_pois_plot_X_transformed <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    # Generate data and apply transformation
    X <- rpois(1000, lambda)
    X_transformed <- 3 * X - 2

    # Compute the empirical CDF
    df <- data.frame(Value = sort(X_transformed), CDF = ecdf(X_transformed)(sort(X_transformed)))
    df_steps <- df %>%
      mutate(Value_next = lead(Value, default = Value[n()]),  # Next value
             CDF_next = CDF) %>%                            # Maintain same CDF for horizontal line
      filter(!is.na(Value_next))                            # Remove last row

    ggplot(df_steps, aes(x = Value, y = CDF)) +
      geom_segment(aes(xend = Value_next, yend = CDF), color = "#56B4E9", size = 1.2) +
      geom_point(data = df, aes(x = Value, y = CDF), color = "#E69F00", size = 1) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Funcția de Repartiție Cumulativă: 3X - 2 ~ Pois(λ)",
        x = "Valoare",
        y = "Probabilitate Cumulativă (CDF)"
      ) +
      theme(
        text = element_text(color = "#000"),
        plot.background = element_rect(fill = "#FFF"),
        panel.background = element_rect(fill = "#FFF"),
        axis.text = element_text(color = "#000"),
        axis.title = element_text(color = "#000"),
        panel.border = element_blank(),
        aspect.ratio = 0.5
      )
  })
}

create_pois_plot_X2 <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()

    # Generate data and apply transformation
    X <- rpois(1000, lambda)
    X_squared <- X^2

    # Compute the frequency for histogram steps
    df <- data.frame(Value = X_squared) %>%
      count(Value, name = "Frequency") %>%
      mutate(Value_next = lead(Value, default = Value[n()])) %>%
      filter(!is.na(Value_next))

    ggplot(df, aes(x = Value, y = Frequency)) +
      geom_segment(aes(xend = Value_next, yend = Frequency), color = "#56B4E9", size = 1.2) +
      geom_point(aes(x = Value, y = Frequency), color = "#E69F00", size = 1) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Distribuția lui X^2 ~ Pois(λ)",
        x = "Valoare",
        y = "Frecvență"
      ) +
      theme(
        text = element_text(color = "#000"),
        plot.background = element_rect(fill = "#FFF"),
        panel.background = element_rect(fill = "#FFF"),
        axis.text = element_text(color = "#000"),
        axis.title = element_text(color = "#000"),
        panel.border = element_blank(),
        aspect.ratio = 0.5
      )
  })
}

create_pois_plot_X_sum <- function(pois_lambda, pois_n) {
  renderPlot({
    lambda <- pois_lambda()
    n <- pois_n()

    # Generate data for the sum of Poisson variables
    X_sum <- replicate(1000, sum(rpois(n, lambda)))

    # Compute the frequency for histogram steps
    df <- data.frame(Value = X_sum) %>%
      count(Value, name = "Frequency") %>%
      mutate(Value_next = lead(Value, default = Value[n()])) %>%
      filter(!is.na(Value_next))

    ggplot(df, aes(x = Value, y = Frequency)) +
      geom_segment(aes(xend = Value_next, yend = Frequency), color = "#56B4E9", size = 1.2) +
      geom_point(aes(x = Value, y = Frequency), color = "#E69F00", size = 1) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = paste("Suma celor", n, "X_i ~ Pois(λ)"),
        x = "Valoare",
        y = "Frecvență"
      ) +
      theme(
        text = element_text(color = "#000"),
        plot.background = element_rect(fill = "#FFF"),
        panel.background = element_rect(fill = "#FFF"),
        axis.text = element_text(color = "#000"),
        axis.title = element_text(color = "#000"),
        panel.border = element_blank(),
        aspect.ratio = 0.5
      )
  })
}


# create_pois_plot_X_transformed <- function(pois_lambda, pois_n) {
#   renderPlot({
#     lambda <- pois_lambda()
#
#     X <- rpois(1000, lambda)
#     X_transformed <- 3 * X - 2
#
#     # Compute the empirical CDF
#     cdf <- ecdf(X_transformed)
#
#     # Plot the CDF
#     plot(cdf, main = "CDF of 3X - 2 ~ Pois(λ)", col = 'blue', lwd = ,
#          xlab = "Value", ylab = "Cumulative Probability")
#   })
# }
#
# create_pois_plot_X2 <- function(pois_lambda, pois_n) {
#   renderPlot({
#     lambda <- pois_lambda()
#
#     X <- rpois(1000, lambda)
#     X_squared <- X^2
#
#     # Plot the distribution
#     hist(X_squared, main = "X^2 ~ Pois(λ)", breaks = 30, col = 'lightcoral', xlab = "")
#   })
# }
#
# create_pois_plot_X_sum <- function(pois_lambda, pois_n) {
#   renderPlot({
#     lambda <- pois_lambda()
#     n <- pois_n()
#
#     X_sum <- replicate(1000, sum(rpois(n, lambda)))
#
#     # Plot the distribution
#     hist(X_sum, main = paste("Sum of", n, "X_i ~ Pois(λ)"), breaks = 30, col = 'lightyellow', xlab = "")
#   })
# }


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