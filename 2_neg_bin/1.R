negative_binomial_pmf_1 <- function(k, r, p) {
  binomial_coefficient <- choose(k + r - 1, k)
  probability <- binomial_coefficient * p^r * (1 - p)^k
  return(probability)
}

# Funcție pentru generarea graficului
render_plot_1 <- function(r_value, p_value) {
  renderPlot({
    r <- r_value()
    p <- p_value()
    x <- 0:100

    pmf_values <- sapply(x, function(k) negative_binomial_pmf_1(k, r, p))
    cmf_values <- cumsum(pmf_values)

    df <- data.frame(x = x, pmf = pmf_values, cmf = cmf_values)
    scaling_factor <- max(df$pmf) / max(df$cmf)

    render_plot(df, scaling_factor)
})
}