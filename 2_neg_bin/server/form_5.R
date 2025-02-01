negative_binomial_pmf_5 <- function(k, n, p) {
  binomial_coefficient <- choose(n, k)
  probability <- binomial_coefficient * p^k * (1-p)^(n-k)
  return(probability)
}

render_plot_5 <- function(r_value, p_value) {
  renderPlot({
    r <- r_value()
    p <- p_value()
    x <- 0:100

    pmf_values <- sapply(x, function(k) negative_binomial_pmf_5(k, k+r, p))
    cmf_values <- cumsum(pmf_values)

    df <- data.frame(x = x, pmf = pmf_values, cmf = cmf_values)
    scaling_factor <- max(df$pmf) / max(df$cmf)

    render_plot(df, scaling_factor)
  })
}
