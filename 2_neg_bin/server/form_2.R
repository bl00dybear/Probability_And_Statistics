negative_binomial_pmf_2 <- function(n, r, p) {
  binomial_coefficient <- choose(n-1, r-1)
  probability <- binomial_coefficient * p^r * (1 - p)^(n-r)
  return(probability)
}

render_plot_2 <- function(r_value, p_value) {
  renderPlot({
    r <- r_value()
    p <- p_value()
    x <- r:100

    pmf_values <- sapply(x, function(k) negative_binomial_pmf_2(k, r, p))
    cmf_values <- cumsum(pmf_values)

    df <- data.frame(x = x, pmf = pmf_values, cmf = cmf_values)
    scaling_factor <- max(df$pmf) / max(df$cmf)

    render_plot(df, scaling_factor)
    })
}