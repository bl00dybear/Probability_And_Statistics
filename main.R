library(shiny)
library(bslib)
library(ggplot2)

# Funcție pentru a crea un tab UI
# Funcție pentru a crea un tab UI care include și graficele dinamice
create_tab <- function(tab_title, title, img1_src, img2_src, tab_number) {
  tabPanel(
    tab_title,
    div(
      class = "container",
      h1(title),
      div(
        class = "d-flex justify-content-center",
        img(src = img1_src, style = "filter: invert(1);height:4vh;"),
        img(src = img2_src, style = "filter: invert(1);height:4vh;")
      ),
      div(
        class = "row",
        div(
          class = "col-4",
          tags$h3("Input:"),
          sliderInput(paste0("r", tab_number), "Numărul de succese (r):", min = 1, max = 50, value = 10, step = 1),
          checkboxInput(paste0("fix_r", tab_number), "Fixează r", value = TRUE),
          sliderInput(paste0("p", tab_number), "Probabilitatea de succes (p):", min = 0.01, max = 1, value = 0.5, step = 0.01),
          checkboxInput(paste0("fix_p", tab_number), "Fixează p", value = FALSE)
        ),
        div(
          class = "col-8",
          h4("Reprezentare Grafică"),
          div(
            style = "border: 1px solid #E69F00; padding: 10px; margin-bottom: 10px;",
            plotOutput(outputId = paste0("mass_function_plot_", tab_number)),
            )
        )
      )
    )
  )
}




# Funcție pentru actualizarea valorilor reactive și animație
create_reactive_values <- function(input, session, suffix) {
  rv <- reactiveValues(current_r = 10, current_p = 0.5)
  
  r_value <- reactive({
    if (!is.null(input[[paste0("fix_r", suffix)]]) && input[[paste0("fix_r", suffix)]]) {
      input[[paste0("r", suffix)]]
    } else {
      rv$current_r
    }
  })
  
  p_value <- reactive({
    if (!is.null(input[[paste0("fix_p", suffix)]]) && input[[paste0("fix_p", suffix)]]) {
      input[[paste0("p", suffix)]]
    } else {
      rv$current_p
    }
  })
  
  observe({
    invalidateLater(500, session)  # Actualizare periodică
    isolate({
      if (!is.null(input[[paste0("fix_r", suffix)]]) && !input[[paste0("fix_r", suffix)]]) {
        rv$current_r <- rv$current_r + 0.5
        if (rv$current_r > 50) rv$current_r <- 1
      }
      if (!is.null(input[[paste0("fix_p", suffix)]]) && !input[[paste0("fix_p", suffix)]]) {
        rv$current_p <- rv$current_p + 0.01
        if (rv$current_p > 1) rv$current_p <- 0.01
      }
    })
  })
  
  list(r_value = r_value, p_value = p_value, rv = rv)
}

render_plot <- function(df,scaling_factor) {
  ggplot(df, aes(x = x)) +
      geom_bar(aes(y = pmf), stat = "identity", fill = "#E69F00", alpha = 0.8, width = 0.8) +
      geom_step(aes(y = cmf * scaling_factor), color = "#56B4E9", size = 1.2) +
      theme_minimal(base_family = "Inconsolata") +
      labs(title = "PMF și CMF pentru Distribuția Binomială Negativă", x = "", y = "Probabilitate (PMF)") +
      scale_y_continuous(sec.axis = sec_axis(~ . / scaling_factor, name = "Probabilitate Cumulativă (CMF)")) +
      theme(text = element_text(color = "#000"),plot.background = element_rect(fill = "#FFF"),panel.background = element_rect(fill = "#FFF"),axis.text = element_text(color = "#000"),axis.title = element_text(color = "#000"),axis.title.y.right = element_text(color = "#56B4E9"),axis.text.y.right = element_text(color = "#56B4E9"),panel.border = element_blank(),aspect.ratio = 0.5)
}

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

negative_binomial_pmf_3 <- function(n, r, p) {
  binomial_coefficient <- choose(n-1, r-1)
  probability <- binomial_coefficient * p^(n-r) * (1 - p)^r
  return(probability)
}

render_plot_3 <- function(r_value, p_value) {
  renderPlot({
    r <- r_value()
    p <- p_value()
    x <- r:100

    pmf_values <- sapply(x, function(k) negative_binomial_pmf_1(k, r, p))
    cmf_values <- cumsum(pmf_values)

    df <- data.frame(x = x, pmf = pmf_values, cmf = cmf_values)
    scaling_factor <- max(df$pmf) / max(df$cmf)

    render_plot(df, scaling_factor)
  })
}

negative_binomial_pmf_4 <- function(k, r, p) {
  binomial_coefficient <- choose(k+r-1, k)
  probability <- binomial_coefficient * p^k * (1-p)^r
  return(probability)
}

render_plot_4 <- function(r_value, p_value) {
  renderPlot({
    r <- r_value()
    p <- p_value()
    x <- 0:100

    pmf_values <- sapply(x, function(k) negative_binomial_pmf_4(k, r, p))
    cmf_values <- cumsum(pmf_values)

    df <- data.frame(x = x, pmf = pmf_values, cmf = cmf_values)
    scaling_factor <- max(df$pmf) / max(df$cmf)

    render_plot(df, scaling_factor)
  })
}

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

ui <- fluidPage(
  theme = bs_theme(version = 4, bg = "#101010", fg = "#FFF", primary = "#E69F00", base_font = font_google("Inconsolata")),
  navbarPage(
    "Repartitia Negativ Binomiala",
    create_tab(
              "Formularea 1",
              "Numărul de eșecuri înainte de a obține un număr fix de succese (r)", 
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/1a26b86be5f29ff5c8455dd3f357faeb8aaed623",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/29944ccb6e33fb4970c050a4cc81f3b4ca9aa5b1",
              1),
    create_tab(
              "Formularea 2", 
              "Numărul total de încercări necesare pentru a obține un număr fix de succese (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/122b97516ba764db7dfc9ecc39f68a12d1db6be3",
              2),
    create_tab(
              "Formularea 3",
              "Numărul total de încercări necesare pentru a obține un număr fix de eșecuri (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/7fd538f62a633cdeae41ffe6c8bd2811a8371b1d",
              3),
    create_tab(
              "Formularea 4",
              "Numărul de succese înainte de a obține un număr fix de eșecuri (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/1a26b86be5f29ff5c8455dd3f357faeb8aaed623",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/5d39d1344aec87bea613a8161cdecfbd180ff203",
              4),
    create_tab(
              "Formularea 5",
              "Numărul de succese într-un număr fix de încercări (n)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/d23e3ebeb017f43015cc710cb39ac89641aea136",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/09ed5ae47e20d49e0a5a42d68fe19e15326bd60e",
              5)
    ),
  )
)

server <- function(input, output, session) {
  # Creare reactive pentru fiecare tab
  reactive_values1 <- create_reactive_values(input, session, "1")
  reactive_values2 <- create_reactive_values(input, session, "2")
  reactive_values3 <- create_reactive_values(input, session, "3")
  reactive_values4 <- create_reactive_values(input, session, "4")
  reactive_values5 <- create_reactive_values(input, session, "5")
  
  output$mass_function_plot_1 <- render_plot_1(reactive_values1$r_value, reactive_values1$p_value)
  
  output$mass_function_plot_2 <- render_plot_2(reactive_values2$r_value, reactive_values2$p_value)

  output$mass_function_plot_3 <- render_plot_3(reactive_values3$r_value, reactive_values3$p_value)

  output$mass_function_plot_4 <- render_plot_4(reactive_values4$r_value, reactive_values4$p_value)

  output$mass_function_plot_5 <- render_plot_5(reactive_values5$r_value, reactive_values5$p_value)
}

shinyApp(ui = ui, server = server)
