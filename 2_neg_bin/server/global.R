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
      theme(text = element_text(color = "#000"),plot.background = element_rect(fill = "#FFF"),panel.background = element_rect(fill = "#FFF"),axis.text = element_text(color = "#000"),axis.title = element_text(color = "#000"),axis.title.y.right = element_text(color = "#000"),axis.text.y.right = element_text(color = "#000"),panel.border = element_blank(),aspect.ratio = 0.5)
}