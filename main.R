library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bg = "#101010",
    fg = "#FFF",
    primary = "#E69F00",
    base_font = font_google("Inconsolata")
  ),
  navbarPage(
    "Repartitia Negativ Binomiala",
    tabPanel(
      "Formularea 1",
      div(
        class = "container",
        h1("Numarul de esecuri inainte de a obține un numar fix de succese (r)"),
        div(
          class = "d-flex justify-content-center",
          img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/1a26b86be5f29ff5c8455dd3f357faeb8aaed623",
            style = "filter: invert(1);height:4vh;"
          ),
          img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/29944ccb6e33fb4970c050a4cc81f3b4ca9aa5b1",
            style = "filter: invert(1);height:4vh;"
          )
        ),
        div(
          class = "row",
          div(
            class = "col-4",
            tags$h3("Input:"),
            sliderInput("r", "Numarul de succese (r):", min = 1, max = 50, value = 10, step = 1),
            checkboxInput("fix_r", "Fixeaza r", value = TRUE),
            sliderInput("p", "Probabilitatea de succes (p):", min = 0.01, max = 1, value = 0.5, step = 0.01),
            checkboxInput("fix_p", "Fixeaza p", value = FALSE)
          ),
          div(
            class = "col-8",
            h4("Reprezentare Grafica"),
            plotOutput("mass_function_plot"),
            plotOutput("cdf_function_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values pentru animație
  rv <- reactiveValues(current_r = 10, current_p = 0.5)

  r_value <- reactive({
    req(input$r) # Verifică dacă input$r este disponibil
    if (isTRUE(input$fix_r)) input$r else rv$current_r
  })

  p_value <- reactive({
    req(input$p) # Verifică dacă input$p este disponibil
    if (isTRUE(input$fix_p)) input$p else rv$current_p
  })

  # Gestionează fixarea r și p
  observeEvent(input$fix_r, {
    if (!isTRUE(input$fix_r) && !isTRUE(input$fix_p)) {
      updateCheckboxInput(session, "fix_p", value = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fix_p, {
    if (!isTRUE(input$fix_r) && !isTRUE(input$fix_p)) {
      updateCheckboxInput(session, "fix_r", value = TRUE)
    }
  }, ignoreInit = TRUE)

  # Animație pentru modificarea valorilor r și p
  observe({
    invalidateLater(500, session) # Reevaluează la fiecare 500 ms

    isolate({
      if (!isTRUE(input$fix_r)) {
        rv$current_r <- rv$current_r + 0.5
        if (rv$current_r > 50) rv$current_r <- 1
      }

      if (!isTRUE(input$fix_p)) {
        rv$current_p <- rv$current_p + 0.01
        if (rv$current_p > 1) rv$current_p <- 0.01
      }
    })
  })

  # Plot funcția de masă
  output$mass_function_plot <- renderPlot({
    r <- r_value()
    p <- p_value()

    x <- 0:100
    y <- dnbinom(x, size = r, prob = p)
    dataframe <- data.frame(Esecuri = x, Probabilitate = y)

    ggplot(dataframe, aes(x = Esecuri, y = Probabilitate)) +
      geom_bar(stat = "identity", fill = "#E69F00", alpha = 0.8) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Functia de masa (PMF)",
        x = "Numarul de esecuri",
        y = "Probabilitate"
      ) +
      theme(
        text = element_text(color = "#FFF"),
        plot.background = element_rect(fill = "#101010"),
        panel.background = element_rect(fill = "#101010"),
        axis.text = element_text(color = "#FFF"),
        axis.title = element_text(color = "#FFF")
      )
  })

  # Plot funcția de repartiție
  output$cdf_function_plot <- renderPlot({
    r <- r_value()
    p <- p_value()

    x <- 0:100
    y <- pnbinom(x, size = r, prob = p)
    df <- data.frame(Esecuri = x, Probabilitate_Cumulata = y)

    ggplot(df, aes(x = Esecuri, y = Probabilitate_Cumulata)) +
      geom_line(color = "#E69F00", size = 1.5) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Functia de repartitie cumulativa (CDF)",
        x = "Numarul de esecuri",
        y = "Probabilitate Cumulata"
      ) +
      theme(
        text = element_text(color = "#FFF"),
        plot.background = element_rect(fill = "#101010"),
        panel.background = element_rect(fill = "#101010"),
        axis.text = element_text(color = "#FFF"),
        axis.title = element_text(color = "#FFF")
      )
  })
}


shinyApp(ui = ui, server = server)
