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
        h1("Numărul de eșecuri înainte de a obține un număr fix de succese (r)"),
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
            sliderInput("r", "Numărul de succese (r):", min = 1, max = 50, value = 10, step = 1),
            checkboxInput("fix_r", "Fixează r", value = TRUE),
            sliderInput("p", "Probabilitatea de succes (p):", min = 0.01, max = 1, value = 0.5, step = 0.01),
            checkboxInput("fix_p", "Fixează p", value = FALSE)
          ),
          div(
            class = "col-8",
            h4("Reprezentare Grafică"),
            plotOutput("mass_function_plot"),
            plotOutput("cdf_function_plot")
          )
        )
      )
    
    ),
    tabPanel(
      "Formularea 2",
      div(
        class = "container",
        h1("Numărul total de încercări necesare pentru a obține un număr fix de succese (r)"),
        div(
            class="d-flex justify-content-center",
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
            style = "filter: invert(1);height:4vh;"
            ),
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/122b97516ba764db7dfc9ecc39f68a12d1db6be3",
            style = "filter: invert(1);height:4vh;"
            )
          ),
        div(
          class = "row", 
          div(
            class = "col-4", 
            tags$h3("Input:"),
            textInput("txt3", "Favorite Color:", ""),
            textInput("txt4", "Favorite Animal:", "")
          ),
          div(
            class = "col-8", 
            h1("Header 2"),
            h4("Output 2"),
            verbatimTextOutput("txtout2")
          )
        )
      )
    ),
    tabPanel(
      "Formularea 3",
      div(
        class = "container",
        h1("Numărul total de încercări necesare pentru a obține un număr fix de eșecuri (r)"),
        div(
            class="d-flex justify-content-center",
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
            style = "filter: invert(1);height:4vh;"
            ),
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/7fd538f62a633cdeae41ffe6c8bd2811a8371b1d",
            style = "filter: invert(1);height:4vh;"
            )
          ),
        div(
          class = "row", 
          div(
            class = "col-4", 
            tags$h3("Input:"),
            textInput("txt3", "Favorite Color:", ""),
            textInput("txt4", "Favorite Animal:", "")
          ),
          div(
            class = "col-8", 
            h1("Header 2"),
            h4("Output 2"),
            verbatimTextOutput("txtout2")
          )
        )
      )
    ),
    tabPanel(
      "Formularea 4",
      div(
        class = "container",
        h1("Numărul de succese înainte de a obține un număr fix de eșecuri (r)"),
        div(
            class="d-flex justify-content-center",
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/1a26b86be5f29ff5c8455dd3f357faeb8aaed623",
            style = "filter: invert(1);height:4vh;"
            ),
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/5d39d1344aec87bea613a8161cdecfbd180ff203",
            style = "filter: invert(1);height:4vh;"
            )
          ),
        div(
          class = "row", 
          div(
            class = "col-4", 
            tags$h3("Input:"),
            textInput("txt3", "Favorite Color:", ""),
            textInput("txt4", "Favorite Animal:", "")
          ),
          div(
            class = "col-8", 
            h1("Header 2"),
            h4("Output 2"),
            verbatimTextOutput("txtout2")
          )
        )
      )
    ),
    tabPanel(
      "Formularea 5",
      div(
        class = "container",
        h1("Numărul de succese într-un număr fix de încercări (n)"),
        div(
            class="d-flex justify-content-center",
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/d23e3ebeb017f43015cc710cb39ac89641aea136",
            style = "filter: invert(1);height:4vh;"
            ),
            img(
            src = "https://wikimedia.org/api/rest_v1/media/math/render/svg/09ed5ae47e20d49e0a5a42d68fe19e15326bd60e",
            style = "filter: invert(1);height:4vh;"
            )
          ),
        div(
          class = "row", 
          div(
            class = "col-4", 
            tags$h3("Input:"),
            textInput("txt3", "Favorite Color:", ""),
            textInput("txt4", "Favorite Animal:", "")
          ),
          div(
            class = "col-8", 
            h1("Header 2"),
            h4("Output 2"),
            verbatimTextOutput("txtout2")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values pentru animație (initializează fără `input$r`)
  rv <- reactiveValues(current_r = 10, current_p = 0.5)
  
  # Reactive expressions pentru r și p
  r_value <- reactive({
    if (input$fix_r) input$r else rv$current_r
  })
  
  p_value <- reactive({
    if (input$fix_p) input$p else rv$current_p
  })
  
  # Restricție: nu permite ambele checkbox-uri să fie debifate simultan
  observeEvent(input$fix_r, {
    if (!input$fix_r && !input$fix_p) {
      updateCheckboxInput(session, "fix_p", value = TRUE)
    }
  })
  
  observeEvent(input$fix_p, {
    if (!input$fix_r && !input$fix_p) {
      updateCheckboxInput(session, "fix_r", value = TRUE)
    }
  })
  
  # Timer pentru animație
  observe({
    invalidateLater(1000, session)
    
    isolate({
      if (!input$fix_r) {
        rv$current_r <- rv$current_r + 0.5
        if (rv$current_r > 50) rv$current_r <- 1
      }
      
      if (!input$fix_p) {
        rv$current_p <- rv$current_p + 0.01
        if (rv$current_p > 1) rv$current_p <- 0.01
      }
    })
  })
  
  # Reprezentare funcția de masă
  output$mass_function_plot <- renderPlot({
    r <- r_value()
    p <- p_value()
    
    x <- 0:100
    y <- dnbinom(x, size = r, prob = p)
    df <- data.frame(Eșecuri = x, Probabilitate = y)
    
    ggplot(df, aes(x = Eșecuri, y = Probabilitate)) +
      geom_bar(stat = "identity", fill = "#E69F00", alpha = 0.8) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Funcția de masă (PMF)",
        x = "Numărul de eșecuri",
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
  
  # Reprezentare funcția de repartiție
  output$cdf_function_plot <- renderPlot({
    r <- r_value()
    p <- p_value()
    
    x <- 0:100
    y <- pnbinom(x, size = r, prob = p)
    df <- data.frame(Eșecuri = x, Probabilitate_Cumulată = y)
    
    ggplot(df, aes(x = Eșecuri, y = Probabilitate_Cumulată)) +
      geom_line(color = "#E69F00", size = 1.5) +
      theme_minimal(base_family = "Inconsolata") +
      labs(
        title = "Funcția de repartiție cumulativă (CDF)",
        x = "Numărul de eșecuri",
        y = "Probabilitate Cumulată"
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
