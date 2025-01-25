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
        ),
        div(
          h2("1. Controlul calitatii in fabricare"),
          h3("Situatie:"),
          p("Un producator de componente electronice testeaza piesele produse pe linia de asamblare. Fiecare piesa are o probabilitate de p=0.9 sa treaca testul de control al calitatii (adica sa fie un succes). Producatorul doreste sa determine cate piese defecte (esecuri) se vor intalni inainte ca 10 piese sa fie calificate drept conforme (r=10)."),
          h3("Numar de esecuri:"),
          p("Aceasta se refera la numarul de piese defecte inainte ca 10 piese sa treaca testul."),
          h3("Aplicatie a repartitiei:"),
          p("Se utilizeaza formula din repartitia negativ binomiala pentru a calcula probabilitatea ca un anumit numar de piese defecte sa fie intalnite inainte de atingerea celor 10 succese."),

          h2("2. Experiment de marketing digital"),
          h3("Situatie:"),
          p("Un specialist in marketing lanseaza o campanie de reclame online. Fiecare click pe reclama este considerat un succes, iar probabilitatea unui click este p=0.05. Specialistul vrea sa afle cate afisari fara click (esecuri) sunt necesare inainte de a obtine 5 click-uri (r=5)."),
          h3("Numar de esecuri:"),
          p("Numarul de afisari ale reclamelor fara click-uri reprezinta numarul de esecuri inainte de atingerea celor 5 click-uri dorite."),
          h3("Aplicatie a repartitiei:"),
          p("Se aplica repartitia negativ binomiala pentru a calcula probabilitatea ca un anumit numar de afisari fara click sa aiba loc inainte ca cele 5 click-uri sa fie inregistrate.")
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
            sliderInput("r2", "Numărul de succese (r):", min = 1, max = 50, value = 10, step = 1),
            checkboxInput("fix_r2", "Fixează r", value = TRUE),
            sliderInput("p2", "Probabilitatea de succes (p):", min = 0.01, max = 1, value = 0.5, step = 0.01),
            checkboxInput("fix_p2", "Fixează p", value = FALSE)
          ),
          div(
            class = "col-8",
            h4("Reprezentare Grafică"),
            plotOutput("mass_function_plot2"),
            plotOutput("cdf_function_plot2")
          )
        ),
        div(
          h2("1. Controlul calitatii in fabricare"),
          h3("Situatie:"),
          p("Un producator de componente electronice testeaza piesele produse pe linia de asamblare. Fiecare piesa are o probabilitate de p=0.9 sa treaca testul de control al calitatii (adica sa fie un succes). Producatorul doreste sa determine cate piese defecte (esecuri) se vor intalni inainte ca 10 piese sa fie calificate drept conforme (r=10)."),
          h3("Numar de esecuri:"),
          p("Aceasta se refera la numarul de piese defecte inainte ca 10 piese sa treaca testul."),
          h3("Aplicatie a repartitiei:"),
          p("Se utilizeaza formula din repartitia negativ binomiala pentru a calcula probabilitatea ca un anumit numar de piese defecte sa fie intalnite inainte de atingerea celor 10 succese."),

          h2("2. Experiment de marketing digital"),
          h3("Situatie:"),
          p("Un specialist in marketing lanseaza o campanie de reclame online. Fiecare click pe reclama este considerat un succes, iar probabilitatea unui click este p=0.05. Specialistul vrea sa afle cate afisari fara click (esecuri) sunt necesare inainte de a obtine 5 click-uri (r=5)."),
          h3("Numar de esecuri:"),
          p("Numarul de afisari ale reclamelor fara click-uri reprezinta numarul de esecuri inainte de atingerea celor 5 click-uri dorite."),
          h3("Aplicatie a repartitiei:"),
          p("Se aplica repartitia negativ binomiala pentru a calcula probabilitatea ca un anumit numar de afisari fara click sa aiba loc inainte ca cele 5 click-uri sa fie inregistrate.")
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
  r_value <- reactive({
  if (!is.null(input$fix_r) && input$fix_r) {
    input$r
  } else {
    rv$current_r
  }
})

  p_value <- reactive({
    if (!is.null(input$fix_p) && input$fix_p) input$p else rv$current_p
  })
  # observeEvent e ca un trigger cu parametrii:
  # evenimentul ce trebuie urmarit, codul care se va executra cand expresia se modifica
  # nu permitem sa fie ambele casute debifatezx
  observeEvent(input$fix_r, {
  if (isTRUE(input$fix_r) && !isTRUE(input$fix_p)) {
      updateCheckboxInput(session, "fix_p", value = TRUE)
    }
  })

  observeEvent(input$fix_p, {
    if (!isTRUE(input$fix_r) && isTRUE(input$fix_p)) {
      updateCheckboxInput(session, "fix_r", value = TRUE)
    }
  })


  observe({
    invalidateLater(500, session)# expresia reactiva care activeaza observe

    #folosim isolate pentru a nu avea reactivitate extra
    #adica modificam valorile fara a declansa o reactie dupa modificare
    isolate({
      if (!is.null(input$fix_r) && !isTRUE(input$fix_r))  {
        rv$current_r <- rv$current_r + 0.5
        if (rv$current_r > 50) rv$current_r <- 1
      }

      if (!is.null(input$fix_p) && !isTRUE(input$fix_p)) {
        rv$current_p <- rv$current_p + 0.01
        if (rv$current_p > 1) rv$current_p <- 0.01
      }
    })
  })

  #renderPlot genereaza graficul functiei de masa
  output$mass_function_plot <- renderPlot({
    #functiile de extragere a parametrilor
    r <- r_value()
    p <- p_value()

    x <- 0:100 # nr de esecuri posibile inainte de a obtine cele r succese
    y <- dnbinom(x, size = r, prob = p) # functia de masa a probabilitatilor pt nb
    # x e numarul de esecuri
    # size e numarul de succese dorite
    # prob e prob de succes pt fiecare incercare
    # y e un vector de probabilitati de succes la fiecare incercare
    dataframe <- data.frame(Eșecuri = x, Probabilitate = y)

    ggplot(dataframe, aes(x = Eșecuri, y = Probabilitate)) +
      geom_bar(stat = "identity", fill = "#E69F00", alpha = 0.8) + # grafic cu bare verticale
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
    y <- pnbinom(x, size = r, prob = p) # P(X<=x) pt toate val lui x
    # size e numarul de succese
    # prob e probabilitatea de succes
    df <- data.frame(Esecuri = x, Probabilitate_Cumulata = y)

    ggplot(df, aes(x = Esecuri, y = Probabilitate_Cumulata)) +
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

  ######################################################################

  rv2 <- reactiveValues(current_r2 = 10, current_p2 = 0.5)
  r_value2 <- reactive({
    if (!is.null(input$fix_r2) && input$fix_r2) input$r2 else rv2$current_r2
  })

  p_value2 <- reactive({
    if (!is.null(input$fix_p2) && input$fix_p2) input$p2 else rv2$current_p2
  })

  observeEvent(input$fix_r2, {
    if (isTRUE(input$fix_r2) && !isTRUE(input$fix_p2)) {
      updateCheckboxInput(session, "fix_p2", value = TRUE)
    }
  })

  observeEvent(input$fix_p2, {
    if (!isTRUE(input$fix_r2) && isTRUE(input$fix_p2)) {
      updateCheckboxInput(session, "fix_r2", value = TRUE)
    }
  })

  observe({
  invalidateLater(500, session)

  isolate({
    # Verificare fix_r2
    if (!is.null(input$fix_r2) && !isTRUE(input$fix_r2)) {
      rv$current_r2 <- ifelse(is.null(rv$current_r2) || is.na(rv$current_r2), 0, rv$current_r2)
      rv$current_r2 <- rv$current_r2 + 0.5
      if (rv$current_r2 > 50) rv$current_r2 <- 1
    }

    # Verificare fix_p2
    if (!is.null(input$fix_p2) && !isTRUE(input$fix_p2)) {
      rv$current_p2 <- ifelse(is.null(rv$current_p2) || is.na(rv$current_p2), 0, rv$current_p2)
      rv$current_p2 <- rv$current_p2 + 0.01
      if (rv$current_p2 > 1) rv$current_p2 <- 0.01
    }
  })
})


  output$mass_function_plot2 <- renderPlot({
    r <- r_value2()
    p <- p_value2()

    print(r)

    x <- 0:100
    ##!!
    y <- dnbinom(x + r, size = r, prob = p) 
    dataframe2 <- data.frame(Eșecuri = x, Probabilitate = y)

    ggplot(dataframe2, aes(x = Eșecuri, y = Probabilitate)) +
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
  output$cdf_function_plot2 <- renderPlot({
    r <- r_value2()
    p <- p_value2()

    x <- 0:100
    y <- pnbinom(x + r, size = r, prob = p) 
    dataframe2 <- data.frame(Esecuri = x, Probabilitate_Cumulata = y)

    ggplot(dataframe2, aes(x = Esecuri, y = Probabilitate_Cumulata)) +
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
