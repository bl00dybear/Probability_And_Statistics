source("server/server.R")

create_tab <- function(tab_title, title, distribution) {
  tabPanel(
    tab_title,
    div(
      class = "container",
      h1(title),
      div(
        class = "row",
        div(
          class = "col-4",
          tags$h3("Input:"),
          switch(
            distribution,
            BINOMIALA = create_binom_slider(),
            NORMALA_STANDARD = create_std_normal_slider(),
            NORMALA = create_normal_slider(),
            EXPONENTIALA = create_exponential_slider(),
            POISSON = create_pois_slider()
          )
        ),
        div(
          class = "col-8",
          h4("Reprezentare Grafică"),
          get_output_distribution(distribution)
        )
      )
    )
  )
}

ui <- fluidPage(
  theme = bs_theme(
    version = 4, 
    bg = "#101010", 
    fg = "#FFF", 
    primary = "#E69F00", 
    base_font = font_google("Inconsolata")
  ),
  navbarPage(
    "CDF",
    create_tab(
      "Normala Standard",
      "Functia de repartitie (CDF)",
      NORMALA_STANDARD
    ),
    create_tab(
      "Normala",
      "Functia de repartitie (CDF)",
      NORMALA
    ),
    create_tab(
      "Exponentiala",
      "Functia de repartitie (CDF)",
      EXPONENTIALA
    ),
    create_tab(
      "Binomiala",
      "Functia de repartitie (CDF)",
      BINOMIALA
    ),
    create_tab(
      "Poisson",
      "Functia de repartitie (CDF)",
      POISSON
    )
  )
)

