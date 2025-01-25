
library(shiny)
library(bslib)

source("server/poisson.R")
source("server/binom.R")

# Functie pentru a crea o fila
create_tab <- function(tab_title, title, img1_src, img2_src, distribution) {
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
          # sliderInput(paste0("r", tab_number), "Numărul de succese (r):", min = 1, max = 50, value = 10, step = 1),
          # checkboxInput(paste0("fix_r", tab_number), "Fixează r", value = TRUE),
          # sliderInput(paste0("p", tab_number), "Probabilitatea de succes (p):", min = 0.01, max = 1, value = 0.5, step = 0.01),
          # checkboxInput(paste0("fix_p", tab_number), "Fixează p", value = FALSE)

          switch(
            distribution,
            # NORMALA_STANDARD = create_norm_std_slider(),
            # NORMALA = create_norm_slider(),
            BINOMIALA = create_binom_slider(),
            # EXPONENTIALA = create_exp_slider(),
            POISSON = create_pois_slider()
          )
        ),
        div(
          class = "col-8",
          h4("Reprezentare Grafică"),
          get_output_distribution(distribution)

          # # switch pentru a selecta tipul de distributie
          # switch(
          #   distribution,
          #   NORMALA_STANDARD = plotOutput("norm_std_server"),
          #   NORMALA = textOutput("norm_server"),
          #   BINOMIALA = textOutput("binom_server"),
          #   EXPONENTIALA = textOutput("exp_server"),
          #   POISSON = textOutput("pois_server")
          # ),

        )
      )
    )
  )
}

# Definirea UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bg = "#101010", fg = "#FFF", primary = "#E69F00", base_font = font_google("Inconsolata")),
  navbarPage(
    "Distributii",
    create_tab(
      "Normala Standard",
      "O distributie normala cu media 0 si deviatia standard 1.",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      NORMALA_STANDARD
    ),
    create_tab(
      "Normala",
      "O distributie de probabilitate continua caracterizata de o medie (μ) si o deviatie standard (σ).",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      NORMALA
    ),
    create_tab(
      "Binomiala",
      "O distributie de probabilitate discreta a numarului de succese intr-un numar fix de incercari independente.",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      BINOMIALA
    ),
    create_tab(
      "Exponentiala",
      "O distributie de probabilitate continua care descrie timpul dintre evenimente intr-un proces Poisson.",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      EXPONENTIALA
    ),
    create_tab(
      "Poisson",
      "O distributie de probabilitate discreta care exprima probabilitatea unui numar dat de evenimente intr-un interval fix.",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      "https://wikimedia.org/api/rest_v1/media/math/render/svg/3b66d4401d0c06ed66ea0ddc4b4f28ced2298090",
      POISSON
    )
  )
)

