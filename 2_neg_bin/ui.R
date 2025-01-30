create_tab <- function(tab_title, title, img1_src, img2_src, tab_number, titlu_ex_1, ex_1, titlu_ex_2, ex_2) {
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
            style = "padding: 10px; margin-bottom: 10px;",
            plotOutput(outputId = paste0("mass_function_plot_", tab_number)),
            )
        )
      ),
      div(
        class = "row",
        div(
          class = "col-6",
          h4(titlu_ex_1),
          p(ex_1)
        ),
        div(
          class = "col-6",
          h4(titlu_ex_2),
          p(ex_2)
        )
      )
    )
  )
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
              1,
              "Testarea produselor până la găsirea unui număr fix de defecte",
              "O companie produce componente electronice și dorește să testeze câte componente trebuie verificate până când găsesc un număr fix de componente defecte (r). Fiecare test este un experiment Bernoulli: Succes (componenta este defectă, probabilitate p) sau Eșec (componenta este funcțională, probabilitate 1 - p).",
              "Numărul de încercări până la a prinde un număr fix de pești",
              "Un pescar merge la pescuit și dorește să prindă un număr fix de pești (r). Fiecare aruncare a undiței este un experiment Bernoulli: Succes (prinde un pește, probabilitate p) sau Eșec (nu prinde un pește, probabilitate 1 - p)."),
    create_tab(
              "Formularea 2", 
              "Numărul total de încercări necesare pentru a obține un număr fix de succese (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/122b97516ba764db7dfc9ecc39f68a12d1db6be3",
              2,
              "Testarea medicamentelor până la obținerea unui număr fix de rezultate pozitive",
              "Un cercetător testează un nou medicament și dorește să obțină un număr fix de pacienți care răspund pozitiv la tratament. Fiecare pacient este un experiment Bernoulli: Succes (pacientul răspunde pozitiv, probabilitate p) sau Eșec (pacientul nu răspunde pozitiv, probabilitate 1 - p).",
              "Numărul de aruncări ale unui zar până la obținerea unui număr fix de șase",
              "Un jucător aruncă un zar și dorește să obțină un număr fix de șase. Fiecare aruncare este un experiment Bernoulli: Succes (zarul arată 6, probabilitate p = 1/6) sau Eșec (zarul nu arată 6, probabilitate 1 - p)."),
    create_tab(
              "Formularea 3",
              "Numărul total de încercări necesare pentru a obține un număr fix de eșecuri (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/ccc5e37984e753e27f956045bf796f966d36e2f6",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/7fd538f62a633cdeae41ffe6c8bd2811a8371b1d",
              3,
              "Testarea produselor până la găsirea unui număr fix de produse neconforme",
              "Un controlor de calitate testează produse și dorește să găsească un număr fix de produse neconforme. Fiecare test este un experiment Bernoulli: Succes (produsul este conform, probabilitate p) sau Eșec (produsul este neconform, probabilitate 1 - p).",
              "Numărul de întrebări până la obținerea unui număr fix de răspunsuri greșite",
              "Un student răspunde la întrebări și dorește să obțină un număr fix de răspunsuri greșite. Fiecare întrebare este un experiment Bernoulli: Succes (răspunsul este corect, probabilitate p) sau Eșec (răspunsul este greșit, probabilitate 1 - p)."),
    create_tab(
              "Formularea 4",
              "Numărul de succese înainte de a obține un număr fix de eșecuri (r)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/1a26b86be5f29ff5c8455dd3f357faeb8aaed623",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/5d39d1344aec87bea613a8161cdecfbd180ff203",
              4,
              "Numărul de clienți satisfăcuți înainte de a obține un număr fix de reclamații",
              "Un magazin dorește să obțină un număr fix de reclamații de la clienți. Fiecare client este un experiment Bernoulli: Succes (clientul este satisfăcut, probabilitate p) sau Eșec (clientul depune o reclamație, probabilitate 1 - p).",
              "Numărul de lovituri la țintă înainte de a obține un număr fix de rateuri",
              "Un sportiv trage la țintă și dorește să obțină un număr fix de rateuri. Fiecare tragere este un experiment Bernoulli: Succes (lovitură la țintă, probabilitate p) sau Eșec (ratare, probabilitate 1 - p)."),
    create_tab(
              "Formularea 5",
              "Numărul de succese într-un număr fix de încercări (n)",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/d23e3ebeb017f43015cc710cb39ac89641aea136",
              "https://wikimedia.org/api/rest_v1/media/math/render/svg/09ed5ae47e20d49e0a5a42d68fe19e15326bd60e",
              5,
              "Numărul de răspunsuri corecte la un test cu un număr fix de întrebări",
              "Un student răspunde la un test cu un număr fix de întrebări. Fiecare întrebare este un experiment Bernoulli: Succes (răspunsul este corect, probabilitate p) sau Eșec (răspunsul este greșit, probabilitate 1 - p).",
              "Numărul de produse conform într-un lot de dimensiune fixă",
              "Un lot de produse este testat, iar fiecare produs este un experiment Bernoulli: Succes (produsul este conform, probabilitate p) sau Eșec (produsul este neconform, probabilitate 1 - p).")
    ),
  )