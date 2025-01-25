
create_pois_slider <- function() {
  sliderInput("pois_lambda", "λ:", min = 0.1, max = 10, value = 1, step = 0.1)
}

create_pois_plot_X <- function() {
  # code goes here
}


pois_server <- function (input, output, session) {

  # get reactive values for pois
  pois_lambda <- reactive(input$pois_lambda)

}