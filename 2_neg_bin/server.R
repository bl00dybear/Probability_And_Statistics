source("server/form_1.R")
source("server/form_2.R")
source("server/form_3.R")
source("server/form_4.R")
source("server/form_5.R")
source("server/global.R")


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