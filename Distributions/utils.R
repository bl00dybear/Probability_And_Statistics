
# constants

# distribution names
POISSON <- "POISSON"
NORMALA_STANDARD <- "NORMALA_STANDARD"
NORMALA <- "NORMALA"
BINOMIALA <- "BINOMIALA"
EXPONENTIALA <- "EXPONENTIALA"


# main decision function for the distribution switch
get_output_distribution <- function(distribution) {
    switch(
        distribution,
        NORMALA_STANDARD = list(
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("std_normal_var1"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("std_normal_var2"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("std_normal_var3"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("std_normal_var4"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("std_normal_var5")
        ),
        NORMALA = list(),
        BINOMIALA = list(),
        EXPONENTIALA = list(),
        POISSON = list(
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("pois_plot_X"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("pois_plot_X_transformed"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("pois_plot_X2"),
          tags$div(style = "margin-bottom: 20px;"), 
          plotOutput("pois_plot_X_sum")
        )
    )
}
