library(shiny)
library(mathjaxr)

ui <- fluidPage(
  titlePanel("AS-AD-Modell"),
  withMathJax(helpText(
    "$$C=c_0+c_1(Y-T),\\quad I=b_0-b_1r, \\quad G=\\bar{G}, \\quad T=\\bar{T},\\quad
    \\frac{M^d}{P}=d_1Y-d_2r, \\quad \\frac{M^s}{P}=\\frac{\\bar{M}}{P}, \\quad
    P_{SRAS}=\\alpha(Y-\\bar{Y})+P^e$$"
  )),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId="c_0", label="c_0", value = 100, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "c_1", label = "c_1", value = 0.8, min = 0.01, max = 0.99
      ),
      sliderInput(
        inputId = "T", label = "T", value = 100, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "b_0", label = "b_0", value = 160, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "b_1", label = "b_1", value = 140, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "G", label = "G", value = 100, min = 0.01, max = 400
      ),
      sliderInput(
        inputId = "M_bar", label = "M_bar", value = 175, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "d_1", label = "d_1", value = 0.2, min = 0.01, max = 10
      ),
      sliderInput(
        inputId = "d_2", label = "d_2", value = 350, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId="c_0_base", label="c_0_base", value = 100, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "c_1_base", label = "c_1_base", value = 0.8, min = 0.01, max = 0.99
      ),
      sliderInput(
        inputId = "T_base", label = "T_base", value = 100, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "b_0_base", label = "b_0_base", value = 160, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "b_1_base", label = "b_1_base", value = 140, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "G_base", label = "G_base", value = 100, min = 0.01, max = 400
      ),
      sliderInput(
        inputId = "M_bar_base", label = "M_bar_base", value = 175, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "d_1_base", label = "d_1_base", value = 0.2, min = 0.01, max = 10
      ),
      sliderInput(
        inputId = "d_2_base", label = "d_2_base", value = 350, min = 0.01, max = 1000
      ),
      sliderInput(
        inputId = "alpha_base", label = "alpha_base", value = 0.001, min = 0.0001, max = 0.1
      ),
      sliderInput(
        inputId = "P_e_anfang", label = "P_e_anfang", value = 1, min = 0.01, max = 5
      ),
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 600px; overflow-y: auto; }'
      ))
    ),
    mainPanel(
      plotOutput('graph', click = "plot_click"),
      fluidRow(
        column(width = 6,h4("Gleichgewichtswerte"), tableOutput('table'))
      ),
      tableOutput("info")
    )
  )
)
