library(shiny)
library(shinydashboard)
library(tidyverse)

# header <- dashboardHeader()
# sidebar <- dashboardSidebar()
# body <- dashboardBody()
# dashboardPage(header, sidebar, body)

header <- dashboardHeader(title = "추론 (Inference)")

sidebar <- dashboardSidebar(
      selectInput(
        inputId = "inference",
        label = "Inference for:",
        choices = c("one mean", "two means (independent samples)", "two means (paired samples)", "one proportion", "two proportions", "one variance", "two variances"),
        multiple = FALSE,
        selected = "one mean"
      ),
      hr(),
      conditionalPanel(
        condition = "input.inference == 'one mean'",
        textInput("sample_onemean", "Sample", value = "0.9, -0.8, 1.3, -0.3, 1.7", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        hr(),
        checkboxInput("popsd_onemean", "Variance of the population is known", FALSE),
        conditionalPanel(
          condition = "input.popsd_onemean == 1",
          numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                       value = 1, min = 0, step = 1
          )
        )
      ),
      hr(),
      tags$b("Null hypothesis"),
      conditionalPanel(
        condition = "input.inference == 'one mean'",
        sprintf("\\( H_0 : \\mu = \\)")
      ),
      conditionalPanel(
        condition = "input.inference != 'two variances'",
        numericInput("h0",
                     label = NULL,
                     value = 0.1, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.inference != 'two variances'",
        radioButtons(
          inputId = "alternative",
          label = "Alternative",
          choices = c(
            "\\( \\neq \\)" = "two.sided",
            "\\( > \\)" = "greater",
            "\\( < \\)" = "less"
          )
        )
      ),
      hr(),
      sliderInput("alpha",
                  "Significance level \\(\\alpha = \\)",
                  min = 0.01,
                  max = 0.20,
                  value = 0.05
      )
)

body <- 
  dashboardBody(
              mainPanel(
                br(),
                conditionalPanel(
                  condition = "input.inference == 'one mean'",
                  uiOutput("results_onemean")
                ),
                br(),
                br(),
                plotOutput("plot"),
                br(),
                br()
              )
      )

dashboardPage(header, 
              sidebar, 
              body)