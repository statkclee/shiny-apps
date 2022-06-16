library(shiny)
library(plotly)


### *. 1. 단순선형회귀 ----------------------------------
source("module_regression_simple.R", encoding = "UTF-8")

ui <- shinyUI(

  navbarPage("통계학",
             
    # I. 분포 ---------------------------------
    tabPanel("통계 모형",
       tags$iframe(style="height:768px; width:100%; scrolling=yes", 
                   src="linear_tests_cheat_sheet.pdf")
    ),
             
    # II. 회귀분석 ---------------------------------             
    tabPanel("단순 회귀모형",
       module_reg_simple_UI("regression_simple")
     ),
     
    # IV. footer.html ---------------------------------                   
    tags$footer(
      tags$div(
        class = "footer_container", 
        includeHTML(path = "www/footer.html")
      )
    )
  )
)


server <- shinyServer(function(input, output) {
  
  # # I. 회귀모형 ------------------------

  module_reg_simple_server("regression_simple")
})


shinyApp(ui, server)