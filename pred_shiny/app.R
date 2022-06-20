library(shiny)
library(tidymodels)
library(tidyverse)
library(ranger)

# *. 펭귄 성별 예측 ---------------------------------
source("module_penguin.R")

# I. UI ---------------------------------------------
ui <- shinyUI(
  
  navbarPage("통계학",
             
     ## 1. 성별 예측모형 ---------------------------------
     tabPanel("성별 예측",
          module_penguin_UI("palma-penguin")
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

# II. 서버 ---------------------------------------------
server <- shinyServer(function(input, output) {

  ## 1. 성별예측모형 ------------------------
  module_penguin_server("palma-penguin")

})


shinyApp(ui, server)