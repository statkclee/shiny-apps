library(shiny)
library(tidymodels)
library(tidyverse)
library(r2d3)
library(modelStudio)
library(DALEX)



ui <- fluidPage(
        theme = shinythemes::shinytheme("flatly"),
  
  # 1. 옆 패널 --------------------------------------
  shiny::sidebarLayout(
    sidebarPanel(width = 2,
                 
       tags$h4("타이타닉 생존확률"),
       
       tags$p("처음 뜨는데 다소 시간이 소요됩니다.....")
    ),
    
    # 2. 메인 패널 ------------------------------------
    mainPanel(width = 10,
        shinycssloaders::withSpinner(      
          uiOutput('dashboard')
        )
    )
  )
)
  
# 2. 서버 -----------------------  
server <- function(input, output) {
  
  #:# id of div where modelStudio will appear
  WIDGET_ID = 'MODELSTUDIO'
  
  #:# create modelStudio 
  
  model <- glm(survived ~., data = titanic_imputed, family = "binomial")
  explainer <- DALEX::explain(model,
                              data = titanic_imputed,
                              y = titanic_imputed$survived,
                              label = "Titanic GLM",
                              verbose = FALSE)
  
  ms <- modelStudio(explainer,
                    widget_id = WIDGET_ID,  #:# use the widget_id 
                    show_info = FALSE)
  
  ms$elementId <- NULL                      #:# remove elementId to stop the warning
  
  #:# basic render d3 output
  output[[WIDGET_ID]] <- renderD3({
    ms
  })
  
  #:# use render ui to set proper width and height
  output$dashboard <- renderUI({
    d3Output(WIDGET_ID, width=ms$width, height=ms$height)
  })  
  
}

shinyApp(ui = ui, server = server)
