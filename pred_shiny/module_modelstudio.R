library(shiny)
library(tidymodels)
library(tidyverse)
library(r2d3)
library(modelStudio)
library(DALEX)

# 1. 모듈 UI ----------------------------
module_modelstudio_UI <- function(id) {
  
  ns <- NS(id)

  # 1. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 2,
                 
     tags$h4("타이타닉 생존확률"),
     
     tags$p("처음 뜨는데 다소 시간이 소요됩니다.....")
     
  )
  
  
  # 2. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     shinycssloaders::withSpinner(      
       uiOutput( ns('dashboard') )
     )
  )
  
  # 레이아웃 -----------------------------------------------------------
  tagList(
    withMathJax(), 
    tags$div(
      
      fluidPage(
        theme = shinythemes::shinytheme("flatly"),
        
        sidebarPanel,
        mainPanel
      )
    )
  )
}


# 2. 모듈 서버 ----------------------------
module_modelstudio_server <- function(id) {
  moduleServer(id, function(input, output, session) {

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
    
  })
}
