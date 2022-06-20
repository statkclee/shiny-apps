library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)


ui <- fluidPage(
        theme = shinythemes::shinytheme("flatly"),
  
  # 1. 옆 패널 --------------------------------------
  shiny::sidebarLayout(
    sidebarPanel(width = 2,
                 
       tags$h4("팔머펭귄 성별예측 시스템"),                 
                 
       tags$hr(style="border-color: blue;"),      
       
       hr(),
       sliderInput( "bill_length", "부리 길이(mm)",
                    min = 30,
                    max = 60,
                    value = 45,
                    step = 1
       ),
       sliderInput( "bill_depth", "부리 깊이(mm)",
                    min = 10,
                    max = 25,
                    value = 17,
                    step = 1
       ),
       sliderInput( "flipper_length", "물갈퀴 길이(mm)",
                    min = 170,
                    max = 250,
                    value = 200,
                    step = 1
       ),
       sliderInput( "body_mass", "체중(g)",
                    min = 2700,
                    max = 6300,
                    value = 4000,
                    step = 100
       ),
       tags$hr(style="border-color: blue;"),
       actionButton("run_bttn", "성별 예측"),
       HTML("<br/><br/>상기 입력정보를 바탕으로 펭귄 성별을 예측하고자 하면 '성별예측' 버튼을 클릭하여 확인하세요.")
    ),
    
    # 2. 메인 패널 ------------------------------------
    mainPanel(width = 10,
              
      # 성별 예측 정보 ---------------------
      column( width =  3,
              tags$h2("팔머 펭귄 성별 예측"),
              htmlOutput( "gender_predicted")
      ),
      # 입력 정보 ---------------------
      column( width = 6,
        tags$h2("팔머 펭귄 신체 입력정보"),
        div(h3(textOutput( "bill_length_info")), align = "left"),
        div(h3(textOutput( "bill_depth_info")), align = "left"),
        div(h3(textOutput( "flipper_length_info")), align = "left"),
        div(h3(textOutput( "body_mass_info")), align = "left")
      )
    )
    
  )
)
  
# 2. 서버 -----------------------  
server <- function(input, output) {
  
  ## 2.0. 실행 -----------------------  
  calc_rv <- reactiveValues(doCalc = FALSE)

  observeEvent(input$run_bttn, {
    calc_rv$doCalc <- input$run_bttn
  })
  
  ## 2.1. 예측모형 -----------------------  
  penguin_model <- readRDS(url("https://github.com/statkclee/model/raw/gh-pages/data/penguin_predictvie_model.rds", "rb"))
  
  penguin_tbl <- reactive({
    tibble("species" = "Adelie",
           "bill_length_mm" =  input$bill_length,
           "bill_depth_mm" =  input$bill_depth,
           "flipper_length_mm" =  input$flipper_length,
           "body_mass_g" = input$body_mass)
  })
  
  pred_sex <- reactive({
    predict(penguin_model, penguin_tbl()) %>% unlist %>% as.character
  })
  
  prob_sex <- reactive({
    predict(penguin_model, penguin_tbl(), type="prob")[,1]
  })
  
  
  ## 2.2. 입력정보 -----------------------  
  output$bill_length_info <- shiny::renderText({
    paste("- 부리 길이(mm): ", input$bill_length)
  })
  
  output$bill_depth_info <- shiny::renderText({
    paste("- 부리 깊이(mm): ", input$bill_depth)
  })
  
  output$flipper_length_info <- shiny::renderText({
    paste("- 물갈퀴 길이(mm): ", input$flipper_length)
  })
  
  output$body_mass_info <- shiny::renderText({
    paste("- 체중(g): ", input$body_mass)
  })
  
  ## 2.3. 출력정보 -----------------------  
  output$gender_predicted <- renderUI({
    
    if (calc_rv$doCalc == FALSE) return()
    
    isolate({
      HTML(paste("예측성별: ", pred_sex(), "\n",
            "\n암컷 예측확률: ", scales::percent(as.numeric(prob_sex()), 0.1), sep = "<br/>"))
    })
    
  })
  
  
}

shinyApp(ui = ui, server = server)
