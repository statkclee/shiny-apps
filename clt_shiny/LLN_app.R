# 대수의 법칙

# Load packages -----------------------------------------------------
library(shiny)
library(gridExtra)
library(tidyverse)

# Define UI ---------------------------------------------------------

ui <-shinyUI(fluidPage(
  
  sidebarPanel(
      # 1. 분포 선택 ----
      tags$h4("모집단"),
      radioButtons(inputId = "dist", "모집단 분포 선택:",
                   c("정규분포" = "rnorm",
                     "지수분포" = "rexp"),
                   selected = "rnorm"),

      ## 1.1. 지수분포 모수 ----
      conditionalPanel(
        condition = "input.dist == 'rexp'",
        # condition = paste0("input['", ns("dist"), "'] == 'rexp'"),                 
        sliderInput( "lambda",
                     "모수 범위",
                     value = 1,
                     min = 1,
                     max = 20)
      ),
      ## 1.2. 정규분포 모수 ----
      conditionalPanel(
        condition = "input.dist == 'rnorm'",
        # condition = paste0("input['", ns("dist"), "'] == 'rnorm'"),
        sliderInput( "mu",
                     "평균: ",
                     value = 0,
                     min = -40,
                     max = 50),
        sliderInput( "sd",
                     "표준 편차: ",
                     value = 10,
                     min = 1,
                     max = 30)
      ),
      
      
      # 2. 관측수와 반복횟수 ----
      tags$hr(style="border-color: blue;"),      
      
      sliderInput("n", 
                  "관측점 수:", 
                  value = 100,
                  min = 10, 
                  max = 1000),
      br()
  ),
  
  mainPanel(
    
    shiny::plotOutput("LLN_plot")
    
  )
))

# Define server function --------------------------------------------

server <- shinyServer(function(input, output) {
  
  output$LLN_plot <- renderPlot({
    
    if (input$dist == "rnorm") {
      # 1. 정규분포 -----------------------
      ## LLN 모의실험 ---------------
      normal_mns    <- NULL
      
      for (i in 1:input$n) normal_mns <-  c(normal_mns, rnorm( 1, input$mu, input$sd))
      
      normal_means <- cumsum(normal_mns)/(1:input$n)
      
      ## 시각화 --------------------
      LLN_tbl <- tibble( x = 1:input$n, 
                         y = normal_means)
      
      LLN_tbl %>% 
        ggplot(aes(x=x, y=y)) +
        geom_hline(yintercept = input$mu, color = "blue") +
        geom_point(size = 0.7) +
        geom_line(size = 0.5) +
        theme_light()  +
        labs(title = "정규분포",
             x = "관측점 수",
             y= "누적평균")
      
    } else if(input$dist == "rexp") {
      # 2. 지수분포 -----------------------
      exp_mean   <- 1 / input$lambda
      
      ## LLN 모의실험 ---------------
      mns    <- NULL
      
      for (i in 1:input$n) mns <-  c(mns, rexp( 1, input$lambda))
      
      means <- cumsum(mns)/(1:input$n)
      
      ## 시각화 --------------------
      LLN_tbl <- tibble( x = 1:input$n, 
                         y = means)
      
      LLN_tbl %>% 
        ggplot(aes(x=x, y=y)) +
        geom_hline(yintercept = exp_mean, color = "blue") +
        geom_point(size = 0.7) +
        geom_line(size = 0.5) +
        theme_light()  +
        labs(title = "지수분포",
             subtitle = glue::glue("모수(Lambda): {input$lambda} ,", 
                                   "평균: {scales::comma(exp_mean, accuracy=0.2)}"),
             x = "관측점 수",
             y= "누적평균")
    }
  })
  
  
  
})

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)