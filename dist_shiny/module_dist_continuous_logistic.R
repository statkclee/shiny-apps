
## 연속형: F-분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_logistic_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_logistic")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_logistic")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_logistic")) |> 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       br()
       
     )
  )
  
  # 2. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 2,
                               
     # *. 확률 분포 선택 -------------------------------------
     tags$br(),
     tags$h4("Logistic: 로지스틱-분포"),
     hr(),
     
     # *. 로지스틱-분포 ----------------------------------------
     numericInput( ns("location_logistic"), "위치 \\(\\mu\\):",
                  value = 0, step = 1
     ),
     numericInput( ns("scale_logistic"), "척도 \\(s\\):",
                  value = 1, min = 0, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_logistic"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     tags$hr(),
     conditionalPanel(
       # condition = "input.lower_tail_logistic == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_logistic"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_logistic"), "x:",
                    value = 1.2, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_logistic == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_logistic"), "'] == 'upper.tail'"),              
       numericInput( ns("x2_logistic"), "x:",
                    value = 1.2, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_logistic == 'interval'",
       condition = paste0("input['", ns("lower_tail_logistic"), "'] == 'interval'"),                     
       numericInput( ns("a_logistic"), "a:",
                    value = -1.2, step = 1
       ),
       numericInput( ns("b_logistic"), "b: \\( (a \\leq b) \\)",
                    value = 1.2, step = 1
       )
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
dist_continuous_logistic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_logistic <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Logi(\\mu = \\)", " ", input$location_logistic, ", ", "\\(s = \\)", " ", input$scale_logistic, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_logistic == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x1_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)),
          input$lower_tail_logistic == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x2_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), 4)),
          input$lower_tail_logistic == "interval" ~ paste0("\\(P(\\)", input$a_logistic, " ", "\\(\\leq X\\leq \\)", " ", input$b_logistic, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_logistic > input$b_logistic, "a must be less than or equal to b", round(plogis(input$b_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE) - plogis(input$a_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    logistic_rv_plot <- reactive({
      res <- if ( input$lower_tail_logistic == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
          y[x > input$x1_logistic] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_logistic == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
          y[x < input$x2_logistic] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_logistic == 'interval') {
        funcShaded <- function(x) {
          y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
          y[x < input$a_logistic | x > input$b_logistic] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_logistic <- renderPlot({
      logistic_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_logistic <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\dfrac{e^{-\\dfrac{x-\\mu}{s}}}{s\\Bigg(1 + e^{-\\dfrac{x - \\mu}{s}}\\Bigg)^2} $$"),
        helpText("여기서, \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, s > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\mu = \\)", round(input$location_logistic, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{s^2\\pi^2}{3}} = \\)", round(sqrt(((input$scale_logistic^2) * (pi^2)) / 3), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{s^2\\pi^2}{3} = \\)", round(((input$scale_logistic^2) * (pi^2)) / 3, 3))
      )
    })
  })
}
