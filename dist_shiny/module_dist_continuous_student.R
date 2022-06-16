
## 연속형: t-분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_student_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_student")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_student")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_student")) |> 
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
     tags$h4("t-분포"),
     hr(),
     
     # *. t-분포 ----------------------------------------
     numericInput( ns("df_student"), "자유도 \\(df\\):",
                  value = 10, min = 1, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_student"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
    
     conditionalPanel(
       # condition = "input.lower_tail_student == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_student"), "'] == 'lower.tail'"),                            
       numericInput(ns("x1_student"), "x:",
                    value = 1, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_student == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_student"), "'] == 'upper.tail'"),
       numericInput(ns("x2_student"), "x:",
                    value = 1, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_student == 'interval'",
       condition = paste0("input['", ns("lower_tail_student"), "'] == 'interval'"),       
       numericInput(ns("a_student"), "a:",
                    value = -1, step = 1
       ),
       numericInput(ns("b_student"), "b: \\( (a \\leq b) \\)",
                    value = 1, step = 1
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
dist_continuous_student_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_student <- renderUI({
      withMathJax(
        paste0("\\(X \\sim St(df = \\)", " ", input$df_student, "\\()\\)", " and ", case_when(
          input$lower_tail_student == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x1_student, df = input$df_student, lower.tail = TRUE), 4)),
          input$lower_tail_student == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x2_student, df = input$df_student, lower.tail = FALSE), 4)),
          input$lower_tail_student == "interval" ~ paste0("\\(P(\\)", input$a_student, " ", "\\(\\leq X\\leq \\)", " ", input$b_student, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_student > input$b_student, "a must be less than or equal to b", round(pt(input$b_student, df = input$df_student, lower.tail = TRUE) - pt(input$a_student, df = input$df_student, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    student_rv_plot <- reactive({
      res <- if ( input$lower_tail_student == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dt(x, df = input$df_student)
          y[x > input$x1_student] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = input$df_student)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: St(", input$df_student, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
           ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_student == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dt(x, df = input$df_student)
          y[x < input$x2_student] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = input$df_student)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: St(", input$df_student, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
           ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_student == 'interval') {
        funcShaded <- function(x) {
          y <- dt(x, df = input$df_student)
          y[x < input$a_student | x > input$b_student] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dt, args = list(df = input$df_student)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: St(", input$df_student, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
           ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_student <- renderPlot({
      student_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_student <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\dfrac{\\Gamma \\big(\\dfrac{df+1}{2}\\big)}{\\sqrt{df \\pi} \\Gamma \\big(\\dfrac{df}{2}\\big)} \\Big(1 + \\dfrac{x^2}{df}\\Big)^{-\\dfrac{df+1}{2}}$$"),
        helpText("여기서, \\( -\\infty < x < \\infty, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", ifelse(input$df_student > 1, 0, "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{df}{df - 2}} = \\)", ifelse(input$df_student > 2, round(sqrt(input$df_student / (input$df_student - 2)), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{df}{df-2} = \\)", ifelse(input$df_student > 2, round(input$df_student / (input$df_student - 2), 3), "Undefined"))
      )
    })
  })
}
