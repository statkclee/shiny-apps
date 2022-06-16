
## 연속형: 와이블-분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_weibull_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_weibull")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_weibull")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_weibull")) |> 
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
     tags$h4("Weibull: 와이블-분포"),
     hr(),
     
     # *. 와이블-분포 ----------------------------------------
     numericInput( ns("alpha_weibull"), "형상(Shape) \\(\\alpha\\):",
                  value = 5, min = 0, step = 1
     ),
     numericInput( ns("beta_weibull"), "척도(Scale) \\(\\beta\\):",
                  value = 1, min = 0, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_weibull"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     tags$hr(),
     conditionalPanel(
       # condition = " input.lower_tail_weibull == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_weibull"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_weibull"), "x:",
                    value = 0.8, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_weibull == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_weibull"), "'] == 'upper.tail'"),       
       numericInput( ns("x2_weibull"), "x:",
                    value = 0.8, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_weibull == 'interval'",
       condition = paste0("input['", ns("lower_tail_weibull"), "'] == 'interval'"),              
       numericInput( ns("a_weibull"), "a:",
                    value = 0.8, min = 0, step = 1
       ),
       numericInput( ns("b_weibull"), "b: \\( (a \\leq b) \\)",
                    value = 1.2, min = 0, step = 1
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
dist_continuous_weibull_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_weibull <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Weibull(\\alpha = \\)", " ", input$alpha_weibull, ", ", "\\(\\beta = \\)", " ", input$beta_weibull, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_weibull == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x1_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)),
          input$lower_tail_weibull == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x2_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), 4)),
          input$lower_tail_weibull == "interval" ~ paste0("\\(P(\\)", input$a_weibull, " ", "\\(\\leq X\\leq \\)", " ", input$b_weibull, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_weibull > input$b_weibull, "a must be less than or equal to b", round(pweibull(input$b_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE) - pweibull(input$a_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    weibull_rv_plot <- reactive({
      res <- if ( input$lower_tail_weibull == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
          y[x > input$x1_weibull] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_weibull == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
          y[x < input$x2_weibull] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_weibull == 'interval') {
        funcShaded <- function(x) {
          y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
          y[x < input$a_weibull | x > input$b_weibull] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_weibull <- renderPlot({
      weibull_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_weibull <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\dfrac{\\alpha}{\\beta} \\big(\\dfrac{x}{\\beta}\\big)^{\\alpha-1} e^{-(x / \\beta)^\\alpha} $$"),
        helpText("여기서, \\( x > 0, \\alpha >0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\beta\\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$mu, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\beta^2\\Big(\\Gamma\\big(1 + \\dfrac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big)^2\\Big)} = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\beta^2\\Big(\\Gamma\\big(1 + \\dfrac{2}{\\alpha}\\big) - \\Gamma\\big(1 + \\dfrac{1}{\\alpha}\\big)^2\\Big) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma^2, 3))
      )
    })
  })
}
