
## 연속형: 감마분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_gamma_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_gamma")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_gamma")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_gamma")) |> 
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
     tags$h4("Gamma: 감마분포"),
     hr(),
     
     # *. 지수분포 ----------------------------------------
     numericInput( ns("alpha_gamma"), "형상모수(Shape) \\(\\alpha\\):",
                  value = 3, min = 0, step = 1
     ),
     numericInput( ns("beta_gamma"), "척도모수(Rate) \\(\\beta\\):",
                  value = 2, min = 0, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_gamma"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = "input.lower_tail_gamma == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_gamma"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_gamma"), "x:",
                    value = 2.4, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_gamma == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_gamma"), "'] == 'upper.tail'"),       
       numericInput( ns("x2_gamma"), "x:",
                    value = 2.4, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_gamma == 'interval'",
       condition = paste0("input['", ns("lower_tail_gamma"), "'] == 'interval'"),              
       numericInput( ns("a_gamma"), "a:",
                    value = 0.8, min = 0, step = 1
       ),
       numericInput( ns("b_gamma"), "b: \\( (a \\leq b) \\)",
                    value = 2.4, min = 0, step = 1
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
dist_continuous_gamma_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_gamma <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Gamma(\\alpha = \\)", " ", input$alpha_gamma, ", ", "\\(\\beta = \\)", " ", input$beta_gamma, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_gamma == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x1_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)),
          input$lower_tail_gamma == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x2_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), 4)),
          input$lower_tail_gamma == "interval" ~ paste0("\\(P(\\)", input$a_gamma, " ", "\\(\\leq X\\leq \\)", " ", input$b_gamma, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_gamma > input$b_gamma, "a must be less than or equal to b", round(pgamma(input$b_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE) - pgamma(input$a_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    gamma_rv_plot <- reactive({
      res <- if ( input$lower_tail_gamma == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
          y[x > input$x1_gamma] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_gamma == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
          y[x < input$x2_gamma] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_gamma == 'interval') {
        funcShaded <- function(x) {
          y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
          y[x < input$a_gamma | x > input$b_gamma] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_gamma <- renderPlot({
      gamma_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_gamma <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\dfrac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha -1}e^{-\\beta x} $$"),
        helpText("여기서, \\( x > 0, \\alpha > 0, \\beta > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{\\alpha}{\\beta} = \\)", round(input$alpha_gamma / input$beta_gamma, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{\\alpha}{\\beta^2}} = \\)", round(sqrt(input$alpha_gamma / (input$beta_gamma^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{\\alpha}{\\beta^2} = \\)", round(input$alpha_gamma / (input$beta_gamma^2), 3))
      )
    })
  })
}
