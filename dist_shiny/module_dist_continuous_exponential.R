
## 연속형: 지수분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_exponential_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_exponential")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_exponential")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_exponential")) |> 
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
     tags$h4("Exponential: 지수분포"),
     hr(),
     
     # *. 지수분포 ----------------------------------------
     numericInput( ns("rate_exponential"), "모수(Rate) \\(\\lambda\\):",
                  value = 1, min = 0, step = 0.5
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_exponential"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = " input.lower_tail_exponential == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_exponential"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_exponential"), "x:",
                    value = 2.24, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_exponential == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_exponential"), "'] == 'upper.tail'"),              
       numericInput( ns("x2_exponential"), "x:",
                    value = 2.24, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_exponential == 'interval'",
       condition = paste0("input['", ns("lower_tail_exponential"), "'] == 'interval'"),                     
       numericInput( ns("a_exponential"), "a:",
                    value = 2.24, min = 0, step = 1
       ),
       numericInput( ns("b_exponential"), "b: \\( (a \\leq b) \\)",
                    value = 3.36, min = 0, step = 1
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
dist_continuous_exponential_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_exponential <- renderUI({
      withMathJax(
        paste0("\\(X \\sim \\ Exp(\\lambda = \\)", " ", input$rate_exponential, "\\()\\)", " 그리고 ", 
               case_when(
          input$lower_tail_exponential == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x1_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)),
          input$lower_tail_exponential == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x2_exponential, rate = input$rate_exponential, lower.tail = FALSE), 4)),
          input$lower_tail_exponential == "interval" ~ paste0("\\(P(\\)", input$a_exponential, " ", "\\(\\leq X\\leq \\)", " ", input$b_exponential, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_exponential > input$b_exponential, "a must be less than or equal to b", round(pexp(input$b_exponential, rate = input$rate_exponential, lower.tail = TRUE) - pexp(input$a_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    exponential_rv_plot <- reactive({
      res <- if ( input$lower_tail_exponential == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dexp(x, rate = input$rate_exponential)
          y[x > input$x1_exponential] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Exp(", input$rate_exponential, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_exponential == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dexp(x, rate = input$rate_exponential)
          y[x < input$x2_exponential] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Exp(", input$rate_exponential, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_exponential == 'interval') {
        funcShaded <- function(x) {
          y <- dexp(x, rate = input$rate_exponential)
          y[x < input$a_exponential | x > input$b_exponential] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qexp(0.99999, rate = input$rate_exponential, lower.tail = FALSE), qexp(0.99999, rate = input$rate_exponential, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Exp(", input$rate_exponential, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_exponential <- renderPlot({
      exponential_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_exponential <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\lambda e^{-\\lambda x} $$"),
        helpText("여기서, \\( x > 0, \\lambda > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1}{\\lambda} = \\)", round(1 / input$rate_exponential, 3)),
        helpText("\\(\\sigma = SD(X) = \\dfrac{1}{\\lambda} = \\)", round(1 / input$rate_exponential, 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1}{\\lambda^2} = \\)", round(1 / (input$rate_exponential^2), 3))
      )
    })
  })
}
