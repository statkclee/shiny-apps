
## 연속형: 카이제곱 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_chisquare_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_chisquare")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_chisquare")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_chisquare")) |> 
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
     tags$h4("Chi-Square: 카이제곱분포"),
     hr(),
     
     # *. 카이제곱분포 ----------------------------------------
     numericInput( ns("df_chisquare"), "자유도 \\(df\\):",
                  value = 6, min = 1, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_chisquare"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = "input.lower_tail_chisquare == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_chisquare"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_chisquare"), "x:",
                    value = 9.6, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_chisquare == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_chisquare"), "'] == 'upper.tail'"),       
       numericInput( ns("x2_chisquare"), "x:",
                    value = 9.6, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_chisquare == 'interval'",
       condition = paste0("input['", ns("lower_tail_chisquare"), "'] == 'interval'"),              
       numericInput( ns("a_chisquare"), "a:",
                    value = 9.6, min = 0, step = 1
       ),
       numericInput( ns("b_chisquare"), "b: \\( (a \\leq b) \\)",
                    value = 14.4, min = 0, step = 1
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
dist_continuous_chisquare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_chisquare <- renderUI({
      withMathJax(
        paste0("\\(X \\sim \\chi^2(df = \\)", " ", input$df_chisquare, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_chisquare == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x1_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)),
          input$lower_tail_chisquare == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x2_chisquare, df = input$df_chisquare, lower.tail = FALSE), 4)),
          input$lower_tail_chisquare == "interval" ~ paste0("\\(P(\\)", input$a_chisquare, " ", "\\(\\leq X\\leq \\)", " ", input$b_chisquare, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_chisquare > input$b_chisquare, "a must be less than or equal to b", round(pchisq(input$b_chisquare, df = input$df_chisquare, lower.tail = TRUE) - pchisq(input$a_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    chisquare_rv_plot <- reactive({
      res <- if ( input$lower_tail_chisquare == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$df_chisquare)
          y[x > input$x1_chisquare] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Chi(", input$df_chisquare, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (  input$lower_tail_chisquare == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$df_chisquare)
          y[x < input$x2_chisquare] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Chi(", input$df_chisquare, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (  input$lower_tail_chisquare == 'interval') {
        funcShaded <- function(x) {
          y <- dchisq(x, df = input$df_chisquare)
          y[x < input$a_chisquare | x > input$b_chisquare] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: Chi(", input$df_chisquare, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_chisquare <- renderPlot({
      chisquare_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_chisquare <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ f(x) = \\dfrac{1}{2^{df/2}\\Gamma\\big(\\dfrac{df}{2}\\big)} x^{df/2 - 1} e^{-x/2} $$"),
        helpText("여기서, \\( x > 0, df > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = df = \\)", round(input$df_chisquare, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{2df} = \\)", round(sqrt(2 * input$df_chisquare), 3)),
        helpText("\\(\\sigma^2 = Var(X) = 2df = \\)", round(2 * input$df_chisquare, 3))
      )
    })
  })
}
