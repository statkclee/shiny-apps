
## 연속형: F-분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_fisher_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_fisher")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_fisher")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_fisher")) |> 
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
     tags$h4("Fisher: F-분포"),
     hr(),
     
     # *. F-분포 ----------------------------------------
     numericInput( ns("df1_fisher"), "자유도 \\(df_1\\):",
                  value = 10, min = 1, step = 1
     ),
     numericInput( ns("df2_fisher"), "자유도 \\(df_2\\):",
                  value = 5, min = 1, step = 1
     ),
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_fisher"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     tags$hr(),
     conditionalPanel(
       # condition = "input.lower_tail_fisher == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_fisher"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_fisher"), "x:",
                    value = 3.14, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_fisher == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_fisher"), "'] == 'upper.tail'"),       
       numericInput( ns("x2_fisher"), "x:",
                    value = 3.14, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_fisher == 'interval'",
       condition = paste0("input['", ns("lower_tail_fisher"), "'] == 'interval'"),       
       numericInput( ns("a_fisher"), "a:",
                    value = 2.76, min = 0, step = 1
       ),
       numericInput( ns("b_fisher"), "b: \\( (a \\leq b) \\)",
                    value = 3.14, min = 0, step = 1
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
dist_continuous_fisher_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_fisher <- renderUI({
      withMathJax(
        paste0("\\(X \\sim F(df_1 = \\)", " ", input$df1_fisher, ", ", "\\(df_2\\)", " = ", input$df2_fisher, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_fisher == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x1_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE), 4)),
          input$lower_tail_fisher == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x2_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE), 4)),
          input$lower_tail_fisher == "interval" ~ paste0("\\(P(\\)", input$a_fisher, " ", "\\(\\leq X\\leq \\)", " ", input$b_fisher, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_fisher > input$b_fisher, "a must be less than or equal to b", round(pf(input$b_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE) - pf(input$a_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    fisher_rv_plot <- reactive({
      res <- if ( input$lower_tail_fisher == 'lower.tail') {
        funcShaded <- function(x) {
          y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
          y[x > input$x1_fisher] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
          stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_fisher == 'upper.tail') {
        funcShaded <- function(x) {
          y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
          y[x < input$x2_fisher] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
          stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_fisher == 'interval') {
        funcShaded <- function(x) {
          y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
          y[x < input$a_fisher | x > input$b_fisher] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
          stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_fisher <- renderPlot({
      fisher_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_fisher <- renderUI({
      withMathJax(
        helpText("확률 밀도 분포: $$ f(x) = \\dfrac{\\Gamma\\big(\\dfrac{df_1 + df_2}{2}\\big) \\big(\\dfrac{df_1}{df_2}\\big)^{\\dfrac{df_1}{2}}x^{\\dfrac{df_1}{2}-1}}{\\Gamma\\big(\\dfrac{df_1}{2}\\big)\\Gamma\\big(\\dfrac{df_2}{2}\\big)\\big(1 + \\dfrac{df_1 x}{df_2}\\big)^{\\dfrac{df_1 + df_2}{2}}} $$"),
        helpText("여기서, \\( df_1, df_2 > 0, x \\geq 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{df_2}{df_2 - 2} = \\)", ifelse(input$df2_fisher > 2, round(input$df2_fisher / (input$df2_fisher - 2), 3), "Undefined")),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)}} = \\)", ifelse(input$df2_fisher > 4, round(sqrt((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4))), 3), "Undefined")),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{2df^2_2(df_1 + df_2 - 2)}{df_1(df_2 - 2)^2(df_2 - 4)} = \\)", ifelse(input$df2_fisher > 4, round((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4)), 3), "Undefined"))
      )
    })
  })
}
