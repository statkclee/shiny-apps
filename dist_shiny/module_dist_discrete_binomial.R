library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_binomial_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
                         fluidRow(
                           # 텍스트 ----
                           br(),
                           tags$b("해답:"),
                           uiOutput( ns("results_binomial")),
                           br(),
                           
                           # 그래프 ----
                           plotOutput(outputId = ns("plots_binomial")) %>% 
                             shinycssloaders::withSpinner(
                               type = 2, 
                               color.background = "white"
                             ), 
                           
                           br(),
                           tags$b("상세:"),
                           br(),
                           
                           # 상세 ----
                           uiOutput( ns("parameters_binomial")) |> 
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
                               tags$h3("Binomial: 이항분포"),
                               hr(),
                               
                               # *. 이항 분포 ----------------------------------------
                               numericInput( ns("n_binomial"), "시행 횟수 \\(n\\):",
                                             value = 20, min = 0, step = 1
                               ),
                               numericInput( ns("p_binomial"), "성공 확률 \\(p\\):",
                                             value = 0.5, min = 0, max = 1, step = 0.01
                               ),
                               radioButtons(
                                 inputId = ns("lower_tail_binomial"),
                                 label = NULL,
                                 choices = c(
                                   "아래쪽 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
                                   "위쪽 꼬리 : \\(P(X > x)\\)" = "upper.tail",
                                   "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
                                 )
                               ),
                               conditionalPanel(
                                 # condition = "input.lower_tail_binomial == 'lower.tail'",
                                 condition = paste0("input['", ns("lower_tail_binomial"), "'] == 'lower.tail'"),              
                                 numericInput( ns("x1_binomial"), "x:",
                                               value = 8, min = 0, step = 1
                                 )
                               ),
                               conditionalPanel(
                                 # condition = "input.lower_tail_binomial == 'upper.tail'",
                                 condition = paste0("input['", ns("lower_tail_binomial"), "'] == 'upper.tail'"),                     
                                 numericInput( ns("x2_binomial"), "x:",
                                               value = 8, min = 0, step = 1
                                 )
                               ),
                               conditionalPanel(
                                 # condition = "input.lower_tail_binomial == 'interval'",
                                 condition = paste0("input['", ns("lower_tail_binomial"), "'] == 'interval'"),                            
                                 numericInput( ns("a_binomial"), "a:",
                                               value = 8, min = 0, step = 1
                                 ),
                                 numericInput( ns("b_binomial"), "b: \\( (a \\leq b) \\)",
                                               value = 12, min = 0, step = 1
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
dist_discrete_binomial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_binomial <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Bin(n = \\)", " ", input$n_binomial, ", ", "\\(p = \\)", " ", input$p_binomial, "\\()\\)", "  그리고  ", 
               case_when(
                 input$lower_tail_binomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
                 input$lower_tail_binomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x2_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE), 4)),
                 input$lower_tail_binomial == "interval" ~ paste0("\\(P(\\)", input$a_binomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_binomial, "\\()\\)", " ", "\\( = \\)", " ", 
                                                                  ifelse(input$a_binomial > input$b_binomial, "a 는 b 보다 같거나 작아야 한다.", round(pbinom(input$b_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$a_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)))
               ))
      )
    })
    
    # 2. 그래프 -----------------------
    binomial_rv_plot <- reactive({
      res <- if (input$lower_tail_binomial == 'lower.tail') {
        p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
          mutate(Heads = ifelse(heads <= input$x1_binomial, "2", "Other")) %>%
          ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
          geom_col() +
          geom_text(
            aes(label = round(prob, 3), y = prob + 0.005),
            position = position_dodge(0.9),
            size = 3,
            vjust = 0
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("blue", "gray50")) +            
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " 분포: 이항분포(", input$n_binomial, ", ", input$p_binomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_binomial == 'upper.tail') {
        p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
          mutate(Heads = ifelse(heads > input$x2_binomial, "2", "other")) %>%
          ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
          geom_col() +
          geom_text(
            aes(label = round(prob, 3), y = prob + 0.005),
            position = position_dodge(0.9),
            size = 3,
            vjust = 0
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("blue", "gray50")) +
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " 분포: 이항분포(", input$n_binomial, ", ", input$p_binomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_binomial == 'interval') {
        p <- data.frame(heads = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), prob = dbinom(x = qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), size = input$n_binomial, prob = input$p_binomial)) %>%
          mutate(Heads = ifelse(heads >= input$a_binomial & heads <= input$b_binomial, "2", "other")) %>%
          ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
          geom_col() +
          geom_text(
            aes(label = round(prob, 3), y = prob + 0.005),
            position = position_dodge(0.9),
            size = 3,
            vjust = 0
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("blue", "gray50")) +            
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " 분포: 이항분포(", input$n_binomial, ", ", input$p_binomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_binomial <- renderPlot({
      binomial_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_binomial <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = \\binom{n}{x}p^x(1-p)^{n-x}$$ "),
        helpText("여기서, \\( x = 0, 1, \\dots, n\\) 그리고 \\( 0 \\leq p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = np = \\)", round(input$n_binomial * input$p_binomial, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{np(1-p)} = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = np(1-p) = \\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 3))
      )
    })
  })
}
