library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_hypergeometric_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
   fluidRow(
     # 텍스트 ----
     br(),
     tags$b("해답:"),
     uiOutput( ns("results_hypergeometric")),
     br(),
     
     # 그래프 ----
     plotOutput(outputId = ns("plots_hypergeometric")) %>% 
       shinycssloaders::withSpinner(
         type = 2, 
         color.background = "white"
       ), 
     
     br(),
     tags$b("상세:"),
     br(),
     
     # 상세 ----
     uiOutput( ns("parameters_hypergeometric")) |> 
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
     tags$h4("초기하 분포"),
     hr(),
     
     # *. 초기하 분포 ----------------------------------------
     numericInput( ns("n_hypergeometric"), "표본 크기 \\(n\\):",
                  value = 100, min = 0, step = 1
     ),
     numericInput( ns("N_hypergeometric"), "총 객체수 \\(N\\):",
                  value = 500, min = 0, step = 1
     ),
     numericInput( ns("M_hypergeometric"), "성공 횟수 \\(M\\):",
                  value = 50, min = 0, step = 1
     ),
     
     radioButtons(
       inputId = ns("lower_tail_hypergeometric"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = "input.lower_tail_hypergeometric == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_hypergeometric"), "'] == 'lower.tail'"),                     
       numericInput( ns("x1_hypergeometric"), "x:",
                    value = 8, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_hypergeometric == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_hypergeometric"), "'] == 'upper.tail'"),                     
       numericInput( ns("x2_hypergeometric"), "x:",
                    value = 8, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_hypergeometric == 'interval'",
       condition = paste0("input['", ns("lower_tail_hypergeometric"), "'] == 'interval'"),                            
       numericInput( ns("a_hypergeometric"), "a:",
                    value = 8, min = 0, step = 1
       ),
       numericInput( ns("b_hypergeometric"), "b: \\( (a \\leq b) \\)",
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
dist_discrete_hypergeometric_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_hypergeometric <- renderUI({
      withMathJax(
        paste0("\\(X \\sim HG(n = \\)", " ", input$n_hypergeometric, ", ", "\\(N = \\)", " ", input$N_hypergeometric, ", ", "\\(M = \\)", " ", input$M_hypergeometric, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_hypergeometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
          input$lower_tail_hypergeometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x2_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE), 4)),
          input$lower_tail_hypergeometric == "interval" ~ paste0("\\(P(\\)", input$a_hypergeometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_hypergeometric > input$b_hypergeometric, "a must be less than or equal to b", round(phyper(input$b_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$a_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    hypergemetric_rv_plot <- reactive({
      res <- if (input$lower_tail_hypergeometric == 'lower.tail') {
        p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
          mutate(Heads = ifelse(heads <= input$x1_hypergeometric, "2", "Other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_hypergeometric == 'upper.tail') {
        p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
          mutate(Heads = ifelse(heads > input$x2_hypergeometric, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_hypergeometric == 'interval') {
        p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
          mutate(Heads = ifelse(heads >= input$a_hypergeometric & heads <= input$b_hypergeometric, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_hypergeometric <- renderPlot({
      hypergemetric_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_hypergeometric <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = \\dfrac{\\binom{M}{x} \\binom{N-M}{n-x}}{\\binom{N}{n}}  $$"),
        helpText("\\( x = 0, 1, \\dots , n\\) 에 대해"),
        helpText("여기서 \\( x \\leq M \\) 그리고 \\( n - x \\leq N - M \\)"),
        br(),
        helpText("\\(\\mu = E(X) = n\\dfrac{M}{N} = \\)", round(input$n_hypergeometric * (input$M_hypergeometric / input$N_hypergeometric), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big)} = \\)", round(sqrt(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big) = \\)", round(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1)), 3))
      )
    })
  })
}
