library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_nb_failure_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
   fluidRow(
     # 텍스트 ----
     br(),
     tags$b("해답:"),
     uiOutput( ns("results_nb_failure")),
     br(),
     
     # 그래프 ----
     plotOutput(outputId = ns("plots_nb_failure")) %>% 
       shinycssloaders::withSpinner(
         type = 2, 
         color.background = "white"
       ), 
     
     br(),
     tags$b("상세:"),
     br(),
     
     # 상세 ----
     uiOutput( ns("parameters_nb_failure")) |> 
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
     tags$h4("음이항분포 (실패횟수)"),
     tags$p("r번째 성공전에 실패 횟수"),
     hr(),
     
     # *. 음이항 분포 : 실패횟수 ----------------------------------------
     numericInput( ns("r_negativebinomial"), "성공 횟수 \\(r\\):",
                  value = 5, min = 1, step = 1
     ),
     numericInput( ns("p_negativebinomial"), "성공 확률 \\(p\\):",
                  value = 0.5, min = 0, max = 1, step = 0.01
     ),
     
     radioButtons(
       inputId = ns("lower_tail_negativebinomial"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = "input.lower_tail_negativebinomial == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_negativebinomial"), "'] == 'lower.tail'"),       
       helpText("\\(r\\) 번째 성공 전 실패 횟수"),
       numericInput( ns("x1_negativebinomial"), "x:",
                    value = 2, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_negativebinomial == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_negativebinomial"), "'] == 'upper.tail'"),       
       helpText("\\(r\\) 번째 성공 전 실패 횟수"),
       numericInput( ns("x2_negativebinomial"), "x:",
                    value = 2, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_negativebinomial == 'interval'",
       condition = paste0("input['", ns("lower_tail_negativebinomial"), "'] == 'interval'"),              
       helpText("\\(r\\) 번째 성공 전 실패 횟수"),
       numericInput( ns("a_negativebinomial"), "a:",
                    value = 2, min = 0, step = 1
       ),
       numericInput( ns("b_negativebinomial"), "b: \\( (a \\leq b) \\)",
                    value = 4, min = 0, step = 1
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
dist_discrete_nb_failure_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_nb_failure <- renderUI({
      withMathJax(
        paste0("\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial, ", ", "\\(p = \\)", " ", input$p_negativebinomial, "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_negativebinomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)),
          input$lower_tail_negativebinomial == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE), 4)),
          input$lower_tail_negativebinomial == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial > input$b_negativebinomial, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE) - pnbinom(input$a_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    nb_failure_rv_plot <- reactive({
      res <- if ( input$lower_tail_negativebinomial == 'lower.tail') {
        p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          mutate(Heads = ifelse(heads <= input$x1_negativebinomial, "2", "Other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_negativebinomial == 'upper.tail') {
        p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          mutate(Heads = ifelse(heads > input$x2_negativebinomial, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_negativebinomial == 'interval') {
        p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          mutate(Heads = ifelse(heads >= input$a_negativebinomial & heads <= input$b_negativebinomial, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_nb_failure <- renderPlot({
      nb_failure_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_nb_failure <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = \\binom{x+r-1}{r-1} (1-p)^x p^r $$"),
        helpText("여기서, \\( x = 0, 1, 2, \\dots, r = 1, 2, \\dots \\) 그리고 \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{r(1-p)}{p} = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / input$p_negativebinomial), 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)", round(sqrt((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2))), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{r(1-p)}{p^2} = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2)), 3))
      )
    })
  })
}
