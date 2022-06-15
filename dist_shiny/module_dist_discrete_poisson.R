library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_poisson_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_poisson")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_poisson")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_poisson")) |> 
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
     tags$h3("Poisson: 포아송분포"),
     hr(),

     # *. 포아송 분포 ----------------------------------------
     numericInput(ns("lambda_poisson"), "모수(Rate) \\(\\lambda\\):",
                  value = 4, min = 1, step = 1
     ),
     radioButtons(
       inputId = ns("lower_tail_poisson"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_poisson == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_poisson"), "'] == 'lower.tail'"),
       numericInput(ns("x1_poisson"), "x:",
                    value = 6, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_poisson == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_poisson"), "'] == 'upper.tail'"),                            
       numericInput(ns("x2_poisson"), "x:",
                    value = 6, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_poisson == 'interval'",
       condition = paste0("input['", ns("lower_tail_poisson"), "'] == 'interval'"),
       numericInput(ns("a_poisson"), "a:",
                    value = 6, min = 0, step = 1
       ),
       numericInput(ns("b_poisson"), "b: \\( (a \\leq b) \\)",
                    value = 10, min = 0, step = 1
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
dist_discrete_poisson_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_poisson <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Pois(\\lambda = \\)", " ", input$lambda_poisson, "\\()\\)", "  그리고  ", case_when(
          input$lower_tail_poisson == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x1_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
          input$lower_tail_poisson == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x2_poisson, lambda = input$lambda_poisson, lower.tail = FALSE), 4)),
          input$lower_tail_poisson == "interval" ~ paste0("\\(P(\\)", input$a_poisson, " ", "\\(\\leq X\\leq \\)", " ", input$b_poisson, "\\()\\)", " ", "\\( = \\)", " ", 
                                                          ifelse(input$a_poisson > input$b_poisson, "a 는 b 와 같거나 작아야 한다.", round(ppois(input$b_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$a_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    poisson_rv_plot <- reactive({
      res <- if (input$lower_tail_poisson == 'lower.tail') {
        p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          mutate(Heads = ifelse(heads <= input$x1_poisson, "2", "Other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 포아송(", input$lambda_poisson, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_poisson == 'upper.tail') {
        p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          mutate(Heads = ifelse(heads > input$x2_poisson, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 포아송(", input$lambda_poisson, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_poisson == 'interval') {
        p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          mutate(Heads = ifelse(heads >= input$a_poisson & heads <= input$b_poisson, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 포아송(", input$lambda_poisson, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
        return(res)
    })
    
    output$plots_poisson <- renderPlot({
      poisson_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_poisson <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = \\dfrac{e^{-\\lambda}\\lambda^x}{x!} $$"),
        helpText("\\( x = 0, 1, 2, \\dots\\) 에 대해"),
        helpText("여기서, \\( \\lambda > 0 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\lambda = \\)", round(input$lambda_poisson, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\lambda} = \\)", round(sqrt(input$lambda_poisson), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\lambda = \\)", round(input$lambda_poisson, 3))
      )
    })
  })
}
