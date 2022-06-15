library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_geometry_failure_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_geometry_failure")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_geometry_failure")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_geometry_failure")) |> 
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
     tags$h3("기하분포 (실패횟수)"),
     tags$p("첫번째 성공전에 실패 횟수"),
     hr(),

     # *. 기하 분포 : 실패횟수 ----------------------------------------
     numericInput(ns("p_geometric"), "성공확률 \\(p\\):",
                  value = 0.5, min = 0, max = 1, step = 0.01
     ),
       
     radioButtons(
       inputId = ns("lower_tail_geometric"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = "input.lower_tail_geometric == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_geometric"), "'] == 'lower.tail'"),
       helpText("첫번째 성공에 앞선 실패횟수"),
       numericInput( ns("x1_geometric"), "x:",
                    value = 1, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_geometric == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_geometric"), "'] == 'upper.tail'"),
       helpText("첫번째 성공에 앞선 실패횟수"),
       numericInput( ns("x2_geometric"), "x:",
                    value = 1, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_geometric == 'interval'",
       condition = paste0("input['", ns("lower_tail_geometric"), "'] == 'interval'"),
       helpText("첫번째 성공에 앞선 실패횟수"),
       numericInput( ns("a_geometric"), "a:",
                    value = 1, min = 0, step = 1
       ),
       numericInput( ns("b_geometric"), "b: \\( (a \\leq b) \\)",
                    value = 3, min = 0, step = 1
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
dist_discrete_geometry_failure_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_geometry_failure <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric, "\\()\\)", "  그리고  ", case_when(
          input$lower_tail_geometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric, prob = input$p_geometric, lower.tail = TRUE), 4)),
          input$lower_tail_geometric == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric, prob = input$p_geometric, lower.tail = FALSE), 4)),
          input$lower_tail_geometric == "interval" ~ paste0("\\(P(\\)", input$a_geometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric > input$b_geometric, "a must be less than or equal to b", round(pgeom(input$b_geometric, prob = input$p_geometric, lower.tail = TRUE) - pgeom(input$a_geometric - 1, prob = input$p_geometric, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    geometry_failure_rv_plot <- reactive({
      res <- if ( input$lower_tail_geometric == 'lower.tail') {
        p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
          mutate(Heads = ifelse(heads <= input$x1_geometric, "2", "Other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_geometric == 'upper.tail') {
        p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
          mutate(Heads = ifelse(heads > input$x2_geometric, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_geometric == 'interval') {
        p <- data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
          mutate(Heads = ifelse(heads >= input$a_geometric & heads <= input$b_geometric, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
        return(res)
    })
    
    output$plots_geometry_failure <- renderPlot({
      geometry_failure_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_geometry_failure <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = (1 - p)^x p $$"),
        helpText("여기서, \\( x = 0, 1, 2, \\dots \\) 그리고 \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1-p}{p} = \\)", round((1 - input$p_geometric) / input$p_geometric, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric) / (input$p_geometric^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p_geometric) / (input$p_geometric^2), 3))
      )
    })
  })
}
