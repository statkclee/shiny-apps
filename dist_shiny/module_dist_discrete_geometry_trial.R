library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
dist_discrete_geometry_trial_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
                         fluidRow(
                           # 텍스트 ----
                           br(),
                           tags$b("해답:"),
                           uiOutput( ns("results_geometry_trial")),
                           br(),
                           
                           # 그래프 ----
                           plotOutput(outputId = ns("plots_geometry_trial")) %>% 
                             shinycssloaders::withSpinner(
                               type = 2, 
                               color.background = "white"
                             ), 
                           
                           br(),
                           tags$b("상세:"),
                           br(),
                           
                           # 상세 ----
                           uiOutput( ns("parameters_geometry_trial")) |> 
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
                               tags$h3("기하분포 (시행횟수)"),
                               tags$p("첫번째 성공전에 시행 횟수"),
                               hr(),
                               
                               # *. 기하 분포 : 시행횟수 ----------------------------------------
                               numericInput(ns("p_geometric2"), "성공 확률 \\(p\\):",
                                            value = 0.5, min = 0, max = 1, step = 0.01
                               ),
                               
                               radioButtons(
                                 inputId = ns("lower_tail_geometric2"),
                                 label = NULL,
                                 choices = c(
                                   "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
                                   "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
                                   "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
                                 )
                               ),
                               
                               conditionalPanel(
                                 # condition = "input.lower_tail_geometric2 == 'lower.tail'",
                                 condition = paste0("input['", ns("lower_tail_geometric2"), "'] == 'lower.tail'"),
                                 helpText("첫번째 성공에 앞선 시행횟수"),
                                 numericInput(ns("x1_geometric2"), "x:",
                                              value = 2, min = 1, step = 1
                                 )
                               ),
                               conditionalPanel(
                                 # condition = "input.lower_tail_geometric2 == 'upper.tail'",
                                 condition = paste0("input['", ns("lower_tail_geometric2"), "'] == 'upper.tail'"),
                                 helpText("첫번째 성공에 앞선 시행횟수"),
                                 numericInput(ns("x2_geometric2"), "x:",
                                              value = 2, min = 1, step = 1
                                 )
                               ),
                               conditionalPanel(
                                 # condition = "input.lower_tail_geometric2 == 'interval'",
                                 condition = paste0("input['", ns("lower_tail_geometric2"), "'] == 'interval'"),       
                                 helpText("첫번째 성공에 앞선 시행횟수"),
                                 numericInput( ns("a_geometric2"), "a:",
                                               value = 2, min = 1, step = 1
                                 ),
                                 numericInput( ns("b_geometric2"), "b: \\( (a \\leq b) \\)",
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
dist_discrete_geometry_trial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_geometry_trial <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Geom(p = \\)", " ", input$p_geometric2, "\\()\\)", " 그리고 ", case_when(
          input$lower_tail_geometric2 == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric2 - 1, prob = input$p_geometric2, lower.tail = TRUE), 4)),
          input$lower_tail_geometric2 == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_geometric2, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric2 - 1, prob = input$p_geometric2, lower.tail = FALSE), 4)),
          input$lower_tail_geometric2 == "interval" ~ paste0("\\(P(\\)", input$a_geometric2, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric2, "\\()\\)", " ", "\\( = \\)", " ", 
                                                             ifelse(input$a_geometric2 > input$b_geometric2, "a 는 b와 같거나 작아야 한다.", round(pgeom(input$b_geometric2 - 1, prob = input$p_geometric2, lower.tail = TRUE) - pgeom(input$a_geometric2 - 2, prob = input$p_geometric2, lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    geometry_trial_rv_plot <- reactive({
      res <- if (input$lower_tail_geometric2 == 'lower.tail') {
        p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
          mutate(Heads = ifelse(heads <= input$x1_geometric2, "2", "Other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric2, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_geometric2 == 'upper.tail') {
        p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
          mutate(Heads = ifelse(heads > input$x2_geometric2, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric2, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_geometric2 == 'interval') {
        p <- data.frame(heads = 1:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2))) + 1), prob = dgeom(x = 0:(input$p_geometric2 + (5 * sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)))), prob = input$p_geometric2)) %>%
          mutate(Heads = ifelse(heads >= input$a_geometric2 & heads <= input$b_geometric2, "2", "other")) %>%
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
          ggtitle(paste0(input$distribution, " 분포: 기하(", input$p_geometric2, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_geometry_trial <- renderPlot({
      geometry_trial_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_geometry_trial <- renderUI({
      withMathJax(
        helpText("확률 밀도 함수: $$ p(x) = P(X = x) = (1 - p)^{x-1} p $$"),
        helpText("여기서, \\( x = 1, 2, \\dots \\) 그리고 \\( 0 < p \\leq 1 \\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\dfrac{1}{p} = \\)", round((1) / input$p_geometric2, 3)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p_geometric2) / (input$p_geometric2^2)), 3)),
        helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p_geometric2) / (input$p_geometric2^2), 3))
      )
    })
  })
}
