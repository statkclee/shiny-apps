

# 1. 모듈 UI ----------------------------
dist_continuous_beta_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_beta")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_beta")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_beta")) |> 
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
     tags$h3("Beta: 베타분포"),
     hr(),

     # *. 베타 분포 ----------------------------------------
     numericInput( ns("alpha_beta"), "형상모수 \\(\\alpha\\):",
                  value = 1, min = 0, step = 1
     ),
     numericInput( ns("beta_beta"), "형상모수 \\(\\beta\\):",
                  value = 3, min = 0, step = 1
     ),
     radioButtons(
       inputId = ns("lower_tail_beta"),
       label = NULL,
       choices = c(
         "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
         "Upper tail : \\(P(X > x)\\)" = "upper.tail",
         "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_beta == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_beta"), "'] == 'lower.tail'"),       
       numericInput(ns("x1_beta"), "x:",
                    value = 0.45, min = 0, max = 1, step = 0.01
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_beta == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_beta"), "'] == 'upper.tail'"),       
       numericInput(ns("x2_beta"), "x:",
                    value = 0.45, min = 0, max = 1, step = 0.01
       )
     ),
     conditionalPanel(
       # condition = "input.lower_tail_beta == 'interval'",
       condition = paste0("input['", ns("lower_tail_beta"), "'] == 'interval'"),              
       numericInput(ns("a_beta"), "a:",
                    value = 0.25, min = 0, max = 1, step = 0.01
       ),
       numericInput(ns("b_beta"), "b: \\( (a \\leq b) \\)",
                    value = 0.45, min = 0, max = 1, step = 0.01
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
dist_continuous_beta_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_beta <- renderUI({
        withMathJax(
          paste0("\\(X \\sim Beta(\\alpha = \\)", " ", input$alpha_beta, ", ", "\\(\\beta = \\)", " ", input$beta_beta, "\\()\\)", "  그리고  ", 
            case_when(
              input$lower_tail_beta == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x1_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)),
              input$lower_tail_beta == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x2_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), 4)),
              input$lower_tail_beta == "interval" ~ paste0("\\(P(\\)", input$a_beta, " ", "\\(\\leq X\\leq \\)", " ", input$b_beta, "\\()\\)", " ", "\\( = \\)", " ", 
                                                           ifelse(input$a_beta > input$b_beta, "a 는 b 보다 같거나 적어야함", round(pbeta(input$b_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE) - pbeta(input$a_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)))
          ))
        )
    })
    
    # 2. 그래프 -----------------------
    beta_rv_plot <- reactive({
      res <- if (input$lower_tail_beta == "lower.tail") {
        funcShaded <- function(x) {
          y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
          y[x > input$x1_beta] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: 베타(", input$alpha_beta, ", ", input$beta_beta, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_beta == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
          y[x < input$x2_beta] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: 베타(", input$alpha_beta, ", ", input$beta_beta, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if (input$lower_tail_beta == 'interval') {
        funcShaded <- function(x) {
          y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
          y[x < input$a_beta | x > input$b_beta] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
          stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: 베타(", input$alpha_beta, ", ", input$beta_beta, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_beta <- renderPlot({
      beta_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_beta <- renderUI({
      
        withMathJax(
          helpText("확률 밀도 함수: $$ f(x) = \\dfrac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} x^{\\alpha-1} (1 - x)^{\\beta - 1} $$"),
          helpText("여기서, \\( 0 \\leq x \\leq 1, \\alpha > 0, \\beta > 0\\)"),
          br(),
          helpText("\\(\\mu = E(X) = \\dfrac{\\alpha}{\\alpha + \\beta} = \\)", round(input$alpha_beta / (input$alpha_beta + input$beta_beta), 3)),
          helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)}} = \\)", round(sqrt((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1))), 3)),
          helpText("\\(\\sigma^2 = Var(X) = \\dfrac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta+1)} = \\)", round((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1)), 3))
        )
      
    })
  })
}
