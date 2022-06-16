
## 연속형: 로그정규분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_lognormal_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_lognormal")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_lognormal")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_lognormal")) |> 
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
     tags$h4("Log-Normal: 로그정규분포"),
     hr(),
     
     # *. 로그정규 분포 ----------------------------------------
     numericInput(ns("mean_lognormal"), "평균 \\(\\mu\\):",
                  value = 0, step = 1
     ),
     radioButtons(
       inputId = ns("variance_sd_lognormal"),
       label = NULL,
       choices = c(
         "분산 \\(\\sigma^2\\)" = "variance_true",
         "표준편차 \\(\\sigma\\)" = "variance_false"
       )
     ),
    
    conditionalPanel(
      # condition = "input.variance_sd_lognormal == 'variance_true'",
      condition = paste0("input['", ns("variance_sd_lognormal"), "'] == 'variance_true'"),
      numericInput(ns("variance_lognormal"), "분산 \\(\\sigma^2\\):",
                   value = 1, min = 0, step = 1
      )
    ),
    conditionalPanel(
      # condition = "input.variance_sd_lognormal == 'variance_false'",
      condition = paste0("input['", ns("variance_sd_lognormal"), "'] == 'variance_false'"),      
      numericInput(ns("sd_lognormal"), "표준편차 \\(\\sigma\\):",
                   value = 1, min = 0, step = 1
      )
    ),
  
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_lognormal"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
    
    conditionalPanel(
      # condition = "input.lower_tail_lognormal == 'lower.tail'",
      condition = paste0("input['", ns("lower_tail_lognormal"), "'] == 'lower.tail'"),
      numericInput(ns("x1_lognormal"), "x:",
                   value = 1, min = 0, step = 1
      )
    ),
    conditionalPanel(
      # condition = "input.lower_tail_lognormal == 'upper.tail'",
      condition = paste0("input['", ns("lower_tail_lognormal"), "'] == 'upper.tail'"),      
      numericInput(ns("x2_lognormal"), "x:",
                   value = 1, min = 0, step = 1
      )
    ),
    conditionalPanel(
      # condition = "input.lower_tail_lognormal == 'interval'",
      condition = paste0("input['", ns("lower_tail_lognormal"), "'] == 'interval'"),            
      numericInput(ns("a_lognormal"), "a:",
                   value = 1, min = 0, step = 1
      ),
      numericInput(ns("b_lognormal"), "b: \\( (a \\leq b) \\)",
                   value = 2, min = 0, step = 1
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
dist_continuous_lognormal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_lognormal <- renderUI({
      withMathJax(
        paste0("\\(X \\sim Lognormal(\\mu = \\)", " ", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_lognormal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_lognormal^2)), "\\()\\)", " 그리고 ", 
         case_when(
          input$lower_tail_lognormal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_lognormal, "\\()\\)", " ", "\\( = \\)", " ", round(plnorm(input$x1_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)),
          input$lower_tail_lognormal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_lognormal, "\\()\\)", " ", "\\( = \\)", " ", "\\( = \\)", " ", round(plnorm(input$x2_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = FALSE), 4)),
          input$lower_tail_lognormal == "interval" ~ paste0("\\(P(\\)", input$a_lognormal, " ", "\\(\\leq X\\leq \\)", " ", input$b_lognormal, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_lognormal > input$b_lognormal, "a must be less than or equal to b", round(plnorm(input$b_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE) - plnorm(input$a_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    lognormal_rv_plot <- reactive({
      res <- if ( input$lower_tail_lognormal == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dlnorm(x,
                      meanlog = input$mean_lognormal,
                      sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )
          y[x > input$x1_lognormal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
          stat_function(fun = dlnorm, args = list(
            meanlog = input$mean_lognormal,
            sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 로그정규분포: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_lognormal == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dlnorm(x,
                      meanlog = input$mean_lognormal,
                      sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )
          y[x < input$x2_lognormal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
          stat_function(fun = dlnorm, args = list(
            meanlog = input$mean_lognormal,
            sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 로그정규분포: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_lognormal == 'interval') {
        funcShaded <- function(x) {
          y <- dlnorm(x,
                      meanlog = input$mean_lognormal,
                      sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )
          y[x < input$a_lognormal | x > input$b_lognormal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
          stat_function(fun = dlnorm, args = list(
            meanlog = input$mean_lognormal,
            sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 로그정규분포: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_lognormal <- renderPlot({
      lognormal_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_lognormal <- renderUI({
      withMathJax(
        helpText("확률밀도함수: $$ f(x) = \\dfrac{1}{x\\sqrt{2\\pi \\sigma^2}}e^{-\\dfrac{1}{2\\sigma^2}(ln(x)-\\mu)^2} $$"),
        helpText("여기서, \\( x > 0, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(E(X) = e^{\\mu + \\dfrac{\\sigma^2}{2}} = \\)", round(exp(input$mean_lognormal + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal / 2, (input$sd_lognormal^2) / 2)), 3)),
        helpText("\\(SD(X) = \\sqrt{(e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2}} = \\)", round(sqrt((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)))), 3)),
        helpText("\\(Var(X) = (e^{\\sigma^2} - 1)e^{2\\mu + \\sigma^2} = \\)", round((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))), 3))
      )
    })
  })
}
