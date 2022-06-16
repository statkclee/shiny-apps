library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()

## 연속형: 정규분포 ---------------------
# 1. 모듈 UI ----------------------------
dist_continuous_normal_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       # 텍스트 ----
       br(),
       tags$b("해답:"),
       uiOutput( ns("results_normal")),
       br(),
       
       # 그래프 ----
       plotOutput(outputId = ns("plots_normal")) %>% 
         shinycssloaders::withSpinner(
           type = 2, 
           color.background = "white"
         ), 
       
       br(),
       tags$b("상세:"),
       br(),
       
       # 상세 ----
       uiOutput( ns("parameters_normal")) |> 
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
     tags$h3("Normal: 정규분포"),
     hr(),
     
     # *. 정규 분포 ----------------------------------------
     numericInput(ns("mean_normal"), "평균 \\(\\mu\\):",
                  value = 0, step = 1
     ),
     radioButtons(
       inputId = ns("variance_sd"),
       label = NULL,
       choices = c(
         "분산 \\(\\sigma^2\\)" = "variance_true",
         "표준편차 \\(\\sigma\\)" = "variance_false"
       )
     ),
     
     conditionalPanel(
       # condition = "input.variance_sd == 'variance_true'",
       condition = paste0("input['", ns("variance_sd"), "'] == 'variance_true'"),                     
       numericInput( ns("variance_normal"), "분산 \\(\\sigma^2\\):",
                    value = 1, min = 0, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.variance_sd == 'variance_false'",
       condition = paste0("input['", ns("variance_sd"), "'] == 'variance_false'"),       
       numericInput( ns("sd_normal"), "표준편차 \\(\\sigma\\):",
                    value = 1, min = 0, step = 1
       )
     ),
     
     
     # *. 검정방향 -----------------------------------
     radioButtons(
       inputId = ns("lower_tail_normal"),
       label = NULL,
       choices = c(
         "하단 꼬리 : \\(P(X \\leq x)\\)" = "lower.tail",
         "상단 꼬리 : \\(P(X > x)\\)" = "upper.tail",
         "구간 : \\(P(a \\leq X \\leq b)\\)" = "interval"
       )
     ),
     
     conditionalPanel(
       # condition = " input.lower_tail_normal == 'lower.tail'",
       condition = paste0("input['", ns("lower_tail_normal"), "'] == 'lower.tail'"),       
       numericInput( ns("x1_normal"), "x:",
                    value = 1, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_normal == 'upper.tail'",
       condition = paste0("input['", ns("lower_tail_normal"), "'] == 'upper.tail'"),              
       numericInput( ns("x2_normal"), "x:",
                    value = 1, step = 1
       )
     ),
     conditionalPanel(
       # condition = " input.lower_tail_normal == 'interval'",
       condition = paste0("input['", ns("lower_tail_normal"), "'] == 'interval'"),                     
       numericInput( ns("a_normal"), "a:",
                    value = -1, step = 1
       ),
       numericInput( ns("b_normal"), "b: \\( (a \\leq b) \\)",
                    value = 1, step = 1
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
dist_continuous_normal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 수식 -----------------------
    output$results_normal <- renderUI({
      withMathJax(
        paste0("\\(X \\sim \\mathcal{N}(\\mu = \\)", " ", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_normal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_normal^2)), "\\()\\)", " 그리고 ", 
          case_when(
            input$lower_tail_normal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", "\\((\\)", input$x1_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", round((input$x1_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x1_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)),
            input$lower_tail_normal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", "\\((\\)", input$x2_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", round((input$x2_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x2_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), 4)),
            input$lower_tail_normal == "interval" ~ paste0("\\(P(\\)", input$a_normal, " ", "\\(\\leq X\\leq \\)", " ", input$b_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(\\)", "\\((\\)", input$a_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\(\\leq Z\\leq \\)", " ", "\\((\\)", input$b_normal, " ", "\\(-\\)", " ", input$mean_normal, "\\()\\)", " ", "\\(/\\)", " ", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 2), input$sd_normal), "\\()\\)", " ", "\\( = \\)", " ","\\(P(\\)", round((input$a_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), " ", "\\(\\leq Z\\leq \\)", " ", round((input$b_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_normal > input$b_normal, "a must be less than or equal to b", round(pnorm(input$b_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$a_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)))
        ))
      )
    })
    
    # 2. 그래프 -----------------------
    normal_rv_plot <- reactive({
      res <- if ( input$lower_tail_normal == 'lower.tail') {
        funcShaded <- function(x) {
          y <- dnorm(x,
                     mean = input$mean_normal,
                     sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )
          y[x > input$x1_normal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dnorm, args = list(
            mean = input$mean_normal,
            sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_normal == 'upper.tail') {
        funcShaded <- function(x) {
          y <- dnorm(x,
                     mean = input$mean_normal,
                     sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )
          y[x < input$x2_normal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dnorm, args = list(
            mean = input$mean_normal,
            sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$lower_tail_normal == 'interval') {
        funcShaded <- function(x) {
          y <- dnorm(x,
                     mean = input$mean_normal,
                     sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )
          y[x < input$a_normal | x > input$b_normal] <- NA
          return(y)
        }
        p <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
          stat_function(fun = dnorm, args = list(
            mean = input$mean_normal,
            sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
          )) +
          stat_function(fun = funcShaded, geom = "area", fill = "blue", alpha = 0.8) +
          theme_minimal() +
          ggtitle(paste0(input$distribution, " 분포: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      }
      return(res)
    })
    
    output$plots_normal <- renderPlot({
      normal_rv_plot()
    })
    
    # 3. 상세 -----------------------    
    
    output$parameters_normal <- renderUI({
      withMathJax(
        helpText("확률밀도함수: $$ f(x) = \\dfrac{1}{\\sqrt{2\\pi \\sigma^2}}e^{-\\dfrac{1}{2\\sigma^2}(x-\\mu)^2} $$"),
        helpText("여기서, \\( -\\infty < x < \\infty, -\\infty < \\mu < \\infty, \\sigma > 0\\)"),
        br(),
        helpText("\\(\\mu = E(X) = \\)", round(input$mean_normal, 3)),
        helpText("\\(\\sigma = SD(X) = \\)", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 3), round(input$sd_normal, 3))),
        helpText("\\(\\sigma^2 = Var(X) = \\)", ifelse(input$variance_sd == "variance_true", round(input$variance_normal, 3), round(input$sd_normal^2, 3)))
      )
    })
  })
}
