
library(showtext)
font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


two_variance_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 9,

    fluidRow(
      column(4, uiOutput(ns("results_two_variance")) ),
      column(4, plotOutput(ns("plot_two_variance"))  ),
      column(1, tableOutput(ns("table_two_variance"))))
  )
  
  # 2. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 3,

     tags$br(),
     tags$b("데이터"),
     
     textInput(ns("sample1_twovar"), "표본 1", value = "0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2"),
     textInput(ns("sample2_twovar"), "표본 2", value = "0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1"),
     
     tags$p("※ 콤마로 구분된 숫자 입력. ex) 7.2, 4.7, 5.03, 등"),
     
     
     tags$b("가설검정"),
     
     tags$p("1. 귀무가설"),
     tags$p("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)"),
     
     
     tags$p("2. 검정방향"),
     radioButtons(
       inputId = ns("alternative_twovar"),
       label = "대립가설",
       inline = FALSE,
       choices = c(
         "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
         "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater",
         "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less"
       )
     ),
     
     
     tags$p("3. 유의수준"),
     sliderInput(ns("twovar_alpha"),
                 "유의수준 \\(\\alpha = \\)",
                 min = 0.01,
                 max = 0.20,
                 value = 0.05
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



two_variance_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 텍스트 -------------------------------------
    output$results_two_variance <- renderUI({
      dat1 <- extract(input$sample1_twovar)
      dat2 <- extract(input$sample2_twovar)
      if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
        "Invalid input or not enough observations"
      } # else if (input$h0 <= 0) {
      #   withMathJax(
      #     sprintf("\\( \\sigma^2_1 - \\sigma^2_2 \\) must be > 0")
      #   )
      # }
      else {
        test_confint <- var.test(x = dat1, y = dat2, ratio = 1, alternative = "two.sided", conf.level = 1 - input$twovar_alpha)
        test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$twovar_alpha)
        withMathJax(
          tags$h2("데이터"),
          br(),
          paste(c("\\(표본_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
          br(),
          paste(c("\\(표본_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
          br(),
          paste0("\\(n_1 =\\) ", length(dat1)),
          br(),
          paste0("\\(n_2 =\\) ", length(dat2)),
          br(),
          paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
          br(),
          paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
          br(),
          paste0("\\(s_1 =\\) ", round(sd(dat1), 3)),
          br(),
          paste0("\\(s_2 =\\) ", round(sd(dat2), 3)),
          br(),
          br(),
          tags$h2("신뢰구간 - 양측"),
          br(),
          paste0(
            (1 - input$twovar_alpha) * 100, "% CI 신뢰구간: \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} ; \\dfrac{s^2_1}{s^2_2}F_{\\alpha/2, n_1 - 1, n_2-1} \\Bigg] = \\) ",
            "\\( \\big[ \\)", round(test_confint$estimate, 3), " * (1 / ", round(qf(input$twovar_alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "); ", 
            round(test_confint$estimate, 3), " * ", round(qf(input$twovar_alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "\\( \\big] = \\) ",
            "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
          ),
          br(),
          br(),
          tags$h2("가설 검정"),
          br(),
          if (test$alternative == "two.sided") {
            withMathJax(
              paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) vs. \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
            )
          } else if (test$alternative == "greater") {
            withMathJax(
              paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) vs. \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
            )
          } else {
            withMathJax(
              paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) vs. \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
            )
          },
          br(),
          paste0(
            "2. 검정 통계량 : \\(F_{obs} = \\dfrac{s^2_1}{s^2_2} = \\) ",
            "[", round(var(dat1), 3), " / ", round(var(dat2), 3), "]", " \\( = \\) ",
            round(test$statistic, 3)
          ),
          br(),
          if (test$alternative == "two.sided") {
            withMathJax(
              paste0("3. 임계값 : \\( F_{1-\\alpha/2, n_1 - 1, n_2-1} \\) 와 \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
              paste0("\\( \\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} \\) 와 \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
              paste0("\\( 1/F \\)(", input$twovar_alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") 와 \\( F \\)(", input$twovar_alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
              paste0(round(qf(input$twovar_alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3), " 와 ", round(qf(input$twovar_alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
            )
          } else if (test$alternative == "greater") {
            withMathJax(
              paste0("3. 임계값 : \\( F_{\\alpha, n_1 - 1, n_2-1} =\\) "),
              paste0("\\( F \\)(", input$twovar_alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
              paste0(round(qf(input$twovar_alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
            )
          } else {
            withMathJax(
              paste0("3. 임계값 : \\( F_{1-\\alpha, n_1 - 1, n_2-1} = \\) "),
              paste0("\\( \\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}} = \\) "),
              paste0("\\( 1/F \\)(", input$twovar_alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
              paste0(round(qf(input$twovar_alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3))
            )
          },
          br(),
          paste0("4. 결론 : ", ifelse(test$p.value < input$twovar_alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
          br(),
          br(),
          tags$h2("해석"),
          br(),
          paste0( input$twovar_alpha * 100, "% 유의수준에서, ", 
                  "참 분산 비율이 ", test$null.value, " 이라는 귀무가설을 ", 
                  ifelse(test$p.value < input$twovar_alpha, "기각한다", "기각하지 않는다"), 
                  " \\((p\\)-값 ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
        )
      }
    })
   
    # 2. 그래프 ------------------------------------- 
   output$plot_two_variance <- renderPlot({  
     dat1 <- extract(input$sample1_twovar)
     dat2 <- extract(input$sample2_twovar)
     test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$twovar_alpha)
     if (test$alternative == "two.sided") {
       funcShaded <- function(x) {
         y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
         y[x > qf(1 - input$twovar_alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE) & x < qf(1 - input$twovar_alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE)] <- NA
         return(y)
       }
     } else if (test$alternative == "greater") {
       funcShaded <- function(x) {
         y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
         y[x < qf(input$twovar_alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
         return(y)
       }
     } else if (test$alternative == "less") {
       funcShaded <- function(x) {
         y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
         y[x > qf(1 - input$twovar_alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
         return(y)
       }
     }
     p <- ggplot(data.frame(x = c(0, qf(0.99, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE))), aes(x = x)) +
       stat_function( fun = df, fill="gray90", args = list(df1 = test$parameter[1], df2 = test$parameter[2])) +
       stat_function( fun = funcShaded, geom = "area", fill = "sky blue", alpha = 0.8) +
       
       theme_minimal() +
       geom_vline(xintercept = test$statistic, color = "steelblue") +
       geom_text(aes(x = test$statistic, label = paste0("검정통계량 = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
       ggtitle(paste0("F 분포 F(", test$parameter[1], ", ", test$parameter[2], ")")) +
       theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
       ylab("밀도") +
       xlab("x")
     p
   })
   
   # 3. 표 -------------------------------------
   output$table_two_variance <- renderTable({
     
     var_dat1 <- extract(input$sample1_twovar)
     var_dat2 <- extract(input$sample2_twovar)
     
     tibble(변수1 = var_dat1,
              변수2 = var_dat2 ) %>% 
       xtable::xtable(align="ccc")
     
   })
     
   })
}
