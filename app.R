
library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)

# Define UI for application that draws a histogram
ui <- shiny::tagList(
  withMathJax(), 


  # Sidebar with a slider input for number of bins
  fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    sidebarLayout(
      sidebarPanel(width=2, 
        tags$h2("데이터:"),
        textInput("x", "x", value = "90, 100, 90, 80, 87, 75", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("y", "y", value = "950, 1100, 850, 750, 950, 775", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        hr(),
        tags$h2("그래프:"),
        checkboxInput("se", "회귀선을 감싸는 신뢰구간 추가", TRUE),
        textInput("xlab", label = "xy 축 라벨:", value = "x", placeholder = "x label"),
        textInput("ylab", label = NULL, value = "y", placeholder = "y label"),
      ),
      
      mainPanel(
        
        fluidRow(
          column(2, 
             tags$h3("데이터:"),
             DT::dataTableOutput("tbl")
          ),
          column(5, 
                 tags$h3("회귀 그래프:"),
                 plotlyOutput("plot")
          ),
          column(5, 
                 tags$h3("R 회귀출력결과:"),
                 verbatimTextOutput("summary")
          )
        ),
        
        fluidRow(
          column(4, 
                 tags$hr(),
                 tags$h3("회귀 통계량:"),
                 uiOutput("results")
          ),
          column(4, 
                 tags$hr(),
                 tags$h3("수작업 검증계산:"),
                 tags$h4("1. 기본 계산값"),
                 uiOutput("data"),
                 tags$h4("2. 상세 계산결과"),
                 uiOutput("by_hand")                      
          ),
          column(4, 
                 tags$hr(),                 
                 tags$h3("해석:"),
                 uiOutput("interpretation")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  # Data output
  output$tbl <- DT::renderDataTable({
    y <- extract(input$y)
    x <- extract(input$x)
    DT::datatable(data.frame(x, y),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                    dom = "t",
                    buttons = c("copy", "csv", "excel", "print")
                  )
    )
  })
  
  output$data <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "입력값을 처리하기 부적절하거나 관측점 수가 부족합니다."
    } else if (length(x) != length(y)) {
      "x 와 y 두변수 관측점 갯수가 동일해야 합니다."
    } else {
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  output$by_hand <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
      br(),
      paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
      br(),
      br(),
      paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
    )
  })
  
  output$summary <- renderPrint({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  output$results <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      paste0(
        "* 조정 결정계수 \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3) ),
      tags$br(),
      paste0(
        "* \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3) ),
      tags$br(),
      paste0(
        "* \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3) ),
      tags$br(),
      paste0(
        "* P-값 ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })
  
  output$interpretation <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("[회귀계수를 해석하기 전에 선형회귀 가정(독립, 선형, 정규성, 등분산성)이 만족되는지 확인한다]"),
        br(),
        tags$br(),
        paste0( input$xlab, " = 0 에 대해, ", input$ylab, " 평균이 = ", round(fit$coef[[1]], 3), "이다."),
        br(),
        tags$br(),
        paste0(input$xlab, "이 한단위 증가할때마다, ", input$ylab, " 는 ", abs(round(fit$coef[[2]], 3)), " 만큼",
               ifelse(round(fit$coef[[2]], 3) >= 0, "평균적으로 증가한다.", "평균적으로 감소한다."))
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("[회귀계수를 해석하기 전에 선형회귀 가정(독립, 선형, 정규성, 등분산성)이 만족되는지 확인한다]"),
        br(),
        tags$br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        tags$br(),
        paste0("\\( \\beta_1 \\)", " 는 0 과 유의적인 차이는 없다 (p-value = ", 
               round(summary(fit)$coefficients[2, 4], 3), ")", 
               input$xlab, " 와 ", input$ylab, " 간에 유의적 관계는 없다.")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("[회귀계수를 해석하기 전에 선형회귀 가정(독립, 선형, 정규성, 등분산성)이 만족되는지 확인한다]"),
        br(),
        tags$br(),
        paste0("\\( \\beta_0 \\)", " 는 0 과 유의적인 차이는 없다 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), 
               ") 따라서,", input$xlab, " = 0 일 때, ", input$ylab, "의 평균은 0 과 유의적인 차이는 없다."),
        br(),
        tags$br(),
        paste0(input$xlab, "이 한단위 증가할때마다, ", input$ylab, " 는 ", abs(round(fit$coef[[2]], 3)), " 만큼",
               ifelse(round(fit$coef[[2]], 3) >= 0, "평균적으로 증가한다.", "평균적으로 감소한다."))
      )
    } else {
      withMathJax(
        paste0("[회귀계수를 해석하기 전에 선형회귀 가정(독립, 선형, 정규성, 등분산성)이 만족되는지 확인한다]"),
        br(),
        tags$br(),
        paste0("\\( \\beta_0 \\)", " 와 ", "\\( \\beta_1 \\)", " 가 0 과 유의적인 차이가 없다 (p-values = ", 
               round(summary(fit)$coefficients[1, 4], 3), " 와 ", round(summary(fit)$coefficients[2, 4], 3), 
               ") 따라서 ", input$ylab, " 의 평균이 0과 유의적인 차이는 없다.")
      )
    }
  })
  
  output$plot <- renderPlotly({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab(input$ylab) +
      xlab(input$xlab) +
      theme_minimal()
    ggplotly(p)
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)