
library(shiny)
library(shiny.router)
library(plotly)

source("_common.R")

home_page <- div(
   tabPanel(title = "NHST 검정", 
            sidebarLayout(
              sidebarPanel(
                width = 2,
                shinyWidgets::awesomeRadio(inputId = "nhst_type", 
                                           label = "검정대상", 
                                           choices  = list("평균" = "nhst_mean", 
                                                           "비율" = "nhst_prop", 
                                                           "분산" = "nhst_variance"))
              ),
              mainPanel(
                fluidRow(
                  uiOutput("nhst_img")
                )
              )
            )
   )
)

means_page <- div(
  tabPanel(title = "NHST 검정",   
           sidebarLayout(
             sidebarPanel(
               width = 2,
               
    tags$br(),
    tags$b("데이터"),
    
    textInput("sample_onemean", "표본", value = "3,7,11,0,7,0,4,5,6,2", 
              placeholder = "콤마로 구분된 숫자를 입력하세요. 예를 들어, 7.2, 4.7, 5, 5.03, 등"),
    tags$p("※ 콤마로 구분된 숫자 입력. ex) 7.2, 4.7, 5.03, 등"),
    
    tags$br(),
    
    tags$b("가설검정"),
    
    tags$p("1. 귀무가설"),
    tags$p("\\( H_0 : \\mu = \\)"),
    
    numericInput("onemean_h0",
                 label = NULL,
                 value = 3, step = 0.1, width="100px"),
    
    tags$p("2. 검정방향"),
    radioButtons(
      inputId = "onemean_alternative",
      label = "대립가설",
      inline = TRUE,
      choices = c(
        "\\( \\neq \\)" = "two.sided",
        "\\( > \\)" = "greater",
        "\\( < \\)" = "less"
      ),
    ),
    
    tags$p("3. 모집단 분산"),
    checkboxInput("popsd_onemean", "모집단 분산을 알는 경우:", FALSE),
    conditionalPanel(
      condition = "input.popsd_onemean == 1",
      numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                   value = 2, min = 0, step = 0.1, width = "100px")
    ),
    
    tags$p("4. 유의수준"),
    sliderInput("onemean_alpha",
                "유의수준 \\(\\alpha = \\)",
                min = 0.01,
                max = 0.20,
                value = 0.05
    )
    ),
    
    mainPanel(
        plotOutput("output_nhst_means")
    )
  ))
)

contact_page <- div(
  titlePanel("Contact"),
  p("This is a contact page")
)


router <- make_router(
  route("/", home_page),
  route("nhst_means", means_page),
  route("contact", contact_page)
)


ui <- fluidPage(theme = "router.css",                
                tags$ul(
                  tags$li(a(href = route_link("/"), "NHST")),
                  tags$li(a(href = route_link("nhst_means"), "평균검정")),
                  tags$li(a(href = route_link("contact"), "Contact"))
                ),
  router$ui
  
)

server <- function(input, output, session) {
  
  
  router$server(input, output, session)
  
  output$nhst_img <- renderUI({
    
    if(input$nhst_type ==  "nhst_mean") {
      img(src='nhst-means.png', align = "center")
    } else if(input$nhst_type ==  "nhst_prop") {
      img(src='nhst-proportion.png', align = "center")
    } else {
      img(src='nhst-variance.png', align = "center")
    }
    
  })
  
  output$output_nhst_means <- renderPlot({  
    if ( input$popsd_onemean == FALSE) {
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$onemean_h0, alternative = input$onemean_alternative, conf.level = 1 - input$onemean_alpha)
      if (input$onemean_alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$onemean_alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$onemean_alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$onemean_alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$onemean_alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$onemean_alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$onemean_alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), 
                                   qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8, fill = "sky blue") +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("검정 통계량 = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("t-분포 ~", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("밀도") +
        xlab("x")
      p
      # plotly::ggplotly(p)
    } else if ( input$popsd_onemean == TRUE) {
      dat <- extract(input$sample_onemean)
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$onemean_h0, alpha = input$onemean_alpha, alternative = input$onemean_alternative)
      if (input$onemean_alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$onemean_alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$onemean_alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$onemean_alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$onemean_alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$onemean_alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$onemean_alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", fill = "sky blue", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("검정통계량 = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("정규분포 N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("밀도") +
        xlab("x")
      p
      # plotly::ggplotly(p)
    } 
  } )
  
}

shinyApp(ui, server)

