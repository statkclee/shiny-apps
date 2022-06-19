library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()

source("global.R")

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("principal", "원금 (대출금, 만원)", 10000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "연 이자율 (%)", 4, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "대출기간 (년)",
                  min = 0,
                  max = 40,
                  value = 25,
                  step = 1
      ),
      hr(),
      checkboxInput("plot", "그래프로 보시겠습니까?", TRUE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      conditionalPanel(
        condition = "input.plot == 1",
        plotOutput("distPlot")),
      br(),
      conditionalPanel(
        condition = "input.plot != 1",
        DT::dataTableOutput("tbl")),
      br(),
      p(em("공지사항: 이 Shiny 앱은 어떠한 투자정보와 추천, 혹은 금융분석에 대한 정보를 
           담고 있지 않습니다. 이 앱은 단순히 금융정보를 제공하는 것이고 이를 활용한 
           모든 투자는 본인이 스스로 결정한 것이라 이 앱과는 아무런 관련이 없습니다.
           이 앱에 담긴 정보를 활용한 어떤 결정도 한국 R 사용자회와 무관합니다.")),
      p(em("이 Shiny 앱은 Antoine Soetewey 교수 코드를 참고하였습니다.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "대출 요약", "</h3>",
      "<h4>", "단위: 만원", "</h3>",
      "원금 (대출금): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "연 이자율: ", input$interest, "%",
      "<br>",
      "기간: ", input$length, " 년 (", input$length * 12, " 월)",
      "<br>",
      "<b>", "월 납부액: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "이자포함 총금액: ", "</b>", format(round(input$principal, 0), big.mark = ","), 
      " (원금) + ", format(round(monthPay * 12 * input$length - input$principal, 0), big.mark = ","), 
      " (이자) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 0), big.mark = ","), "</b>"
    ))
  })
  
  output$distPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })
  
  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                              extensions = "Buttons",
                              options = list(
                                lengthChange = TRUE,
                                dom = "Blrtip",
                                buttons = c("csv", "excel", "print"),
                                
                                lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), 
                                                  c("All", "10", "12", "15", "25", "50", "100"))
                              ),
                              rownames = FALSE
    ) %>%
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), 
                     currency = "", interval = 3, mark = ",")
  })

}

# Run the application
shinyApp(ui = ui, server = server)