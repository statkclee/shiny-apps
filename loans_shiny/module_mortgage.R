library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# 1. 모듈 UI ----------------------------
module_mortgage_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     # 대출 요약 ---------------------
     uiOutput( ns("text") ),
     br(),
     # 시각화와 표 ---------------------
     conditionalPanel(
       # condition = "input.plot == 1",
       condition = paste0("input['", ns("plot"), "'] == 1"),
       plotOutput(ns("distPlot"))
     ),
     br(),
     conditionalPanel(
       # condition = "input.plot != 1",
       condition = paste0("input['", ns("plot"), "'] != 1"),
       DT::dataTableOutput( ns("tbl") )
     ),
     br(),
     p(em("공지사항: 이 Shiny 앱은 어떠한 투자정보와 추천, 혹은 금융분석에 대한 정보를 
     담고 있지 않습니다. 이 앱은 단순히 금융정보를 제공하는 것이고 이를 활용한 
     모든 투자는 본인이 스스로 결정한 것이라 이 앱과는 아무런 관련이 없습니다.
     이 앱에 담긴 정보를 활용한 어떤 결정도 한국 R 사용자회와 무관합니다.")),
     p(em("이 Shiny 앱은 Antoine Soetewey 교수 코드를 참고하였습니다.")),
    br()
  )
  
  # 2. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 2,
                               
     # 원금/이자율/대출기간 ----
     numericInput( ns("principal"), "원금 (대출금, 만원)", 10000, 
                   min = 0, step = 1000),
     
     tags$hr(style="border-color: blue;"),      
     
     numericInput( ns("interest"), "연 이자율 (%)", 4, 
                   min = 0, max = 100, step = 0.01),
     hr(),
     sliderInput( ns("length"), "대출기간 (년)",
                 min = 0,
                 max = 40,
                 value = 30,
                 step = 1
     ),
     
     hr(),
     checkboxInput( ns("plot"), "그래프 혹은 표", TRUE),
     
     tags$hr(style="border-color: blue;"),   
     
     HTML("원리금균등상환 : 원금과 이자를 합한 상환금액이 매달 동일")
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
module_mortgage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$text <- renderUI({
      mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
      HTML(paste0(
        "<h3>", "대출 요약", "</h3>",
        "<h4>", "[단위: 만원]", "</h3>",
        "원금 (대출금): ", format(round(input$principal, 2), big.mark = ","), " 만원",
        "<br>",
        "연 이자율: ", input$interest, "%",
        "<br>",
        "기간: ", input$length, " 년 (", input$length * 12, " 월)",
        "<br>",
        "<b>", "월 납부액: ", format(round(monthPay, digits = 0), big.mark = ","), " 만원 </b>",
        "<br>",
        "<b>", "이자포함 총금액: ", "</b>", format(round(input$principal, 0), big.mark = ","), 
        " (원금) + ", format(round(monthPay * 12 * input$length - input$principal, 0), big.mark = ","), 
        " (이자) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 0), big.mark = ","), " 만원</b>"
      ))
    })
    
    output$distPlot <- renderPlot({
      mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
    })
    
    # Data output
    output$tbl <- DT::renderDataTable({
      mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
      df_month <- DT::datatable(data.frame(round(aDFmonth, 2)) %>% 
                                  rename( 월 = Month,
                                         년 = Year,
                                         대출잔액 = Balance,
                                         월상환액 = Payment,
                                         원금     = Principal,
                                         이자     = Interest) %>% 
                                  select(년, 월, 대출잔액, 월상환액, 원금, 이자),
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
        formatCurrency(c("대출잔액", "월상환액", "원금", "이자"), 
                       currency = "", interval = 3, mark = ",")
    })
    
    
  })
}
