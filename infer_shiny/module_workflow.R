
library(showtext)
font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


NHST_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,

    fluidRow(
      uiOutput(outputId = ns("workflow_image"))
    )
  )
  
  # 2. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 2,

    tags$br(),
    # tags$h2("NHST"),
    radioButtons(inputId = ns("nhst_bttn_selected"), "NHST 가설선택",
                 c("평균" = "nhst_means",
                   "비율" = "nhst_prop",
                   "분산" = "nhst_var"),
                 selected = "nhst_means")

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



NHST_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$workflow_image <- renderUI({
      if (input$nhst_bttn_selected == "nhst_means") {
        tags$img(src='nhst-means.png', align = "center")
      } else if (input$nhst_bttn_selected == "nhst_prop") {
        tags$img(src='nhst-proportion.png', align = "center")
      } else {
        tags$img(src='nhst-variance.png', align = "center")
      }
      
    })
    

  })
}
