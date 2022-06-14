

dist_UI <- function(id) {
  
  ns <- NS(id)
  
  # 1. 메인 패널 ------------------------------------
  mainPanel <- mainPanel(width = 10,
                         
     fluidRow(
       uiOutput(outputId = ns("distribution_image"))
     )
  )
  
  # 2. 옆 패널 --------------------------------------
  sidebarPanel <- sidebarPanel(width = 2,
                               
     tags$br(),
     # tags$h2("NHST"),
     radioButtons(inputId = ns("distirbution_bttn_selected"), "분포",
                  c("이산형 확률분포" = "dist_discrete",
                    "표준정규분포" = "dist_normal",
                    "연속형 확률분포" = "dist_continuous"),
                  selected = "dist_normal")
                               
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



dist_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$distribution_image <- renderUI({
      if (input$distirbution_bttn_selected == "dist_discrete") {
        tags$img(src='distribution/distribution-discrete.png', align = "center", width ="77%")
      } else if (input$distirbution_bttn_selected == "dist_normal") {
        tags$img(src='distribution/distribution-normal.png', align = "center", width ="77%")
      } else {
        tags$img(src='distribution/distribution-continuous.png', align = "center", width ="63%")
      }
      
    })
    
    
  })
}
