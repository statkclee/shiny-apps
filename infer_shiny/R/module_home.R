
home_UI <- function() {
  
  ns <- NS(id)
  
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
}


home_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
  
  output$nhst_img <- renderUI({
    
    if(input$nhst_type ==  "nhst_mean") {
      img(src='nhst-means.png', align = "center")
    } else if(input$nhst_type ==  "nhst_prop") {
      img(src='nhst-proportion.png', align = "center")
    } else {
      img(src='nhst-variance.png', align = "center")
    }
    
  })
  })
}
