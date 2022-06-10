
library(shiny)
library(shinythemes)
library(shiny.router)
library(shinyWidgets)

# https://stackoverflow.com/questions/71541259/uri-routing-with-shiny-router-and-navbarpage-in-a-r-shiny-app

ui <- navbarPage(title = "가설검정", id = "navbarID", theme = shinytheme("flatly"), 
                 
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
                  # img(src='nhst-means.png', align = "center")
                )
              )
            )
          ),
         tabPanel("평균", value = "mean_page", "This is Page 2")
)

server <- function(input, output, session){
  
  # NavBar ----------------------------------------------
  
  observeEvent(input$navbarID, {
    # http://127.0.0.1:3252/#page_1
    # http://127.0.0.1:3252/#page_2
    
    newURL <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "#",
      input$navbarID
    )
    updateQueryString(newURL, mode = "replace", session)
  })
  
  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateNavbarPage(session, "navbarID", selected = currentTab)
    }
  })
  
  # NHST 선택지 ----------------------------------------------


  output$nhst_img <- renderUI({
    
    if(input$nhst_type ==  "nhst_mean") {
      img(src='nhst-means.png', align = "center")
    } else if(input$nhst_type ==  "nhst_prop") {
      img(src='nhst-proportion.png', align = "center")
    } else {
      img(src='nhst-variance.png', align = "center")
    }
    
  })
  
  
}

shinyApp(ui, server)
