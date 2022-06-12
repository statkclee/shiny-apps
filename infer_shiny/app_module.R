
library(shiny)
library(shiny.router)
library(plotly)

source("_common.R")


home_UI <- function(id) {
  
  ns <- NS(id)
  
  home_page <- div(
    tabPanel(title = "NHST 검정", 
             sidebarLayout(
               sidebarPanel(
                 width = 2
               ),
               mainPanel(
                   uiOutput(outputId = ns("workflow_var_plot"))
             )
    )
  ))
}


home_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$workflow_mean_plot <- renderUI({
      tags$img(src='nhst-means.png', align = "center")
    })
    output$workflow_prop_plot <- renderUI({
      tags$img(src='nhst-proportion.png', align = "center")
    })
    output$workflow_var_plot <- renderUI({
      tags$img(src='nhst-variance.png', align = "center")
    })
    
  })
}


ui <- shinyUI(
  home_UI("module_home")
  
)

server <- function(input, output, session) {

  
  home_server("module_home")

}

shinyApp(ui, server)

