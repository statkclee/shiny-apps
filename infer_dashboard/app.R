
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)

source("_common.R")

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


ui <- dashboardPage(
  dashboardHeader(title = "추론"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      convertMenuItem(menuItem("NHST 검정", tabName = "sub_nhst", icon = icon("map"), selected = FALSE, show = FALSE,
                               menuSubItem("평균", tabName = "nhst_workflow_mean"),
                               menuSubItem("비율", tabName = "nhst_workflow_proportion"),
                               menuSubItem("분산", tabName = "nhst_workflow_variance")), "sub_nhst")
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem("nhst_workflow_mean", 
              imageOutput("nhst_workflow_mean_img")
      ),
      tabItem("nhst_workflow_proportion", 
              imageOutput("nhst_workflow_proportion_img")
      ),
      tabItem("nhst_workflow_variance", 
              imageOutput("nhst_workflow_variance_img")
      )
    )
  )
)

server <- function(input, output, session) {

  output$nhst_workflow_mean_img <- renderImage({
      filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-means.png")
      list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)

  output$nhst_workflow_proportion_img <- renderImage({
    filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-proportion.png")
    list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)
  
  output$nhst_workflow_variance_img <- renderImage({
    filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-variance.png")
    list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)
}
  


shinyApp(ui, server)