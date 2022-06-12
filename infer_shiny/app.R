library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

# app_ui 
app_ui <- function(request) {
  tagList(
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(title = "module_test",
                                                       enable_rightsidebar = FALSE),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "tabs",
                                    mod_test_sidebar_ui("test_ui_1"))
      ),
      #
      body =  shinydashboard::dashboardBody(shinydashboard::tabItems(
        mod_test_body_ui("test_ui_1"))
      ),
      title = "Testing Shiny modules"
    )
  )
}
# app_server 
app_server <- function(input, output, session) {
  shiny::moduleServer(id = "test_ui_1", module = mod_test_server)
}

##   THE MODULES   #######################################################
# the sidebar module
mod_test_sidebar_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::menuItem("Module Testing",
                           tabName = "tab_testing_mod",
                           icon = icon("th"))
}
#---------------------------------
# the body module b/c wanna use tabs I decided to add one more mod layer 
mod_test_body_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::tabItem(tabName = "tab_testing_mod",
                          mod_test_modules_ui(id)
                          
  )
}
# the ('additional') body_ui "content" module
mod_test_modules_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      title = "Select Cols",
      selectInput(ns("select"), "Select columns", names(mtcars), multiple = TRUE)
    )
    , 
    shinydashboard::box(
      title = "Data Viewer",
      width = 10,
      DT::dataTableOutput(ns('data_table'))
    )
  )
}
#---------------------------------
#module server
mod_test_server <- function(input, output, session) {
  ns <- session$ns
  output[['data_table']] <- renderDataTable({
    #output$data_table <- renderDataTable({
    columns = names(mtcars)
    if (!is.null(input$select)) {
      columns = input$select
    }
    mtcars[,columns,drop=FALSE]
  }, filter = 'top')
}
####################################################################
run_app <- function(...) {
  shiny::shinyApp(
    ui = app_ui, 
    server = app_server)
}
#---------------------------------
run_app()