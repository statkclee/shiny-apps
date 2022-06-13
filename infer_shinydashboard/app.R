
# source(glue::glue("{here::here()}/infer_shinydashboard/module_onemeans.R"))
# source(glue::glue("{here::here()}/infer_shinydashboard/_common.R"))

source("module_onemeans.R", encoding = "UTF-8")
source("_common.R", encoding = "UTF-8")


ui <- shinyUI(
  fluidPage(
  
  titlePanel("가설검정"),
  
      one_means_UI("means_one")      
))


server <- shinyServer(function(input, output) {
  
  one_means_server("means_one")
  
})


shinyApp(ui, server)