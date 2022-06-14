
# source(glue::glue("{here::here()}/infer_shinydashboard/module_onemeans.R"))
# source(glue::glue("{here::here()}/infer_shinydashboard/_common.R"))

source("module_onemeans.R", encoding = "UTF-8")
source("module_workflow.R", encoding = "UTF-8")
source("_common.R", encoding = "UTF-8")


ui <- shinyUI(
  fluidPage(
  
  titlePanel("가설검정"),
  
  NHST_UI("NHST_image")
  # one_means_UI("means_one")
  
))


server <- shinyServer(function(input, output) {
  
  NHST_server("NHST_image")
  # one_means_server("means_one")
  
})


shinyApp(ui, server)