
source(glue::glue("{here::here()}/infer_shinydashboard/module_onemeans.R"))
source(glue::glue("{here::here()}/infer_shinydashboard/_common.R"))

ui <- shinyUI(
  fluidPage(
  
  titlePanel("가설검정"),
  
      one_means_UI("means_one")      
))


server <- shinyServer(function(input, output) {
  
  one_means_server("means_one")
  
})


shinyApp(ui, server)