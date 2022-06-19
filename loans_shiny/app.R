
# *. 주택담보대출 ---------------------------------
source("module_mortgage.R")
source('global.R')


ui <- shinyUI(
  
  navbarPage("통계학",
             
     # I. 주택담보대출 ---------------------------------
     tabPanel("아파트 대출",
          module_mortgage_UI("loans_sim")
     ),

     
     # IV. footer.html ---------------------------------                   
     tags$footer(
       tags$div(
         class = "footer_container", 
         includeHTML(path = "www/footer.html")
       )
     )
  )
)


server <- shinyServer(function(input, output) {

  # I. 주택담보대출 ------------------------
  module_mortgage_server("loans_sim")

})


shinyApp(ui, server)