
# *. 1. 분포 ---------------------------------
source("module_clt.R", encoding = "UTF-8")

ui <- shinyUI(
  
  navbarPage("통계학",
             
     # I. 분포 ---------------------------------
     tabPanel("중심극한정리",
         module_clt_UI("clt_sim")
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
  
  # I. 중심극한정리 ------------------------
  module_clt_server("clt_sim")
  

})


shinyApp(ui, server)