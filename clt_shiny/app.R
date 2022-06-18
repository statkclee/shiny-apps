
# *. 평균/비율 중심극한정리 ---------------------------------
source("module_means_clt.R", encoding = "UTF-8")
source("module_prop_clt.R", encoding = "UTF-8")

ui <- shinyUI(
  
  navbarPage("통계학",
             
     # I. 분포 ---------------------------------
     tabPanel("평균 중심극한정리",
         module_means_clt_UI("clt_sim")
     ),
     tabPanel("비율 중심극한정리",
         module_prop_clt_UI("clt_prop_sim")
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
  
  # I. 평균 중심극한정리 ------------------------
  module_means_clt_server("clt_sim")
  # I. 평균 중심극한정리 ------------------------
  module_prop_clt_server("clt_prop_sim")

})


shinyApp(ui, server)