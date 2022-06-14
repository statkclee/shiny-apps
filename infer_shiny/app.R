
source("_common.R", encoding = "UTF-8")

source("module_workflow.R", encoding = "UTF-8")

source("module_onemeans.R", encoding = "UTF-8")
source("module_twomeans.R", encoding = "UTF-8")

source("module_one_proportion.R", encoding = "UTF-8")
source("module_two_proportion.R", encoding = "UTF-8")


ui <- shinyUI(

  navbarPage("NHST 가설검정",
    # 0. 가설 선택 ---------------------------------             
    tabPanel("가설 선택",
              NHST_UI("NHST_image")
    ),
    # 1. 평균 가설검정 -----------------------------
    navbarMenu("평균",
      tabPanel("1 표본",
               one_means_UI("means_one") 
                ),             
      tabPanel("2 표본",
               two_means_UI("means_two") 
      )
    ),
    # 2. 비율 가설검정 -----------------------------    
    navbarMenu("비율",
       tabPanel("1 표본",
                one_proportion_UI("prop_one") 
       ),             
       tabPanel("2 표본",
                two_proportion_UI("prop_two") 
       )
    )
  )
)


server <- shinyServer(function(input, output) {
  
  NHST_server("NHST_image")
  
  one_means_server("means_one")
  two_means_server("means_two")
  
  one_proportion_server("prop_one")
  two_proportion_server("prop_two")
  
})


shinyApp(ui, server)