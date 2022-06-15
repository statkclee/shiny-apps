
# *. 가설검정 ---------------------------------
source("_common.R", encoding = "UTF-8")
source("module_workflow.R", encoding = "UTF-8")

source("module_one_means.R", encoding = "UTF-8")
source("module_two_means.R", encoding = "UTF-8")

source("module_one_proportion.R", encoding = "UTF-8")
source("module_two_proportion.R", encoding = "UTF-8")

source("module_one_variance.R", encoding = "UTF-8")
source("module_two_variance.R", encoding = "UTF-8")

# *. 분포 ---------------------------------
source("module_distribution.R", encoding = "UTF-8")


ui <- shinyUI(

  navbarPage("NHST 가설검정",
    # I. 분포 ---------------------------------             
    tabPanel("확률 분포",
             dist_UI("dist_image")
    ),
             
    # II. 가설 선택 ---------------------------------             
    tabPanel("가설 선택",
              NHST_UI("NHST_image")
    ),
    ## II-1. 평균 가설검정 -----------------------------
    navbarMenu("평균",
      tabPanel("1 표본",
               one_means_UI("means_one") 
                ),             
      tabPanel("2 표본",
               two_means_UI("means_two") 
      )
    ),
    ## II-2. 비율 가설검정 -----------------------------    
    navbarMenu("비율",
       tabPanel("1 표본",
                one_proportion_UI("prop_one") 
       ),             
       tabPanel("2 표본",
                two_proportion_UI("prop_two") 
       )
    ),
    ## II-3. 분산 가설검정 -----------------------------    
    navbarMenu("분산",
               tabPanel("1 표본",
                        one_variance_UI("var_one") 
               ),             
               tabPanel("2 표본",
                        two_variance_UI("var_two")
               )
    ),
    
    tags$footer(
      tags$div(
        class = "footer_container", 
        
        includeHTML(path = "www/footer.html")
      )
    )
  )
)


server <- shinyServer(function(input, output) {
  
  # I. 확률 분포 ------------------------
  dist_server("dist_image")
  
  # II. 가설검정 ------------------------
  NHST_server("NHST_image")
  
  one_means_server("means_one")
  two_means_server("means_two")
  
  one_proportion_server("prop_one")
  two_proportion_server("prop_two")

  one_variance_server("var_one")
  two_variance_server("var_two")
  
})


shinyApp(ui, server)