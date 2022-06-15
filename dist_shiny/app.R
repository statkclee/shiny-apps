
# *. 가설검정 ---------------------------------
source("_common.R", encoding = "UTF-8")

# *. 0. 분포 ---------------------------------
## *. 1. 이산형 분포 ---------------------------------
source("module_dist_discrete_image.R", encoding = "UTF-8")
### *. 1.1. 베타 분포 ----------------------------------
source("module_dist_discrete_beta.R", encoding = "UTF-8")
### *. 1.2. 이항 분포 ----------------------------------
source("module_dist_discrete_binomial.R", encoding = "UTF-8")
### *. 1.3. 포아송 분포 ----------------------------------
source("module_dist_discrete_poisson.R", encoding = "UTF-8")
### *. 1.4. 기하 분포 : 실패횟수 ----------------------------------
source("module_dist_discrete_geometry_failure.R", encoding = "UTF-8")
### *. 1.4. 기하 분포 : 시행횟수 ----------------------------------
source("module_dist_discrete_geometry_trial.R", encoding = "UTF-8")


ui <- shinyUI(

  navbarPage("확률 분포",
             
    # I. 분포 ---------------------------------
    tabPanel("확률 분포 분류",
             dist_UI("dist_image")
    ),
             
    # II. 이산형 확률분포 ---------------------------------             
    navbarMenu("이산형 확률분포",
      tabPanel("베타 분포",
         dist_discrete_beta_UI("dist_discrete_beta"),
       ),
      tabPanel("이항 분포",
         dist_discrete_binomial_UI("dist_discrete_binomial")
       ),
      tabPanel("포아송 분포",
         dist_discrete_poisson_UI("dist_discrete_poisson")
      ),
      tabPanel("기하분포-실패횟수",
         dist_discrete_geometry_failure_UI("dist_discrete_geometry_failure")
      ),
      tabPanel("기하분포-시행횟수",
         dist_discrete_geometry_trial_UI("dist_discrete_geometry_trial")
      )
   )
  )
)


server <- shinyServer(function(input, output) {
  
  # # I. 확률 분포 ------------------------
  dist_server("dist_image")
  
  # II. 이산형 확률분포 ---------------------------------               
  ## 1.베타 분포 ---------------------------------
  dist_discrete_beta_server("dist_discrete_beta")
  dist_discrete_binomial_server("dist_discrete_binomial")
  dist_discrete_poisson_server("dist_discrete_poisson")
  dist_discrete_geometry_failure_server("dist_discrete_geometry_failure")
  dist_discrete_geometry_trial_server("dist_discrete_geometry_trial")

})


shinyApp(ui, server)