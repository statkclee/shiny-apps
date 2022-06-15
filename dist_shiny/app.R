
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
### *. 1.4. 기하 분포 : 첫번째 실패횟수 ----------------------------------
source("module_dist_discrete_geometry_failure.R", encoding = "UTF-8")
### *. 1.4. 기하 분포 : 첫번째 시행횟수 ----------------------------------
source("module_dist_discrete_geometry_trial.R", encoding = "UTF-8")
### *. 1.5. 음이항 : r번째 성공 실패횟수 ----------------------------------
source("module_dist_discrete_nb_failure.R", encoding = "UTF-8")
### *. 1.5. 음이항 분포 : r번째 시행횟수 ----------------------------------
source("module_dist_discrete_nb_trial.R", encoding = "UTF-8")
### *. 1.6. 초기하 분포 : 비복원 추출 ----------------------------------
source("module_dist_discrete_hypergeometric.R", encoding = "UTF-8")


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
      ),
      tabPanel("음이항분포-실패횟수",
          dist_discrete_nb_failure_UI("dist_discrete_nb_failure")
      ),
      tabPanel("음이항분포-시행횟수",
          dist_discrete_nb_trial_UI("dist_discrete_nb_trial")
      ),
      tabPanel("초기하 분포",
          dist_discrete_hypergeometric_UI("dist_discrete_hypergeometric")
      )
   )
  )
)


server <- shinyServer(function(input, output) {
  
  # # I. 확률 분포 ------------------------
  dist_server("dist_image")
  
  # II. 이산형 확률분포 ---------------------------------               
  dist_discrete_beta_server("dist_discrete_beta")
  dist_discrete_binomial_server("dist_discrete_binomial")
  dist_discrete_poisson_server("dist_discrete_poisson")
  dist_discrete_geometry_failure_server("dist_discrete_geometry_failure")
  dist_discrete_geometry_trial_server("dist_discrete_geometry_trial")
  dist_discrete_nb_failure_server("dist_discrete_nb_failure")
  dist_discrete_nb_trial_server("dist_discrete_nb_trial")
  dist_discrete_hypergeometric_server("dist_discrete_hypergeometric")  

})


shinyApp(ui, server)