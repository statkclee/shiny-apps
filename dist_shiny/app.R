
# *. 가설검정 ---------------------------------
source("_common.R", encoding = "UTF-8")

# *. 0. 분포 ---------------------------------
## *. 1. 이산형 분포 ---------------------------------
source("module_dist_discrete_image.R", encoding = "UTF-8")
### *. 1.1. 이항 분포 ----------------------------------
source("module_dist_discrete_binomial.R", encoding = "UTF-8")
### *. 1.2. 포아송 분포 ----------------------------------
source("module_dist_discrete_poisson.R", encoding = "UTF-8")
### *. 1.3. 기하 분포 : 첫번째 실패횟수 ----------------------------------
source("module_dist_discrete_geometry_failure.R", encoding = "UTF-8")
### *. 1.3. 기하 분포 : 첫번째 시행횟수 ----------------------------------
source("module_dist_discrete_geometry_trial.R", encoding = "UTF-8")
### *. 1.4. 음이항 : r번째 성공 실패횟수 ----------------------------------
source("module_dist_discrete_nb_failure.R", encoding = "UTF-8")
### *. 1.4. 음이항 분포 : r번째 시행횟수 ----------------------------------
source("module_dist_discrete_nb_trial.R", encoding = "UTF-8")
### *. 1.5. 초기하 분포 : 비복원 추출 ----------------------------------
source("module_dist_discrete_hypergeometric.R", encoding = "UTF-8")
## *. 2. 연속형 분포 ---------------------------------
### *. 2.1. t-분포 ----------------------------------
source("module_dist_continuous_student.R", encoding = "UTF-8")
### *. 2.2. 정규 분포 ----------------------------------
source("module_dist_continuous_normal.R", encoding = "UTF-8")
### *. 2.3. 로그정규 분포 ----------------------------------
source("module_dist_continuous_lognormal.R", encoding = "UTF-8")
### *. 2.4. 지수 분포 ----------------------------------
source("module_dist_continuous_exponential.R", encoding = "UTF-8")
### *. 2.5. 카이제곱 분포 ----------------------------------
source("module_dist_continuous_chisquare.R", encoding = "UTF-8")
### *. 2.6. 감마 분포 ----------------------------------
source("module_dist_continuous_gamma.R", encoding = "UTF-8")
### *. 2.7. F-분포 ----------------------------------
source("module_dist_continuous_fisher.R", encoding = "UTF-8")
### *. 2.8. 베타 분포 ----------------------------------
source("module_dist_continuous_beta.R", encoding = "UTF-8")
### *. 2.9. 로지스틱 분포 ----------------------------------
source("module_dist_continuous_logistic.R", encoding = "UTF-8")
### *. 2.10. 와이블 분포 ----------------------------------
source("module_dist_continuous_weibull.R", encoding = "UTF-8")


ui <- shinyUI(

  navbarPage("통계학",
             
    # I. 분포 ---------------------------------
    tabPanel("확률 분포 분류",
             dist_UI("dist_image")
    ),
             
    # II. 이산형 확률분포 ---------------------------------             
    navbarMenu("이산형 확률분포",
      tabPanel("이항 분포",
         dist_discrete_binomial_UI("dist_discrete_binomial")
       ),
      tabPanel("포아송 분포",
         dist_discrete_poisson_UI("dist_discrete_poisson")
      ),
      tabPanel("---------------------"),
      tabPanel("기하분포-실패횟수",
         dist_discrete_geometry_failure_UI("dist_discrete_geometry_failure")
      ),
      tabPanel("기하분포-시행횟수",
         dist_discrete_geometry_trial_UI("dist_discrete_geometry_trial")
      ),
      tabPanel("---------------------"),
      tabPanel("음이항분포-실패횟수",
          dist_discrete_nb_failure_UI("dist_discrete_nb_failure")
      ),
      tabPanel("음이항분포-시행횟수",
          dist_discrete_nb_trial_UI("dist_discrete_nb_trial")
      ),
      tabPanel("---------------------"),
      tabPanel("초기하 분포",
          dist_discrete_hypergeometric_UI("dist_discrete_hypergeometric")
      )
    ),
    # III. 연속형 확률분포 ---------------------------------             
    navbarMenu("연속형 확률분포",
     tabPanel("t-분포",
        dist_continuous_student_UI("dist_continuous_student")
     ),
     tabPanel("정규 분포",
         dist_continuous_normal_UI("dist_continuous_normal")
      ),
     tabPanel("로그-정규 분포",
       dist_continuous_lognormal_UI("dist_continuous_lognormal")
     ),
     tabPanel("---------------------"),
     tabPanel("지수 분포",
        dist_continuous_exponential_UI("dist_continuous_exponential")
     ),
     tabPanel("감마 분포",
              dist_continuous_gamma_UI("dist_continuous_gamma")
     ),
     tabPanel("---------------------"),
     tabPanel("카이제곱 분포",
        dist_continuous_chisquare_UI("dist_continuous_chisquare")
     ),
     tabPanel("F-분포",
        dist_continuous_fisher_UI("dist_continuous_fisher")
     ),     
     tabPanel("---------------------"),
     tabPanel("베타 분포",
         dist_continuous_beta_UI("dist_continuous_beta")
      ),
     tabPanel("로지스틱 분포",
         dist_continuous_logistic_UI("dist_continuous_logistic")
     ), 
     tabPanel("와이블 분포",
         dist_continuous_weibull_UI("dist_continuous_weibull")
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
)


server <- shinyServer(function(input, output) {
  
  # # I. 확률 분포 ------------------------
  dist_server("dist_image")
  
  # II. 이산형 확률분포 ---------------------------------               
  dist_discrete_binomial_server("dist_discrete_binomial")
  dist_discrete_poisson_server("dist_discrete_poisson")
  dist_discrete_geometry_failure_server("dist_discrete_geometry_failure")
  dist_discrete_geometry_trial_server("dist_discrete_geometry_trial")
  dist_discrete_nb_failure_server("dist_discrete_nb_failure")
  dist_discrete_nb_trial_server("dist_discrete_nb_trial")
  dist_discrete_hypergeometric_server("dist_discrete_hypergeometric")  
  
  # III. 연속형 확률분포 ---------------------------------               
  dist_continuous_student_server("dist_continuous_student")
  dist_continuous_normal_server("dist_continuous_normal")
  dist_continuous_lognormal_server("dist_continuous_lognormal")
  dist_continuous_exponential_server("dist_continuous_exponential")
  dist_continuous_beta_server("dist_continuous_beta")
  dist_continuous_chisquare_server("dist_continuous_chisquare")  
  dist_continuous_gamma_server("dist_continuous_gamma")  
  dist_continuous_fisher_server("dist_continuous_fisher")
  dist_continuous_logistic_server("dist_continuous_logistic")  
  dist_continuous_weibull_server("dist_continuous_weibull")  
  
})


shinyApp(ui, server)