
# Shiny ui for Normal & t distributions

source('global.R')

# Define UI for the application

module_tdist_normal_ui <- shiny::tagList(
  
  fluidPage(
    sidebarPanel(width=3,
                 
      tags$h3("표준정규분포와 t-분포 특성"),
      tags$br(),

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("표준 정규 분포: 모수")),
      tags$hr(style="border-color: purple;"),

      tags$br(),

      tags$div(
        tags$span(style="color:darkred",
                  tags$strong(
                    HTML("평균 ("),
                    HTML("&mu;"),
                    HTML(") =   0")
                  )
        )
      ),
        
      tags$br(),

      tags$div(
        tags$span(style="color:darkred",
                  tags$strong(
                    HTML("표준편차 ("),
                    HTML("&sigma;"),
                    HTML(") =   1")
                  )
        )
      ),


      tags$br(),
      tags$br(),
      
      radioButtons(
          inputId = "radio_normal_studnet",
          label = NULL,
          choices = c(
              "두 분포 겹쳐보기 : " = "overlay",
              "두 분포 분리보기 : " = "separate"
          )
      ),

      conditionalPanel(
          condition = "input.radio_normal_studnet == 'overlay'",
          # condition = paste0("input['", ns("lower_tail_beta"), "'] == 'lower.tail'"),       
          tags$hr(style="border-color: purple;"),
          tags$p(style="color:blue", tags$strong("t-분포: 모수")),
          tags$hr(style="border-color: purple;"),
          
          
          tags$br(),
          
          sliderInput(inputId = 'df',
                      label = tags$strong('자유도: df', style="color:darkblue"),
                      value = 1, min = 1, max = 500, step = 1),
          
          tags$br()
        ),

      conditionalPanel(
          condition = "input.radio_normal_studnet == 'separate'",
          tags$hr(style="border-color: purple;"),
          tags$p(style="color:blue", tags$strong("Probability")),
          tags$hr(style="border-color: purple;"),
          
          sliderInput(inputId = 'p',
                      tags$strong('누적확률', style="color:darkblue"),
                      min = 0.05, max = 1, value = 0.05, step = 0.01),
          
          radioButtons(inputId = 'p_tail',
                       label = tags$strong('확률 꼬리:', style="color:darkblue"),
                       choices = c('하단 꼬리 (왼쪽 꼬리)' = 'lower',
                                   '상단 꼬리 (오른쪽 꼬리)' = 'upper',
                                   '양쪽 꼬리' = 'both'),
                       selected = 'both'),
          
          tags$hr(style="border-color: blue;")
      )
  ),


  mainPanel(

    conditionalPanel(
      condition = "input.radio_normal_studnet == 'overlay'",
        box(title = '확률밀도 함수', width = 12,
             plotOutput(outputId = 'dnorm_dt_plot', height = '750'))),
    conditionalPanel(
      condition = "input.radio_normal_studnet == 'separate'",
        box(title = '분포와 확률', width = 12,
             plotOutput(outputId = 'dnorm_plot', height = '375'),
             plotOutput(outputId = 'dt_plot', height = '375')))
  )
))

