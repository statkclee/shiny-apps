
means_UI <- function(id) {
  
  ns <- NS(id)

  box(title = "1 표본 평균 검정",
      tags$br(),
      tags$b("데이터"),
      
      textInput(ns("sample_onemean"), "표본", value = "3,7,11,0,7,0,4,5,6,2", 
                placeholder = "콤마로 구분된 숫자를 입력하세요. 예를 들어, 7.2, 4.7, 5, 5.03, 등"),
      tags$p("※ 콤마로 구분된 숫자 입력. ex) 7.2, 4.7, 5.03, 등"),
      
      tags$br(),
      
      tags$b("가설검정"),
      
      tags$p("1. 귀무가설"),
      tags$p("\\( H_0 : \\mu = \\)"),
      
      numericInput(ns("onemean_h0"),
                   label = NULL,
                   value = 3, step = 0.1, width="100px"),
      
      tags$p("2. 검정방향"),
      radioButtons(
        inputId = ns("onemean_alternative"),
        label = "대립가설",
        inline = TRUE,
        choices = c(
          "\\( \\neq \\)" = "two.sided",
          "\\( > \\)" = "greater",
          "\\( < \\)" = "less"
        )
      ),
      
      tags$p("3. 모집단 분산"),
      checkboxInput(ns("popsd_onemean"), "모집단 분산을 알는 경우:", FALSE),
      conditionalPanel(
        condition = "input.popsd_onemean == 1",
        numericInput(ns("sigma2_onemean"), "\\(\\sigma^2 = \\)",
                     value = 2, min = 0, step = 0.1, width = "100px")
      ),
      
      
      tags$p("4. 유의수준"),
      sliderInput(ns("onemean_alpha"),
                  "유의수준 \\(\\alpha = \\)",
                  min = 0.01,
                  max = 0.20,
                  value = 0.05)
  )
  
  box(
    title = "시각화",
    plotOutput("test_means_one_plot")
  )
  
  box(title = "표",
      tableOutput("test_means_one_tbl")
  )
}

means_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
})
