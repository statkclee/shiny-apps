

home_page <- div(
  tabPanel(title = "NHST 검정", 
           sidebarLayout(
             sidebarPanel(
               width = 2,
               shinyWidgets::awesomeRadio(inputId = "nhst_type", 
                                          label = "검정대상", 
                                          choices  = list("평균" = "nhst_mean", 
                                                          "비율" = "nhst_prop", 
                                                          "분산" = "nhst_variance"))
             ),
             mainPanel(
               fluidRow(
                 uiOutput("nhst_img")
               )
             )
           )
  )
)
