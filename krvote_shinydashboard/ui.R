
# 1. Header ------
header <- dashboardHeader(title = "선거통계")

# 2. Sidebar ------
sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "sido_menu",
              selectInput(inputId = "type_select", label = "투표지표", choices = c("투표율", "정당별 득표수/율")),
              selectInput(inputId = "sido_select", label = "시도", choices = sido_name_v)
    
  )
)

# 3. Body ------
body <- dashboardBody(
  
  conditionalPanel(condition = 'input.type_select == "투표율"',
                   box(title = "투표율",
                       shiny::tableOutput("sido_casting"))
                   ),
  conditionalPanel(condition = 'input.type_select == "정당별 득표수/율"',
                   box(title = "정당별 득표수",
                       shiny::tableOutput("sido_party_vote")
                   ),
                   box(title = "정당별 득표율",
                       shiny::tableOutput("sido_party_rate"))
  )
)

ui <- dashboardPage( title = "선거통계",
                     header, sidebar, body)

