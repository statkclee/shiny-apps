
# 1. Header ------
header <- dashboardHeader(title = "선거통계")

# 2. Sidebar ------
sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "sido_menu",
              selectInput(inputId = "type_select", label = "투표지표", choices = c("투표율", "정당별 득표수", "정당별 득표율")),
              selectInput(inputId = "sido_select", label = "시도", choices = c("서울특별시", "부산광역시", "대구광역시", "인천광역시", 
                                                                             "광주광역시", "대전광역시", "울산광역시", "세종특별자치시", 
                                                                             "경기도", "강원도", "충청북도", "충청남도", "전라북도", 
                                                                             "전라남도", "경상북도", "경상남도", "제주특별자치도"
              ))
    
  )
)

# 3. Body ------
body <- dashboardBody(
  
  # 투표수/투표율 + 지도 ----------------------
  conditionalPanel(condition = 'input.type_select == "투표율"',
                   box(title = "투표율",
                       shiny::tableOutput("sido_casting")),
                   box(title = "구시군 득표율 지도",
                       shiny::plotOutput("sido_casting_map"))
                   ),
  # 득표수 + 지도 ----------------------
  conditionalPanel(condition = 'input.type_select == "정당별 득표수"',
                   box(title = "정당별 득표수",
                       shiny::tableOutput("sido_party_vote")),
                   box(title = "구시군 (정당별) 득표수 지도",
                       shiny::plotOutput("sido_party_vote_map"))
                   ),
  conditionalPanel(condition = 'input.type_select == "정당별 득표율"',
                   box(title = "정당별 득표율",
                       shiny::tableOutput("sido_party_rate")),
                   box(title = "구시군 (정당별) 득표수 지도",
                       shiny::plotOutput("sido_party_rate_map"))
  )
)

ui <- dashboardPage( title = "선거통계",
                     header, sidebar, body)

