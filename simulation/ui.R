library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# 1. Header ------
header <- dashboardHeader(title = "모의실험")

# 2. Sidebar ------
sidebar <- dashboardSidebar(
  sidebarMenu(id = "simulation_menu",
    menuItem("동전 던지기", tabName = "toss_coin_menu", icon = icon(name = "bitcoin", lib = "font-awesome"), 
             badgeColor = "green"), 
    actionButton("coin_button", "던져보자"),
    sliderInput(inputId = "coin_prob", label = "동전앞면 출현 확률", min =0, max =1, value =0.5, step=0.01),
    menuItem("주사위를 굴려보자!", tabName = "roll_dice_menu", 
             icon = icon(name = "gamepad", lib = "font-awesome", badgeLabel = "new")),
    actionButton("dice_button", "굴려보자")
    )    
)

# 3. Body ------
body <- dashboardBody(
  
  conditionalPanel(
    condition = "input.simulation_menu == 'toss_coin_menu'",
    ### 3.1. 동전 던지기 ----------  
    tabItem(tabName = "toss_coin",
            box(width = 6, title = "동전 사진", status = "primary", solidHeader = TRUE,
                imageOutput("coin_image", height = 300)),
            box(width = 3, title = "동전 앞뒤 결과", status = "primary", solidHeader = TRUE,
                verbatimTextOutput("toss_coin_text")
            ),
            box(width = 3, title = "동전 앞 확률", status = "primary", solidHeader = TRUE,
                verbatimTextOutput("toss_coin_prob_text")
            ),
            box(width = 6, title = "동전 던지기 이력", status = "primary", solidHeader = TRUE,
                verbatimTextOutput("toss_coin_history_text")
            )            
    )
  ),

  conditionalPanel(
    condition = "input.simulation_menu == 'roll_dice_menu'",
    ### 3.2. 주사위 던지기 ----------
    tabItem(tabName = "roll_dice",
            box(title = "주사위 사진",
              width = 6,
                     imageOutput("dice_image", height = 300)
            ),
            box(title = "주사위 눈",
                width = 6,
                verbatimTextOutput("roll_dice_text")
            )
      )
  )
  
)

ui <- dashboardPage( title = "모의실험",
                     header, sidebar, body)

