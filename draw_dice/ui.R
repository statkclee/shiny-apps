library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# 1. Header ------
header <- dashboardHeader(title = "모의실험")

# 2. Sidebar ------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("동전 던지기", tabName = "toss_coin", icon = icon(name = "bitcoin", lib = "font-awesome"), 
             badgeColor = "green"), actionButton("coin_button", "Toss !!!"),
    menuItem("주사위를 굴려보자!", tabName = "roll_dice", 
             icon = icon(name = "gamepad", lib = "font-awesome", badgeLabel = "new")),
             actionButton("dice_button", "실행!!!")
    )    
)

# 3. Body ------
body <- dashboardBody(

  ### 3.1. 동전 던지기 ----------  
  tabItem(tabName = "toss_coin",
          box(width = 6, title = "동전 사전", status = "primary", solidHeader = TRUE,
              "Toss a Coin", br(),
              imageOutput("coin_image", height = 300)),
          box(width = 6, title = "동전 앞뒤 결과", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("toss_coin_text")
          )
  ),
  
  ### 3.2. 주사위 던지기 ----------
  tabItem(tabName = "roll_dice",
          # tags$h2("심심할 때 주사위 던져보기"),
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

ui <- dashboardPage( title = "모의실험",
                     header, sidebar, body)

