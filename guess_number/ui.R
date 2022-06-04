
# https://www.youtube.com/watch?v=87rpiuRyhaQ&t=960s

library(shiny)

ui <- fluidPage(

  tabsetPanel(
    id = "guessing_area",
    type = "hidden",
    selected = "guess_panel",
    
    tabPanelBody(
      "correct_panel",
      h1("맞췄습니다. 축하합니다.")

    ),
    
    tabPanelBody(
      "incorrect_panel",
      h1("틀렸습니다~~~~")
    ),
    
    tabPanelBody(
      "guess_panel",
      
      h1("번호 맞추기"),
      p("번호 맞추기 게임에 오신 것을 환영합니다!!! 한국 R 사용자회에서 제작하여 공급하는 오픈 디지털 콘텐츠입니다."),
      
      h1("게임 방법"),
      p("숫자 1 ~ 20 사이 정수를 컴퓨터가 갖고 있습니다. 3번의 기회를 드릴텐데... 맞춰보세요!!!"),
      
      h1("숫자를 입력하세요"),
      numericInput(
        inputId = "guess",
        label   = "숫자를 입력하세요",
        value = NULL, min = 1, max = 20,
        step = 1
      ),
      
      actionButton(
        inputId = "submit",
        label   = "추측한 숫자를 제출합니다!!!"
      ),
      
      h1("컴퓨터 응답"),
      textOutput(
        outputId = "feedback"
      )
      
    )
  )
  
)