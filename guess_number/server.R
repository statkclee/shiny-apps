
library(shiny)

shinyServer(function(input, output) {
  
  number <-  sample(1:20, 1)
  counter <-  reactiveVal(0)
  guess <- reactiveVal()

  observeEvent({input$submit},
    {counter(counter() +1)
      guess(input$guess)
  })
  
  output$feedback <- renderText({
    req(guess(), counter())
    if (counter() > 3) {
      paste0("기회를 3번 모두 사용하셨습니다!!!")
    } else {
      if(guess() < number) {
      "추측한 숫자가 너무 작아요"
      } else if(guess() > number) {
        "추측한 숫자가 너무 커요"
      } else {
        "제대로 맞췄습니다!!! 축하합니다."
      }
    }
  })

})
