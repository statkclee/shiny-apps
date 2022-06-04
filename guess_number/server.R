
library(shiny)

shinyServer(function(input, output) {
  
  number <-  sample(1:20, 1)
  counter <-  reactiveVal(0)
  guess <- reactiveVal()

  observeEvent({input$submit},
    {counter(counter() +1)
      guess(input$guess)
  })
  
  observeEvent({
    counter()},
    { req(guess())
      if (counter() > 3 ){
        updateTabsetPanel(
          inputId = "guessing_area",
          selected = "incorrect_panel"
      ) 
     } else if (guess() == number) {
        updateTabsetPanel(
          inputId = "guessing_area",
          selected = "correct_panel"
      )
    }
  })
  

  output$feedback <- renderText({
    req(guess())
    if(guess() < number) {
      "추측한 숫자가 너무 작아요"
    } else if(guess() > number) {
        "추측한 숫자가 너무 커요"
    }
  })

})
