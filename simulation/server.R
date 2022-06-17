
shinyServer(function(input, output, session) {
  
  # 1. Get Simulation Result !!! -----
  ## 1.1. Get Coin Value --------------
  
  coin_prob <- reactive({
    as.numeric(input$coin_prob)
  })
  
  coin_value <- reactive({
    input$coin_button
    coin_value <- isolate(sample(c("앞", "뒤"), size=1, prob = c(coin_prob(), 1 - coin_prob()), replace=TRUE))
  })
  
  ### 동전 던지기 이력
  coin_toss_val <- c()

  coin_toss_history <- reactive({
    coin_toss_val <<- c(coin_toss_val, coin_value())
  })
  
    
  ## 1.2. Get Dice Value --------------
  dice_value <- reactive({
    input$dice_button
    dice_value <- isolate(sample(1:6, size=1, prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6), replace=TRUE))
  })
  
  ### 주사위 던지기 이력 ------------------
  dice_draw_val <- c()
  
  dice_draw_history <- reactive({
    dice_draw_val <<- c(dice_draw_val, dice_value())
  })
  
  
  # 2. Value Box --------------------    
  ## 2.1. Dice Value Box ------------
  output$dice_valuebox <- renderValueBox({
    valueBox(
      value = sample(1:6, size=1, prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6), replace=TRUE),
      subtitle = "Die Mean Value",
      icon = icon("gamepad")
    )
  })
  ## 2.2. Coin Value Box ------------
  output$coin_valuebox <- renderValueBox({
    valueBox(
      value = sample(0:1, size=1, prob = c(1/2, 1/2), replace = TRUE),
      subtitle = "Coin Mean Value",
      icon = icon("bitcoin")
    )
  })
  
  # 3. Display a Simulation with Text --------------------
  ## 3.1. Display Coin Text Result --------------------
  output$toss_coin_text <- renderText({
    print(coin_value())
  })
  
  output$toss_coin_prob_text <- renderText({
    print(coin_prob())
  })

  output$toss_coin_history_text <- renderText({
    print(coin_toss_history())
  })
  
  
  ## 3.2. Display Dice Text Result --------------------
  output$roll_dice_text <- renderText({
    print(dice_value())
  })
  
  output$draw_dice_history_text <- renderText({
    print(dice_draw_history())
  })
  
  
  # 4. Display a Simulation with Image --------------------
  ## 3.1. Display Coin Text Result --------------------
  output$coin_image <- renderImage({
    
    if(coin_value() == "앞") {
      list(src = "www/coin_head.jpg", width = 200, contentType = "image/jpg", alt = "동전 앞면")
    } else {
      list(src = "www/coin_tail.jpg", width = 200, contentType = "image/jpg", alt = "동전 뒷면")
    }
    
  }, deleteFile = FALSE)  
  
  ## Display Image Result --------------------
  output$dice_image <- renderImage({
    
    if(dice_value() == 1) {
      list(src = "www/noun-dice-1.png", width = 200, contentType = "image/png", alt = "Dice 1")
    } else if(dice_value() == 2) {
      list(src = "www/noun-dice-2.png", width = 200, contentType = "image/png", alt = "Dice 2")
    } else if(dice_value() == 3) {
      list(src = "www/noun-dice-3.png", width = 200, contentType = "image/png", alt = "Dice 3")
    } else if(dice_value() == 4) {
      list(src = "www/noun-dice-4.png", width = 200, contentType = "image/png", alt = "Dice 4")
    } else if(dice_value() == 5) {
      list(src = "www/noun-dice-5.png", width = 200, contentType = "image/png", alt = "Dice 5")
    } else {
      list(src = "www/noun-dice-6.png", width = 200, contentType = "image/png", alt = "Dice 6")
    }
    
  }, deleteFile = FALSE)  
  

})