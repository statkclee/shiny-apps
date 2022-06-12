
shinyServer(function(input, output, session) {
  
  # 1. 투표율 ----------------------
  casting_tbl <- reactive(
    
    sido_casting %>% 
      filter(시도명 == input$sido_select)
  )
   
  output$sido_casting <- renderTable({
    
    casting_tbl()
    
  })

  # 2. 정당별 득표수 ----------------------
  party_vote_tbl <- reactive(
  
    sido_party %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_party_vote <- renderTable({
    
    party_vote_tbl()
    
  })
  
  # 3. 정당별 득표율 ----------------------
  party_vote_rate_tbl <- reactive(
    
    sido_party_rate %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_party_rate <- renderTable({
    
    party_vote_rate_tbl()
    
  })
  


})