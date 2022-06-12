
shinyServer(function(input, output, session) {
  
  # 1. 투표율 표 ----------------------
  casting_tbl <- reactive(
    
    sido_casting %>% 
      filter(시도명 == input$sido_select)
  )
   
  output$sido_casting <- renderTable({
    
    casting_tbl()
    
  })

  # 2. 투표율 지도 ----------------------
  casting_map_tbl <- reactive(
    
    sgg_casting_sf %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_casting_map <- renderPlot({
    
    casting_map_tbl() %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = 투표율)) +
      theme_void(base_family = "Nanum Gothic") +
      scale_fill_viridis_c(option = "plasma", begin = 0.0) +
      geom_sf_text(aes(label = glue::glue("{구시군명}\n{scales::percent(투표율, accuracy=0.1)}")),
                   size = 3, family = "Nanum Gothic")
    
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