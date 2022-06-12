
shinyServer(function(input, output, session) {
  
  # 1. 투표율 표 ----------------------
  casting_tbl <- reactive(
    
    sido_casting %>% 
      filter(시도명 == input$sido_select)
  )
   
  output$sido_casting <- renderTable({
    
    casting_tbl()
    
  })

  ## 1.2. 투표율 지도 ----------------------
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
  
  ## 2.1. 득표수 지도 ----------------------
  
  vote_map_tbl <- reactive(
    
    sgg_party_sf %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_party_vote_map <- renderPlot({
    
    vote_map_tbl() %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = 표차이)) +
      theme_void() +
      scale_fill_gradient2(low='red', mid = "white", high='blue',
                           labels = scales::comma) +
      geom_sf_text(aes(label = glue::glue("{구시군명}\n", 
                                          "민주당: {scales::comma(민주당, accuracy=1)}\n",
                                          "국민의힘: {scales::comma(국민의힘, accuracy=1)}\n",
                                          "그외정당: {scales::comma(그외정당, accuracy=1)}")),
                   size = 3)
  })
  
  # 3. 정당별 득표율 ----------------------
  party_vote_rate_tbl <- reactive(
    
    sido_party_rate %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_party_rate <- renderTable({
    
    party_vote_rate_tbl()
    
  })

  ## 3.1. 투표율 지도 ----------------------
  vote_rate_map_tbl <- reactive(
    
    sgg_party_rate_sf %>% 
      filter(시도명 == input$sido_select)
  )
  
  output$sido_party_rate_map <- renderPlot({
    
    vote_rate_map_tbl() %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = 표차이)) +
      theme_void() +
      scale_fill_gradient2(low='red', mid = "white", high='blue',
                           labels = scales::percent) +
      geom_sf_text(aes(label = glue::glue("{구시군명} > ",
                                          "민주당: {scales::percent(민주당, accuracy=0.1)}\n",
                                          "국민의힘: {scales::percent(국민의힘, accuracy=0.1)}\n",
                                          "그외정당: {scales::percent(그외정당, accuracy=0.1)}")),
                   size = 4)
  })


})