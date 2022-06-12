
shinyServer(function(input, output, session) {
  
  # 1. 투표율 ----------------------
  casting_tbl <- reactive(
    sido_casting <- 
      # read_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_casting.rds"))
      readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_casting.rds?raw=true", "rb"))
  )
   
  output$sido_casting <- renderTable({
    
    casting_tbl()
    
  })

  # 2. 정당별 득표수 ----------------------
  party_vote_tbl <- reactive(
    sido_party <-
      # read_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_party.rds"))
      readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_party.rds?raw=true"))
  )
  
  output$sido_party_vote <- renderTable({
    
    party_vote_tbl()
    
  })
  
  # 3. 정당별 득표율 ----------------------
  party_vote_rate_tbl <- reactive(
    sido_party_rate <- 
      # read_rds(glue::glue("{here::here()}/krvote_shinydashboard/www/sido_party_rate.rds"))
      readRDS(url("https://github.com/statkclee/shiny-apps/blob/main/krvote_shinydashboard/www/sido_party_rate.rds?raw=true"))
  )
  
  output$sido_party_rate <- renderTable({
    
    party_vote_rate_tbl()
    
  })
  


})