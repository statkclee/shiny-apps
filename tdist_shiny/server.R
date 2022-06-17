
# Shiny server for Normal distribution


#_________________________________________________________________________________________


shinyServer(function(input, output) {


#_________________________________________________________________________________________
# Reactive


inputVal_reactive <- reactive({

  pmean <- 0
  psd <- 1

  df1 <- input$df

  p <- input$p  # user probability
  p_tail <- input$p_tail


  fn_InputData(pmean = pmean, psd = psd,
               df1 = df1,
               p = p, p_tail = p_tail)


})



#_________________________________________________________________________________________


output$dnorm_dt_plot <- renderPlot({

  fn_dnorm_dt(inputVal_reactive())

})



output$dnorm_plot <- renderPlot({

  fn_dnorm(inputVal_reactive())

})




output$dt_plot <- renderPlot({

  fn_dt(inputVal_reactive())


})




#_________________________________________________________________________________________


})


#_________________________________________________________________________________________

