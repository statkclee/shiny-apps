
## 비율에 대한 중심극한정리-----------------
# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gridExtra)

library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# Define UI --------------------------------------------------------------------

module_prop_clt_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(

      tags$h3("비율 중심극한정리"),        
      sliderInput( ns("p"), 
                  "모집단 비율:", 
                  value = .5,
                  step = .01,
                  min = 0, 
                  max = 1),
      br(),
      tags$hr(style="border-color: blue;"),

      sliderInput( ns("n"), 
                  "표본 크기:", 
                  value = 200,
                  min = 2, 
                  max = 1000),
      br(),
      
      sliderInput( ns("k"), 
                  "반복 횟수:", 
                  value = 1000,
                  min = 10, 
                  max = 1000)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
        tabPanel("모집단 분포", br(), 
                 plotOutput( ns("pop.dist"), height = "450px")),
        tabPanel("표본", br(), br(), 
                 plotOutput( ns("sample.dist") ), br(), 
                 div(h3(textOutput( ns("num.samples") )), align ="center")),
        tabPanel( "표집분포", 
          fluidRow( column(8, br(), br(), 
                           div(textOutput( ns("CLT.descr") ), align = "justify"), br()), 
                    column(4, br(), plotOutput( ns("pop.dist1"), height = "200px"))),
          plotOutput( ns("sampling.dist")) , 
          div(textOutput( ns("plot.descr")), align = "center"), br())
      )
    )
  )
}

# Define server function --------------------------------------------

seed <- as.numeric(Sys.time())

module_prop_clt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    rand_draw = function(n, p) 
    {
      vals = NULL
      vals = do.call(rbinom, list(n=n, size=1, prob=p))      
      return(vals)
    }
    
    rep_rand_draw = repeatable(rand_draw)  
    
    parent = reactive({
      n = 1e5
      return(rep_rand_draw(input$n, input$p))
    })
    
    samples = reactive({
      pop = parent()
      n = input$n
      k = input$k
      return(replicate(k, sample(pop, n, replace=TRUE)))
    })
    
    # plot 1 ---------------------------   
    
    output$pop.dist = renderPlot({
      popsize = 1000
      counts = data.frame(number = c("0","1"), 
                          freq= c(popsize*(1-input$p), popsize*input$p)/popsize)
      
      
      ggplot (counts, aes(x = number, y = freq)) + 
        geom_bar(stat = "identity", color ="#263056", fill="#007CB7") + 
        labs( x="",  y = "상대 빈도", 
              title = paste0("모집단 분포: \n p = ", input$p), size=14, face="bold") + 
        scale_y_continuous(limits= c(0, 1)) + 
        theme_light(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) 
    })
    
    # plot 2 ----------------------------------
    
    output$sample.dist <- renderPlot({ 
      
      x = samples()
      
      plot <- list()
      
      for(i in 1:8){
        df <- tibble(obs = x[,i])
        counts <- df %>% count(obs)
        
        
        plot[[i]] <- ggplot(counts, aes(x = obs, y = n)) + 
          geom_bar(stat = "identity", color ="#263056", fill="#007CB7") +
          scale_y_continuous(limits= c(0,1.2*max(counts$n))) +
          scale_x_discrete(limits= c(0, 1)) + theme_light(base_size = 12) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +  
          labs( x="",  y = "빈도수",
                title = paste(i, " 번째 표본"), size=14, face="bold")
        
        mean_samp = round(mean(x[,i]),2)
        
        sd_samp = round(sd(x[,i]),2)
        
        y_pos = max(counts$n) + 0.07*max(counts$n)
        
        
        # #added if statement to check if count 1 or count 2 are NA. this check
        # #eliminated the error messages in the app
        
        if(!is.na(counts$n[1]) & !is.na(counts$n[2])) {
          if(counts$n[1] > counts$n[2]) {
            
            plot[[i]] <- plot[[i]] +  annotate("text", x = 1, y = y_pos,
                                               label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                               color = "black", size = 3) 
          }
          else {
            
            plot[[i]] <- plot[[i]] +  annotate("text", x = 0, y = y_pos,
                                               label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                               color = "black", size = 3) 
          }}
        else {
          plot[[i]] <- plot[[i]] +  annotate("text", x = 0.5, y = y_pos,
                                             label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                             color = "black", size = 3) 
        }
      }
      grid.arrange(plot[[1]], plot[[2]],plot[[3]],plot[[4]],plot[[5]],plot[[6]],plot[[7]],plot[[8]], ncol=4)
    })
    
    # text
    output$num.samples = renderText({
      k = input$k
      paste0( k," 번 표본추출을 계속해서 반복.....")
    })
    
    # plot 3 ----------------------------------------------
    
    output$pop.dist1 = renderPlot({
      popsize = 1000
      counts = data.frame(number = c("0","1"), 
                          freq= c(popsize*(1-input$p), popsize*input$p)/popsize)
      
      
      ggplot (counts, aes(x = number, y = freq)) + 
        geom_bar(stat = "identity", color ="#263056", fill="#007CB7") + 
        labs( x="",  y = "상대 빈도",
              title = paste0("모집단 분포: \n p = ", input$p), size=8, face="bold") +
        scale_y_continuous(limits= c(0, 1)) + 
        theme_light(base_size = 8) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    })
    
    
    output$sampling.dist = renderPlot({
      n = input$n
      p = input$p
      k = input$k
      pop = parent()
      ndist = tibble(means = colMeans(samples()))
      
      ndens=density(ndist$means)
      nhist=hist(ndist$means, plot=FALSE)
      
      m_samp =  round(mean(ndist$means),2)
      sd_samp = round(sd(ndist$means),2)
      
      x_range = max(ndist$means) - min(ndist$means)
      y_pos = max(ndens$y) - 0.1*max(ndens$y)
      x_pos = if_else(m_samp > 0, min(ndist$means) + 0.1*x_range, max(ndist$means) - 0.1*x_range)
      
      # minor change in the way the title is displayed
      
      ggplot(ndist, aes(x=ndist$means, y = ..density..)) +
        geom_histogram(bins = 20, color ="#263056", fill="#98DDDE") +
        stat_density(geom = "line", color = "#263056", size = 2) +
        labs(title = paste("표집 분포*:"), 
             x = "표본평균", y = "") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("p_hat 평균","=", bquote(.(m_samp)),"\n", 
                               "p_hat 표준편차", "=", bquote(.(sd_samp))),
                 color = "black", size = 5) +
        theme_light(base_size = 17) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) 
    })
    
    # text ---------------------
    output$plot.descr = renderText({
      n = input$n
      p = input$p
      k = input$k
      
      paste0("* 모집단으로부터", n, " 개 관측점으로 구성된",
             k, " 번 반복 확률 표본에서 나온 평균 분포")
      
    })
    
    # text -------------------------
    output$CLT.descr = renderText({
      
      n = input$n ; p = input$p ; q = 1-p
      
      pop = parent()
      m_pop =  p
      
      n = input$n
      se=round(sqrt(p*(1-p)/n),4)
      
      paste0("중심극한정리(Central Limit Theorem, CLT)에 따르면, 
           표본 평균의 분포(표집분포, Sampling distribution)는 거의 정규분포가 되야한다. ",
           "표집분포의 평균은 근사적으로 모집단 평균 (", m_pop, ")과 ", 
           "표준오차(표본 비율의 표준편차)는 근사적으로 성공확률(p) 곱하기 실패확률(1-p) 제곱근을", 
           "표본크기로 나눈 값과 일치해야 한다 (sqrt(", p, "*", q, "/",n, ") =", se,")." )
    })

  })
}
