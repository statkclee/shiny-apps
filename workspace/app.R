# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gridExtra)

library(showtext)
# font_add_google(name = "Nanum Gothic", regular.wt = 400)
showtext_auto()


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  # Title ----
  titlePanel("평균 중심극한정리", windowTitle = "평균 중심극한정리"),
  
  sidebarLayout(
    sidebarPanel(
        # 1. 분포 선택 ----
        tags$h3("모집단"),
        radioButtons(inputId = "dist", "모집단 분포 선택:",
                     c("정규분포" = "rnorm",
                       "균등분포" = "runif"),
                     selected = "rnorm"),

        # Distribution parameters / features ----
        uiOutput("mu"),
        uiOutput("sd"),
        uiOutput("minmax"),

        tags$hr(style="border-color: blue;"),
        
        ## 1.1. 정규분포 모수 ----
        tags$h4("분포 모수(Parameters)"),
        conditionalPanel(
          condition = "input.dist == 'rnorm'",
          sliderInput("mu",
                      "평균: ",
                      value = 0,
                      min = -40,
                      max = 50),
          sliderInput("sd",
                      "표준 편차: ",
                      value = 20,
                      min = 1,
                      max = 30)
         ),
        
        ## 1.2. 균등분포 모수 ----
        conditionalPanel(
          condition = "input.dist == 'runif'",
          sliderInput("minmax",
                      "최대값과 최소값 범위",
                      value = c(5, 15),
                      min = 0,
                      max = 20)
        ),
        
        tags$hr(style="border-color: blue;"),        
                        
        # 표본크기와 반복횟수 ----
        # 표본크기 ----
        tags$h3("표본크기와 반복횟수"),
        sliderInput(inputId = "n",
                    "표본크기:", 
                    value = 30,
                    min = 2,
                    max = 500),
        br(),
        
        # 반복횟수 ----
        sliderInput(inputId = "k",
                    "반복 횟수:",
                    value = 200,
                    min = 10,
                    max = 1000)
      ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        # First tab ----
        tabPanel(
          title = "모집단 분포",
          # Population plot ----
          plotOutput("pop_dist", height = "500px"),
          br()
        ),
        # Second tab ----
        tabPanel(
          title = "표본",
          # Sample plots ----
          br(),
          plotOutput("sample_dist", height = "600px"),
          #  Number of samples text ----
          div(h3(textOutput("num_samples")), align = "center"),
          br()
        ),
        # Third tab ----
        tabPanel(
          title = "표본 분포",
          
          fluidRow(
            column(width = 7,
                   br(), br(),
                   # CLT description ----
                   div(textOutput("CLT_descr"), align = "justify")),
            column(width = 5,
                   br(),
                   # Population plot ----
                   plotOutput("pop_dist_two", width = "85%", height = "200px"))
          ),
          
          fluidRow(
            column(width = 12,
                   br(),
                   # Sampling plot ----
                   plotOutput("sampling_dist"),
                   # Sampling description ----
                   div(textOutput("sampling_descr", inline = TRUE), align = "center"))
          )
        )
      )
    )
  )
)

# Define server function --------------------------------------------

seed <- as.numeric(Sys.time())

server <- function(input, output, session) {
  

  # 균등분포 != 0 되지 않게 함 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  
  # 난수 표본 생성 ----
  rand_draw <- function(dist, n, mu, sd, min, max){
    
    vals <- NULL
    
    if (dist == "rnorm"){
      req(mu, sd)
      vals <- do.call(dist, list(n=n, mean=mu, sd=sd))
    }
    
    else if (dist == "runif"){
      req(min, max)
      vals <- do.call(dist, list(n=n, min=min, max=max))
    }
    return(vals)
  }
  
  rep_rand_draw <- repeatable(rand_draw)
  
  # Defining some reactive variables to use later ----
  parent <- reactive({
    
    n_sample <- 1e5
    
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2]))
  })
  
  samples <- reactive({
    
    pop <- parent()
    n <- input$n
    k <- input$k
    
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  u_min <- reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  
  u_max <- reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  
  # plot 1 a) ----
  output$pop_dist <- renderPlot({
    
    distname <- switch(input$dist,
                      rnorm = "모집단 분포: 정규분포",
                      runif = "모집단 분포: 균등분포")
    
    pop <- parent()
    
    m_pop <- round(mean(pop), 2)
    sd_pop <- round(sd(pop), 2)
    
    pop <- tibble(samples = pop)
    pdens <- density(pop$samples)
    
    x_range <- max(pop$samples) - min(pop$samples)
    y_pos <- max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      
      mu <- input$mu
      
      x_pos <- ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#195190") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom="line", color = "#195190", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, 
             x = "x",
             y = "밀도") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("x 평균", "=", bquote(.(m_pop)),
                               "\n", "x 표준편차", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
        
        x_pos <- max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white", fill = "#195190") +
          stat_density(geom = "line", color = "#195190", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, 
               x = "x",
               y = "밀도") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("mean of x", "=", bquote(.(m_pop)),
                                 "\n", "SD of x", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())}
      
    }
  })
  
  # plot 1 b) ----
  # this is the population plot in the third tab. it is plot 1 a) with a few
  # changes to make it fit the smaller pace in the tab. (apparently not possible to use the
  # same output id in different tabs.)
  
  output$pop_dist_two <- renderPlot({
    
    distname <- switch(input$dist,
                      rnorm = "모집단 분포: 정규분포",
                      runif = "모집단 분포: 균등분포")
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 27,
                     max(100, max(pop$samples)) - 27)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#195190") +
        stat_density(geom="line", color = "#195190", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, 
             x = "x",
             y = "밀도") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)),
                               "\n", "SD of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white", fill = "#195190") +
          stat_density(geom = "line", color = "#195190", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("mean of x", "=", bquote(.(m_pop)),
                                 "\n", "SD of x", "=", bquote(.(sd_pop))),
                   color = "black", size = 3) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())}
      
    } 
  })
  
  # plot 2 ----
  output$sample_dist <- renderPlot({
    
    y <- samples()
    x <- samples() %>% as_tibble()
    
    plots = list(rep(NA, 8))
    
    for(i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.1*x_range, 
                     max(y[,i]) - 0.1*x_range)
      
      plots[[i]] <- ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7) +
        labs(title = paste(i, " 번째 표본"), x = "", y = "") +
        theme_light(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("표본평균", "=", bquote(.(mean)),
                               "\n", "표본 SD", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  
  # text for sample plots ----
  output$num_samples  <- renderText({
    
    k = input$k
    paste0( k," 번 표본추출을 계속해서 반복.....")
    
  })
  
  # plot 3 ----
  output$sampling_dist <- renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "정규 모집단",
                      runif  = "균등분포 모집단")
    
    n = input$n
    k = input$k
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist = tibble(means = colMeans(samples()))
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot=FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
    
    p = ggplot(data = ndist, aes(x = means, y = ..density..)) +
      geom_histogram(bins = 20, color = "white", fill = "#009499") +
      stat_density(geom = "line", color = "#009499", size = 1) +
      labs(title = paste("표본 분포*"),
           x = "표본 평균",
           y = "" ) +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("x_bar 평균", "=", bquote(.(m_samp)),
                             "\n", "x_bar 표준오차", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_light(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        p
      }
    } else {
      p
    }
  })
  
  # description for sampling distribution plot ----
  output$sampling_descr <- renderText({
    
    distname = switch(input$dist,
                      rnorm = "정규분포 모집단",
                      runif = "균등분포 모집단")
    
    k = input$k
    n = input$n

    paste("*", distname, "에서", 
          n, "개 관측점으로 구성된", 
          k, "번 반복한 확률표본의 평균분포")
  })
  
  # description for CLT ----
  output$CLT_descr = renderText({
    
    pop = parent()
    m_pop =  round(mean(pop),2)
    s_pop = round(sd(pop),2)
    
    n = input$n
    se = round(s_pop/sqrt(n),2)

    paste0("중심극한정리(Central Limit Theorem, CLT)에 따르면, 
           표본 평균의 분포(표집분포, Sampling distribution)는 거의 정규분포가 되야한다.",
           "표집분포의 평균은 근사적으로 모집단 평균 (", m_pop, ")과", 
           "표준오차(표본 평균의 표준편차)는 근사적으로 모집단 표준편차를 표본크기 제곱근을 나눈 값과 
           일치해야 한다 (", s_pop, "/sqrt(",n, ") = ", se,")", 
           "표집분포(Sampling distribution) 그래프가 다음에 나와 있다. 비교를 통해 명확히 하기 위해, 
           모집단 분포 그래프가 우측에 나와 있다."
           )    
    
  })
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)