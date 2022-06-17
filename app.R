## Nicole's Second Shiny Demo App
## N. Radziwill, 12/6/2015, http://atomic-temporary-5081318.wpcomstaging.com
## Used code from http://github.com/homerhanumat as a base
###########################################################
## ui
###########################################################

ui <- fluidPage(
  titlePanel('Sampling Distributions and the Central Limit Theorem'),
  sidebarPanel(
    helpText('Choose your source distribution and number of items, n, in each
sample. 10000 replications will be run when you click "Sample Now".'),
h6(a("Read an article about this simulation at http://www.r-bloggers.com",
     href="http://www.r-bloggers.com/sampling-distributions-and-central-limit-theorem-in-r/", target="_blank")),
sliderInput(inputId="n","Sample Size n",value=30,min=5,max=100,step=2),
radioButtons("src.dist", "Distribution type:",
             c("Exponential: Param1 = mean, Param2 = not used" = "E",
               "Normal: Param1 = mean, Param2 = sd" = "N",
               "Uniform: Param1 = min, Param2 = max" = "U",
               "Poisson: Param1 = lambda, Param2 = not used" = "P",
               "Cauchy: Param1 = location, Param2 = scale" = "C",
               "Binomial: Param1 = size, Param2 = success prob" = "B",
               "Gamma: Param1 = shape, Param2 = scale" = "G",
               "Chi Square: Param1 = df, Param2 = ncp" = "X",
               "Student t: Param1 = df, Param2 = not used" = "T")),
numericInput("param1","Parameter 1:",10),
numericInput("param2","Parameter 2:",2),
actionButton("takeSample","Sample Now")
  ), # end sidebarPanel
mainPanel(
  # Use CSS to control the background color of the entire page
  tags$head(
    tags$style("body {background-color: #9999aa; }")
  ),
  plotOutput("plotSample")
) # end mainPanel
) # end UI

##############################################################
## server
##############################################################

library(shiny)
r <- 10000 # Number of replications... must be ->inf for sampling distribution!

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

server <- function(input, output) {
  set.seed(as.numeric(Sys.time()))
  
  # Create a reactive container for the data structures that the simulation
  # will produce. The rv$variables will be available to the sections of your
  # server code that prepare output for the UI, e.g. output$plotSample
  rv <- reactiveValues(sample = NULL,
                       all.sums = NULL,
                       all.means = NULL,
                       all.vars = NULL)
  
  # Note: We are giving observeEvent all the output connected to the UI actionButton.
  # We can refer to input variables from our UI as input$variablename
  observeEvent(input$takeSample,
               {
                 my.samples <- switch(input$src.dist,
                                      "E" = matrix(rexp(input$n*r,input$param1),r),
                                      "N" = matrix(rnorm(input$n*r,input$param1,input$param2),r),
                                      "U" = matrix(runif(input$n*r,input$param1,input$param2),r),
                                      "P" = matrix(rpois(input$n*r,input$param1),r),
                                      "C" = matrix(rcauchy(input$n*r,input$param1,input$param2),r),
                                      "B" = matrix(rbinom(input$n*r,input$param1,input$param2),r),
                                      "G" = matrix(rgamma(input$n*r,input$param1,input$param2),r),
                                      "X" = matrix(rchisq(input$n*r,input$param1),r),
                                      "T" = matrix(rt(input$n*r,input$param1),r))
                 
                 # It was very important to make sure that rv contained numeric values for plotting:
                 rv$sample <- as.numeric(my.samples[1,])
                 rv$all.sums <- as.numeric(apply(my.samples,1,sum))
                 rv$all.means <- as.numeric(apply(my.samples,1,mean))
                 rv$all.vars <- as.numeric(apply(my.samples,1,var))
               }
  )
  
  output$plotSample <- renderPlot({
    # Plot only when user input is submitted by clicking "Sample Now"
    if (input$takeSample) {
      # Create a 2x2 plot area & leave a big space (5) at the top for title
      par(mfrow=c(2,2), oma=c(0,0,5,0))
      hist(rv$sample, main="Distribution of One Sample",
           ylab="Frequency",col=1)
      hist(rv$all.sums, main="Sampling Distribution of the Sum",
           ylab="Frequency",col=2)
      hist(rv$all.means, main="Sampling Distribution of the Mean",
           ylab="Frequency",col=3)
      hist(rv$all.vars, main="Sampling Distribution of the Variance",
           ylab="Frequency",col=4)
      mtext("Simulation Results", outer=TRUE, cex=3)
    }
  }, height=660, width=900) # end plotSample
  
} # end server
