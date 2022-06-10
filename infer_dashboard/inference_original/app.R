library(shiny)
library(ggplot2)
# library(EnvStats)

# Define UI for application that draws a histogram
ui <- shiny::tagList(
  withMathJax(), 
  includeCSS(path = "www/css/styles.css"), 
  
  tags$head(
    tags$link(
      rel = "shortcut icon", 
      href = "https://antoinesoetewey.com/favicon.ico"
    )
  ), 
  
  tags$div(
    tags$div(
      class = "app_title", 
      
      titlePanel(
        title = "추론 (Inference)", 
        windowTitle = "Inference"
      ),
      
      tags$h4(
        tags$a(
          href = "https://r2bit.com/", 
          target = "_blank", 
          "한국 R 사용자회"
        )
      )
    ),
    
    # Sidebar with a slider input for number of bins
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "inference",
            label = "Inference for:",
            choices = c("one mean", "two means (independent samples)", "two means (paired samples)", "one proportion", "two proportions", "one variance", "two variances"),
            multiple = FALSE,
            selected = "one mean"
          ),
          hr(),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            textInput("sample_onemean", "Sample", value = "0.9, -0.8, 1.3, -0.3, 1.7", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
            hr(),
            checkboxInput("popsd_onemean", "Variance of the population is known", FALSE),
            conditionalPanel(
              condition = "input.popsd_onemean == 1",
              numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                           value = 1, min = 0, step = 1
              )
            )
          ),
          hr(),
          tags$b("Null hypothesis"),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            sprintf("\\( H_0 : \\mu = \\)")
          ),
          conditionalPanel(
          condition = "input.inference != 'two variances'",
          numericInput("h0",
                       label = NULL,
                       value = 0.1, step = 0.1)
          ),
          conditionalPanel(
            condition = "input.inference != 'two variances'",
            radioButtons(
              inputId = "alternative",
              label = "Alternative",
              choices = c(
                "\\( \\neq \\)" = "two.sided",
                "\\( > \\)" = "greater",
                "\\( < \\)" = "less"
              )
            )
          ),
          hr(),
          sliderInput("alpha",
                      "Significance level \\(\\alpha = \\)",
                      min = 0.01,
                      max = 0.20,
                      value = 0.05
          ),
        ),
        
        mainPanel(
          br(),
          conditionalPanel(
            condition = "input.inference == 'one mean'",
            uiOutput("results_onemean")
          ),
          br(),
          br(),
          plotOutput("plot"),
          br(),
          br()
        )
      ),
    )      
  ), 
  
  tags$footer(
    tags$div(
      class = "footer_container", 
      
      includeHTML(path = "www/html/footer.html")
    )
  )
  
)
  

server <- function(input, output) {
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }

  output$results_onemean <- renderUI({
    
    dat <- extract(input$sample_onemean)
    
    if (anyNA(dat) | length(dat) < 2) {
      "Invalid input or not enough observations"
    } else if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      test_confint <- t.test(x = dat, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Data"),
        br(),
        paste(c(paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        tags$em("(two-sided)"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat)), 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
