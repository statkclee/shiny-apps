server <- function(input, output, session) {

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
  
  output$plot <- renderPlot({
    if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one mean" & input$popsd_onemean == TRUE) {
      dat <- extract(input$sample_onemean)
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } 
  })
  
}