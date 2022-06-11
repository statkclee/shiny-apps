

server <- function(input, output, session) {
  # NHST 가설검정 작업흐름도 -----------------------
  output$nhst_workflow_mean_img <- renderImage({
      filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-means.png")
      list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)

  output$nhst_workflow_proportion_img <- renderImage({
    filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-proportion.png")
    list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)
  
  output$nhst_workflow_variance_img <- renderImage({
    filename <- glue::glue("{here::here()}/infer_dashboard/www/nhst-variance.png")
    list(src = filename,
         contentType = "image/png",
         width = "auto",
         height = "auto") 
  }, deleteFile = FALSE)
  
  # 1 표본 평균 -----------------------------------
  output$test_means_one_plot <- renderPlot({
      if ( input$popsd_onemean == FALSE) {
        dat <- extract(input$sample_onemean)
        test <- t.test(x = dat, mu = input$onemean_h0, alternative = input$onemean_alternative, conf.level = 1 - input$onemean_alpha)
        if (input$onemean_alternative == "two.sided") {
          funcShaded <- function(x) {
            y <- dt(x, df = test$parameter)
            y[x < qt(input$onemean_alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$onemean_alpha / 2, df = test$parameter) ] <- NA
            return(y)
          }
        } else if (input$onemean_alternative == "greater") {
          funcShaded <- function(x) {
            y <- dt(x, df = test$parameter)
            y[x < qt(input$onemean_alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$onemean_alternative == "less") {
          funcShaded <- function(x) {
            y <- dt(x, df = test$parameter)
            y[x > qt(input$onemean_alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), 
                                     qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
          geom_area(stat="function", fun = dt, fill="gray90", args = list(df = test$parameter)) +
          geom_area(stat="function", fun = funcShaded, fill = "sky blue", alpha = 0.8) +
          theme_minimal() +
          geom_vline(xintercept = test$statistic, color = "steelblue") +
          geom_text(aes(x = test$statistic, label = paste0("검정 통계량 = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("t-분포 ~", " t(", round(test$parameter, 3), ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } else if ( input$popsd_onemean == TRUE) {
        dat <- extract(input$sample_onemean)
        test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$onemean_h0, alpha = input$onemean_alpha, alternative = input$onemean_alternative)
        if (input$onemean_alternative == "two.sided") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$onemean_alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$onemean_alpha / 2, mean = 0, sd = 1) ] <- NA
            return(y)
          }
        } else if (input$onemean_alternative == "greater") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x < qnorm(input$onemean_alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
            return(y)
          }
        } else if (input$onemean_alternative == "less") {
          funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x > qnorm(input$onemean_alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
            return(y)
          }
        }
        p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
          geom_area(stat="function", fun = dnorm, fill="gray90", args = list(mean = 0, sd = 1)) +
          geom_area(stat="function", fun = funcShaded, fill = "sky blue", alpha = 0.8) +
          theme_minimal() +
          geom_vline(xintercept = test$statistic, color = "steelblue") +
          geom_text(aes(x = test$statistic, label = paste0("검정통계량 = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
          ggtitle(paste0("정규분포 N(0,1)")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("밀도") +
          xlab("x")
        p
      } 
    })

}
  

