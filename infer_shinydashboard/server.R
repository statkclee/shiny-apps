

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
  
  means_server("module_one-mean")
}
  

