# global.R -------------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(keras)
library(DT)

classify_image <- function(src_img){
  
  img <- image_load(src_img, target_size = c(224,224))
  x <- image_to_array(img)
  
  x <- array_reshape(x, c(1, dim(x)))
  x <- imagenet_preprocess_input(x)
  
  model <- application_resnet50(weights = 'imagenet')
  
  preds <- model %>% predict(x)
  
  # Table Output
  main_tbl <- imagenet_decode_predictions(preds, top = 5)[[1]]
  main_tbl$explore_class_on_imagenet <- sprintf('Explore %s on ImageNet', main_tbl$class_name,  main_tbl$class_description)
  
  return(main_tbl)
}

# ui.R -------------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("데이터 사이언스 언어 R - 기계 이미지 인식"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Image Input: Select a file ----
      fileInput("img_file", "Choose Image file: ", 
                accept = c('image/png', 'image/jpeg')),
      
      # Horizontal line ----
      tags$hr(),
      
      # Check Keon-Woong Moon image ----
      checkboxInput("flag_check", "기본 예제 사진(태극기)", TRUE)
    ),
    
    # Show Image & Keras Output
    mainPanel(
      imageOutput("uploaded_image", height = 300),
      tags$br(),
      tags$br(),
      tags$br(),
      DT::dataTableOutput("keras_table")
    )
  )
))


# server.R -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$uploaded_image <- renderImage({
    
    src <- input$img_file
    src <- src$datapath
    
    if (is.null(src)) {
      list(src = "www/Flag_of_South_Korea.jpg",
           height = "300",
           alt = "Classify Image")
    } else {
      return(list(
        src = src,
        height = "300",
        alt = "Classifed Image"
      ))
    }
    
  }, deleteFile = FALSE)
  
  output$keras_table <- DT::renderDataTable({
    
    uploaded_img_file <- input$img_file
    uploaded_img_path <- uploaded_img_file$datapath
    
    if (is.null(uploaded_img_path)) {
      if(input$flag_check) {
        withProgress(message = '사진 판별중...', value = 1,
                     classify_image("www/Flag_of_South_Korea.jpg")
        )
      } else {
        NULL   
      }
    } else {
      withProgress(message = 'Predicting...', value = 1,
                   img2tbl_df <- classify_image(uploaded_img_path)
      )
      DT::datatable(img2tbl_df)
    }
  })
}


shinyApp(ui, server)
