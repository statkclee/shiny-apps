library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)

penguin_model <- readRDS(url("https://github.com/statkclee/model/raw/gh-pages/data/penguin_predictvie_model.rds", "rb"))

penguin_tbl <- 
  tibble("species" = "Adelie",
         "bill_length_mm" =  30,
         "bill_depth_mm" =  40,
         "flipper_length_mm" = 50,
         "body_mass_g" = 1000)
  
predict(penguin_model, penguin_tbl)

penguin_model

