# 0. 패키지 -----------------------
library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(shiny)
library(r2d3)
library(DALEXtra)
library(modelStudio)

# 1. 데이터와 예측 모형 ---------------------------
data <- titanic_imputed
data$survived <- as.factor(data$survived)
rec <- recipe(survived ~ ., data = data) %>%
  step_normalize(fare)
model <- decision_tree(tree_depth = 25) %>%
  set_engine("rpart") %>%
  set_mode("classification")

wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model)


model_fitted <- wflow %>%
  fit(data = data)

explainer <- explain_tidymodels(model_fitted, data = titanic_imputed, y = titanic_imputed$survived)

modelStudio(explainer)


