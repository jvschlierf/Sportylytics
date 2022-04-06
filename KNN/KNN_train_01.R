# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model 2: K-Nearest Neighbor

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
my_packages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                 "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "caret", "fastDummies")
lapply(my_packages, require, character.only = TRUE)

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/data_Bplayers_2000_TEST.csv')

pre_treat <- function(dataset){
  
  #Create dummies for position
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #drop columns we won't use
  drops <- c("Player", 'season', 'tm' , 'lg','Salary_Cap','Salary', 'pos')
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  dataset
}



train_basket = pre_treat(train_basket)
test_basket = pre_treat(test_basket)

#Splitting off independent variables
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]

#training a simple k=5 model without any dimensionality reduction techniques
model_1 = knnreg( Salary_Cap_Perc ~ ., data=train_basket)
model_1

#more complex model that tries different k's
model_2 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  method = 'knn')
model_2

plot(model_2)

#next, let's try a model where (as a pre-processing step), we standardize and center our features
model_3 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  preProcess = c("center", "scale"),
  method = 'knn')

plot(model_3)

ctrl <- trainControl(method="repeatedcv",repeats = 3)

model_4 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  trControl = ctrl,
  preProcess = c("center", "scale"),
  method = 'kknn',
  tuneLength = 20)

plot(model_4)

#predict using different models
predictions_1 <- predict(model_1, newdata=test_basket_x)
predictions_2 <- predict(model_2, newdata=test_basket_x)
predictions_3 <- predict(model_3, newdata=test_basket_x)
predictions_4 <- predict(model_4, newdata=test_basket_x)


# Root Mean Standard Error on Test Set
RMSE_01 <- sqrt(mean((test_basket_y - predictions_1)^2))
RMSE_02 <- sqrt(mean((test_basket_y - predictions_2)^2))
RMSE_03 <- sqrt(mean((test_basket_y - predictions_3)^2))
RMSE_04 <- sqrt(mean((test_basket_y - predictions_4)^2))

print(c(RMSE_01, RMSE_02, RMSE_03, RMSE_04))

cor(test_basket_y, predictions_1) ^ 2
cor(test_basket_y, predictions_2) ^ 2
cor(test_basket_y, predictions_3) ^ 2
cor(test_basket_y, predictions_4) ^ 2

#save models so we don;'t have to retrain every time
saveRDS(model_1, file = "knn_1.Rds")
saveRDS(model_2, file = "knn_2.Rds")
saveRDS(model_3, file = "knn_3.Rds")
saveRDS(model_4, file = "knn_4.Rds")

# use following code to read models
model_1 <- readRDS(file = "knn_01.Rds")
model_2 <- readRDS(file = 'knn_02.Rds')
model_3 <- readRDS(file = 'knn_03.Rds')
model_4 <- readRDS(file = 'knn_04.Rds')

