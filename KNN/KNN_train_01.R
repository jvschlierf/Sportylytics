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
model = knnreg( Salary_Cap_Perc ~ ., data=train_basket)
model

#more complex model that tries different k's
model_knn <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  method = 'knn',)
model_knn

plot(model_knn)

#predict using different models
predictions_5 = predict(model, newdata=test_basket_x)
predictions_9 = predict(model_knn, newdata=test_basket_x)

# RMSE for both
RMSE_simple = sqrt(mean((test_basket_y - predictions_5)^2))
RMSE_complex = sqrt(mean((test_basket_y - predictions_9)^2))

print(c(RMSE_simple, RMSE_complex))

cor(test_basket_y, predictions_5) ^ 2
cor(test_basket_y, predictions_9) ^ 2

#save models so we don;'t have to retrain every time
saveRDS(model, file = "knn_simple.Rds")
saveRDS(model_knn, file = "knn_complex.Rds")

# use following code to read models
# test <- readRDS(file = "knn_complex.Rds")


