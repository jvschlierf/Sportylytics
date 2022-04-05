# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model X: Support Vector Machine


#Clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Package download & import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp", "e1071", "kernlab")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}



### 1. PREPROCESSING ###

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/data_Bplayers_2000_TEST.csv')

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos')
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  dataset
}

train_basket = pre_treat(train_basket)
test_basket = pre_treat(test_basket)

#Divide X and Y in test
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]

#Cross validation

set.seed(123)

trctrl <- trainControl(method = "cv", number = 5,)



### 2. MODELS ###

#Different kernel options for SVM, let's see how they perform

svm_radial <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmRadial', 
                    preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_radial)

svm_linear <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmLinear',
                    preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_linear)

svm_poly <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmPoly', 
                  preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_poly)

#Predict using different kernels
pred_radial = predict(svm_radial, newdata=test_basket_x)
pred_linear = predict(svm_linear, newdata=test_basket_x)
pred_poly = predict(svm_poly, newdata=test_basket_x)

#RMSE
RMSE_radial = sqrt(mean((test_basket_y - pred_radial)^2))
RMSE_linear = sqrt(mean((test_basket_y - pred_linear)^2))
RMSE_poly = sqrt(mean((test_basket_y - pred_poly)^2))

print(c(RMSE_radial, RMSE_linear, RMSE_poly))

cor(test_basket_y, pred_radial)^2
cor(test_basket_y, pred_linear)^2
cor(test_basket_y, pred_poly)^2

### We proceed with the best performing, the radial kernel



### 3. HYPER-PARAMETERS TUNING (ALL VARIABLES) ###

tuneGrid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5), sigma = c(0.05, 0.01, 0.015))

svm_tuned <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmRadial', 
                    preProcess = c("center", "scale"), trCtrl = trctrl, 
                    tuneGrid = tuneGrid)
print(svm_tuned)

#Prediction
pred_tuned = predict(svm_tuned, newdata=test_basket_x)
RMSE_tuned = sqrt(mean((test_basket_y - pred_tuned)^2))
print(c(RMSE_radial, RMSE_tuned))
cor(test_basket_y, pred_tuned)^2

#Save model
saveRDS(svm_tuned, file = "svm_tuned.Rds")

#Load model
# svm_tuned <- readRDS(file = "svm_tuned.Rds")



### 4. FEATURES SELECTION ###

#Divide X and Y in train
train_basket_x = subset(train_basket, select = -Salary_Cap_Perc) # feature and target array
train_basket_y = train_basket[, "Salary_Cap_Perc"]

#Recursive feature elimination
svmFeatures <- rfe(train_basket_x, train_basket_y, sizes = c(5, 10, 20, 30, 40, 52),
                   rfeControl = rfeControl(functions = caretFuncs, method = 'cv', number = 5), method = "svmRadial")

print(svmFeatures)

#Save features
saveRDS(svmFeatures, file = "svmFeatures.Rds")

#Load model
# svmFeatures <- readRDS(file = "svmFeatures.Rds")