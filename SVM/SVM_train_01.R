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
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv')

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link')
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
# RMSE        Rsquared   MAE       
# 0.03935554  0.7041773  0.02696496


svm_linear <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmLinear',
                    preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_linear)
# RMSE        Rsquared   MAE       
# 0.04505973  0.6282156  0.03143813


svm_poly <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmPoly', 
                  preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_poly)
# degree  scale  C     RMSE        Rsquared    MAE       
# 2       0.001  0.25  0.04467952  0.64633559  0.03117615

#Predict using different kernels
pred_radial = predict(svm_radial, newdata=test_basket_x)
pred_linear = predict(svm_linear, newdata=test_basket_x)
pred_poly = predict(svm_poly, newdata=test_basket_x)

#RMSE
RMSE_radial = sqrt(mean((test_basket_y - pred_radial)^2))
RMSE_linear = sqrt(mean((test_basket_y - pred_linear)^2))
RMSE_poly = sqrt(mean((test_basket_y - pred_poly)^2))

print(c(RMSE_radial, RMSE_linear, RMSE_poly))
# 0.03712062 0.04282188 0.03664644

cor(test_basket_y, pred_radial)^2  # 0.7266582
cor(test_basket_y, pred_linear)^2  # 0.6400517
cor(test_basket_y, pred_poly)^2    # 0.734441

### We proceed with the best performing, the radial kernel

#Save model
saveRDS(svm_radial, file = "svm_radial.Rds")
saveRDS(svm_poly, file = "svm_poly.Rds")

#Load model
# svm_radial <- readRDS(file = "svm_radial.Rds")
# svm_poly <- readRDS(file = "svm_poly.Rds")



### 3. HYPER-PARAMETERS TUNING (ALL VARIABLES) ###

tuneGrid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5), sigma = c(0.05, 0.01, 0.015))

svm_radial_tuned <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmRadial', 
                    preProcess = c("center", "scale"), trCtrl = trctrl, 
                    tuneGrid = tuneGrid)

print(svm_radial_tuned)
# C     sigma  RMSE        Rsquared   MAE       
# 1.50  0.010  0.03879415  0.7183434  0.02665650

#Prediction
pred_tuned = predict(svm_tuned, newdata=test_basket_x)
RMSE_tuned = sqrt(mean((test_basket_y - pred_tuned)^2))
print(c(RMSE_radial, RMSE_tuned))  # 0.03712062 0.03619767
cor(test_basket_y, pred_tuned)^2   # 0.7398564

#Save model
saveRDS(svm_radial_tuned, file = "svm_radial_tuned.Rds")

#Load model
# svm_radial_tuned <- readRDS(file = "svm_radial_tuned.Rds")
### RMSE_radial_tuned = 0.03619767


######### SVM POLY TUNED #########


### 4. FEATURES SELECTION ###

#Divide X and Y in train
train_basket_x = subset(train_basket, select = -Salary_Cap_Perc) # feature and target array
train_basket_y = train_basket[, "Salary_Cap_Perc"]


#Recursive feature elimination (using cross validation)
svm_features_cv <- rfe(train_basket_x, train_basket_y, sizes = c(5, 10, 20, 30, 40, 52),
                   rfeControl = rfeControl(functions = caretFuncs, method = 'cv', number = 5), method = "svmRadial")

#Save features
# saveRDS(svm_features_cv, file = "svm_features_cv.Rds")

#Load features
svm_features_cv <- readRDS(file = "svm_features_cv.Rds")
print(svm_features_cv)


### No significant improvement, we keep the full model
### Best RMSE = 

