# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model X: Random Forest

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp","randomForest")

# Uploading libraries
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

#Divide X and Y in test
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]

### Random Forest ###
set.seed(123)
rf = randomForest(train_basket$Salary_Cap_Perc~. ,
                  data = train_basket)
print(rf)
rf_pred = predict(rf, test_basket_x)
rmse = sqrt(mean((test_basket_y - rf_pred)^2))
rmse 
#0.04233882
saveRDS(rf, file = "rf_simple.Rds")

#Choose the best values for m 
set.seed(123)
t = tuneRF(train_basket[,-4], train_basket[,4], 
       stepFactor = 0.5,
       plot = TRUE, 
       mtryStart = 5,
       ntreeTry=2000, #didn't improve much increasing the size
       trace = TRUE,
       improve = 0.01)
#best with m = 20
best.m <- t[t[, 2] == min(t), 1]
set.seed(123)
rfm = randomForest(train_basket$Salary_Cap_Perc~. ,
                  data = train_basket,
                  ntree = 2000,
                  mtry = 20,
                  importance = TRUE)

print(rfm)
rfm_pred = predict(rfm, test_basket_x)
rmse_m = sqrt(mean((test_basket_y - rfm_pred)^2))
print(rmse_m)  ##0.04222568

saveRDS(rfm, file = "rf_m.Rds")

model <- readRDS(file = "rf_m.Rds")
model_pred = predict(model, test_basket_x)
rmse = sqrt(mean((test_basket_y - model_pred)^2))
print(rmse)  

#Evaluate variable importance
importance(model)
varImpPlot(model, n.var = 15)
