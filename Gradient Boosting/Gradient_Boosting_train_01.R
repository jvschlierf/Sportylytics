# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model 3: Gradient Boosting

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                 "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "caret", 
                 "gbm", "fastDummies")

#Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

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

#Now we can tune Gradient Boosting
model_gbm = gbm(train_basket$Salary_Cap_Perc ~.,
                data = train_basket,
                distribution = "gaussian",
                cv.folds = 5,
                n.minobsinnode = 10,
                shrinkage = 0.001,
                n.trees = 15000)

#Now we test the model
pred_basket_y = predict.gbm(model_gbm, test_basket_x)

#Analysis of results
RMSE = sqrt(min(model_gbm$cv.error))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

rsq_RSq <- (cor(pred_basket_y, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,3), '\n')

# visualize the model, actual and predicted data
x_ax = 1:length(pred_basket_y)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_basket_y, col="red", pch=20, cex=.9) 

#Relative influence
summary(
  model_gbm, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
  )