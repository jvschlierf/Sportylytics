# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model X: Support Vector Machine

# Clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package download & import
my_packages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                 "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "caret", "e1071")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

# Load data (both train and test)
train_basket <- read.csv('../Dataset/data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/data_Bplayers_2000_TEST.csv')

