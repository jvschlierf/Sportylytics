# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model 3: Gradient Boosting

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp")

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

### 2. TUNING OF GRADIENT BOOSTING MODEL ###

# randomize data
random_index <- sample(1:nrow(train_basket), nrow(train_basket))
random_basket_train <- train_basket[random_index, ]

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  print(i)
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm_tune <- gbm(
    formula = Salary_Cap_Perc ~ .,
    distribution = "gaussian",
    cv.folds = 5,
    data = random_basket_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .8,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm_tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#      shrinkage  interaction.depth   n.minobsinnode   bag.fraction    optimal_trees    min_RMSE
#1       0.01            5                   5             0.65            1971        0.04176487
#2       0.01            5                   5             0.80            3186        0.04182010
#3       0.01            5                   10            0.65            4637        0.04195413
#4       0.01            5                   15            0.65            3559        0.04199782
#5       0.10            5                   5             0.80            449         0.04201419
#6       0.01            5                   5             1.00            2232        0.04205214
#7       0.01            5                   10            0.80            2197        0.04209576
#8       0.01            5                   15            0.80            4443        0.04212771
#9       0.10            5                   15            0.80            224         0.04214367
#10      0.10            5                   10            0.80            228         0.04218651

### 3. TRAINING ON THE BEST MODEL ###

set.seed(123)

# train GBM model
gbm_final <- gbm(
  formula = Salary_Cap_Perc ~ .,
  distribution = "gaussian",
  data = train_basket,
  n.trees = 1971,
  cv.folds = 5,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

saveRDS(gbm_final, file = "gbm_final.Rds")

### 4. PREDICTION ###

# use following code to read models
# gbm_final <- readRDS(file = "gbm_final.Rds")

pred_basket_y = predict.gbm(gbm_final, test_basket_x)

### 5. ANALYSIS OF RESULTS ###
RMSE = sqrt(mean((test_basket_y - pred_basket_y)^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
#RMSE is 0.042

rsq <- (cor(pred_basket_y, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,3), '\n')
#R-square is 0.66

gbm.perf(gbm_final, method = "cv")

# visualize the model, actual and predicted data
x_ax = 1:length(pred_basket_y)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_basket_y, col="red", pch=20, cex=.9) 

#Relative influence
summary(gbm_final, cBars = 10, method = relative.influence, las = 2)

#Partial dependence
gbm_final %>%
     partial(pred.var = "pts", n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
     autoplot(rug = TRUE, train = train_basket)