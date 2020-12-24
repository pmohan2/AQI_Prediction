setwd("/Users/praveenmohan/Desktop/Fall2020/IE500/untitled folder/Model")

# Libraries
library(dplyr)
library(caret)
library(tidyverse)
library(earth)
library(parallel)
library(doParallel)
library(plotmo)
library(ggplot2)
library(vip)
cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)

df <- read.csv("Final.csv")

df$City <- as.factor(df$City)
df$State <- as.factor(df$State)
df$Region <- as.factor(df$Region)
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)
df$Season <- as.factor(df$Season)

df1 <- df
df <- df[,-c(2, 4, 5)]

# Model Building

## 1. Linear Regression____________________

# Finding significant factors for LR Model
L1 <- lm(AQI~., data = df)
summary(L1)
L2 <- lm(AQI~., data = df[,-c(11, 7, 13)])
summary(L2)

# 10-Fold Cross Validation
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10,  allowParallel = T)
LR_final <- train(AQI ~., data = df[,-c(2, 4, 11, 7, 13)], method = "lm",
                  trControl = train.control)
# Results
summary(LR_final)
print(LR_final)
#___________________________________________

## 2. Generalized Linear Model
GLM <- train(AQI ~., data = df[,-c(2, 4, 11, 7, 13)], method = "glm",
             trControl = train.control)
# Results
summary(GLM)
print(GLM)
#____________________________________________

## 3. Stepwise Regression (Note: Approx time to run - 3 min)
SR <- train(AQI ~., data = df[,-c(2, 4, 11, 7, 13)], method = "lmStepAIC", 
            trControl = train.control, trace = FALSE)
# Results
summary(SR)
print(SR)
#______________________________________________

## 4. Decision Tree (Note: Approx time to run - 2 min)
DT <- train(AQI ~., data = df1, method = "rpart", 
            trControl = train.control, tuneLength = 15)
# Results
print(DT)
#______________________________________________

## 5. LASSO Regression
LAS <- train(AQI~., data = df1, method = "glmnet", tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-1, 1, by = 0.01)),  trControl = train.control)
LAS_final <- train(AQI~., data = df, method = "glmnet", tuneGrid = expand.grid(alpha = 1, lambda = LAS$bestTune$lambda),  trControl = train.control)

# Results
summary(LAS_final)
print(LAS_final)
#________________________________________________

## 6. RIDGE Regression
RG <- train(AQI~., data = df1, method = "glmnet", tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-1, 1, by = 0.01)),  trControl = train.control)
RG_final <- train(AQI~., data = df, method = "glmnet", tuneGrid = expand.grid(alpha = 0, lambda = RG$bestTune$lambda),  trControl = train.control)
# Results
summary(RG_final)
print(RG_final)
#________________________________________________

## 7. Multivariate Regression Splines (MARS) (Note: Approx time to run - 10 min)

MARS <- train(AQI~., data = df1, method = "earth", tuneGrid = expand.grid(degree = 2, nprune = 40),  
              trControl = trainControl( method = 'cv', verboseIter = F, savePredictions = T, allowParallel = T))

# Results
summary(MARS)
print(MARS)
#________________________________________________

## 8. GAM (Note: Approx time to run - 10 min)
GAM <- train(AQI ~ City + Population +
               + Thermal + PM + BTX + NO + NO2 + NOx 
             + CO + SO2 + O3, data = df, method = "gam", family = "gaussian",  
             tuneGrid = data.frame(method = "GCV.Cp", select = FALSE), 
             trControl = train.control)
# Results
summary(GAM)
print(GAM)
#_________________________________________________

# Best Model: Multivariate Regression Splines (MARS)

# Visualizing the model
# Parameters and Coefficients
MARS$finalModel$coefficients %>%
  as.data.frame() %>%
  mutate( parameter = row.names(.)) %>%
  select( parameter, coef = y ) %>%
  knitr::kable( align = c('lc'), digits = 2 )

# Final Model
MARS$finalModel
plot(MARS$finalModel, which = 1)
p1 <- vip(MARS, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(MARS, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")
gridExtra::grid.arrange(p1, p2, ncol = 2)


