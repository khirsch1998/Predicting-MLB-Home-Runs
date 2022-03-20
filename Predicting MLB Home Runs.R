install.packages("tidyverse")
install.packages("GGally")
install.packages("knitr")
install.packages("klaR")
install.packages("car")
install.packages("corrplot")
install.packages("gvlma")
install.packages("scoring")
install.packages("pls")
# Load Libraries 
library(tidyverse)
library(GGally)
library(knitr)
library(klaR)
library(car)
library(corrplot)
library(lmtest)
library(gvlma)
library(scoring)
library(caret)
library(pls)

# Import data for the Initial Model
baseballdata <- read.csv("C:/Users/iansu/OneDrive/Documents/Masters/INF 624/INF-624-Project-Replicability-Assessment/Combined Baseballdata1.csv", header=TRUE)
# Import data for the Subsequent Model
baseballdata2 <- read.csv("C:/Users/iansu/OneDrive/Documents/Masters/INF 624/INF-624-Project-Replicability-Assessment/FinalModel_data.csv", header=TRUE)

#### Initial Model Build and Validation

# Define training and testing sets for the initial model (80% in training set and 20% in testing set) 
train <- baseballdata[1:138, c("HR", "RBI", "BB", "K", "AVG", "OBP", "SLG")]
test <- baseballdata[139:nrow(baseballdata), c("HR","RBI", "BB", "K", "AVG", "OBP", "SLG")]

# Build the HR Model as a Multivariate Linear Regression
HR_Model <-lm(HR ~ RBI + BB + K + AVG + OBP + SLG, data=train)  

# Summary of the HR Model
summary(HR_Model)
round(summary(HR_Model)$coef, 3)

# Residual Plot of the HR model
## Create a separate dataframe of the predicted values and residuals
HR_Model_pred_resid = data.frame(Prediction=HR_Model$fitted.values, Residual=HR_Model$residuals)
## Create a residual plot of the model using ggplot
HR_Model_resid_plot = ggplot(HR_Model_pred_resid, aes(x=Prediction, y=Residual)) +
  geom_point(col = 'blue') +
  labs(title="HR Model Residual Plot", x="HR Prediction", y="Residual")
plot(HR_Model_resid_plot)

# Outlier Test using the car package
outlierTest(HR_Model)

# Multicollinearity Test
## Condition Index on the HR Model using klaR package
cond.index(HR ~ RBI + BB + K + AVG + OBP + SLG, train) 

#Partial Least Squares using pls package
model <- plsr(HR ~ RBI + BB + K + AVG + OBP + SLG, data=baseballdata, validation="CV")
summary(model)
validationplot(model, val.type="R2")

#define training and testing sets to calculate Root Mean Square Error (RMSE)
pls_train <- baseballdata[1:138, c("HR", "RBI", "BB", "K", "AVG", "OBP", "SLG")]
y_test <- baseballdata[139:nrow(baseballdata), c("HR")]
test <- baseballdata[139:nrow(baseballdata), c("RBI", "BB", "K", "AVG", "OBP", "SLG")]

#calculate RMSE
pcr_pred <- predict(model, test, ncomp=4)
sqrt(mean((pcr_pred - y_test)^2))

## Correlation Matrix on the HR Model using corrplot package
### Build new data frame of Independent variables (IVs)
IVs <- data.frame(train$RBI, train$BB, train$K, train$AVG, train$OBP, train$SLG)
### Build corrplot with IVs
corrplot(cor(IVs), method = "number", type = "upper", bg = "grey20")

# Remove OBP and K from HR Model to build HR Model2
HR_Model2 <- lm(HR ~ RBI + BB + AVG + SLG, data=train)
summary(HR_Model2)

# Retest for Multicollinearity on HR Model2
## Condition Index on HR Model2
cond.index(HR ~ RBI + BB + AVG + SLG, train)

# Heteroscedasticity Test
## Breusch-Pagan Test on HRModel2 using lmtest package
bptest(HR_Model2)

## Global Validation of Linear Models Assumptions using gvlma package 
gvlma(HR_Model2)

# Autocorrelation Test
## Durbin-Watson Test using lmtest package
dwtest(HR_Model2)

# HR Model Accuracy Test
## Predict HRs with the test data
HRPredict <- predict(HR_Model2, newdata = test)  

## Make new data frame with the actuals and predicted values
actuals_preds <- data.frame(cbind(actuals=test$HR, predicteds=HRPredict))  

## Test Accuracy 
correlation_accuracy <- round(cor(actuals_preds),3)
correlation_accuracy  # 96.7%

#### Subsequent Model Build and Validation

# define new training and testing sets for the Subsequent Model (80% in training set and 20% in testing set) 
train <- baseballdata2[1:146, c("HR", "RBI", "BB", "K", "AVG", "OBP", "SLG")]
test <- baseballdata2[147:nrow(baseballdata2), c("HR","RBI", "BB", "K", "AVG", "OBP", "SLG")]

# Build the Final HR Model
Final_HR_Model <-lm(HR ~ RBI + BB + AVG + SLG, data=train) 

# Summary of the Final HR Model
summary(Final_HR_Model)

# Residual Plot of the Final HR model
## Create a separate dataframe of the predicted values and residuals
Final_HR_Model_pred_resid = data.frame(Prediction=Final_HR_Model$fitted.values, Residual=Final_HR_Model$residuals)
## Create a residual plot of the model using ggplot
Final_HR_Model_resid_plot = ggplot(Final_HR_Model_pred_resid, aes(x=Prediction, y=Residual)) +
  geom_point(col = 'Red') +
  labs(title="Final HR Model Residual Plot", x="HR Prediction", y="Residual")
plot(Final_HR_Model_resid_plot)

# Outlier Test on the Final HR Model
outlierTest(Final_HR_Model)

# Multicollinearity Test 
## Condition Index on Final HR Model
cond.index(HR ~ RBI + BB + AVG + SLG, train)

## Correlation Matrix on the Final HR Model
### Build data frame of the Final HR Model Independent variables (IVs)
IVs <- data.frame(train$RBI, train$BB, train$AVG, train$SLG)
### Create corrplot with the Final HR Model IVs
corrplot(cor(IVs), method = "number", type = "upper", bg = "grey20")

# Heteroscedasticity Test 
## Breusch-Pagan Test on the Final HR Model
bptest(Final_HR_Model)

## Global Validation of Linear Models Assumptions on the Final HR Model
gvlma(Final_HR_Model)

# Autocorrelation Test 
## Durbin-Watson Test on the Final HR Model
dwtest(Final_HR_Model)

# Final HR Model Accuracy Test
## Predict HRs with the Final HR Model test data
HRPredict <- predict(Final_HR_Model, newdata = test)  

## Make new data frame with the actuals and predicted values
actuals_preds <- data.frame(cbind(actuals=test$HR, predicteds=HRPredict))  

# Test Accuracy 
correlation_accuracy <- round(cor(actuals_preds),3)
correlation_accuracy  # 95.3%