install.packages("e1071")
library("e1071")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)

# Setting the working directory
setwd('C:/Users/Subhasish Sahu/Documents/R/data_2017')

titanic_data <-read.csv("train.csv")
View(titanic_data)
str(titanic_data)

#clean up columns/ drop based on business intelligence
titanic_final = titanic_data[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Survived")]
View(titanic_final)
summary(titanic_final)

#find missing values and replace it by the mean value
mean(titanic_final$Age, na.rm=TRUE)
titanic_final$Age[is.na(titanic_final$Age)] = mean(titanic_final$Age, na.rm = TRUE)

# split the data into training and test
set.seed(1234) # for reproducibility
titanic_final$rand <- runif(nrow(titanic_final))
df.train <- titanic_final[titanic_final$rand <= 0.7,]
df.test <- titanic_final[titanic_final$rand > 0.7,]
nrow(df.train)
nrow(df.test)

#support vector machines
Svm = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch, data = titanic_final, mtry=2,importance=TRUE)
print(Svm)


titanic_trainisvm_prob = predict(Svm, type=c("response"))
titanic_trainisvm_pred <- ifelse(titanic_trainisvm_prob>=.5,'pred_yes','pred_no')
View(df.train)


#confusion matrix

table(titanic_trainisvm_pred,df.train$Survived)
df.test$prob = predict(titanic_model, newdata = df.test,  type=c("response"))

df.test$spred <- ifelse(df.test$prob>=.5,'pred_yes','pred_no')
table(df.test$spred,df.test$Survived)

