install.packages("titanic")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("DAAG")
library(titanic)
library(rpart.plot)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
library(InformationValue)
library(rpart)
library(randomForest)
library("DAAG")

cat("\014") # Clearing the screen

getwd()
setwd("C:\\Users\\Subha\\Documents\\R_Data") #This working directory is the folder where all the bank data is stored
rm(list = ls())

titanic_data<-read.csv('train.csv')
titanic_train= titanic_data[c("Pclass" ,"Sex" ,"Age" ,"SibSp", "Parch","Survived")]

#titanic test
titanic_test <-read.csv('test-3.csv')

# number of survived vs number of dead
CrossTable(titanic_train$Survived)

View(titanic_train)

# replacing NA in age column by it's mean
titanic_train$Age[is.na(titanic_train$Age)]= mean(titanic_train$Age[!is.na(titanic_train$Age)])
summary(titanic_train)

#splitting titanic train into 70,30
set.seed(1234) # for reproducibility
titanic_train$rand <- runif(nrow(titanic_train))
titanic_train_start <- titanic_train[titanic_train$rand <= 0.7,]
titanic_test_start <- titanic_train[titanic_train$rand > 0.7,]
View(titanic_train_start)

########## Model building ##########

full.model.titanic.mean <- glm(formula = Survived ~ Pclass+Sex+Age+SibSp+Parch,data = titanic_train_start,family = binomial) #family = binomial implies that it is logistic regression
summary(full.model.titanic.mean)

#removing insignificant variables
titanic_train_start$Parch<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass+Sex+Age+SibSp,
                               data=titanic_train_start, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)

#All variables significant
#Testing performance on Train set

titanic_train_start$prob = predict(full.model.titanic.mean, type=c("response"))
titanic_train_start$Survived.pred = ifelse(titanic_train_start$prob>=.5,'pred_yes','pred_no')
table(titanic_train_start$Survived.pred,titanic_train_start$Survived)

#Testing performance on test set
nrow(titanic_test)
titanic_test_start$prob = predict(full.model.titanic.mean, newdata=titanic_test_start, type=c("response"))
titanic_test_start$Survived.pred = ifelse(titanic_test_start$prob>=.5,'pred_yes','pred_no')
table(titanic_test_start$Survived.pred,titanic_test_start$Survived)

########## END - Model with mean included instead of NA #########

### Testing for Jack n Rose's survival ###
df.jackrose <- read.csv('Book1.csv')
df.jackrose$prob = predict(full.model.titanic.mean, newdata=df.jackrose, type=c("response"))
df.jackrose$Survived.pred = ifelse(df.jackrose$prob>=.5,'pred_yes','pred_no')
head(df.jackrose)

# Jack dies, Rose survives

### END - Testing on Jack n Rose ###

## START  K-fold cross validation ##

# Defining the K Fold CV function here
Kfold_func <- function(dataset,formula,family,k)
{
  object <- glm(formula=formula, data=dataset, family = family)
  CVbinary(object, nfolds= k, print.details=TRUE)
}

#Defining the function to calculate Mean Squared Error here
MeanSquareError_func <- function(dataset,formula)
{
  LM_Object <- lm(formula=formula, data=dataset)
  LM_Object_sum <-summary(LM_Object)
  MSE <- mean(LM_Object_sum$residuals^2)
  print("Mean squared error")
  print(MSE)
}

#Performing KFold CV on Training set by calling the KFOLD CV function here
Kfoldobj <- Kfold_func(titanic_train_start,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

#Calling the Mean Squared Error function on the training set here
MSE_Train <-MeanSquareError_func(titanic_train_start,Survived ~ Pclass + Sex + SibSp + Age)

#confusion matrix on training set
table(titanic_train_start$Survived,round(Kfoldobj$cvhat))
print("Estimate of Accuracy")
print(Kfoldobj$acc.cv)

#Performing KFold CV on test set by calling the KFOLD CV function here
Kfoldobj.test <- Kfold_func(titanic_test_start,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

#Calling the Mean Squared Error function on the test set here
MSE_Test <-MeanSquareError_func(titanic_test_start,Survived ~ Pclass + Sex + SibSp + Age)

#Confusion matrix on test set
table(titanic_test_start$Survived,round(Kfoldobj.test$cvhat))
print("Estimate of Accuracy")
print(Kfoldobj.test$acc.cv)

## END K-FOLD CROSS VALIDATION ##
