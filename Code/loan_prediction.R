# install.packages('Amelia')
library(Amelia)
library(tidyverse)
library(randomForest)
library(e1071)
library(caret)
library(corrplot)
library(pROC)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caTools)

#---------------------------------------------------------------------------------------------------------------#
#------------------------------------------IMPORTING THE DATASET------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#set the path in the below function setwd() to the project folder path in your local system before running the script
setwd("C:\\Users\\Kiruba\\Data Analytics\\DMML\\Project\\ComparativeStudyofMLAlgo")

loan_data <- read.csv("Datasets\\credit_train.csv")

dim(loan_data)
str(loan_data)
summary(loan_data)

#---------------------------------------------------------------------------------------------------------------#
#--------------------------------DATA CLEANING, TRANSFORMATION AND EXPLORATION----------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#null values checking
missmap(loan_data)

sum(is.na(loan_data$Months.since.last.delinquent)) /  nrow(loan_data)


#handling na values
apply(loan_data, 2, function(x) sum(is.na(x)))
temp_data <- loan_data %>% filter(!is.na(Annual.Income))
loan_data <- temp_data
apply(loan_data, 2, function(x) sum(is.na(x)))
nrow(loan_data)
table(loan_data$Years.in.current.job)
loan_data <- loan_data %>% filter(!is.na(Maximum.Open.Credit))
apply(loan_data, 2, function(x) sum(is.na(x)))
loan_data <- loan_data %>% filter(!is.na(Tax.Liens))
apply(loan_data, 2, function(x) sum(is.na(x)))
loan_data <- loan_data %>% filter(!is.na(Bankruptcies))
apply(loan_data, 2, function(x) sum(is.na(x)))
loan_data <- loan_data[,-c(1,2,13)]
apply(loan_data, 2, function(x) sum(is.na(x)))

nrow(loan_data)
sum(is.na(loan_data$Loan.Status))


# Identify outliers        
outliers <- boxplot(loan_data$Current.Loan.Amount, plot = FALSE)$out

# Remove outliers
loan_data <- loan_data[!(loan_data$Current.Loan.Amount %in% outliers), ]

outliers <- boxplot(loan_data$Credit.Score, plot = FALSE)$out
loan_data <- loan_data[!(loan_data$Credit.Score %in% outliers), ]

outliers <- boxplot(loan_data$Annual.Income, plot = FALSE)$out
loan_data <- loan_data[!(loan_data$Annual.Income %in% outliers), ]

outliers <- boxplot(loan_data$Current.Credit.Balance, plot = FALSE)$out
loan_data <- loan_data[!(loan_data$Current.Credit.Balance %in% outliers), ]

outliers <- boxplot(loan_data$Monthly.Debt, plot = FALSE)$out
loan_data <- loan_data[!(loan_data$Monthly.Debt %in% outliers), ]

outliers <- boxplot(loan_data$Current.Loan.Amount, plot = FALSE)$out
loan_data <- loan_data[!(loan_data$Current.Loan.Amount %in% outliers), ]
subset <- loan_data[,c(4,6,7,11,12,13,14,15,17,18)]
boxplot(loan_data)

loan_data$Loan.Status <- as.factor(loan_data$Loan.Status)
loan_data$Term <- as.factor(loan_data$Term)
table(loan_data$Home.Ownership)
loan_data$Home.Ownership <- as.factor(loan_data$Home.Ownership)
table(loan_data$Purpose)
loan_data$Purpose <-as.factor(loan_data$Purpose)
loan_data$Amount=scale(loan_data$Amount)
table(loan_data$Loan.Status)
loan_data$Years.in.current.job <- as.factor(loan_data$Years.in.current.job)
loan_data$years.in.job <- as.factor(loan_data$years.in.job)

#changing level names to R compatible as caret package not running with incompatible names
levels(loan_data$Loan.Status) <- make.names(levels(loan_data$Loan.Status))
levels(loan_data$Purpose) <- make.names(levels(loan_data$Purpose))
levels(loan_data$Years.in.current.job) <- make.names(levels(loan_data$Years.in.current.job))
levels(loan_data$Home.Ownership) <- make.names(levels(loan_data$Home.Ownership))
levels(loan_data$Term) <- make.names(levels(loan_data$Term))



#-------------------------------------------------------------------------------------------------------------#
#----------------------------------------Train-Test Split-----------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#



set.seed(1234)
split_sample <- sample.split(loan_data$Loan.Status,SplitRatio=.8)
train_data.loan <- subset(loan_data[,-6],split_sample==TRUE)
test_data.loan <- subset(loan_data[,-6],split_sample==FALSE)
dim(train_data.loan)
dim(test_data.loan)
dim(loan_data)
str(train_data.loan)


#---------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------TRAINING THE MODELS---------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#RANDOM FOREST
gc()
library(randomForest)
Sys.time()
train_data.loan
train_data.loan$Loan.Status <- as.factor(train_data.loan$Loan.Status)

#using smote oversampling
ctrl_data <- trainControl(method = "cv",
                          number = 10,
                          verboseIter = T,
                          classProbs = T,
                          sampling = "smote",
                          summaryFunction = twoClassSummary,
                          savePredictions = T)

set.seed(1234)


#train random forest model
model_rf_smote <- train(Loan.Status ~ ., data = train_data.loan, method = "rf", trControl = ctrl_data, verbose = T, metric = "ROC")
rf.model <- model_rf_smote
rf.model

#make predctions
rf.predict <- predict(rf.model,test_data.loan[,-1])

#confusion matrix and ROC
confusionMatrix(rf.predict,test_data.loan$Loan.Status)
rf.y<- as.numeric(ifelse(test_data.loan$Loan.Status=='Fully.Paid',1,0))
table(rf.y)
table(train_data.loan$Loan.Status)
rf.pred <-as.factor(ifelse(rf.predict =='Fully.Paid',1,0))
table(rf.pred)
plot(roc(rf.pred,rf.y))

