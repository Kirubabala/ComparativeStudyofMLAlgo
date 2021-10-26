# install.packages('party')
# install.packages('DataExplorer')
# install.packages('Amelia')
library(Amelia)
library(tidyverse)
#library(class)
library(e1071)
library(caret)
library(corrplot)
library(pROC)
library(ggplot2)
#library(caTools)
#library(party)
# install.packages('data.table',type='source')
#library(data.table)
#library(DataExplorer)

#-------------------------------------------------------------------------------------------------------------#
#-----------------------------------IMPORTING THE DATASET-----------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#

bank_main_data.raw <- read.csv(file = "C:\\Users\\Kiruba\\Data Analytics\\DMML\\Project\\Datasets\\finaldata\\bank-additional-full.csv",
                              sep = ";",
                            stringsAsFactors = TRUE)
head(bank_main_data.raw)
str(bank_main_data.raw)
summary(bank_main_data.raw)

#-------------------------------------------------------------------------------------------------------------#
#-----------------------------------EXPLORING THE DATA--------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#

#plot missing values using missmap() from Amelia
missmap(bank_main_data.raw)
#structure


#Checking for outliers
boxplot(bank_main_data.raw)


summary(bank_main_data.raw$duration)

#understanding the data using ggplot visualizations
ggplot(data = bank_main_data.raw, aes(x=bank_main_data.raw$age))+
  geom_density()+
  labs(x="Age of the person", y="Density")

ggplot(data = bank_main_data.raw, aes(x=bank_main_data.raw$education))+
  geom_bar()+
  labs(x="Education", y="Count")+
  coord_flip()


#marital status count

ggplot(bank_main_data.raw, aes(x=bank_main_data.raw$marital))+
  geom_bar()+
  labs(x="Marital status", y="Number of persons in category")

#Job status count

ggplot(bank_main_data.raw, aes(x=bank_main_data.raw$job))+
geom_bar()+
  labs(x="Categories in job", y="Number of persons in category")+
  coord_flip()


ggplot(data = bank_main_data.raw, aes(x=bank_main_data.raw$loan, fill=bank_main_data.raw$y))+
  labs(x="Loan", y="Count", fill="Subscribed or not?")+
  geom_bar()

ggplot(bank_main_data.raw,aes(x=bank_main_data.raw$cons.price.idx,y=bank_main_data.raw$emp.var.rate)) +
  geom_point() +
  labs(x='Consumer price index',y='Employee varaiance rate')

ggplot(bank_main_data.raw,aes(x=bank_main_data.raw$cons.conf.idx,y=bank_main_data.raw$emp.var.rate)) +
  geom_point() +
  labs(x='Consumer confidence index',y='Employee varaiance rate')


#Checking correlation plot 
sample.bank_main_data.raw %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#-------------------------------------------------------------------------------------------------------------#
#----------------------------------------Train-Test Split-----------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#

#Split training and test sets (80:20)

set.seed(150)
#filtering unknows data from loan column
sample.bank_main_data.raw <-bank_main_data.raw %>% filter(loan!='unknown') %>% droplevels()


# partitioned.sample.bank_main_data.raw <- createDataPartition(
#     sample.bank_main_data.raw$y,
#     times = 1,
#     p = 0.8,
#     list = F
#   )

split_index = createDataPartition(sample.bank_main_data.raw$y, p = .80, list = F)
sample.train = sample.bank_main_data.raw[split_index, ]
sample.test = sample.bank_main_data.raw[-split_index, ]

sample.train$y <- as.factor(sample.train$y)


# sample.train = sample.bank_main_data.raw[partitioned.sample.bank_main_data.raw, ]
# sample.test = sample.bank_main_data.raw[-partitioned.sample.bank_main_data.raw, ]
prop.table(table(sample.train$y))
prop.table(table(sample.test$y))
nrow(sample.train)
nrow(sample.test)



-------------------------------------------------------------------------------------------------------------#
#---------------------------------------------TRAINING THE MODELS--------------------------------------------#
#------------------------------------------------------------------------------------------------------------#




#--------------------Logistic Regression----------------------#

library(e1071)
glm.market <- glm(y~.,data = sample.train,family=binomial)
summary(glm.market)
#prediction
pred.glm <- predict(glm.market,sample.test,type='response')


#class(pred.glm1)
pred.glm <- as.factor(ifelse(pred.glm>0.5,1,0))
y.glm <- ifelse(sample.test$y=='yes',1,0)
y.glm <- as.factor(y.glm)


confusionMatrix(pred.glm, y.glm)

#plotting ROC curve
roc.glm <- roc(pred.glm,as.numeric(y.glm))
auc(roc.glm)
plot(roc.glm,col='red')



#------------------SVM----------------------------#

#training svm
svm.model <- svm(as.factor(sample.train$y)~ .,probability=TRUE, data = sample.train[,-21], kernel="linear")
summary(svm.model)

#predicting with svm model
svm.pred <- predict(svm.model, sample.test[-21],probability = TRUE)

#confusion matrix
confusionMatrix(as.factor(sample.test$y), svm.pred)


svm.pred <- as.factor(ifelse(svm.pred =='yes',1,0))
svm.y <- ifelse(sample.test[,21]=='yes',1,0)

#plotting the roc
svm.roc <- roc(svm.pred,svm.y)
plot(svm.roc)


#plotting ROC of both models in same plot
plot(roc.glm,col='red')
plot(svm.roc,col='blue',add=TRUE) 
auc(glm.roc)
legend(x = "bottomright", 
       legend = c("SVM", "Logistic Regression"),
       fill = c(1,2))


