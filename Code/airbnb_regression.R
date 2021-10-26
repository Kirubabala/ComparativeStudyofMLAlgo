install.packages('xgboost')
library(xgboost)
library(caret)
library(dplyr)
library(Amelia)


#---------------------------------------------------------------------------------------------------------------#
#------------------------------------------IMPORTING THE DATASET------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
#set the path in the below function setwd() to the project folder path in your local system before running the script
setwd("C:\\Users\\Kiruba\\Data Analytics\\DMML\\Project\\ComparativeStudyofMLAlgo")

airbnb_data <- read.csv("Datasets\\AB_NYC_2019.csv", stringsAsFactors = TRUE)

head(airbnb_data)
str(airbnb_data)
summary(airbnb_data)

#---------------------------------------------------------------------------------------------------------------#
#--------------------------------DATA CLEANING, TRANSFORMATION AND EXPLORATION----------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#plotting missing values
missmap(airbnb_data)

#Checking for outliers
boxplot(airbnb_data)
boxplot(airbnb_data$price)

#removing extreme values as very less rows
airbnb_data <- airbnb_data %>% filter(price<200) %>% filter(!is.na(reviews_per_month))

cor(airbnb_data[,c(7,8,10,11,12,14,15,16)])

#removing ID columns and removing some columns to avoid multicollinearity among predictors
airbnb_data <- airbnb_data[,-c(1,2,3,4,13)]
str(airbnb_data)
#checking normality of price column
qqnorm(airbnb_data$price)
qqline(airbnb_data$price)
hist(airbnb_data$price)

#---------------------------------------------------------------------------------------------------------------#
#----------------------------------------TRAINING AND TEST DATA SPLIT-------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#Splitting train and test into 80:20 ratio
split_index_airbnb = createDataPartition(airbnb_data$price, p = .80, list = F)
airbnb.train = airbnb_data[split_index_airbnb, ]
airbnb.test = airbnb_data[-split_index_airbnb, ]
airbnb.train_x = data.matrix(airbnb.train[, -7])
airbnb.train_y = airbnb.train[,7]
airbnb.test_x = data.matrix(airbnb.test[, -7])
airbnb.test_y = airbnb.test[, 7]


#---------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------TRAINING THE MODELS---------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
train1 <- airbnb.train_x
set.seed(10012021)
my_control <-trainControl(method="cv", number=5)
nrow(train1)
length(airbnb.test_y)
#cross validation
xgb_caret <- train(x=train1, y=airbnb.train_y, method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune
#test and train into dmatrix
dtrain <- xgb.DMatrix(data = as.matrix(airbnb.train_x), label= airbnb.train_y)
dtest <- xgb.DMatrix(data = as.matrix(airbnb.test_x))


#substitute best tune models parameters
default_param<-list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eta = 0.01,
  max_depth = 6,
  gamma = 0,
  min_child_weight = 4,
  subsample=1,
  colsample_bytree=1
)

#cross validation to determine appropriate number of rounds value

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 2000, nfold = 5,
                 showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

#training using best iteration through cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 1034)
xgb.pred <- predict(xgb_mod, dtest)

mse = mean((airbnb.test_y - xgb.pred)^2)
mae = MAE(airbnb.test_y, xgb.pred)
rmse =RMSE(airbnb.test_y, xgb.pred)
 
paste("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
 
x = 1:length(airbnb.test_y)
plot(x, airbnb.test_y, col = "red", type = "l")
lines(x, predictions_XGB, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original price", "predicted price"), 
col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))
 
#residual sum of squares and total sum of squares
rss <- sum((xgb.pred - airbnb.test_y) ^ 2)  
tss <- sum((airbnb.test_y - mean(airbnb.test_y)) ^ 2)
r2 <- 1 - rss/tss
r2


#-----------Multiple Linear Regression------#
str(airbnb.train)

lm.model <- lm(price~.,data=airbnb.train[,-2])
summary(lm.model)
class(airbnb.train)
# 
# levels(airbnb.test$neighbourhood)
# levels(airbnb.train$neighbourhood)
# test_now <- airbnb.test %>% filter(!neighbourhood %in% c('Belle Harbor', 'Holliswood', 'Lighthouse Hill'))
lm.pred <- predict(lm.model,airbnb.test[,-2])
mse = mean((airbnb.test[,6] - lm.pred)^2)
mae = MAE(airbnb.test[,6], lm.pred)
rmse = RMSE(airbnb.test[,6], lm.pred)

paste("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


