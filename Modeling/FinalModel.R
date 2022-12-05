library(randomForest)
require(caTools)
library(magrittr)
library(dplyr)
library(caret)
library(e1071)
library(ggplot2)
library(mlbench)
library(caret)
library(moments)

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(std_stars,OutdoorSeating,Ambience_classy,Ambience_casual,Caters,DriveThru,RestaurantsReservations,RestaurantsDelivery,Type,RestaurantsTakeOut))

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

rf <- randomForest(std_stars ~ .,data=data)
rf
pred = predict(rf, newdata=data[-1])
confusionMatrix(as.factor(pred), as.factor(data$std_stars))

sample = sample.split(data$std_stars, SplitRatio = 0.85)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-21])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$std_stars))
