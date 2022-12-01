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

################################################## Predicting using all variables ##############################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
head(data)
dim(data)
#sapply(data, class)
data <- subset(data, select = -c(business_id, address,X,hours, name,state,latitude,longitude,review_count,is_open,categories,DietaryRestrictions,weight,stars))
str(data)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$std_stars<-as.numeric(data$std_stars)
str(data)

sample = sample.split(data$std_stars, SplitRatio = .80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-92])
#cm = table(test[,3], pred)
confusionMatrix(as.factor(pred), as.factor(test$std_stars))
######################################################### Finding imp variables for whole data set #################################
set.seed(7)
# load the dataset
#data<-subset(data,select=-c(weight))
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(std_stars~., data=data, method="lvq", trControl=control) 
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
#plot(importance)
#Imp<-varImp(model,scale=FALSE)$importance
#Imp
#write.csv(Imp,"C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Imp.csv")
write.csv(importance,"C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Imp20.csv")
############################################################ Predicting with 20 variables #########################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(OutdoorSeating,Ambience_classy,Ambience_casual,Caters,HasTV,Ambience_trendy,Ambience_romantic,Ambience_intimate,RestaurantsDelivery,Ambience_hipster,RestaurantsReservations,Ambience_divey,Ambience_touristy,Ambience_upscale,Type,BusinessParking_street,DriveThru,RestaurantsTakeOut,RestaurantsAttire_.casual.,WheelchairAccessible,std_stars))
str(data)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.80)
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

######################################################## Useless Code ############################################################
#rf <- randomForest(stars ~ hours_per_week_y,data=train)
#rf
#pred = predict(rf, newdata=test[-23])
#cm = table(test[,3], pred)

#confusionMatrix(as.factor(pred), as.factor(test$stars))
#plot(data$hours_per_week_y,data$stars)

#ggplot(data, aes(x = stars,y=review_count)) +
#  geom_line()

###################################################### Splitting Data based on stars ############################################
# s<-split(data,data$stars)
# s
# data_1.5<-s$`1.5`
# data_2<-s$`2`
# data_2.5<-s$`2.5`
# data_3<-s$`3`
# data_3.5<-s$`3.5`
# data_4<-s$`4`
# data_4.5<-s$`4.5`
# data_5<-s$`5`
################################################### Prediction with 5 variables #################################################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(OutdoorSeating,Ambience_classy,Ambience_casual,Caters,HasTV,std_stars))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-6])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$std_stars))
############################################################

rf <- randomForest(std_stars ~ .,data=data)
rf
pred = predict(rf, newdata=data[-6])
confusionMatrix(as.factor(pred), as.factor(data$std_stars))

################################################### Prediction with 10 variables #################################################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(OutdoorSeating,Ambience_classy,Ambience_casual,Caters,HasTV,std_stars,Ambience_trendy,Ambience_romantic,Ambience_intimate,RestaurantsDelivery,Ambience_hipster))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-6])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$std_stars))
############################################################

rf <- randomForest(std_stars ~ .,data=data)
rf
pred = predict(rf, newdata=data[-6])
confusionMatrix(as.factor(pred), as.factor(data$std_stars))

################################################### Prediction with 7 variables #################################################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(OutdoorSeating,Ambience_classy,Ambience_casual,Caters,HasTV,std_stars))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-6])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$std_stars))
############################################################

rf <- randomForest(std_stars ~ .,data=data)
rf
pred = predict(rf, newdata=data[-6])
confusionMatrix(as.factor(pred), as.factor(data$std_stars))

################################################### Prediction with imp variables #################################################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(RestaurantsReservations,Ambience_classy,Ambience_casual,Caters,HasTV,std_stars,RestaurantsDelivery,DriveThru,RestaurantsTakeOut))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#sample_weights<- train$weight
#train <- subset(train, select = -c(weight))
#test<-subset(test,select = -c(weight))

rf <- randomForest(std_stars ~ .,data=train)
rf
pred = predict(rf, newdata=test[-6])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$std_stars))
############################################################

rf <- randomForest(std_stars ~ .,data=data)
rf
pred = predict(rf, newdata=data[-6])
confusionMatrix(as.factor(pred), as.factor(data$std_stars))
