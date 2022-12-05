library(Metrics)
library(arm)
library(nnet)
library(MASS)
library(tidyverse)

rest=read.csv("Final_Business_0_1.csv")

rest2=read.csv("std_stars.csv")
data<-merge(rest,rest2,by="business_id")

data$DriveThru=as.factor(data$DriveThru)
data$RestaurantsDelivery=as.factor(data$RestaurantsDelivery)
data$Caters=as.factor(data$Caters)
data$Ambience_casual=as.factor(data$Ambience_casual)
data$OutdoorSeating=as.factor(data$OutdoorSeating)
data$Type=as.factor(data$Type)
data$RestaurantsTakeOut=as.factor(data$RestaurantsTakeOut)
data$RestaurantsReservations=as.factor(data$RestaurantsReservations)
str(data)



model=lm(std_stars.y ~ Type+Ambience_casual + Ambience_classy + WheelchairAccessible+ hours_per_week_y + HasTV + Caters 
         ,data=data)
summary(model)
anova(model)

rmse(rest$std_stars,predict(model))
plot(predict(model),resid(model),xlab="Predicted Ratings",ylab="Standardized Residuals")+abline(h=0)
#check for normality
qqnorm(rstandard(model))
qqline(rstandard(model))

