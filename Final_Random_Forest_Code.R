################################################### Prediction with imp variables #################################################################

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
data<-subset(data,select=c(RestaurantsReservations,Ambience_classy,Ambience_casual,Caters,HasTV,std_stars,RestaurantsDelivery,DriveThru,RestaurantsTakeOut))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
#data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
#data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$std_stars, SplitRatio = 0.75)
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
