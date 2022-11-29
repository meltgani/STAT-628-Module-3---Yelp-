library(randomForest)
require(caTools)
library(magrittr)
library(dplyr)
library(caret)
library(e1071)

data <- read.csv("C:/Users/aahuj/Desktop/STAT-628-Module-3---Yelp-/Final_Business_0_1.csv")
head(data)
dim(data)
#sapply(data, class)
data <- subset(data, select = -c(business_id, address,X,hours, name,state,latitude,longitude,review_count,is_open,categories,DietaryRestrictions))
str(data)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)
data$hours_per_week_y<- as.numeric(data$hours_per_week_y)
data$weight<-as.numeric(data$weight)
str(data)

sample = sample.split(data$stars, SplitRatio = .70)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
sample_weights<- train$weight
train <- subset(train, select = -c(weight))
test<-subset(test,select = -c(weight))

rf <- randomForest(stars ~ .,data=train,weights = sample_weights)
rf
pred = predict(rf, newdata=test[-3])
#cm = table(test[,3], pred)

confusionMatrix(as.factor(pred), as.factor(test$stars))

###################################################################################################################################

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data<-subset(data,select=-c(weight))
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(stars~., data=data, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
