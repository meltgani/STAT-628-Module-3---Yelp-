library(ggplot2)
library(tidyverse)
library(Metrics)
rest=read.csv("restaurant6.csv")
rest

rest %>%
  group_by(Type) %>%
  summarise(average=mean(hours_per_week)) %>%
  ggplot()+
  geom_col(aes(average,Type),width=0.7)+
  scale_x_continuous(expand=c(0,0))+
  labs(
    x = "Average Hours Per week",
      )

########
rest3=read.csv("Restaurant7.csv")


rest3 %>%
  group_by(city) %>%
  summarise(total=n()) %>%
  ggplot()+
  geom_col(aes(city,total))+
  scale_y_continuous(expand=c(0,0))+
  labs(
    y = "Number of Restaurants",
  )

#####

rest2=read.csv("Restaurant7.csv")
rest2

rest2$Type=as.factor(rest2$Type) 
typeof(rest2$Type)
str(rest2)


  ggplot(rest2)+
  geom_boxplot(aes(Type,stars))+
    labs(
      y = "Ratings",
    )
    
########

rest4=read.csv("restaurant8.csv")
  
  
  
  rest4$day_of_the_week=as.factor(rest4$day_of_the_week) 
  str(rest4)
  Days=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
rest4<- rest4 %>%
  mutate(day_of_the_week= factor(day_of_the_week,levels=Days)) %>%
  arrange(day_of_the_week)
  
  
  ggplot(rest4)+
    geom_col(aes(day_of_the_week,hours_per_day))+
    facet_wrap( ~ Type)+
    theme(axis.text.x=element_text(angle=60,vjust = 0.7))+
    labs(
      y = "Total Hours Per Day",
      x= "Day of Week"
    )
    
###########
  
rest=read.csv("Final_Business_0_1.csv")
rest2=read.csv("std_stars.csv")
data<-merge(rest,rest2,by="business_id")


data$DriveThru=as.factor(data$DriveThru)
ggplot(data)+
  geom_boxplot(aes(DriveThru,std_stars.y))+
  labs(
      y = "Ratings",
    )

data$RestaurantsDelivery=as.factor(data$RestaurantsDelivery)
ggplot(data)+
  geom_boxplot(aes(RestaurantsDelivery,std_stars.y))+
  labs(
    y = "Ratings",
  )
 
data$Caters=as.factor(data$Caters)
ggplot(data)+
  geom_boxplot(aes(Caters,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

data$Ambience_casual=as.factor(data$Ambience_casual)
ggplot(data)+
  geom_boxplot(aes(Ambience_casual,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

data$OutdoorSeating=as.factor(data$OutdoorSeating)
ggplot(data)+
  geom_boxplot(aes(OutdoorSeating,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

data$Type=as.factor(data$Type)
ggplot(data)+
  geom_boxplot(aes(Type,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

data$RestaurantsTakeOut=as.factor(data$RestaurantsTakeOut)
ggplot(data)+
  geom_boxplot(aes(RestaurantsTakeOut,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

data$RestaurantsReservations=as.factor(data$RestaurantsReservations)
ggplot(data)+
  geom_boxplot(aes(RestaurantsReservations,std_stars.y))+
  labs(
    y = "Ratings",
  ) 

