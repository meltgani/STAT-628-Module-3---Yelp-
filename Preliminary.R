library(ggplot2)
library(tidyverse)
library(Metrics)
rest=read.csv("~/Desktop/STAT628/Module3/STAT-628-Module-3---Yelp-/restaurant6.csv")
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
rest3=read.csv("~/Desktop/STAT628/Module3/STAT-628-Module-3---Yelp-/Restaurant7.csv")


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

library(ggplot2)
library(tidyverse)
library(Metrics)
rest2=read.csv("~/Desktop/STAT628/Module3/STAT-628-Module-3---Yelp-/Restaurant7.csv")
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

  library(ggplot2)
  library(tidyverse)
  library(Metrics)
  rest4=read.csv("~/Desktop/STAT628/Module3/STAT-628-Module-3---Yelp-/restaurant8.csv")
  
  
  
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
    
