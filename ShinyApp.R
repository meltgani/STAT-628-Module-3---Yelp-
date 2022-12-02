library(dplyr)
library(randomForest)
library(Metrics)
library(shiny)
library(shinydashboard)

data <- read.csv("Final_Business_0_1.csv")
data<-subset(data,select=c(RestaurantsReservations,Ambience_classy,Ambience_casual,Caters,HasTV,RestaurantsDelivery,DriveThru,RestaurantsTakeOut,Type))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)


ui<-dashboardPage(
  dashboardHeader(title = "Business Owner Guide"),
  dashboardSidebar(sidebarMenu(
    menuItem("Prediction", tabName = "prediction", icon = icon("dashboard")),
    menuItem("Recommendation", tabName = "recommendations", icon = icon("th"))
  )),
  dashboardBody(
    tabItems( tabItem(tabName="prediction",
                      selectInput("type", "Type of Restuarant:",c("Italian"='italian',"American"="america","Fast Food"="fast",
                                                                  "Deli"="deli","Bakery"="bakery","Mexican"="mexican",
                                                                  "Coffee"="coffee","Breakfast"="breakfast","Japanese"="japanese",
                                                                  "Healthy"="healthy","Pizza"="pizza")),
                      br(),
                      selectInput("ambi", "Ambiance Classy:",c("Yes" = "1","No" = "0")),
                      br(),
                      selectInput("ambi2", "Ambiance Casual:",c("Yes" = "1","No" = "0")),
                      br(),
                      selectInput("reservation","Restaurant Reservation:",c("Yes"="1","No"="0")),
                      br(),
                      selectInput("caters","Caters:",c("Yes"="1","No"="0")),
                      br(),
                      selectInput("tv","Have a TV:",c("Yes"="1","No"="0")),
                      br(),
                      selectInput("delivery","Restaurant Delivery:",c("Yes"="1","No"="0")),
                      br(),
                      selectInput("drivethru","Drive-Thru:",c("Yes"="1","No"="0")),
                      br(),
                      selectInput("takeout","Restaurant TakeOut:",c("Yes"="1","No"="0")),
                      
                      submitButton('pred','Submit'),
                      fluidRow(box(title="Rating",textOutput("stars"),height=200))
                      
    ),
    
    tabItem(tabName="recommendations",
            fluidRow(
              box(title="Things To Avoid",textOutput("bad"),height=200),
              box(title="Things to Include",textOutput("good"),height=200)
            )
    )
    )
    )
   
) 


server <- function(input,output){
 output$bad<-renderText({
   if (input$type=="italian") { 
     "Things to be careful about are the server ‘never came  table’ , and customers ‘asked speak  manager’ ."
   } else if (input$type=="america") {
     "Things to be careful about are customers said ‘took long time’, ‘worst service’,’never checked us’. "
   } else if  (input$type=="fast") {
     "Things to be careful about are customers said ‘horrible customer service’ , ‘got food poisoning’ and ‘never go back’."
   } else if (input$type=="deli") {
     "Things to be careful about where customers said ‘ terrible customer service’ ,’counter taking forever’ ,’raised price’."
   } else if  (input$type=="bakery") {
     "Things to be careful about where customers ‘’ordered dozen but disappointed’. "
   } else if (input$type=="mexican") {
     "Things to be careful about are ‘worst chipotle ever’  and ‘worst customer service’. "
   } else if  (input$type=="coffee") {
     "Things to be careful about are 'bathroom disgustingly dirty’ ,‘slow lazy worker’ , ‘run like disorganized’,’busy time’  and ‘clover machine’ not working. "
   } else if (input$type=="breakfast") {
     "Things to be careful about are ‘minus one star’ ,’food took long’  , ‘server never came’ and ‘got food poisoning’. "
   } else if  (input$type=="japanese") {
     "Things to be careful about are ‘worst customer service’ , ‘long wait time’ , ‘read yelp reviews’, ‘. "
   }else if  (input$type=="healthy") {
     "Things to be careful about are  ‘disappointed spicy ketchup’ , ‘quite pricey beans’, and ‘hard fried chicken’ ."
     }
   else {
     "Things to be careful about are  ‘poor customer service’ , ‘tasted like cardboard’ , and  ‘ staff could care less’. "
   }
 
   
   })

 output$good<-renderText({
   if (input$type=="italian") { 
     "You should consider serving ‘authentic italian food’ such as  ‘olio e limone’ , ‘olive oil cake’ , ‘al burro e’ , ‘burro e salvia’ and ‘ravioli al burro’.  You should also have a ‘great wine list’."
   } else if (input$type=="america") {
     "You should consider serving ‘fried chicken sandwich’ , ‘mac n cheese’ ,’good beer selection’, ‘lemon poppy seed’ , ‘chicken enchilada soup’, and ‘molten chocolate cake’."
   } else if  (input$type=="fast") {
     "You should consider serving ‘spicy chicken sandwich’ , ‘animal style fries’ , and ‘buffalo chicken fries’. You should also have ‘late night food’ and ‘drive thru line’. "
     
   } else if (input$type=="deli") {
     "You should consider serving ‘chipotle turkey club’ , ‘crispy chicken west’, ‘gluten free bread’ , ‘grilled cheese tomato’ , and ‘ prosciutto brie sandwich’. Your ‘staff super friendly’. "
   } else if  (input$type=="bakery") {
     "You should consider serving ‘kahlua french toast’ , ‘chocolate chip cookie’ , ‘ham cheese croissant’ , ‘fried egg sandwich’ , ‘squeezed orange juice’ , ‘pain au chocolat’ , ‘gluten free’ . Your ‘staff super friendly’. "
     
   } else if (input$type=="mexican") {
     "You should consider serving ‘authentic mexican food’ such as ‘pico de gallo’, ‘great salsa bar’ , ‘carne asada burrito’ ‘ al pastor tacos’  , ‘tacos del mar’ , ’chicken tacos’,’agaves burrito’,  ‘marinated pork’, and ‘fish tacos’."
   } else if  (input$type=="coffee") {
     "You should consider serving ‘tugboat eggs benedict’ , ‘fried chicken sandwich’,’gluten free bread’,and ’best damn oatmeal’ . Your ‘staff super friend’ and location should have ‘great outdoor seating’."
     
   } else if (input$type=="breakfast") {
     "You should consider serving ‘corned beef hash’ , ‘chicken fried steak’ , ‘lemon ricotta pancakes’ ‘chocolate chip muffins’ ,’crab cake benedict’ , ‘maple bacon biscuit’, and ’potato skin hash’."
   } else if  (input$type=="japanese") {
     "You should consider serving ‘ ramon pork belly’ , ‘spicy ramen pork belly’ ,‘ramen pork shoulder’, ‘black kuro ramen’. You should also make sure you serve ‘soft boiled eggs’. You should have ‘ worlds best service’."
     
   }else if  (input$type=="healthy") {
     "You should consider serving ‘vegan gluten free’ , ‘artichoke crab cakes’, ‘vanilla ice cream’ ,’egg white frittata’, ‘baklava ice cream’ , ‘soy ice cream’ , ‘best acai bowls’ , and  ‘gluten free pancakes’ ."
   }
   else {
     "You should consider serving “deep dish pizza’ , ‘thin crust pizza’ , ‘gluten free pizza’ , ‘bbq chicken pizza’ , ‘wood fire pizza’ , and ‘chipotle chicken pizza’."
   }
})
 

 
 rating<-reactive({
   model_rf <- readRDS(file ='rf_2.rda')
   
   
   x.new<-rbind(data,data.frame(input$type,input$ambi,input$ambi2,input$reservation,input$caters,input$tv,input$delivery,input$drivethru,input$takeout,stringsAsFactor = FALSE))
   x.new[sapply(x.new, is.character)] <- lapply(x.new[sapply(x.new, is.character)],as.factor)
   y_pred = predict(model_rf, newdata = x.new[nrow(x.new),])
   print(x.new)
   return(x.new)
  
 })
output$stars<-renderText({rating()})
}

shinyApp(ui,server)


