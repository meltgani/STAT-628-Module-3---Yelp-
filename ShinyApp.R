library(dplyr)
library(randomForest)
library(Metrics)
library(shiny)
library(shinydashboard)

data <- read.csv("Final_Business_0_1.csv")
data<-subset(data,select=c(Type,RestaurantsReservations,Ambience_classy,Ambience_casual,Caters,OutdoorSeating,std_stars,RestaurantsDelivery,DriveThru,RestaurantsTakeOut))
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)],as.factor)

model <- randomForest(std_stars ~ .,data=data,ntree = 500, mtry = 4, importance = TRUE)


ui<-dashboardPage(
  dashboardHeader(title = "Business Owner Guide"),
  dashboardSidebar(sidebarMenu(
    menuItem("General Information",tabName = "general_info",icon=icon("info-circle")),
    menuItem("Prediction", tabName = "prediction", icon = icon("dashboard")),
    menuItem("Recommendation", tabName = "recommendations", icon = icon("th"))
  )),
  dashboardBody(
    tabItems( tabItem(tabName="general_info",img(src="pic.png",height=400,width=1000),
                      br(),
                      br(),
                      h1("Are you a new business owner looking to open up a restaurant in California? Well, you
    have come to the right place!We developed this app to allow you to get an intital rating. We also added suggestions and menu
                                                   recommendations that you could consider implementing to boost your ratings"
                      ),
                      br(nrow=20),
                      h4("For questions contact Sheng Huang at shuang452@wisc.edu")),
              
              
              tabItem(tabName="prediction",
                      selectInput("type", "Type of Restuarant:",c("Italian"='Italian',"American"="American","Fast Food"="Fast Food",
                                                                  "Deli"="Deli","Bakery"="Bakery","Mexican"="Mexican",
                                                                  "Coffee"="Coffee","Breakfast"="Breakfast","Japanese"="Japanese",
                                                                  "Healthy"="Healthy","Pizza"="Pizza")),
                      
                      selectInput("ambi", "Ambiance Classy:",c("Yes" = "1","No" = "0")),
                      selectInput("ambi2", "Ambiance Casual:",c("Yes" = "1","No" = "0")),
                      selectInput("reservation","Restaurant Reservation:",c("Yes"="1","No"="0")),
                      selectInput("caters","Caters:",c("Yes"="1","No"="0")),
                      selectInput("outdoorseating","Outdoor Seating:",c("Yes"="1","No"="0")),
                      selectInput("delivery","Restaurant Delivery:",c("Yes"="1","No"="0")),
                      selectInput("drivethru","Drive-Thru:",c("Yes"="1","No"="0")),
                      selectInput("takeout","Restaurant TakeOut:",c("Yes"="1","No"="0")),
                      actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                      tableOutput('tabledata') # Prediction results table
                      
              ),
              
              tabItem(tabName="recommendations",
                      fluidRow(
                        box(title="Things To Avoid",textOutput("bad"),height=200),
                        box(title="Things to Include",textOutput("good"),height=200)
                      )
              ),
              tabItem(tabName="general_info")
    )
  )
  
) 


server <- function(input,output){
  output$bad<-renderText({
    if (input$type=="Italian") { 
      "Things to be careful about are the server ‘never came  table’ , and customers ‘asked speak  manager’ ."
    } else if (input$type=="American") {
      "Things to be careful about are customers said ‘took long time’, ‘worst service’,’never checked us’. "
    } else if  (input$type=="Fast Food") {
      "Things to be careful about are customers said ‘horrible customer service’ , ‘got food poisoning’ and ‘never go back’."
    } else if (input$type=="Deli") {
      "Things to be careful about where customers said ‘ terrible customer service’ ,’counter taking forever’ ,’raised price’."
    } else if  (input$type=="Bakery") {
      "Things to be careful about where customers ‘’ordered dozen but disappointed’. "
    } else if (input$type=="Mexican") {
      "Things to be careful about are ‘worst chipotle ever’  and ‘worst customer service’. "
    } else if  (input$type=="Coffee") {
      "Things to be careful about are 'bathroom disgustingly dirty’ ,‘slow lazy worker’ , ‘run like disorganized’,’busy time’  and ‘clover machine’ not working. "
    } else if (input$type=="Breakfast") {
      "Things to be careful about are ‘minus one star’ ,’food took long’  , ‘server never came’ and ‘got food poisoning’. "
    } else if  (input$type=="Japanese") {
      "Things to be careful about are ‘worst customer service’ , ‘long wait time’ , ‘read yelp reviews’, ‘. "
    }else if  (input$type=="Healthy") {
      "Things to be careful about are  ‘disappointed spicy ketchup’ , ‘quite pricey beans’, and ‘hard fried chicken’ ."
    }
    else {
      "Things to be careful about are  ‘poor customer service’ , ‘tasted like cardboard’ , and  ‘ staff could care less’. "
    }
    
    
  })
  
  output$good<-renderText({
    if (input$type=="Italian") { 
      "You should consider serving ‘authentic italian food’ such as  ‘olio e limone’ , ‘olive oil cake’ , ‘al burro e’ , ‘burro e salvia’ and ‘ravioli al burro’.  You should also have a ‘great wine list’."
    } else if (input$type=="American") {
      "You should consider serving ‘fried chicken sandwich’ , ‘mac n cheese’ ,’good beer selection’, ‘lemon poppy seed’ , ‘chicken enchilada soup’, and ‘molten chocolate cake’."
    } else if  (input$type=="Fast") {
      "You should consider serving ‘spicy chicken sandwich’ , ‘animal style fries’ , and ‘buffalo chicken fries’. You should also have ‘late night food’ and ‘drive thru line’. "
      
    } else if (input$type=="Deli") {
      "You should consider serving ‘chipotle turkey club’ , ‘crispy chicken west’, ‘gluten free bread’ , ‘grilled cheese tomato’ , and ‘ prosciutto brie sandwich’. Your ‘staff super friendly’. "
    } else if  (input$type=="Bakery") {
      "You should consider serving ‘kahlua french toast’ , ‘chocolate chip cookie’ , ‘ham cheese croissant’ , ‘fried egg sandwich’ , ‘squeezed orange juice’ , ‘pain au chocolat’ , ‘gluten free’ . Your ‘staff super friendly’. "
      
    } else if (input$type=="Mexican") {
      "You should consider serving ‘authentic mexican food’ such as ‘pico de gallo’, ‘great salsa bar’ , ‘carne asada burrito’ ‘ al pastor tacos’  , ‘tacos del mar’ , ’chicken tacos’,’agaves burrito’,  ‘marinated pork’, and ‘fish tacos’."
    } else if  (input$type=="Coffee") {
      "You should consider serving ‘tugboat eggs benedict’ , ‘fried chicken sandwich’,’gluten free bread’,and ’best damn oatmeal’ . Your ‘staff super friend’ and location should have ‘great outdoor seating’."
      
    } else if (input$type=="Breakfast") {
      "You should consider serving ‘corned beef hash’ , ‘chicken fried steak’ , ‘lemon ricotta pancakes’ ‘chocolate chip muffins’ ,’crab cake benedict’ , ‘maple bacon biscuit’, and ’potato skin hash’."
    } else if  (input$type=="Japanese") {
      "You should consider serving ‘ ramon pork belly’ , ‘spicy ramen pork belly’ ,‘ramen pork shoulder’, ‘black kuro ramen’. You should also make sure you serve ‘soft boiled eggs’. You should have ‘ worlds best service’."
      
    }else if  (input$type=="Healthy") {
      "You should consider serving ‘vegan gluten free’ , ‘artichoke crab cakes’, ‘vanilla ice cream’ ,’egg white frittata’, ‘baklava ice cream’ , ‘soy ice cream’ , ‘best acai bowls’ , and  ‘gluten free pancakes’ ."
    }
    else {
      "You should consider serving “deep dish pizza’ , ‘thin crust pizza’ , ‘gluten free pizza’ , ‘bbq chicken pizza’ , ‘wood fire pizza’ , and ‘chipotle chicken pizza’."
    }
  })
  
  
  datasetInput <- reactive({
    df <- data.frame(
      Name = c("Type",
               "RestaurantsReservations",
               "Ambience_classy",
               "Ambience_casual",
               'Caters',
               'OutdoorSeating',
               'RestaurantsDelivery',
               'DriveThru',
               'RestaurantsTakeOut'),
      Value = as.character(c(input$type,
                             input$reservation,
                             input$ambi,
                             input$ambi2,
                             input$caters,
                             input$outdoorseating,
                             input$delivery,
                             input$drivethru,
                             input$takeout)),
      stringsAsFactors = FALSE)
    
    
    print(df)
    std_stars <- "std_stars"
    df <- rbind(df, std_stars)
    input <- t(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    str(test)
    test$Type <- factor(test$Type, levels = c("Italian", "American", "Fast Food", "Deli", "Bakery", "Mexican", "Coffee", 
                                              "Breakfast", "Japanese", "Healthy", "Pizza"))
    test$RestaurantsReservations<- factor(test$RestaurantsReservations, levels = c("0","0.5","1"))
    test$Ambience_classy <- factor(test$Ambience_classy, levels = c("0","0.5","1"))
    test$Ambience_casual <- factor(test$Ambience_casual, levels = c("0","0.5","1"))
    test$Caters <- factor(test$Caters, levels = c("0","0.5","1"))
    test$RestaurantsDelivery <- factor(test$RestaurantsDelivery, levels = c("0","0.5","1"))
    test$DriveThru <- factor(test$DriveThru, levels = c("0","0.5","1"))
    test$RestaurantsTakeOut <- factor(test$RestaurantsTakeOut, levels = c("0","0.5","1"))
    test$OutdoorSeating <- factor(test$OutdoorSeating, levels = c("0","0.5","1"))
    test$std_stars <- factor(test$std_stars, levels = c("1.5","2","2.5","3","3.5","4","4.5","5"))
    str(test)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
  })
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

shinyApp(ui,server)