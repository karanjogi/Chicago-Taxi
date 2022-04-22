<<<<<<< HEAD
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(stringr)
# library(shinyWidgets)
# library(gridExtra)
# library(scales)
library(DT)

taxi <- read_delim("taxi.tsv",delim="\t",show_col_types = FALSE)

names(taxi) <- c("Trip_Start_Time","Trip_Seconds","Trip_Miles","Pickup_Community_Area",
                 "Dropoff_Community_Area","Company")

neighborhoods_geojson <- "Boundaries - Community Areas (current).geojson"

neighborhoods_raw <- sf::read_sf(neighborhoods_geojson)

neighborhoods_df <- data_frame(neighborhoods_raw)
areas <- data_frame(neighborhoods_df$area_numbe, tolower(neighborhoods_df$community))

names(areas) <- c("area_no", "community_name")
areas$area_no <- sapply(areas$area_no, as.integer)

areas <- rbind(areas, list(100, "outside community"))
areas <- rbind(areas, list(0, "All"))

#Create a dataframe to store daily data
daily_data<-count(taxi,date(Trip_Start_Time))
names(daily_data) <- c("Date","Rides") 

#Create a dataframe to store hourly data
hourly_data <-  count(taxi,format(taxi$Trip_Start_Time,"%X"),format(taxi$Trip_Start_Time,"%H"))
names(hourly_data) <- c("Hour","Hour1","Rides")
hourly_data <- arrange(hourly_data, Hour1)

#Create a dataframe to store weekday data
weekday_data <- count(taxi, wday(Trip_Start_Time, label=TRUE))
names(weekday_data) <- c("Weekday", "Rides")

#Create a dataframe to store monthly data
monthly_data <- count(taxi , month(Trip_Start_Time, label=TRUE))
names(monthly_data) <- c("Month","Rides")

#Create a dataframe to store binned miles data
milesbins <- taxi  %>% mutate(bins = cut(Trip_Miles, breaks=10))
binned_miles <- count(milesbins, bins)
names(binned_miles) <- c("Binned_Mileage","Rides")

#Create a dataframe to store binned trip time data
timebins <- taxi  %>% mutate(bins = cut(Trip_Seconds, breaks=10))
binned_time <- count(timebins, bins)
names(binned_time) <- c("Binned_Trip_Time", "Rides")



ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("About Page", tabName = "widgets"),
      menuItem("Main Page", tabName = "dashboard",selected=TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="widgets",
        h2("About Page"),
        p("The app is written as part of course Project")),
      tabItem(
        tabName="dashboard",
        fluidRow(column(3,
                        selectInput(inputId="community",label="Choose a Community Area",areas$community_name,selected="All"),
                        radioButtons("viztype", "View", choices = c("Table","Graph"), selected="Graph",
                                     inline=TRUE),
                        radioButtons("hour", "Time", choices = c("12hour", "24hour"), selected="12hour",
                                     inline=TRUE),
                        background="blue"),
                 
                 
                 column(5,
                        conditionalPanel(
                          condition ="input.viztype =='Table'",
                          dataTableOutput("dailytable")
                        ),
                        conditionalPanel(
                          condition ="input.viztype == 'Graph'",
                          plotOutput("plotdaily")
                        )
                 ),
                 
                 column(2,
                        conditionalPanel(
                          condition = "input.hour == '12hour' ",
                          conditionalPanel(
                            condition ="input.viztype =='Table'",
                            dataTableOutput("hourtable12")
                          ),
                          conditionalPanel(
                            condition ="input.viztype =='Graph'",
                            plotOutput("plothour12")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.hour == '24hour' ",
                          conditionalPanel(
                            condition ="input.viztype =='Table'",
                            dataTableOutput("hourtable24")
                          ),
                          conditionalPanel(
                            condition ="input.viztype =='Graph'",
                            plotOutput("plothour24")
                          )
                        )
                 ),
                 
                 column(2,
                        conditionalPanel(
                          condition ="input.viztype =='Table'",
                          dataTableOutput("monthtable")
                        ),
                        conditionalPanel(
                          condition ="input.viztype =='Graph'",
                          plotOutput("plotmonth")
                        )
                 )
        ),
        
        fluidRow(column(3,
                        conditionalPanel(
                          condition ="input.viztype =='Table'",
                          dataTableOutput("weektable")
                        ),
                        conditionalPanel(
                          condition ="input.viztype =='Graph'",
                          plotOutput("plotweek")
                        )
        ),
        
        column(3,
               conditionalPanel(
                 condition ="input.viztype =='Table'",
                 dataTableOutput("milestable")
               ),
               conditionalPanel(
                 condition ="input.viztype =='Graph'",
                 plotOutput("plotmiles")
               )
        ),
        
        column(3,
               conditionalPanel(
                 condition ="input.viztype =='Table'",
                 dataTableOutput("timetable")
               ),
               conditionalPanel(
                 condition ="input.viztype =='Graph'",
                 plotOutput("plottime")
               )
        ),
        
        column(3,
               leafletOutput("community_areas"))),
        fluidRow(column(6,
                        plotOutput("communitybar")
        )
        )
      )
    )
  )
)


server <- function(input, output){
  
  output$plotdaily <- renderPlot(
    ggplot(daily_data, aes(x=Date, y=Rides)) + geom_bar(stat="identity", fill="steelblue")
  )
  
  output$dailytable <- DT::renderDataTable({
    daily_data }, rownames=FALSE, options=list(pageLength=7))
  
  output$plothour12 <- renderPlot(
    hourly_data  %>% arrange(Hour1)  %>% 
      mutate(Hour=factor(Hour, levels=Hour))  %>% 
      ggplot(aes(x=Hour, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +  
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$plothour24 <- renderPlot(
    ggplot(hourly_data, aes(x=Hour1, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$hourtable12 <- DT::renderDataTable({
    select(hourly_data, Hour, Rides)}, rownames=FALSE, options=list(pageLength=7))
  
  output$hourtable24 <- DT::renderDataTable({
    select(hourly_data, Hour1, Rides)}, rownames=FALSE, options=list(pageLength=7))
  
  output$plotweek <- renderPlot(
    ggplot(weekday_data, aes(x=Weekday, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$weektable <- DT::renderDataTable({
    weekday_data}, rownames=FALSE, options=list(pageLength=7))
  
  output$plotmonth <- renderPlot(
    ggplot(monthly_data, aes(x=Month, y=Rides)) + geom_bar(stat="identity", fill="steelblue")
  )
  
  output$monthtable <- DT::renderDataTable({
    monthly_data}, rownames=FALSE, options=list(pageLength=7))
  
  output$plotmiles <- renderPlot(
    ggplot(binned_miles, aes(x=Binned_Mileage, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$milestable <- DT::renderDataTable({
    binned_miles}, rownames=FALSE, options=list(pageLength=7))
  
  output$plottime <- renderPlot(
    ggplot(binned_time, aes(x=Binned_Trip_Time, y=Rides)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$timetable <- DT::renderDataTable({
    binned_time}, rownames=FALSE, options=list(pageLength=7))
  
  observeEvent(input$community,
               if(input$community!="All"){
                 area_community <- subset(areas,community_name==input$community)[[1]]
                 taxi_community <- subset(taxi,taxi["Dropoff_Community_Area"]==area_community)
                 length <- nrow(taxi_community)
                 taxi_community <- count(taxi_community,Pickup_Community_Area)
                 taxi_community["n"] <- (taxi_community["n"]/length)*100
                 names(taxi_community) <- c("area_numbe","Rides")
                 neighborhoods_raw <- merge(neighborhoods_raw,taxi_community,by="area_numbe")
                 View(neighborhoods_raw)
                 pal <- colorQuantile("YlOrRd", domain = neighborhoods_raw$Rides,6)
                 output$community_areas <- renderLeaflet({
                   leaflet::leaflet() %>%
                     addTiles()  %>%
                     leaflet::addProviderTiles(providers$CartoDB.Positron)  %>%
                     leaflet::addPolygons(data = neighborhoods_raw,color = "#444444", weight = 1, smoothFactor = 0.5,
                                          opacity = 1.0, fillOpacity = 0.5,fillColor = ~pal(Rides))  %>%
                     leaflet::addLegend(data = neighborhoods_raw,pal = pal, values = ~Rides, opacity = 0.7, title = NULL,
                                        position = "bottomright")
                 })
                 output$communitybar <- renderPlot({
                   ggplot(taxi_community,aes(x=area_numbe,y=Rides))+ 
                     geom_bar(stat="identity",fill="steelblue")
                 })
               })
}


shinyApp(ui = ui, server = server)
=======
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(stringr)
# library(shinyWidgets)
# library(gridExtra)
# library(scales)
library(DT)

taxi <- read_delim("taxi.tsv",delim="\t",show_col_types = FALSE)

names(taxi) <- c("Trip_Start_Time","Trip_Seconds","Trip_Miles","Pickup_Community_Area",
                 "Dropoff_Community_Area","Company")

neighborhoods_geojson <- "Boundaries - Community Areas (current).geojson"

neighborhoods_raw <- sf::read_sf(neighborhoods_geojson)

neighborhoods_df <- data_frame(neighborhoods_raw)
areas <- data_frame(neighborhoods_df$area_numbe, tolower(neighborhoods_df$community))

names(areas) <- c("area_no", "community_name")
areas$area_no <- sapply(areas$area_no, as.integer)

areas <- rbind(areas, list(100, "outside community"))
areas <- rbind(areas, list(0, "All"))



ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 3"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("", tabName = "cheapBlankSpace"),
    menuItem("About Page", tabName = "widgets"),
    menuItem("Main Page", tabName = "dashboard",selected=TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="widgets",
        h2("About Page"),
        p("The app is written as part of course Project")),
      tabItem(
        tabName="dashboard",
        fluidRow(column(2,
                        selectInput(inputId="community",label="Choose a Community Area",areas$community_name,selected="All"),
                        radioButtons("heatmaptype","Heatmap",choices = c("To","From"),selected="To",
                                     inline=TRUE)),
          column(4,
                        conditionalPanel(
                          condition ="input.dailytype =='Table'",
                          dataTableOutput("dailytable")
                        ),
                        conditionalPanel(
                          condition ="input.dailytype=='Graph'",
                          plotOutput("plotdaily")
                        ),
                        radioButtons("dailytype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE)),
                 column(3,
                        conditionalPanel(
                          condition = "input.hour == '12hour' ",
                          conditionalPanel(
                            condition ="input.hourtype =='Table'",
                            dataTableOutput("hourtable12")
                          ),
                          conditionalPanel(
                            condition ="input.hourtype =='Graph'",
                            plotOutput("plothour12")
                          )),
                        conditionalPanel(
                          condition = "input.hour == '24hour' ",
                          conditionalPanel(
                            condition ="input.hourtype =='Table'",
                            dataTableOutput("hourtable24")
                          ),
                          conditionalPanel(
                            condition ="input.hourtype =='Graph'",
                            plotOutput("plothour24")
                          )),
                        radioButtons("hourtype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE),
                        radioButtons("hour","Time",choices = c("12hour","24hour"),selected="12hour",
                                     inline=TRUE)
                 ),
                 column(3,
                        conditionalPanel(
                          condition ="input.monthtype =='Table'",
                          dataTableOutput("monthtable")
                        ),
                        conditionalPanel(
                          condition ="input.monthtype =='Graph'",
                          plotOutput("plotmonth")
                        ),
                        radioButtons("monthtype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE))),
        fluidRow(column(3,
                        conditionalPanel(
                          condition ="input.weektype =='Table'",
                          dataTableOutput("weektable")
                        ),
                        conditionalPanel(
                          condition ="input.weektype =='Graph'",
                          plotOutput("plotweek")
                        ),
                        radioButtons("weektype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE)),
                 column(3,
                        conditionalPanel(
                          condition ="input.milestype =='Table'",
                          dataTableOutput("milestable")
                        ),
                        conditionalPanel(
                          condition ="input.milestype =='Graph'",
                          plotOutput("plotmiles")
                        ),
                        radioButtons("milestype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE)),
                 column(3,
                        conditionalPanel(
                          condition ="input.timetype =='Table'",
                          dataTableOutput("timetable")
                        ),
                        conditionalPanel(
                          condition ="input.timetype =='Graph'",
                          plotOutput("plottime")
                        ),
                        radioButtons("timetype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE)),
                 column(3,
                        leafletOutput("community_areas"))),
        fluidRow(column(6,
                        plotOutput("communitybar")))
            )
          )
        )
      )


server <- function(input, output){
  
  dailydata <- reactive({
    data<-count(taxi,date(Trip_Start_Time))
    names(data) <- c("Date","Rides")
    data
    })
  
  output$plotdaily <- renderPlot(
    ggplot(dailydata(),aes(x=Date,y=Rides)) + geom_bar(stat="identity",fill="steelblue")
  )
  
  output$dailytable <- DT::renderDataTable({
    dailydata()},rownames=FALSE,options=list(pageLength=7))
  
  hourdata <- reactive({
    data <-  count(taxi,format(taxi$Trip_Start_Time,"%X"),format(taxi$Trip_Start_Time,"%H"))
    names(data) <- c("Hour","Hour1","Rides")
    data <- arrange(data,Hour1)
    data
    })
  
  output$plothour12 <- renderPlot(
    hourdata()  %>% arrange(Hour1)  %>% 
      mutate(Hour=factor(Hour, levels=Hour))  %>% 
      ggplot(aes(x=Hour,y=Rides)) + geom_bar(stat="identity",fill="steelblue") +  
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$plothour24 <- renderPlot(
    ggplot(hourdata(),aes(x=Hour1,y=Rides)) + geom_bar(stat="identity",fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$hourtable12 <- DT::renderDataTable({
    select(hourdata(),Hour,Rides)},rownames=FALSE,options=list(pageLength=7))
  
  output$hourtable24 <- DT::renderDataTable({
    select(hourdata(),Hour1,Rides)},rownames=FALSE,options=list(pageLength=7))
  
  
  weekdata <- reactive({
    data<-count(taxi,wday(Trip_Start_Time,label=TRUE))
    names(data) <- c("Weekday","Rides")
    data
    })
  
  output$plotweek <- renderPlot(
    ggplot(weekdata(),aes(x=Weekday,y=Rides)) + geom_bar(stat="identity",fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$weektable <- DT::renderDataTable({
    weekdata()},rownames=FALSE,options=list(pageLength=7))
  
  monthdata <- reactive({
    data<-count(taxi,month(Trip_Start_Time,label=TRUE))
    names(data) <- c("Month","Rides")
    data
    })
  
  output$plotmonth <- renderPlot(
    ggplot(monthdata(),aes(x=Month,y=Rides)) + geom_bar(stat="identity",fill="steelblue")
  )
  
  output$monthtable <- DT::renderDataTable({
    monthdata()},rownames=FALSE,options=list(pageLength=7))
  
  binmiles <- reactive({
    milesbins <- taxi  %>% mutate(bins = cut(Trip_Miles,breaks=10))
    binmiles <- count(milesbins,bins)
    names(binmiles) <- c("Binned_Mileage","Rides")
    binmiles
  })
  
  output$plotmiles <- renderPlot(
    ggplot(binmiles(),aes(x=Binned_Mileage,y=Rides)) + geom_bar(stat="identity",fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$milestable <- DT::renderDataTable({
    binmiles()},rownames=FALSE,options=list(pageLength=7))
  
  bintime <- reactive({
    timebins <- taxi  %>% mutate(bins = cut(Trip_Seconds,breaks=10))
    bintime <- count(timebins,bins)
    names(bintime) <- c("Binned_Trip_Time","Rides")
    bintime
  })
  
  output$plottime <- renderPlot(
    ggplot(bintime(),aes(x=Binned_Trip_Time,y=Rides)) + geom_bar(stat="identity",fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$timetable <- DT::renderDataTable({
    bintime()},rownames=FALSE,options=list(pageLength=7))
  
  observeEvent({
    input$community
    input$heatmaptype
    },
               if(input$community!="All"){
                 area_community <- subset(areas,community_name==input$community)[[1]]
                 if(input$heatmaptype == "To"){
                   taxi_community <- subset(taxi,taxi["Dropoff_Community_Area"]==area_community)
                   length <- nrow(taxi_community)
                   taxi_community <- count(taxi_community,Pickup_Community_Area)
                 }
                 else{
                   taxi_community <- subset(taxi,taxi["Pickup_Community_Area"]==area_community)
                   length <- nrow(taxi_community)
                   taxi_community <- count(taxi_community,Dropoff_Community_Area)
                 }
                 taxi_community["n"] <- (taxi_community["n"]/length)*100
                 names(taxi_community) <- c("area_numbe","Rides")
                 neighborhoods_raw <- merge(neighborhoods_raw,taxi_community,by="area_numbe")
                 View(neighborhoods_raw)
                 pal <- colorBin("YlOrRd", domain = neighborhoods_raw$Rides,6)
                 output$community_areas <- renderLeaflet({
                   leaflet::leaflet() %>%
                     addTiles()  %>%
                     leaflet::addProviderTiles(providers$CartoDB.Positron)  %>%
                     leaflet::addPolygons(data = neighborhoods_raw,color = "#444444", weight = 1, smoothFactor = 0.5,
                                          opacity = 1.0, fillOpacity = 0.5,fillColor = ~pal(Rides))  %>%
                     leaflet::addLegend(data = neighborhoods_raw,pal = pal, values = ~Rides, opacity = 0.7, title = NULL,
                                        position = "bottomright")
                 })
                 output$communitybar <- renderPlot({
                   ggplot(taxi_community,aes(x=area_numbe,y=Rides))+ 
                     geom_bar(stat="identity",fill="steelblue")
                 })
               })
  }


shinyApp(ui = ui, server = server)
>>>>>>> 10042ced1d55964e9297e03c91515d9c59805778
