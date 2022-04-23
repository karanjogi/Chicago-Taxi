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

#Create a dataframe to store daily rides for each community
daily_to_community_data <- taxi %>%
                            group_by(Dropoff_Community_Area, date(Trip_Start_Time)) %>%
                            summarise(n = n())

#Create a dataframe to store daily rides for each community
daily_from_community_data <- taxi %>%
  group_by(Pickup_Community_Area, date(Trip_Start_Time)) %>%
  summarise(n = n())

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
taxi["Trip_Kilometers"] <- taxi["Trip_Miles"] * 1.609
milesbins <- taxi  %>% mutate(bins = cut(Trip_Miles, breaks=8))
kmbins <- taxi %>% mutate(bins = cut(Trip_Kilometers, breaks=8))
binned_km <- count(kmbins, bins)
binned_miles <- count(milesbins, bins)
names(binned_km) <- c("Binned_Mileage","Rides")
names(binned_miles) <- c("Binned_Mileage","Rides")


#Create a dataframe to store binned trip time data
taxi["Trip_Minutes"] <- taxi["Trip_Seconds"]/60
timebins <- taxi  %>% mutate(bins = cut(Trip_Minutes, breaks=12))
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
        fluidRow(column(2,
                        selectInput(inputId="community",label="Choose a Community Area",areas$community_name,selected="All"),
                        radioButtons("heatmaptype","Heatmap",choices = c("To","From"),selected="To",
                                     inline=TRUE),
                        radioButtons("viztype", "View", choices = c("Table","Graph"), selected="Graph",
                                     inline=TRUE),
                        radioButtons("hour", "Time", choices = c("12hour", "24hour"), selected="12hour",
                                     inline=TRUE),
                        radioButtons("distance", "Distance", choices = c("Km", "Miles"), selected="Miles",
                                     inline=TRUE)),
                 
                 
                 column(4,
                        conditionalPanel(
                          condition ="input.viztype =='Table'",
                          dataTableOutput("dailytable")
                        ),
                        conditionalPanel(
                          condition ="input.viztype == 'Graph'",
                          plotOutput("plotdaily")
                        )
                 ),
                 
                 column(3,
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
                 
                 column(3,
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
                        plotOutput("communitybar"))
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
  
  observeEvent(input$distance,
               if(input$distance == "Miles"){
                 output$plotmiles <- renderPlot(
                   ggplot(binned_miles, aes(x=Binned_Mileage, y=Rides)) + 
                     geom_bar(stat="identity", fill="steelblue") +
                     theme(axis.text.x = element_text(angle = 90))
                 )
                 
                 output$milestable <- DT::renderDataTable({
                   binned_miles}, rownames=FALSE, options=list(pageLength=7))
               }
               else{
                 output$plotmiles <- renderPlot(
                   ggplot(binned_km, aes(x=Binned_Mileage, y=Rides)) + 
                     geom_bar(stat="identity", fill="steelblue") +
                     theme(axis.text.x = element_text(angle = 90))
                 )
                 
                 output$milestable <- DT::renderDataTable({
                   binned_km}, rownames=FALSE, options=list(pageLength=7))
               })
  
  
  output$plottime <- renderPlot(
    ggplot(binned_time, aes(x=Binned_Trip_Time, y=Rides)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$timetable <- DT::renderDataTable({
    binned_time}, rownames=FALSE, options=list(pageLength=7))
  
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
                 taxi_community <- subset(taxi,taxi["Dropoff_Community_Area"]==area_community)
                 length <- nrow(taxi_community)
                 taxi_community <- count(taxi_community,Pickup_Community_Area)
                 taxi_community["n"] <- (taxi_community["n"]/length)*100
                 names(taxi_community) <- c("area_numbe","Rides")
                 neighborhoods_raw <- merge(neighborhoods_raw,taxi_community,by="area_numbe",all.x=TRUE)
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
