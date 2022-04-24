library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(stringr)
library(DT)

taxi <- read_delim("taxi.tsv",delim="\t",show_col_types = FALSE)

names(taxi) <- c("Trip_Start_Time","Trip_Seconds","Trip_Miles","Pickup_Community_Area",
                 "Dropoff_Community_Area","Company")

neighborhoods_geojson <- "Boundaries - Community Areas (current).geojson"

neighborhoods_raw <- sf::read_sf(neighborhoods_geojson)

neighborhoods_df <- tibble(neighborhoods_raw)
areas <- tibble(neighborhoods_df$area_numbe, tolower(neighborhoods_df$community))

names(areas) <- c("area_no", "community_name")
areas$area_no <- sapply(areas$area_no, as.integer)

areas <- rbind(areas, list(0, "All"))
areas_w_outside <- rbind(areas, list(100, "outside community"))

#Create a dataframe to store daily rides for each community
daily_to_community_data <- taxi %>%
  group_by(Dropoff_Community_Area, Trip_Start_Time) %>%
  summarise(n = n())

names(daily_to_community_data) <- c("Dropoff_Community_Area","Trip_Start_Time","n")

#Create a dataframe to store daily rides for each community
daily_from_community_data <- taxi %>%
  group_by(Pickup_Community_Area, Trip_Start_Time) %>%
  summarise(n = n())

names(daily_from_community_data) <- c("Pickup_Community_Area","Trip_Start_Time","n")


#Create a dataframe to store binned miles data
taxi["Trip_Kilometers"] <- taxi["Trip_Miles"] * 1.609

taxi["Trip_Minutes"] <- taxi["Trip_Seconds"]/60

taxi["Trip_Kilometers"] <- taxi["Trip_Miles"] * 1.609

milesbins <- taxi  %>% mutate(bins = cut(Trip_Miles, breaks=8))
kmbins <- taxi %>% mutate(bins = cut(Trip_Kilometers, breaks=8))
# binned_km <- count(kmbins, bins)
# binned_miles <- count(milesbins, bins)
# names(binned_km) <- c("Binned_Mileage","Rides")
# names(binned_miles) <- c("Binned_Mileage","Rides")


# Create a dataframe to store binned trip time data

taxi["Trip_Minutes"] <- taxi["Trip_Seconds"]/60

timebins <- taxi  %>% mutate(bins = cut(Trip_Minutes, breaks=12))
# binned_time <- count(timebins, bins)
# names(binned_time) <- c("Binned_Trip_Time", "Rides")

taxi_companies <- unique(taxi$Company)
taxi_companies <- append(taxi_companies,"All")



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
      menuItem("Main Page", tabName = "dashboard",selected=TRUE),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem(strong("Input for the dashboard"), tabName = "cheapBlankSpace"),
      selectInput(inputId="community",label="Choose a Community Area",areas$community_name, selected="All"),
      selectInput(inputId="taxi",label="Choose a taxi company",taxi_companies,selected="All"),
      radioButtons("outside", "Outside of Chicago", choices = c("Enable","Disable"), selected="Disable",
                   inline=TRUE),
      radioButtons("heatmaptype","Heatmap",choices = c("To","From"),selected="To",
                   inline=TRUE),
      radioButtons("viztype", "View", choices = c("Table","Graph"), selected="Graph",
                   inline=TRUE),
      radioButtons("hour", "Time", choices = c("12hour", "24hour"), selected="12hour",
                   inline=TRUE),
      radioButtons("distance", "Distance", choices = c("Km", "Miles"), selected="Miles",
                   inline=TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="widgets",
        h2("About Page"),
        p("The app is written by Abhijeet and Karan Jogi as part of course Project"),
        p("The data is collected from https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy
          and the for mapping data is collected from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6"),
        p("The graphs are made using ggplot2 package and for map leaflet.js for R is used and the for data 
          manipulation base R is used.")),
      tabItem(
        tabName="dashboard",
        fluidRow(
          column(6,
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
        br(),
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
        
        column(2,
               conditionalPanel(
                 condition ="input.viztype =='Table'",
                 dataTableOutput("milestable")
               ),
               conditionalPanel(
                 condition ="input.viztype =='Graph'",
                 plotOutput("plotmiles")
               )
        ),
        
        column(2,
               conditionalPanel(
                 condition ="input.viztype =='Table'",
                 dataTableOutput("timetable")
               ),
               conditionalPanel(
                 condition ="input.viztype =='Graph'",
                 plotOutput("plottime")
               )
        ),
        
        column(5,
               leafletOutput("community_areas"))),
        br(),
        fluidRow(column(6,
                        plotOutput("communitybar"))
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  observe({
    if(input$outside == 'Enable') {
      updateSelectInput(session, "community",
                        choices = areas_w_outside,
                        selected = "All"
      )
    }
    else {
      updateSelectInput(session, "community",
                        choices = areas,
                        selected = "All"
      )
    }
  })
  
  area_community <- eventReactive({
    input$community
    input$outside
  },if(input$community!="All"){
    area_community <- subset(areas,community_name==input$community)[[1]]
    area_community
  }
  )
  
  taxi_outside_filter <- eventReactive({
    input$outside
    input$community
  },
  if(input$outside == 'Enable'){
    taxi
  }
  else{
    taxi %>% filter(Pickup_Community_Area != 100 & Dropoff_Community_Area != 100)
  }
  )
  
  taxi_filter_company <- eventReactive({
    input$taxi
    input$outside
  },
  if(input$taxi != 'All'){
    taxi_outside_filter() %>% filter(Company == input$taxi)
  }
  else{
    taxi_outside_filter()
  }
  )
  
  taxi_community <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },if(input$community!="All"){
    if(input$heatmaptype == "To"){
      taxi_filter_company() %>% filter(Dropoff_Community_Area == area_community())
    }
    else{
      taxi_filter_company() %>% filter(Pickup_Community_Area == area_community())
    }
  }
  else{
    taxi_filter_company()
  }
  )
  
  daily_community <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },
  if(input$community!="All"){
    if(input$heatmaptype=="To"){
      daily_community <- taxi_community() %>%
        group_by(Dropoff_Community_Area, Trip_Start_Time) %>%
        summarise(n = n())
      names(daily_community) <- c("Dropoff_Community_Area","Trip_Start_Time","n")
      daily_community
    }
    else{
      daily_community <- taxi_community() %>%
        group_by(Pickup_Community_Area, Trip_Start_Time) %>%
        summarise(n = n())
      names(daily_community) <- c("Pickup_Community_Area","Trip_Start_Time","n")
      daily_community
    }
  }
  else {
    daily_community <- taxi_community() %>%
      group_by(Pickup_Community_Area, Trip_Start_Time) %>%
      summarise(n = n())
    names(daily_community) <- c("Pickup_Community_Area","Trip_Start_Time","n")
    daily_community
  }
  )
  
  daily_data <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  }, 
  {
    d_data <- daily_community() %>% 
      group_by(date(Trip_Start_Time)) %>%
      summarise(n = sum(n)) 
    names(d_data) <- c("Date","Rides")
    d_data
  })
  
  output$plotdaily <- renderPlot(
    ggplot(daily_data(), aes(x=Date, y=Rides)) + geom_bar(stat="identity", fill="steelblue")
  )
  
  output$dailytable <- DT::renderDataTable({
    daily_data() }, rownames=FALSE, options=list(pageLength=7))
  
  hourly_data <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  }, 
  {
    h_data<- daily_community() %>% 
      group_by(format(Trip_Start_Time,"%X"),format(Trip_Start_Time,"%H")) %>% 
      summarise(n = sum(n))
    names(h_data) <- c("Hour","Hour1","Rides")
    h_data <- arrange(h_data, Hour1)
    h_data
  })
  
  output$plothour12 <- renderPlot({
    hourly_data() %>% arrange(Hour1) %>% 
      mutate(Hour=factor(Hour, levels=c("12:00:00 AM","1:00:00 AM","2:00:00 AM","3:00:00 AM",
                                               "4:00:00 AM","5:00:00 AM","6:00:00 AM","7:00:00 AM",
                                               "8:00:00 AM","9:00:00 AM","10:00:00 AM","11:00:00 AM",
                                               "12:00:00 PM","1:00:00 PM","2:00:00 PM","3:00:00 PM",
                                               "4:00:00 PM","5:00:00 PM","6:00:00 PM","7:00:00 PM",
                                               "8:00:00 PM","9:00:00 PM","10:00:00 PM","11:00:00 PM"))) %>% 
      ggplot(aes(x=Hour, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +  
      theme(axis.text.x = element_text(angle = 90))
  }
  )
  
  output$plothour24 <- renderPlot(
    ggplot(hourly_data(), aes(x=Hour1, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$hourtable12 <- DT::renderDataTable({
    select(hourly_data(),Hour, Rides)}, rownames=FALSE, options=list(pageLength=7))
  
  output$hourtable24 <- DT::renderDataTable({
    select(hourly_data(), Hour1, Rides)}, rownames=FALSE, options=list(pageLength=7))
  
  weekday_data <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  }, 
  {
    w_data <- daily_community() %>%  group_by(wday(Trip_Start_Time,label=TRUE)) %>%
      summarise(n = sum(n)) 
    names(w_data) <- c("Weekday","Rides")
    w_data
  })
  
  output$plotweek <- renderPlot(
    ggplot(weekday_data(), aes(x=Weekday, y=Rides)) + 
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$weektable <- DT::renderDataTable({
    weekday_data()}, rownames=FALSE, options=list(pageLength=7))
  
  monthly_data <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  }, 
  {
    m_data <- daily_community() %>% 
      group_by(month(Trip_Start_Time,label=TRUE)) %>%
      summarise(n = sum(n)) 
    names(m_data) <- c("Month","Rides")
    m_data
  })
  
  output$plotmonth <- renderPlot(
    ggplot(monthly_data(), aes(x=Month, y=Rides)) + geom_bar(stat="identity", fill="steelblue")
  )
  
  output$monthtable <- DT::renderDataTable({
    monthly_data()}, rownames=FALSE, options=list(pageLength=7))
  
  binned_miles_r <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },
  {milesbins_c <- taxi_community() %>% mutate(bins = cut(Trip_Miles, breaks=8))
  binned_miles_c <- count(milesbins_c, bins)
  names(binned_miles_c) <- c("Binned_Mileage","Rides")
  binned_miles_c})
  
  binned_km_r <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },{
    kmbins_c <- taxi_community() %>% mutate(bins = cut(Trip_Miles*1.609, breaks=8))
    binned_km_c <- count(kmbins_c, bins)
    names(binned_km_c) <- c("Binned_Mileage","Rides")
    binned_km_c
  })
  
  
  observeEvent(input$distance,
               if(input$distance == "Miles"){
                 output$plotmiles <- renderPlot(
                   ggplot(binned_miles_r(), aes(x=Binned_Mileage, y=Rides)) +
                     geom_bar(stat="identity", fill="steelblue") +
                     theme(axis.text.x = element_text(angle = 90))
                 )
                 
                 output$milestable <- DT::renderDataTable({
                   binned_miles_r()}, rownames=FALSE, options=list(pageLength=7))
               }
               else{
                 output$plotmiles <- renderPlot(
                   ggplot(binned_km_r(), aes(x=Binned_Mileage, y=Rides)) +
                     geom_bar(stat="identity", fill="steelblue") +
                     theme(axis.text.x = element_text(angle = 90))
                 )
                 
                 output$milestable <- DT::renderDataTable({
                   binned_km_r()}, rownames=FALSE, options=list(pageLength=7))
               })
  
  binned_time <- eventReactive({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },
  {
    binned_time_c <- taxi_community() %>% mutate(bins = cut(Trip_Minutes, breaks=8))
    binned_miles_c <- count(binned_time_c, bins)
    names(binned_miles_c) <- c("Binned_Trip_Time","Rides")
    binned_miles_c
  })
  
  output$plottime <- renderPlot(
    ggplot(binned_time(), aes(x=Binned_Trip_Time, y=Rides)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  output$timetable <- DT::renderDataTable({
    binned_time()}, rownames=FALSE, options=list(pageLength=7))
  
  observeEvent({
    input$community
    input$heatmaptype
    input$taxi
    input$outside
  },
  if(input$community!="All"){
    if(input$heatmaptype == "To"){
      if(input$taxi=="All"){
        length <- nrow(taxi_community())
        taxi_community <- count(taxi_community() ,Pickup_Community_Area)
      }
      else{
        length <- nrow(taxi_community())
        taxi_community <- count(taxi_community(), Pickup_Community_Area)
      }
    }
    else{
      if(input$taxi=="All"){
        length <- nrow(taxi_community())
        taxi_community <- count(taxi_community(),Dropoff_Community_Area)
      }
      else{
        length <- nrow(taxi_community())
        taxi_community <- count(taxi_community(), Dropoff_Community_Area)
      }
    }
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
                           position = "bottomright") %>% 
        leaflet::addRectangles(
          lng1=-87.542883, lat1=41.892358,
          lng2=-87.490020, lat2=41.806340,
          fillColor = "transparent"
        )
    })
    output$communitybar <- renderPlot({
      taxi_community <- merge(areas,taxi_community,by.x="area_no",by.y="area_numbe",all.y=TRUE)
      taxi_community[is.na(taxi_community)] <- "z Outside"
      ggplot(taxi_community,aes(x=community_name,y=Rides))+ 
        geom_bar(stat="identity",fill="steelblue") +
        theme(axis.text.x = element_text(angle = 90))
    })
  }
  else if(input$taxi!="All"){
    taxi_community <- taxi_community()
    if(input$heatmaptype == "To"){
      length <- nrow(taxi_community)
      taxi_community <- count(taxi_community,Pickup_Community_Area)
    }
    else{
      length <- nrow(taxi_community)
      taxi_community <- count(taxi_community,Dropoff_Community_Area)
    }
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
                           position = "bottomright") %>% 
        leaflet::addRectangles(lng1=-87.542883, lat1=41.892358,
                               lng2=-87.490020, lat2=41.806340,
                               fillColor = "transparent"
        )
    })
    output$communitybar <- renderPlot({
      taxi_community <- merge(areas,taxi_community,by.x="area_no",by.y="area_numbe",all.y=TRUE)
      taxi_community[is.na(taxi_community)] <- "z Outside"
      ggplot(taxi_community,aes(x=community_name,y=Rides))+ 
        geom_bar(stat="identity",fill="steelblue")  +
        theme(axis.text.x = element_text(angle = 90))
    })
  })
  
}


shinyApp(ui = ui, server = server)