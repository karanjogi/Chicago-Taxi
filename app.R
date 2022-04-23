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

areas <- rbind(areas, list(100, "outside community"))
areas <- rbind(areas, list(0, "All"))

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
        br(),
        fluidRow(column(6,
                        plotOutput("communitybar"))
        )
      )
    )
  )
)


server <- function(input, output){
  
  area_community <- eventReactive({
    input$community
  },if(input$community!="All"){
    area_community <- subset(areas,community_name==input$community)[[1]]
    area_community
    }
  )
  
  taxi_community <- eventReactive({
    input$community
    input$heatmaptype
  },if(input$community!="All"){
    if(input$heatmaptype=="To"){
      taxi_community <- subset(taxi,Dropoff_Community_Area==area_community())
      taxi_community
    }
    else{
      taxi_community <- subset(taxi,Pickup_Community_Area==area_community())
      taxi_community
    }
  })
  
  daily_community <- eventReactive({
    input$community
    input$heatmaptype
  },
  if(input$community!="All"){
    if(input$heatmaptype=="To"){
      daily_community <- daily_to_community_data %>% subset(Dropoff_Community_Area==area_community())
      daily_community
    }
    else{
      daily_community <- daily_from_community_data %>% subset(Pickup_Community_Area==area_community())
      daily_community
    }
    })
  
  daily_data <- eventReactive({
    input$community
    input$heatmaptype}, 
    if(input$community == "All"){
      d_data<- daily_to_community_data %>%
        group_by(date(Trip_Start_Time)) %>%
        summarise(n = sum(n))
      names(d_data) <- c("Date","Rides")
      d_data
    }
    else{
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
    input$heatmaptype}, 
    if(input$community == "All"){
      h_data<- daily_from_community_data  %>% 
        group_by(format(Trip_Start_Time,"%X"),format(Trip_Start_Time,"%H")) %>% 
        summarise(n = sum(n))
      names(h_data) <- c("Hour","Hour1","Rides")
      h_data <- arrange(h_data, Hour1)
      h_data
    }
    else{
        h_data<- daily_community() %>% 
          group_by(format(Trip_Start_Time,"%X"),format(Trip_Start_Time,"%H")) %>% 
          summarise(n = sum(n))
        names(h_data) <- c("Hour","Hour1","Rides")
        h_data <- arrange(h_data, Hour1)
        h_data
      })
  
  
  output$plothour12 <- renderPlot({
    hourly_data() %>% arrange(Hour1) %>% 
      mutate(Hour=factor(Hour, levels=Hour)) %>% 
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
    input$heatmaptype}, 
    if(input$community == "All"){
      w_data<- daily_to_community_data %>% group_by(wday(Trip_Start_Time,label=TRUE)) %>%
        summarise(n = sum(n))
      names(w_data) <- c("Weekday","Rides")
      w_data
    }
    else{
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
    input$heatmaptype}, 
    if(input$community == "All"){
      m_data<- daily_to_community_data %>%
        group_by(month(Trip_Start_Time,label=TRUE)) %>%
        summarise(n = sum(n))
      names(m_data) <- c("Month","Rides")
      m_data
    }
    else{
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
  },if(input$community == "All"){
    binned_miles <- count(milesbins, bins)
    names(binned_miles) <- c("Binned_Mileage","Rides")
    binned_miles
  }
  else{
    milesbins_c <- taxi_community() %>% mutate(bins = cut(Trip_Miles, breaks=8))
    binned_miles_c <- count(milesbins_c, bins)
    names(binned_miles_c) <- c("Binned_Mileage","Rides")
    binned_miles_c
  })
  
  binned_km_r <- eventReactive({
    input$community
    input$heatmaptype
  },if(input$community == "All"){
    binned_km <- count(kmbins, bins)
    names(binned_km) <- c("Binned_Mileage","Rides")
    binned_km
  }else{
    kmbins_c <- taxi_community() %>% mutate(bins = cut(Trip_Kilometers, breaks=8))
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
  },if(input$community == "All"){
    binned_time <- count(timebins, bins)
    names(binned_time) <- c("Binned_Trip_Time", "Rides")
    binned_time
  }
  else{
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
  },
  if(input$community!="All"){
    if(input$heatmaptype == "To"){
      if(input$taxi=="All"){
        taxi_community <- subset(taxi,taxi["Dropoff_Community_Area"]==area_community())
        length <- nrow(taxi_community)
        taxi_community <- count(taxi_community,Pickup_Community_Area)
      }
      else{
        taxi_community <- subset(taxi,taxi["Dropoff_Community_Area"]==area_community() & 
                                   taxi["Company"] == input$taxi)
        length <- nrow(taxi_community)
        taxi_community <- count(taxi_community,Pickup_Community_Area)
      }
    }
    else{
      if(input$taxi=="All"){
      taxi_community <- subset(taxi,taxi["Pickup_Community_Area"]==area_community())
      length <- nrow(taxi_community)
      taxi_community <- count(taxi_community,Dropoff_Community_Area)
      }
      else{
        taxi_community <- subset(taxi,taxi["Pickup_Community_Area"]==area_community() &
                                   taxi["Company"] == input$taxi)
        length <- nrow(taxi_community)
        taxi_community <- count(taxi_community,Dropoff_Community_Area)
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
                           position = "bottomright")
    })
    output$communitybar <- renderPlot({
      # taxi_community <- merge(areas,taxi_community,by.x="area_no",by.y="Pickup_Community_Area")
      ggplot(taxi_community,aes(x=area_numbe,y=Rides))+ 
        geom_bar(stat="identity",fill="steelblue")
    })
  }
  else if(input$taxi!="All"){
    taxi_community <- subset(taxi,taxi["Company"]==input$taxi)
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
                           position = "bottomright")
    })
    output$communitybar <- renderPlot({
      # taxi_community <- merge(areas,taxi_community,by.x="area_no",by.y="Pickup_Community_Area")
      ggplot(taxi_community,aes(x=area_numbe,y=Rides))+ 
        geom_bar(stat="identity",fill="steelblue")
    })
  })
  
}


shinyApp(ui = ui, server = server)
