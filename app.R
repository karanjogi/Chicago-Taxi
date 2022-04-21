library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(stringr)
library(shinyWidgets)
library(gridExtra)
library(scales)
library(DT)

taxi <- read_delim("taxi_reduced.tsv",delim="\t",show_col_types = FALSE)
names(taxi) <- c("Trip_Start_Time","Trip_Seconds","Trip_Miles","Pickup_Community_Area",
                 "Dropoff_Community_Area","Company")
taxi$Trip_Seconds <- sapply(taxi$Trip_Seconds, as.integer)
taxi$Pickup_Community_Area <- sapply(taxi$Pickup_Community_Area, as.integer)
taxi$Dropoff_Community_Area <- sapply(taxi$Dropoff_Community_Area, as.integer)


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
        fluidRow(column(12,
                        conditionalPanel(
                          condition ="input.dailytype =='Table'",
                          dataTableOutput("dailytable")
                        ),
                        conditionalPanel(
                          condition ="input.dailytype=='Graph'",
                          plotOutput("plotdaily")
                        ),
                        radioButtons("dailytype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE),background="blue")),
        fluidRow(column(4,
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
                 column(4,
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
                 column(4,
                        conditionalPanel(
                          condition ="input.monthtype =='Table'",
                          dataTableOutput("monthtable")
                        ),
                        conditionalPanel(
                          condition ="input.monthtype =='Graph'",
                          plotOutput("plotmonth")
                        ),
                        radioButtons("monthtype","View",choices = c("Table","Graph"),selected="Graph",
                                     inline=TRUE)))
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
      theme(axis.text.x = element_text(angle = 45))
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
  
  }


shinyApp(ui = ui, server = server)
