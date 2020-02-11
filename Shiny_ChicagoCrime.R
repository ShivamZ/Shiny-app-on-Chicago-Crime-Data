if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shiny")) install.packages("shiny")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggmap")) install.packages("ggmap")
if (!require("data.table")) install.packages("data.table")
if (!require("rjson")) install.packages("rjson")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("leaflet")) install.packages("leaflet")
if (!require("RCurl")) install.packages("RCurl")
if (!require("dplyr")) install.packages("dplyr")


library(widgetframe)
library(tidyverse)
library(shiny)
library(lubridate)
library(data.table)
library(ggmap)
library(dplyr)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(ggplot2)


crime_data <- read.csv("Chicago_Crimes_2018.csv")


#############  DATA FRAME SETUP FOR Q1   ###################
myMonth=as.POSIXlt(crime_data$Date, format="%d/%m/%Y")

crime_data$month <- format(myMonth, "%B")

crime_freq_df <- data.frame(table(crime_data$Primary.Type,crime_data$month))


#naming the columns of the Data Frame
names(crime_freq_df) <- c("Type of Crimes", "Month","Frequency")


#############  DATA FRAME SETUP FOR Q2   ###################
freqCrime_type <-data.frame(table(crime_data$Location.Description))
crime.matrix <- data.matrix(freqCrime_type)



#############  DATA FRAME SETUP FOR Q3   ###################

crime_data$Crime_Hour <- format(as.POSIXct(strptime(crime_data$Date,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
crime_type_hour.df <- data.frame(table(crime_data$Primary.Type,crime_data$Crime_Hour))
names(crime_type_hour.df) <- c("Crimes Type", "Hour of the Day","Frequency")

## generating heatmap for the given condition
heatmap_crime_hr <- crime_type_hour.df %>% ggplot(aes(x = `Crimes Type`, y = `Hour of the Day`, fill = Frequency))+geom_tile()+coord_flip()+ggtitle("Heatmap - Type of Crime Vs Hour of The Day")+theme(plot.title = element_text(size = 25, face = "bold"))


################## DATA FRAME FOR SETUP FOR Q4 ##################

crime_data$frmtDate <- format(myMonth, "%D")
crime_data2 <- crime_data

####removing the null values in latitude and longitude columns from data frame

crime_data2 <- crime_data2[!is.na(crime_data2$Latitude)& !is.na(crime_data2$Longitude),]



##defining variables for unique month 
unq_month <- unique(crime_freq_df$Month)
## not in chronological order

#defining variable for unique crime type
unq_crime <- unique(freqCrime_type$Var1)

##defining variable for unique date(formatted)
unq_date <-  unique(crime_data$frmtDate)


###############INITIALISING THE UI PART OF SHINY APP #####################

ui <- fluidPage(
  
  mainPanel(
    h1("CHICAGO CRIME RECORDS", align = "center"),
    tabsetPanel(type = "tabs",
                tabPanel(
                  "Crime Frequency per month in year 2018", 
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId="Month_Select_Crime",
                                  label="Select Month",
                                  choices = unq_month,
                                  selected=1)),
                    mainPanel(
                      tableOutput(outputId = "crime_per_month_list")  , 
                      plotOutput(outputId = "barchart_crime",height = 500)
                    )
                    )),                         ## tab panel ends
                
                tabPanel(
                  "Crime Frequency as per location", 
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId="Select_Crime",
                                  label="Select Crime",
                                  choices = unq_crime,
                                  selected=1)),
                    mainPanel(
                      tableOutput(outputId = "freq_crime_location")
                      
                    )
                  )) ,                          ## tab panel ends
                
                
               tabPanel(
                 "Heatmap depicting Type of Crime and Hour of Day", 
                    mainPanel(
                       plotOutput(outputId = "heatmap_crime_hour",height = 600,width = 900)
                    
                           )
                      ),                        ##tab panel ends
               tabPanel(
                 "Map for the crimes in Chicago",
                 sidebarPanel(selectInput(inputId ="Select_date",
                                          label="Select the Date",
                                          choices=unq_date)),
                 mainPanel(
                   leafletOutput(outputId = "worldmap_crime",height ="800px",width = "900px")
                   
                 )
               )                                 ## tab panel ends
               
               )                               
                
                
    )
  )                                           ##FLUID PAGE ENDS


server <- function(input, output) {
  
################ TAB 1 - Crime frequency per month for the year 2018 ###############
  
  crimePerMonth <- reactive({
    month_sel_crime <- input$Month_Select_Crime
    result_crime_month <- filter(crime_freq_df, crime_freq_df$Month == month_sel_crime )
    
    return(result_crime_month)
  } )
  output$crime_per_month_list <- renderTable(crimePerMonth())
  
  output$barchart_crime <- renderPlot({
    chartData <- crimePerMonth()
    plotCrime <- ggplot(data=chartData[,c(1,3)],aes(x=chartData[,1],y=chartData[,3]))+geom_bar(stat="identity", color="blue")+geom_tile()+coord_flip()+ggtitle("\nBarplot for the Type of Crime Vs Frequency")
    return(plotCrime+labs(x="Crime Types", y = "Frequencies"))
      })


############### TAB 2 #################
  crimeFreqResult <- reactive({
    sel_crime <- input$Select_Crime
    result_crime <- filter(freqCrime_type, freqCrime_type$Var1 == sel_crime )
    result_crimeFreq_loc <- na.omit(result_crime)
    return(result_crimeFreq_loc)
  } )
  output$freq_crime_location <- renderTable(crimeFreqResult())
  
  
  
  
############## TAB 3 ##################
  
output$heatmap_crime_hour <- renderPlot({heatmap_crime_hr}) 
  

############## TAB 4 ###################
   
  map_crim_date <- reactive({
    sel_date <- input$Select_date
    result_map <- filter(crime_data2, crime_data2$frmtDate == sel_date )
    
    mapCrime <-  result_map  %>% mutate(popupDets=str_c(str_c("Location Type:",result_map$Location.Description,sep=" "),
                                                         str_c("Crime Type:",result_map$Primary.Type,sep=""),
                                                         sep="<br/>")) %>%
      leaflet() %>%
      addTiles()%>%
      addMarkers(lng=crime_data2$Longitude, lat=-crime_data2$Latitude,clusterOptions = markerClusterOptions(),popup = ~popupDets)  
    
    
    return(mapCrime)
  } )
  
  output$worldmap_crime <- renderLeaflet({map_crim_date()}) 

}


shinyApp(ui = ui, server = server)
