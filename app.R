library(DT)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(readr)
library(airportr)
library(stringr)
library(zoo)



ui <- dashboardPage(
  dashboardHeader(title = "The Flight Board"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("Passengers", tabName = "page2", icon = icon("male")),
      menuItem("Carriers", tabName = "page3", icon = icon("plane")),
      menuItem("Airports", tabName = "page4", icon = icon("building")),
      menuItem("Dataset", tabName = "page5", icon = icon("table")),
      menuItem(
        "About Us",
        tabName = "page6",
        icon = icon("info-circle")
      )
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML(
        '
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #556D8F;
                              color: white;
                              font-family: helvetica;
                              }

                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #556D8F;
                              }

                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #6f90bf;
                              }

                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #556D8F;
                              font-family: helvetica;
                              }

                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f5f9ff;
                              color:black;
                              }

                              /* sidebar when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                              color:white;
                              background-color:  #6f90bf;
                              }

                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #f5f9ff;
                              backround-size: cover
                              }'
      )
    )),
    
    tabItems(
      tabItem(
        tabName = "page1",
        div(
          img(
            src = "plane.jpg",
            alt = "Plane and Sunset",
            width = '100%',
            height = '100%',
            style = "max-width:1000px"
          ),
          style = "text-align: center;"
        ),
        h1("Welcome to The Flight Board", align = "center"),
        ##Overview
        "The Flight Board has been designed to allow users to explore flight data from the perspectives of passengers, carriers, and airports. The data was sourced from Transportation.gov and contains 228,000 observations of travel between airport pairs (departure and arrival airports) between 1993 and 2021.",
        br(),
        br(),
        ##Goals
        "Our goals for this project include:",
        br(),
        br(),
        "1. To demonstrate to users that a single data set can be used for many different purposes and may be tailored to different target groups. Within The Flight Board, each of our identified stakeholder groups has different questions or problems for which they seek answers or insights.",
        br(),
        br(),
        "2. To encourage users to explore the data using their own selection of airports, carriers, airports, destinations, price ranges, and date ranges. The intent is to allow each individual to discover patterns and trends within the data that answer their own specific questions.",
        br(),
        br(),
        "3. To explore the various ways in which data may be visualized with the intent of discovering optimal visualizations choices that support users' exploration of the data."
      ),
      
      tabItem(
        tabName = "page2",
            fluidRow(
              column(12,
            
            h1("Passenger Dashboard"),
            "It has been said that the journey of a thousand miles begins with a single step. The goal of our passenger dashboard is to help you decide where that first step might take you.",
            br(),
            br(),
            "On this page, you will find visualizations designed to provide you with insight on airfare, flight destination, number of passengers, etc., that will help you plan your travel in the United States. Please feel free to explore!",
            br(),
            br(),
            box(
              title = "Explore Travel Destinations", 
              width = 12,
              "Curious about where you can fly? Designate a starting airport to begin exploring possible destinations. ",
              br(),
              br(),
              
              fluidRow(
                column(6, 
                       wellPanel(selectInput(inputId = "Departure_City", label = "Where are you flying out from?", "Cities"),
                                 style = "background: #f5f9ff")),
                column(6, 
                       wellPanel(selectInput(inputId = "Destination_City", label = "Choose the destination to see airfare trend:", "Cities"),
                                 style = "background: #f5f9ff"))
                ),
              br(),
              
              column(
                6,
                DT::dataTableOutput("myTable")
              ),
              column(
                6,
                plotOutput("ET_Plot"))
            ),
            br(),
            br(),
            box(
              title = "Explore Direct Flight Destinations",
              width = 12,
              "Don't like layovers or multiple leg flights? You're not alone! Designate a starting airport below to see all possible destinations with direct flights.",
              br(),
              br(),
              fluidRow(column(1,
                              ""),
                      column(3,
                              wellPanel(
                                selectInput(inputId = "PK_State", label = "State", "State Origin"),
                                style = "background: #ECECEC"
                              )),
                       
                       column(7,
                              wellPanel(
                                selectInput(inputId = "Port_Start", label = "Airport", "Airport Origin"),
                                style = "background: #ECECEC"
                              )),
                      column(1,
                             "")
                      ), 
              
              column(
                12,
                leafletOutput("PK_Map")
            )
            )
              )
            )
          ),
          tabItem(tabName = "page3",
                  fluidRow(
                    column(
                      12,
                      h1("Carrier Dashboard"),
                      "The airline industry is a competitive environment, especially in those large metropolitan areas where there are multiple airports customers can choose from. Our carrier dashboard allows you to explore which carriers fly in each airport and the fares offered by the various carriers at the multiple airports within the same city market.",
                      br(),
                      br(),
                      "You can also examine the market share over specific city pairs over the years, seeing the difference in fares, and deducing passenger retention from the number of passengers.",
                      br(),
                      br(),
                      box(
                        title = "City Market Dashboard.",
                        width = 12,
                        "For a given departure city market, identify associated airports you can depart from, and fares trends over the years. 
                        Designate a starting city market below to see all possible city market destinations with direct flights.
                        You have to choose destination airport that you'd like to explore for your desired destination city.
                        Some of the Departure City examples are New York, Los Angeles, and San Francisco. Beware that some destination cities aren't as busy as other major cities"),
                        br(),
                        br(),
                      fluidRow(
                        column(
                          6,
                          wellPanel(selectInput(inputId="DepartureCityMarket", label = h5("Select Departure City"),"Cities"),
                                    style="background:#f5f9ff")
                          ),
                          column(
                          6, 
                          wellPanel(selectInput(inputId="ArrivalCity", label = h5("Select Destination City"),"Cities"),
                                    style="background:#f5f9ff"
                          )
                        ),
                        column(
                        9, 
                        wellPanel(selectInput(inputId="ArrivalAirport", label = h5("Select Destination Airport"),"Airports"),
                                  style="background:#f5f9ff"
                          )
                        ),
                        column(
                          12,
                          plotOutput("ts_plot1")
                        ),
                        column(
                          12,
                          plotOutput("ts_plot2")
                          ),
                      )
                      ),
                        br(),
                        br(),
                      box(
                        title = "Carrier Fare Differences",
                        width = 12,
                        "For a given city market in the last 5 years, which largest market share carrier is cheaper from the different airports within that city market.",
                        br(),
                        br(),
                        fluidRow(
                          column(
                          12,
                          sliderInput("carrier_year", "Year:", min = 2016, max = 2021, value = 2016, sep="",
                                    step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                        plotOutput("ts_plot3")
                        )
                        )
                      ),
                      br(),
                      br(),
                      box(
                        title = "Average Daily No. of Passengers Comparison",
                        width = 12,
                        "Compare the trend of the average daily no. of passengers between different depature airports in the same city market",
                        br(),
                        br(),
                        fluidRow(
                          column(
                            12,
                            plotOutput("ts_plot4")
                          )
                      )
                        )
                  )
                      ),
                      
          tabItem(
            tabName = "page4",
            fluidRow(
              column(
                12,
                h1("Airport Dashboard"),
                "Airports are designed to optimize both safety and effective operations. If you were designing an airport, understanding carrier flight volumes and passenger counts would allow you to optimize terminal and gate assignments, staffing requirements, and placement of amenities (e.g., restaurants, restrooms, transportation services). The Airport Dashboard visualizations allow you to view passenger and carrier volumes for insight.",
                br(),
                br(),
                box(
                  title = "Airport's Current Passenger Trends",
                  width = 12,
                  "Are you curious about how your airport's passenger loads have changed over time? Select your airport to see its passenger load trend. You will also see largest carriers at your airport based on the last year's volumes.",
                  br(),
                  br(),
                  fluidRow(
                    column(
                      6,
                      wellPanel(selectInput(inputId="TADepAirport1", label = h5("Select Airport"),"Airports"),
                            style="background:#f5f9ff"),
                    ),
                    column(
                      6
                      )
                  ),
                  fluidRow(
                    column(
                      6,
                      plotOutput("TA_Plot1")
                    ),
                    column(
                      6,
                      plotOutput("TA_Plot2")
                    )
                  )
                )
              )
            )
          ),
          tabItem(
            tabName = "page5",
            fluidRow(
              column(12,
            h1("Dataset Information"),
            "Our team developed ‘The Flight Board’ using a dataset from ",
            a("Transportation.gov", href = "https://data.transportation.gov/Aviation/Consumer-Airfare-Report-Table-1a-All-U-S-Airport-P/tfrh-tu9e", target =
                "_blank"),
            ". The full dataset contains 228,000 observations of airport pair markets (departure airport – arrival airport) between 1993 and 2021. A one-year sample of the data is available below. For the complete dataset, please visit ", a("Transportation.gov", href = "https://data.transportation.gov/Aviation/Consumer-Airfare-Report-Table-1a-All-U-S-Airport-P/tfrh-tu9e", target =
                "_blank"),
          
            br(),
            br(),
            dataTableOutput("dataTable")
              )
            )
            ),
          tabItem(
            tabName = "page6",
            fluidRow(
              column(12,
            h1("About Us"),
            br(),
            br(),
            box(
              title = "Team 101",
              width = 12,
              fluidRow(
                column(
                  3,
                  img(src = "Eunice.jpg", 
                      alt = "Eunice Tseng", 
                      width ='100%',
                      style = "max-width:125px"),
                  align = "center",
                  h4("Eunice Tseng", align = "center"),
                  h6("ctseng14@jhu.edu", align = "center")
                ),
                column(
                  3,
                  img(src = "Patrick.jpg", 
                      alt = "Patrick King", 
                      width ='100%',
                      style = "max-width:125px"),
                  align = "center",
                  h4("Patrick King", align = "center"),
                  h6("pking19@jhu.edu", align = "center")
                ),
                column(
                  3,
                  img(src = "Timothy.jpg", 
                      alt = "Timothy Sham", 
                      width ='100%',
                      style = "max-width:125px"),
                  align = "center",
                  h4("Timothy Sham", align = "center"),
                  h6("lsham1@jhu.edu", align = "center")
                ),
                column(
                  3,
                  img(src = "Todd 2.jpeg", 
                      alt = "Todd Andrew", 
                      width ='100%',
                      style = "max-width:125px"),
                  align = "center",
                  h4("Todd Andrew", align = "center"),
                  h6("tandre15@jhu.edu", align = "center")
                )
              )
            )
            )
            ),
            box(title = "Acknowledgements", width = 12,
                fluidRow(
                  column(
                    12,
                    "This project was undertaken in the Data Visualization course in the Johns Hopkins Carey Business School's Master of Business Administration (MBA) program. Please ",
                    a("click here", href = "https://carey.jhu.edu/programs/mba-programs", target =
                        "_blank"),
                    " for more information about Johns Hopkins MBR program.",
                    br(),
                    br(),
                    "The team members would like to thank ",
                    a(
                      "Dr. Mohammed Ali Alamdar Yazdi",
                      href = "https://carey.jhu.edu/faculty/faculty-directory/mohammad-ali-alamdar-yazdi-phd",
                      target = "_blank"
                    ),
                    " for his teaching and guidance throughout this course. He has significantly increased our skills in data cleaning, transformation, manipuation, and visualization methods using the R programming language. We look forward to continuing to build these skills throughout our careers.",
                    br(),
                    br(),
                    "Thank you, Dr. Yazdi, for your willingness to share your time and expertise with your students. It is very much appreciated!",
                  )
                )),
          )
        )
      )
    )
    
    
    
    server <- function(input, output, session) {

      #Passenger Tab 
      ETdata = read_csv("Airfare_Report.csv")
      ETlocations = read_csv("GeoLocation.csv")
      
      #selective input
      observe({
        updateSelectInput(session, "Departure_City", choices = ETlocations$City)
      })
      
      observe({
        updateSelectInput(session, "Destination_City", choices = ET_total_fare_avg()$Destination)
      })
      
      #select departure city
      ET_departure1 <-  reactive({
        req(input$Departure_City)
        ETdata %>%
          filter(city1 == input$Departure_City) %>%
          rename(Departure = city1) %>%
          rename(Destination = city2)
      })
      
      ET_departure2 <-reactive({
        req(input$Departure_City)
        ETdata %>%
          filter(city2 == input$Departure_City) %>%
          rename(Departure = city2) %>%
          rename(Destination = city1) 
      })
      
      
      ET_departure <- reactive({
        rbind(ET_departure1(), ET_departure2()) 
      })
      
      #return list of overall avg airfare
      ET_total_fare_avg <- reactive({
        ET_departure() %>%
          group_by(Destination) %>%
          summarise(Average_Fare = round(mean(fare), digits =2))
      })
      
      output$myTable <- DT::renderDataTable({
        DT::datatable(ET_total_fare_avg())        
      })
      
      #categorize by quarter
      ET_Q1 <- reactive({
        ET_departure() %>%
          filter(quarter == "1") %>%
          group_by(Destination) %>%
          summarise(Q1_Average_Fare = mean(fare))
      })
      
      ET_Q2 <- reactive({
        req(input$Destination_City)
        ET_departure() %>%
          filter(quarter == "2") %>%
          group_by(Destination) %>%
          summarise(Q2_Average_Fare = mean(fare))
      })
      
      ET_Q3 <- reactive({
        req(input$Destination_City)
        ET_departure() %>%
          filter(quarter == "3")%>%
          group_by(Destination) %>%
          summarise(Q3_Average_Fare = mean(fare))
      })
      
      ET_Q4 <- reactive({
        req(input$Destination_City)
        ET_departure() %>%
          filter(quarter == "4")%>%
          group_by(Destination) %>%
          summarise(Q4_Average_Fare= mean(fare))
      })
      
      ET_First_half <- reactive({
        full_join(ET_Q1(),ET_Q2(), by="Destination")
      })
      ET_Last_half <- reactive({
        full_join(ET_Q3(),ET_Q4(), by="Destination")
      })
      ET_Season_fare <- reactive({
        full_join(ET_First_half(),ET_Last_half(),by="Destination")
      })
      
      
      SelectET <- reactive({
        req(input$Destination_City)
        ET_Season_fare() %>%
          filter(Destination == input$Destination_City)
      })
      
      
      FareET <- reactive({
        c(SelectET()$Q1_Average_Fare,
          SelectET()$Q2_Average_Fare,
          SelectET()$Q3_Average_Fare,
          SelectET()$Q4_Average_Fare)
      })
      
      Quarter <- c('1', '2','3','4')
      
      ET_df <- reactive({
        data.frame(x=Quarter,y=FareET()) %>%
          mutate(FareET = replace_na(FareET(), mean(FareET(), na.rm = TRUE))) %>%
          rename(AvgQFare = FareET)
      })
      
      output$ET_Plot = renderPlot({
        ggplot(ET_df(), aes(x=Quarter, y=AvgQFare, fill=AvgQFare)) +
          geom_bar(stat = "identity") +
          coord_cartesian(ylim = c(min(FareET())-50, max(FareET())+10)) +
          scale_fill_gradient(low = "#88D0C7", high = "#D08891", aesthetics = "fill")+
          labs(title=~bold("Average Fare Trend"), subtitle= paste(input$Departure_City, 'to',SelectET()$Destination), y = "Airfare", fill='Average Fare', caption = "*some routes do not seasonal change in airfare*")+
          theme_classic()+
          theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
      })
      
#Passenger Tab
  #MAP
      
      PK_Port4 <- read_csv("Port1.csv")
      PK_Connect4 <- read_csv("Connections.csv")
      
      #UI Input Variables
      
      Port_State <- PK_Port4$state %>%
        unique() %>%
        sort()
      
      observe({
        updateSelectInput(session, "PK_State", choices = Port_State, selected = "New Mexico")
      })
      
      PK_State2Port <- reactive({
        PK_Port4 %>%
          filter(state==input$PK_State)
      })
      
      Port_Select <- reactive({
        PK_State2Port()$name %>%
        unique() %>%
        sort()
      })
      
      observe({
        updateSelectInput(session, "Port_Start", choices = Port_Select(), selected = "Albuquerque International Sunport")
      })
      
      #UI Inputs for Leaflet Map
      PK_PortName <- reactive({
        PK_Port4 %>%
        filter(name==input$Port_Start)
      })
      
      PK_Leave <- reactive({
        PK_Connect4 %>%
          filter(depart==PK_PortName()$port[1]) %>%
          select(lat2,lat1,lon2,lon1, everything())
      })
      
      
      
      #Consolidated Coordinates for addPolylines
      
      #Latitude Vector
      PK_lat <- reactive({
        PK_Leave() %>%
        pivot_longer(lat2:lat1, names_to = "type", values_to = "values") %>%
        pull(values)
      })
      
      #Longitude Vector
      PK_lon <- reactive({
        PK_Leave() %>%
        pivot_longer(lon2:lon1, names_to = "type", values_to = "values") %>%
        pull(values)
      })
      
      #Leaflet Map
      output$PK_Map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
          addCircleMarkers(
            lat = PK_lat(),
            lng = PK_lon(),
            radius = 3,
            weight = 4,
            color = "#FF3399",
            opacity = 100
          )%>%
        addCircleMarkers(
          lat = PK_Port4$lat,
          lng = PK_Port4$lon,
          label = paste(PK_Port4$name, " (",PK_Port4$port,")"),
          radius = 3,
          weight = 2,
          color = "#C0C0C0",
          opacity = 50
        ) %>%
        addPolylines(
          lat = PK_lat(),
          lng = PK_lon(),
          color = "#606060",
          weight = .4,
          opacity = 100
        ) %>%
        addMarkers(
          lat = PK_Leave()$lat1[1],
          lng = PK_Leave()$lon1[1],
          label = input$Port_Start
        )
      })
      
      #Carrier Tab
      
      #Import Data
      tsdata = read_csv("ts_multiairport.csv")
      
      #Comparison of the fare trend of largest market share carriers over the years 
      # between airports in the same city market
      
      ts_dep_city <- tsdata$Departure_City_Market %>%
        unique() %>%
        sort()
      
      observe({
        updateSelectInput(session,"DepartureCityMarket", choices = ts_dep_city, selected = "Chicago, IL")
      })
      
      ts_citymarket <- reactive ({
        tsdata %>%
          filter(input$DepartureCityMarket == Departure_City_Market)
      })
      
      ts_destination <- reactive ({
        ts_citymarket()$Arrival_City_Market %>%
          unique() %>%
          sort()
      })
      
      observe({
        updateSelectInput(session,"ArrivalCity", choices = ts_destination())
      })

      observeEvent(input$ArrivalCity, {
        updateSelectInput(session,"ArrivalAirport", 
                          choices = tsdata$Arrival_Airport_Name[tsdata$Departure_City_Market == input$DepartureCityMarket
                                                                 & tsdata$Arrival_City_Market == input$ArrivalCity])
      })
      
      #ts_plot1
      
      #Comparison of the fare trend of largest market share carriers over the years 
      # between airports in the same city market
      
      output$ts_plot1 <- renderPlot({
        tsdata %>% 
          filter(Departure_City_Market == input$DepartureCityMarket  & 
                   Arrival_Airport_Name == input$ArrivalAirport) %>%
          ggplot( aes(group=Departure_Airport_Code, color=Departure_Airport_Code, y=Largest_Carriers_Fare, x=Year)) +
          geom_point() +
          geom_smooth(se = FALSE) +
          scale_y_continuous(expand = expansion(c(0, 0.1))) +
          labs(title = "Fare Trend Comparison of Largest Market Carriers",
               subtitle = "Between Airports In The Same City Market",
               y="Average Fare (US$)", x="Year")+
          theme(plot.title = element_text(face="bold"))
      })
      
      #ts_plot2
      
      #Comparison of the fare trend of lowest fare carriers over the years 
      # between airports in the same city market
      
      output$ts_plot2 <- renderPlot({tsdata %>% 
          filter(Departure_City_Market == input$DepartureCityMarket  & 
                   Arrival_Airport_Name == input$ArrivalAirport) %>%
          ggplot( aes(group=Departure_Airport_Code, color=Departure_Airport_Code, y=Low_Fare_Carriers_Fare, x=Year)) +
          geom_point() +
          geom_smooth(se = FALSE) +
          scale_y_continuous(expand = expansion(c(0, 0.1))) +
          labs(title = "Fare Trend Comparison of Lowest Fare Carriers", subtitle = "Between Airports In The Same City Market",
               y="Average Fare (US$)", x="Year")+
          theme(plot.title = element_text(face="bold"))
          })
      
      #ts_plot3
      
      #Comparison of the fare of largest market share carrier between airports in the same city market
      
      output$ts_plot3 <- renderPlot({tsdata %>% 
        filter(Departure_City_Market == input$DepartureCityMarket  & 
                 Arrival_Airport_Name == input$ArrivalAirport &
                Year == input$carrier_year) %>%
        ggplot( aes(fill=Largest_Carrier, y=Largest_Carriers_Fare, x=Departure_Airport_Code)) +
        geom_bar(position="dodge", stat="identity", width = 0.5) +
          scale_fill_brewer(palette = "Set1") +
          ylim(0, 600) +
          labs(title = "Comparison of Yearly Average Fare In The Same City", subtitle = "Between Different Carriers from Airports In The Same City Market",
               y="Average Fare (US$)", x="Year")+
          theme(plot.title = element_text(face="bold"))
        
      })
      
      #ts_plot4
      
      #Comparison of the trend of number of passengers over the years 
      # between airports in the same city market
      
      output$ts_plot4 <- renderPlot({
        tsdata %>% 
          filter(Departure_City_Market == input$DepartureCityMarket  & 
                   Arrival_Airport_Name == input$ArrivalAirport) %>%
          ggplot( aes(group=Departure_Airport_Code, color=Departure_Airport_Code, y=Number_of_Passengers, x=Year)) +
          geom_point() +
          geom_smooth(se = FALSE) +
          scale_y_continuous(expand = expansion(c(0, 0.1))) +
          xlim(2000, 2023) +
          labs(title = "No. of Passengers Trend Comparison",
               subtitle = "Between Airports In The Same City Market",
               y="Average No. of Passengers per day", x="Year")+
          theme(plot.title = element_text(face="bold"))
      })
      
      
      #Airport Tab
      #Data Prep

      TA_data = read_csv("tladata5.csv")
      
      TA_dep_airports<-read_csv("tladata5.csv", col_select = c(3))
      TA_dep_airports<-distinct(data.frame(TA_dep_airports))
      TA_dep_airports<-TA_dep_airports %>% arrange(Departure.Airport.Name)
      
      TA_airport<-reactive({
        TA_data %>% 
        filter(input$TADepAirport1 ==`Departure Airport Name`) %>%
        group_by(Year) %>% 
        summarize('Total Passengers'=sum(`Number of Passengers`))
      })
   
     TA_carriers<-reactive({
       TA_data %>% 
        filter("2020"==Year & input$TADepAirport1 ==`Departure Airport Name`) %>% 
        group_by(`Largest Carrier`) %>% 
        summarize(Total_Passengers=sum(`Number of Passengers`)) %>% 
        arrange(desc(Total_Passengers)) %>% 
        head(n=5)
     })
    
      
     observe({
       updateSelectInput(session,"TADepAirport1", choices = TA_dep_airports$Departure.Airport.Name, selected = "Albuquerque International Sunport")
     })
     
     
      #Build TA_Plot1
      output$TA_Plot1<-renderPlot({
        TA_airport() %>% 
          ggplot(
            mapping=aes(
              x=Year, 
              y=`Total Passengers`
            )
          )+
          geom_point()+
          geom_smooth(color='#ffbd4a', se=FALSE)+
          xlim(1993,2020)+
          theme_classic()+
          labs(
            title = "Your Airport's Passenger Count Trend Line",
            subtitle = 'For the years 1993 to 2020',
            x = '',
            y = 'Passengers'
          )
      })
      
      #Build TA_Plot2
      output$TA_Plot2<-renderPlot({
        TA_carriers() %>%
          ggplot(
            mapping=aes(
              x=reorder(`Largest Carrier`,Total_Passengers),
              y=Total_Passengers
            )
          )+
          geom_bar(
            stat="identity",
            fill='#556D8F',
            width=0.5
          )+
          theme_classic()+
          labs(
            title = "Your Airport's Current Top Carriers by Passenger Volume",
            subtitle = 'Displaying 2020, the most recent complete year',
            x = '',
            y = 'Passengers'
          )+
          coord_flip()

      })

  
      #DataTable Tab
      output$dataTable <- renderDataTable({
        return(datatable(TA_data %>% 
                           filter(Year==2020), rownames = FALSE))
      })
  }
      
    
     
    
    shinyApp(ui = ui, server = server)
    