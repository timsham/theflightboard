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

data <-
  read_csv("Consumer_Airfare_Report__Table_1a_-_All_U.S._Airport_Pair_Markets.csv")

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
                                 style = "background: #f9f3e8")),
                column(6, 
                       wellPanel(selectInput(inputId = "Destination_City", label = "Choose the destination to see airfare trend:", "Cities"),
                                 style = "background: #f9f3e8"))
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
              "Don't like layovers or multiple leg flights? You're not alone! Designate a starting airport below to see all possible destinations with direct flights. You can also choose additional options including your preferred carrier and the time of year at which you'd like to travel.",
              br(),
              br(),
              fluidRow(column(
                2,
                h4("Select Options"),
                img(
                  src = "widgetplaceholder.png",
                  alt = "Widget Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "mapplaceholder.png",
                  alt = "Map Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "mapplaceholder.png",
                  alt = "Map Placeholder",
                  width = '100%',
                  height = '100%'
                )
              )
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
                      "You can also examine the market share over specific city pairs over the years, seeing the difference in fares, and deducing passenger retention from the number of passengers. You can explore the myth that suggests that carriers' charges are correlated to the distance traveled and determine if the fares are instead tied to supply and demand.",
                      br(),
                      br(),
                      box(
                        title = "City Market Dashboard.",
                        width = 12,
                        "For a given city market, identify associated airports, carriers, and fares by time of year. Designate a starting city market below to see all possible city market destinations with direct flights. You can also choose additional options including the year and quarter of the data that you'd like to explore",
                        br(),
                        br(),
                        fluidRow(column(
                          2,
                          h4("Select Options"),
                          img(
                            src = "widgetplaceholder.png",
                            alt = "Widget Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "barchartplaceholder.png",
                            alt = "Bar Chart Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "imageplaceholder.png",
                            alt = "Table Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ))
                      ),
                      br(),
                      br(),
                      box(
                        title = "City Market Pair Dashboard",
                        width = 12,
                        "For a given city market pair, explore if there is a correlation between the distance of the city pairs and average fare.",
                        br(),
                        br(),
                        fluidRow(column(
                          2,
                          h4("Select Options"),
                          img(
                            src = "widgetplaceholder.png",
                            alt = "Widget Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "imageplaceholder.png",
                            alt = "Image Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "imageplaceholder.png",
                            alt = "Image Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ))
                      ),
                      br(),
                      br(),
                      box(
                        title = "Airport Pair Dashboard",
                        width = 12,
                        "For a given airport pair, explore the passenger totals per carrier, how these have changed over time, and how the airfare rates have changed over time.",
                        br(),
                        br(),
                        fluidRow(column(
                          2,
                          h4("Select Options"),
                          img(
                            src = "widgetplaceholder.png",
                            alt = "Widget Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "mapplaceholder.png",
                            alt = "Map Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        ),
                        column(
                          5,
                          img(
                            src = "mapplaceholder.png",
                            alt = "Map Placeholder",
                            width = '100%',
                            height = '100%'
                          )
                        )),
                        fluidRow(column(2),
                                 column(
                                   5,
                                   img(
                                     src = "imageplaceholder.png",
                                     alt = "Table Placeholder",
                                     width = '100%',
                                     height = '100%'
                                   )
                                 ),
                                 column(5))
                      )
                    )
                  )),
          tabItem(
            tabName = "page4",
            fluidRow(
              column(12,
            h1("Airport Dashboard"),
            "Airports are designed to optimize both safety and effective operations. If you were designing an airport, understanding carrier flight volumes and passenger counts would allow you to optimize terminal and gate assignments, staffing requirements, and placement of amenities (e.g., restaurants, restrooms, transportation services). The Airport Dashboard visualizations allow you to explore flight and passenger volumes for insight.",
            br(),
            br(),
            "As you design your airport, you may also want to identify other airports that are similar to yours in terms of carriers served, flight volumes, and passenger counts. These elements may be helpful if you're looking to compare services, contracts, or performance with airports of similar service loads.",
            br(),
            br(),
            box(
              title = "Airport Expected Passenger Loads.",
              width = 12,
              "Are you curious about what passenger load your airport should expect? Designate a starting airport to begin expected passenger loads. You can also choose additional options including carriers and specified date range.",
              br(),
              br(),
              fluidRow(column(
                2,
                h4("Select Options"),
                img(
                  src = "widgetplaceholder.png",
                  alt = "Widget Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                10,
                img(
                  src = "plotplaceholder.png",
                  alt = "Plot Placeholder",
                  width = '500px',
                  height = '200px'
                )
              ))
            ),
            box(
              title = "Airport Expected Passenger Loads Changes Over Time.",
              width = 12,
              "Are you curious about how the passenger loads have changed over time? Designate a starting airport to explore how the passenger loads have evolved. You can also choose additional options including carriers and specified date range.",
              br(),
              br(),
              fluidRow(column(
                2,
                h4("Select Options"),
                img(
                  src = "widgetplaceholder.png",
                  alt = "Widget Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                10,
                img(
                  src = "plotplaceholder.png",
                  alt = "Plot Placeholder",
                  width = '500px',
                  height = '200px'
                )
              ))
            ),
            box(
              title = "Top 10 Airports by Flight and Passenger Volumes",
              width = 12,
              "Let's investigate how your airport's flight and passenger volumes compare to the top 10 airports in the United States. Designate a starting airport to begin comparing flight and passenger volumes. You can also specified a specific date range that you'd like to focus on.",
              br(),
              br(),
              fluidRow(column(
                2,
                h4("Select Options"),
                img(
                  src = "widgetplaceholder.png",
                  alt = "Widget Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "imageplaceholder.png",
                  alt = "Image Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "imageplaceholder.png",
                  alt = "Image Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ))
            ),
            box(
              title = "Airports grouped by Flight Volume and Passenger Count",
              width = 12,
              "Are you curious about which other aiports are most similar to yours based on flight and passenger volumes? Designate a starting airport and a time range to see how your airport compares to others.",
              br(),
              br(),
              fluidRow(column(
                2,
                h4("Select Options"),
                img(
                  src = "widgetplaceholder.png",
                  alt = "Widget Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "imageplaceholder.png",
                  alt = "Image Placeholder",
                  width = '100%',
                  height = '100%'
                )
              ),
              column(
                5,
                img(
                  src = "imageplaceholder.png",
                  alt = "Image Placeholder",
                  width = '100%',
                  height = '100%'
                )
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
            ". The dataset contains 228,000 observations of airport pair markets (departure airport – arrival airport) between 1993 and 2021. The data elements in the dataset are listed at the bottom of this page.",
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
      output$dataTable <- renderDataTable({
        return(datatable(data, rownames = FALSE))
      })
      
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
           
    }
    
    shinyApp(ui = ui, server = server)
    