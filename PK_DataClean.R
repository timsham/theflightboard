library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(dbplyr)
library(dplyr)
library(DT)
library(plotly)
library(airportr)

Test <- read_csv("Consumer_Airfare_Report.csv")
Geo <- read_csv("airports.csv")
State <-read_csv("us-airports.csv")
Geo=Geo %>%
  rename(port=faa)

#Truncating Dataset
Test=Test %>%
  select(-one_of('tbl', 'citymarketid_1', 'citymarketid_2', 'airportid_1', 'airportid_2','tbl1apk')) %>%
  select(-(nsmiles:fare)) %>%
  select(-(large_ms:fare_low))

#Extracting Citymarket Location
Port1=Test %>%
  filter(!grepl(pattern = "^[A-Z]",x = Geocoded_City1),
         !is.na(Geocoded_City1)) %>%
  separate(Geocoded_City1, into = c("Lat1", "Lon1"), sep = ", ") %>%
  mutate(Lat1 = gsub(pattern = "\\(", replacement = "", x = Lat1),
         Lon1 = gsub(pattern = "\\)", replacement = "", x = Lon1),
         Lat1 = as.numeric(Lat1),
         Lon1 = as.numeric(Lon1)) %>%
  select(airport_1, Lat1, Lon1) %>%
  distinct() %>%
  rename(port=airport_1)

Port2=Test %>%
  filter(!grepl(pattern = "^[A-Z]",x = Geocoded_City2),
         !is.na(Geocoded_City2)) %>%
  separate(Geocoded_City2, into = c("Lat1", "Lon1"), sep = ", ") %>%
  mutate(Lat1 = gsub(pattern = "\\(", replacement = "", x = Lat1),
         Lon1 = gsub(pattern = "\\)", replacement = "", x = Lon1),
         Lat1 = as.numeric(Lat1),
         Lon1 = as.numeric(Lon1)) %>%
  select(airport_2, Lat1, Lon1) %>%
  distinct() %>%
  rename(port=airport_2)

Port3=Port1 %>%
  rbind(Port2)

Missed_Ports = data_frame(port=c("ACV","AGS","BFI","BMI","BTR","CHA","CHO","DAB","EVV","GFK","GPT","HDN","HHH","HTS","HVN","IAG","JAN","LAN","LBE",
                                 "LNK","MBS","MLB","MOB","MRY","MTJ","ORH","ROA","SFB","SHV","SPI","STC","TOL","TVC","USA"),
                          name=c("Arcata Airport","Augusta Regional Airport","King County International Airport","Central Illinois Regional Airport",
                                 "Baton Rouge Metropolitan Airport","Lovell Field","Charlottesville–Albemarle Airport",
                                 "Daytona Beach International Airport","Evansville Regional Airport","Grand Forks International Airport",
                                 "Gulfport–Biloxi International Airport","Yampa Valley Airport ","Hilton Head Airport","Tri-State Airport","Tweed-New Haven Airport",
                                 "Niagara Falls International Airport","Jackson–Medgar Wiley Evers International Airport","Capital Region International Airport",
                                 "Arnold Palmer Regional Airport","Lincoln Airport ","MBS International Airport","Melbourne Orlando International Airport",
                                 "Mobile Regional Airport","Monterey Regional Airport ","Montrose Regional Airport","Worcester Regional Airport",
                                 "Roanoke–Blacksburg Regional Airport","Orlando Sanford International Airport","Shreveport Regional Airport",
                                 "Abraham Lincoln Capital Airport","St. Cloud Regional Airport","Eugene F. Kranz Toledo Express Airport",
                                 "Cherry Capital Airport","Concord-Padgett Regional Airport"),
                          lat=c(40.977778,33.37,47.53,40.477222,30.532778,35.035278,38.138611,29.184722,38.038333,47.949167,30.407222,40.481111,
                                32.224444,38.366944,41.263889,43.107222,32.311111,42.778639,40.274722,40.851111,43.532778,
                                28.102778,30.691389,36.586944,38.509794,42.267222,37.325556,28.777778,32.446667,39.844167,45.546667,41.586806,
                                44.741667,35.387778),
                          lon=c(-124.108333,-81.964444,-122.301944,-88.915833,-91.15,-85.203889,-78.452778,-81.060556,
                                -87.530833,-97.176111,-89.07,-107.217778,-80.6975,-82.558611,-72.886667,-78.946111,-90.075833,-84.586194,-79.406667,
                                -96.759167,-84.079722,-80.645278,-88.242778,-121.843056,-107.894242,-71.875833,-79.975556,-81.2375,-93.825556,
                                -89.678056,-94.06,-83.807833,-85.582222,-80.709167)
)
head(State)

State1 = State %>%
  select(iata_code,region_name) %>%
  rename(port=iata_code,state=region_name)

Port4=Port3 %>%
  left_join(Geo, by="port") %>%
  select(-(alt:dst)) %>%
  select(-(Lat1:Lon1)) %>%
  rbind(Missed_Ports) %>%
  left_join(State1, by="port") %>%
  unique()


write.csv(Port4,"Port1.csv", row.names = FALSE)

#Connection CSV
Port4 <- read_csv("Port1.csv")
Connect1= Test %>%
  distinct(airport_1,airport_2, .keep_all=TRUE) %>%
  select(-one_of('Geocoded_City1', 'Geocoded_City2', "city1", "city2", "quarter","carrier_lg")) %>%
  rename(depart=airport_1,land=airport_2)
Connect2 = Connect1[,c(1,3,2)]
Connect2.1=Connect2 %>%
  rename(depart=land,land=depart)
Connect3 = Connect1 %>%
  rbind(Connect2.1)
Port_depart=Port4 %>%
  rename(depart=port) %>%
  select(-one_of("name"))
Port_land=Port4 %>%
  rename(land=port)%>%
  select(-one_of("name"))
Connect3.1=Connect3 %>%
  left_join(Port_depart, by="depart") %>%
  rename(lat1=lat,lon1=lon) %>%
  left_join(Port_land, by="land") %>%
  rename(lat2=lat,lon2=lon) %>%
  distinct(depart,land, .keep_all=TRUE)

Connect4=Connect3.1[complete.cases(Connect3.1), ]


write.csv(Connect4,"Connections.csv", row.names = FALSE)