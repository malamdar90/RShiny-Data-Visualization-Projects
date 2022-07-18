library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(ggthemes)
library(DT)
library(plotly)
library(ggrepel)
library(tidyverse)
library(scales)
library(mosaic)
library(mapproj)
#library(emoGG)
library(readr)


##### Data Preparation #####
# page2 u.s. map data preparation
covid_19 <- read_csv("data/us-states.csv", col_names = TRUE) 
state = read_csv("data/states_map.csv") %>% select(-c(1))
population <- read_csv("data/state-population.csv")
# Prepare the datasets for being merged
covid_19 <- covid_19 %>% mutate(region=tolower(state)) %>% filter(date>as.Date("2020-03-22"))
population <- population %>% mutate(region=tolower(state))


# page2 states map data preparation
covid_county <- read_csv("data/dataitem1.csv", col_names = TRUE) %>% select(-c(6:7)) 
county = read_csv("data/county_map.csv")

# page2 trend data preparation
data <- read_csv("data/us-states.csv")
pp <- read_csv("data/state-population.csv")
data <- left_join(data, pp, by = "state")
data$Confirmedbypop <- round(data$cases/data$population,3)
data$Fatality <- round(data$deaths/data$cases,3)
df <- data %>% group_by(state, date)
df$date <- as.Date(df$date, format="%m/%d/%Y")




# page3-1 flights data preparation
states_flight <- read_csv("data/states_flight.csv",
                          col_types = cols(Date = col_character())) %>%
  filter(lat >= 25) %>%
  filter(lat <= 50) %>%
  filter(long >= -150) %>%
  filter(long <= -75)
states_flight$Date=as.Date(states_flight$Date, "%Y/%m/%d")

# page3-2 air travelers data preparation
traveler <- read_csv("data/Total Traveler.csv", col_names = TRUE)
traveler$Date = as.Date(traveler$Date)
traveler$Day<- format(as.Date(traveler$Date),"%m/%d")

# page3-2 total travelers data preparation
trip <- read_csv("data/travel.csv")
trip$Date=as.Date(trip$Date, "%Y/%m/%d")
trip <- trip %>% filter((Date>=as.Date("2020-03-01") & Date<as.Date("2020-06-28")) | (Date>=as.Date("2019-03-01") & Date<as.Date("2019-06-28")))
all <- trip%>%group_by(Date)%>%summarise(Travelers=sum(Travelers))
all$state="ALL"
print(trip)
print(all)

trip1 <- trip
  
trip1$year<- as.character(format(trip1$Date,'%Y'))
trip1$Date<- format(as.Date(trip1$Date),"%m/%d")
trip1$Date1<- as.Date(trip1$Date,format="%m/%d")

# page3-3 restriction data preparation 
restriction <- read_csv("data/Travel Restrictions.csv", col_names = TRUE)
restriction <- na.omit(restriction)
continent <- read_csv("data/continent.csv", col_names = TRUE) %>% select(c(1,3,5))
continent <- continent %>% mutate(Abb=Three_Letter_Country_Code)
res_con <- right_join(restriction, continent, by = "Abb")
res_con <- na.omit(res_con) %>% select(-c(10))

#page4 data preparation

TableCovid19Cases = read_csv("data/01_Covid19Cases.csv")
TableTrends = read_csv("data/02_Trends.csv")
TableFlightsMap = read_csv("data/03_FlightsMap.csv")
TableTravelers = read_csv("data/04_Travelers.csv")
TableRestrictions = read_csv("data/05_TravelRestrictions.csv")
TableFlightTravelers = read_csv("data/06_FlightTravelers.csv")

 
##### ui #####

ui <- dashboardPage(
  skin = "black", 
  header = dashboardHeader(title="COVID-19 & Flights"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "page1", icon = icon("info-circle")),
      menuItem("Covid-19 Cases", tabName = "page2", icon = icon("map-o")),
      menuItem("Flights Impact Analysis", tabName = "page3", icon = icon("fighter-jet"),
               menuSubItem("Flights Map", tabName = "page3-1", icon = icon("fighter-jet")),
               menuSubItem("Travelers Change Trend", tabName = "page3-2", icon = icon("fighter-jet")),
               menuSubItem("Global Air Restriction ", tabName = "page3-3", icon = icon("fighter-jet"))),
      menuItem("Data", tabName = "page4", icon = icon("database")))),
  
  body = dashboardBody(
    tabItems(
      tabItem("page1",
              fluidPage(title = NULL,
                        fluidRow(box(title = "About the Topic", status = "primary", 
                                     solidHeader = TRUE,  width = 12,
                                     tags$blockquote(tags$h5("2020 is a tough year. Covid-19 has impacted many aspects of our life and changed the world.In this website, there will be a dashboard showing cases in the US in terms of county level and state level. Critical trend will also be provided in the same page. Moreover, we specifically research the impact on flights.")),
                                     tags$b("Following insights will be provided by this webside:"),
                                     br(),
                                     tags$li("How severe is the COVID-19 in the USA by each state and county?"),
                                     tags$li("What is the trend of COVID-19 cases across the state? "),
                                     tags$li("How does the epidemic situation change from March to the present?"),
                                      tags$li("How does the number of domestic flights of the US change during the COVID-19?"),
                                       tags$li("What are the impacts of the COVID-19 on flights and traverls?")

                                       )),
                        
                        fluidRow(box(title = "About the Reference", status = "info",solidHeader = TRUE,
                                     width = 12, tags$h5("Thanks for the articles and insights belows:"),
                                     tags$li(tags$a(href="https://www.nytimes.com/interactive/2020/02/21/business/coronavirus-airline-travel.html","New York Times: 13,000 Missing Flights: The Global Consequences of the Coronavirus")),
                                     tags$li(tags$a(href="https://www.visualcapitalist.com/global-flight-capacity-coronavirus/","Visual Capitalist: You are Grounded: The COVID-19 Effect on Global Flight Capacity")),
                                     tags$li(tags$a(href="https://www.fastcompany.com/90485284/air-traffic-data-shows-much-less-crowded-skies-since-the-coronavirus-spread/","Fast Company: Air Traffic Data Shows Less Crowded Skies Since the Coronavirus Spread")),
                                     tags$li(tags$a(href="https://www.cnn.com/interactive/2020/health/coronavirus-us-maps-and-cases/","CNN Health: Tracking Covid-19 Cases in the US"))
                        )))),
                       
            
      tabItem("page2",
              fluidPage(title = NULL,
                        fluidRow(box(width =12,title = "How to Use", status = "primary" , solidHeader = TRUE,collapsible = TRUE,
                                     tags$p("In the U.S. Map tab, the map shows you the information of COVID19 on a specific date. Interact with the panel and choose what you are interested in. The map displays confirmed/death/recovered cases by county/state on a chosen date. In the Critical Trend tab,  the plot shows you some critical trends of COVID19. Choose the state and metric you are interested in. The plot displays the chosen state in red line with other states in light grey."))),
                        fluidRow(
                          column(width = 3,
                                 valueBox("14.4M","Global Total Cases", icon= icon("list"),width = 14),
                                 valueBox("3.82M","US Total Cases", icon= icon("list"),width = 14, color="light-blue"),
                                 valueBox("606K","Global Deaths", icon= icon("list"),width = 14),
                                 valueBox("140K","US Deaths", icon= icon("list"),width = 14, color="light-blue"),
                                 valueBox("4%","Global Fatality Rate", icon= icon("list"),width = 14),
                                 tags$h5("Above data updated by July 21st, 2020")),
                                 

                          column(width = 9,
                                 tabBox(title = NULL,height = "600px", width = 700,
                                   tabPanel("U.S Map",
                                            box(
                                              width = 12,
                                              height = "550px",
                                              title = "", 
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              enable_sidebar = TRUE,
                                              sidebar_width = 25,
                                              sidebar_start_open = TRUE,
                                              sidebar_content = tagList(
                                                    radioButtons("groupby",label = NULL, choices=c("cases","deaths","ConfirmedByPopulation","FatalityRate")),
                                                    sliderInput("year",label = NULL, min = min(as.Date(covid_19$date,'%Y-%m-%d')),
                                                                max = max(as.Date(covid_19$date,'%Y-%m-%d')),
                                                                value = min(as.Date(covid_19$date, '%Y-%m-%d')),
                                                                animate = T)),
                                              loadingState(),
                                              plotlyOutput("plotmap", height = 430, width = 650))),
                                   
                                   tabPanel("Major States Map",
                                            box(
                                              width = 12,
                                              height = "550px",
                                              title = "", 
                                              closable = TRUE, 
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              enable_sidebar = TRUE,
                                              sidebar_width = 25,
                                              sidebar_start_open = TRUE,
                                              sidebar_content = tagList(
                                                selectInput("stateselect", label = NULL, choices=c("New York","California","Florida","Texas","New Jersey")),
                                                radioButtons("stategroupby",label = NULL, choices=c("Confirmed","Deaths","Active")),
                                                sliderInput("stateyear",label = NULL, min = min(as.Date(covid_19$date,'%Y-%m-%d')),
                                                            max = max(as.Date(covid_19$date,'%Y-%m-%d')),
                                                            value = min(as.Date(covid_19$date, '%Y-%m-%d')),
                                                            animate = T)),
                                              loadingState(),
                                              plotlyOutput("plotstate",height= 450, width = 650))),
                                   
                                   
                                   tabPanel("Critical Trend",
                                            absolutePanel(left = 40, selectInput("trend1", label = NULL, choices=c(state.name))),
                                            absolutePanel(left = 340, selectInput("trend2", label = NULL,
                                                                                  choices=c("Confirmed Cases" = "cases","Deaths" = "deaths","Confirmed by Poplulation" = "Confirmedbypop","Fatality" = "Fatality"),
                                                                                  selected = NULL, multiple = FALSE,
                                                                                  selectize = TRUE, width = 200, size = NULL)),
                                            absolutePanel(left = 20, top = 90, plotlyOutput("plottrend", height = 500, width = 800)))))))),
      tabItem("page3-1",
              fluidPage(
                fluidRow(
                  box(width =12,title = "How to Use", status = "primary" , solidHeader = TRUE,collapsible = TRUE,
                      tags$p("The two flight maps show the status of flights over the United States during COVID19. Interact with date sliders and choose two dates you would like to compare. The density of the small airplane icons shows the busyness of flights over the United States. " ),
                      tags$p("Data below each map shows the total number of flights on the corresponding date (at 6 PM)."))
                ),
                fluidRow(
                  box(width = 6,
                              plotOutput("flight1"),
                              title = NULL,
                              icon = "fa fa-th",
                              gradientColor = "aqua",
                              boxToolSize = "xs",
                              closable = TRUE,
                              footer = sliderInput("flighttime1",label = NULL, min = as.Date("2020-03-16"),
                                                   max = as.Date("2020-07-13"),
                                                   value = as.Date("2020-03-16"),step = 7, width=580)),

                  box(width = 6,
                              plotOutput("flight2"),
                              title = NULL,
                              icon = "fa fa-th",
                              gradientColor = "aqua",
                              boxToolSize = "xs",
                              closable = TRUE,
                              footer = sliderInput("flighttime2",label = NULL, min = as.Date("2020-03-16"),
                                                   max = as.Date("2020-07-13"),
                                                   value = as.Date("2020-03-30"),step = 7, width=580))

                ),
                fluidRow(valueBoxOutput("flightvalue1",width = 6),valueBoxOutput("flightvalue2",width = 6))

                  
                )),
      
      tabItem("page3-2",
              fluidRow(
                box(width =12,title = "How to Use", status = "primary" , solidHeader = TRUE,collapsible = TRUE,
                    tags$p("The upper plot on this page shows the total number of aircraft passengers in 2019 and 2020. Click on the check button to show the important declarations. The lower plot shows the number of all types of travelers (including aircraft travelers) in the chosen state in the US during COVID19. Interact with the select box to choose the state (or all) you are interested in."))
              ),
              fluidRow(
                box(
                  title = NULL,
                  status = "primary",
                  width = 12,
                  fluidRow(
                    column(width = 8,
                           plotlyOutput("travelers")
                    ),
                    column(
                      width = 4,
                      boxPad(
                        color = "light-blue",
                        descriptionBlock(
                          header = tags$b(br()), 
                          text = tags$h6(br()), rightBorder = FALSE
                        ),
                        descriptionBlock(
                          header = tags$b(br()), 
                          text = tags$h6(br()), rightBorder = FALSE
                        ),
                        descriptionBlock(
                          header = tags$b(tags$h1(tags$h1("-84.52%"))), 
                          text = tags$h4("Significan Reduction in Aircraft Passengers 2020"), rightBorder = FALSE
                        ),
                        descriptionBlock(
                          header = tags$b(br()), 
                          text = tags$h6(br()), rightBorder = FALSE
                        ),
                        descriptionBlock(
                          checkboxInput("restrictions", label = "Show Important Declarations", value = FALSE),
                          header = tags$b(br()), 
                          text = tags$h6(br()), rightBorder = FALSE
                        ),
                        descriptionBlock(
                          header = tags$b(br()), 
                          text = tags$h6(br()), rightBorder = FALSE
                        )
                        ))))),
              fluidRow(box(status = "info",title = NULL, width = 12,
                    selectInput("whichstate", label = NULL,
                                choices=c(state.abb),
                                selected = NULL, multiple = FALSE,
                                selectize = TRUE, width = 200, size = NULL),
                    plotlyOutput("totaltravelers"),
                    "There might be some data missing. AK is not included.")
              )),
      
      tabItem("page3-3",
              
              fluidRow(
                box(width =12,title = "How to Use", status = "primary" , solidHeader = TRUE,collapsible = TRUE,
                    tags$p("In the Flight Restriction tab, the graph shows you the information on the number of flight restrictions of each country. Interact with the select box to choose the continent you want to analyze."))),
              
              fluidRow(box(width = 12, title = NULL, status = "info",
                selectInput("bycontinent", label = NULL,
                                   choices=c("North America","South America","Europe","Asia","Africa", "Oceania"),
                                   selected = NULL, multiple = FALSE,
                                   selectize = TRUE, width = 200, size = NULL),
                box(
                  plotOutput("restriction",height = "500px"),
                  title = "The Number of Global Air Restrictions",
                  width = 12,
                  icon = NULL,
                  gradientColor = NULL, 
                  boxToolSize = "xs", 
                  closable = TRUE,
                  footer = NULL)))),
      
      tabItem("page4",
              tabBox(id = NULL, selected = NULL, title = NULL, width = 12,
                     tabPanel("TableCovid19Cases", 
                              tags$a("Dataset",href="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"), 
                              br(), dataTableOutput("myTable1")),
                     tabPanel("TableTrends",tags$a("Dataset",href="https://github.com/nytimes/covid-19-data/blob/master/us-states.csv"), 
                              br(), dataTableOutput("myTable2")),
                     tabPanel("TableFlightsMap", tags$a("Dataset",href="https://opensky-network.org/datasets/"), 
                              br(), dataTableOutput("myTable3")),
                     tabPanel("TableTravelers", tags$a("Dataset",href="https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv"), 
                              br(), dataTableOutput("myTable4")),
                     tabPanel("TableRestrictions", tags$a("Dataset",href="https://data.humdata.org/dataset/covid-19-global-travel-restrictions-and-airline-information"), 
                              br(), dataTableOutput("myTable5")),
                     tabPanel("TableFlightTravelers", tags$a("Dataset",href="https://www.tsa.gov/coronavirus/passenger-throughput"), 
                              br(),dataTableOutput("myTable6"))
                
                ))
          
    )))

                          
server <- function(input, output, session) {
  
  datatrystate<-reactive({
    # Filter the date
    thisDate = input$year
    covid_19_date <- covid_19 %>% filter(date == thisDate)
    # Merge state with covid_19
    merged_state <- left_join(state, covid_19_date, by = "region")
    merged_state$long <- as.numeric(merged_state$long)
    merged_state$lat <- as.numeric(merged_state$lat)
    merged_state <- left_join(merged_state, population, by = "region")
    merged_state$ConfirmedByPopulation <- round(merged_state$cases/merged_state$population,3)
    merged_state$FatalityRate <- round(merged_state$deaths/merged_state$cases,3)

    return(merged_state %>% select(c("long","lat","group","state.x","date","region", Number=input$groupby)))
    })
  
  output$plotmap = renderPlotly({
    
    if (input$groupby == "cases" | input$groupby == "deaths"){
    p <- ggplot(data = datatrystate(),
                aes(x = long, y = lat,
                    fill = log(Number), text = state.x, label= Number, 
                    group = group))+
      geom_polygon(color = "black",
                   size = 0.05) +
      labs(title="US COVID-19 Map by State", fill = NULL) +
      theme_map() +
      scale_fill_gradient2(na.value = "white", low = "white", high = "firebrick3") +
      guides(fill=FALSE)}
    
    else {
    p <- ggplot(data = datatrystate(),
                aes(x = long, y = lat,
                    fill = Number, text = state.x, label= Number, 
                    group = group))+
      geom_polygon(color = "black",
                   size = 0.05) +
      labs(title="US COVID-19 Map by State", fill = NULL) +
      theme_map() +
      scale_fill_gradient2(na.value = "white", low = "white", high = "firebrick3") +
      guides(fill=FALSE)}
    
    ggplotly(p,tooltip = c("text","label"))
    
  })
  
  datatrycounty<-reactive({
  # select only a specific state data
  covid_county <- covid_county %>% filter(covid_county$Province_State==input$stateselect) 
  county$id = as.numeric(county$id)
  names(county)[8] = "FIPS"
  thisDate = input$stateyear
  covid_date <- covid_county %>% filter(Date == thisDate)
  merged_county <- left_join(county, covid_date, by = "FIPS")
  merged_county <- na.omit(merged_county)
  return(merged_county %>% select(c("long","lat","Admin2","group","Date", Number=input$stategroupby)))
})
  
  output$plotstate = renderPlotly({
    p <- ggplot(data = datatrycounty(),
                aes(x = long, y = lat,
                    fill = log(Number), text = Admin2, label=Number,  
                    group = group))+
      geom_polygon(color = "black",
                   size = 0.05) +
      labs(title = paste("US Top Five States:", input$stateselect) , fill = NULL) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      coord_equal() +
      theme_map() +
      scale_fill_gradient2(na.value = "white", low = "white", high = "red3") +
      guides(fill=FALSE)
    ggplotly(p,tooltip = c("text","label"))
    })

  df1<-reactive({
    return(df %>% select(c("state","date", Number=input$trend2)))
  })
  
  df2<-reactive({
    return(df1() %>% subset(state == input$trend1))
  })
  
  df3<-reactive({
    return(df1() %>% subset(date == max(date) & state == input$trend1))
  })
  
  output$plottrend = renderPlotly({

    p <- ggplot(data=df1(), mapping=aes(x=date,y=Number, group=state))
    p <- p + theme_bw() + theme(panel.border=element_blank())
    p <- p+ geom_line(data=df1(), color="gray70", size = 0.1, alpha = 0.9)
    p <- p+ geom_line(data=df2(), color="darkred", size = 1) +
      geom_text(
        data = df3(),
        mapping = aes(label = state),
        size = 4,color="darkred",
        segment.color = NA)

    p <- p + labs(x="Date", y=input$trend2) + scale_y_continuous(labels = scales::comma)

    p <- p+xlim(as.Date(c("2020-03-10", "2020-07-12")))

    ggplotly(p)
  })

  output$flight1 = renderPlot({
    p <-  ggplot(data = states_flight,
                 aes(x = long, y = lat,
                     label= region,
                     group = group))+
      geom_polygon(data = states_flight%>%filter(sf=="s"),fill = "white",color = "black",
                   size = 0.05) +
      labs(title = paste("   US Flights Map",input$flighttime1)) +
      coord_equal() +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme_map()
    p <- p+  geom_emoji(states_flight%>%filter(Date==as.Date(input$flighttime1)),mapping= aes(x=long, y=lat),emoji="2708",size=0.01)
    p <- p+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    p
  })

  output$flight2 = renderPlot({
    p <-  ggplot(data = states_flight,
                 aes(x = long, y = lat,
                     label= region,
                     group = group))+
      geom_polygon(data = states_flight%>%filter(sf=="s"),fill = "white",color = "black",
                   size = 0.05) +
      labs(title = paste("   US Flights Map",input$flighttime2)) +
      coord_equal() +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme_map()
    p <- p+  geom_emoji(states_flight%>%filter(Date==as.Date(input$flighttime2)),mapping= aes(x=long, y=lat),emoji="2708",size=0.01)
    p <- p+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    p
  })

  output$flightvalue1 <- renderValueBox({
    infobox <- states_flight%>%group_by(Date)%>%summarise(N=n())
    infobox <- infobox%>%filter(Date==input$flighttime1)

    valueBox(
      paste("Total Number:",infobox$N), input$flighttime1, icon = icon("thumbs-up", lib = "glyphicon"),
      color = "aqua"
    )
  })

  output$flightvalue2 <- renderValueBox({
    infobox <- states_flight%>%group_by(Date)%>%summarise(N=n())
    infobox <- infobox%>%filter(Date==input$flighttime2)

    valueBox(width = 6,
      paste("Total Number:",infobox$N), input$flighttime2, icon = icon("thumbs-up", lib = "glyphicon"),
      color = "aqua"
    )
  })

  output$travelers = renderPlotly({
    # plot the graph
    p <- traveler %>% ggplot(mapping = aes(x=Date, label=Day)) + 
      geom_line(aes(y = ThisYearTotalTravelers), color = "darkred") + 
      geom_line(aes(y = LastYearTotalTravelers), color = "steelblue") +
      scale_y_continuous(breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000),
                         labels=c("0","500K","1M","1.5M","2M","2.5M","3M")) +
      xlab("Date") +
      ylab("Number of Aircraft Travelers") +
      labs(title = "The Number of Aircraft Travelers") +
      annotate(geom = "text",
               size=5, 
               color="darkred",
               x=max(traveler$Date), y=400000, 
               label = "2020") +
      annotate(geom = "text",
               size=5, 
               color="steelblue",
               x=max(traveler$Date), y=2250000,  
               label = "2019")+ 
      theme(panel.background = element_blank()) +
      theme(axis.line = element_line(colour = "black"))

    if (input$restrictions == TRUE){
      p = p +
        geom_point(data = subset(traveler, Date == as.Date("2020-03-13")),
                   mapping=aes(x=Date, y=ThisYearTotalTravelers),
                   color = "darkred") +
        geom_text(data = subset(traveler, Date == as.Date("2020-03-13")),
                  aes(x=as.Date("2020-04-13"), y=ThisYearTotalTravelers, label=Restriction))

      p = p +
        geom_point(data = subset(traveler, Date == as.Date("2020-03-31")),
                   mapping=aes(x=Date, y=ThisYearTotalTravelers),
                   color = "darkred") +
        geom_text(data = subset(traveler, Date == as.Date("2020-03-31")),
                  aes(x=as.Date("2020-04-12"), y=ThisYearTotalTravelers+200000, label="Global Level 4 Health Advisory: \n Do Not Travel"))
    }
    ggplotly(p, tooltip = c("label","y"))
  })

  output$totaltravelers = renderPlotly({
    
    p <-  ggplot(data=trip1, mapping = aes(x = Date1,label=Date, y = Travelers, color=year))+  
      geom_line(data=subset(trip1, state== input$whichstate ))+
      labs(x = "Date", y = "Number of Travelers", title=paste("All Types Travelers in", input$whichstate)) 
    
    p <- p  + theme(panel.background = element_blank()) + 
      scale_y_continuous(labels = scales::comma)+scale_color_manual(values = c("steelblue","darkred"))+ 
      geom_vline(xintercept = as.numeric(as.Date("2020-03-13")),linetype = "longdash")+ 
      annotate(geom="text", x=as.Date("2020-04-01"), y=min(subset(trip1, state==input$whichstate)$Travelers, na.rm = getOption("na.rm", TRUE)), label="Emergency Declaration on 3.13")+ 
      theme(axis.line = element_line(colour = "black")) 
    
    ggplotly(p,tooltip =c("label","y"))
    
  })

  output$restriction = renderPlot({

    res_summary <- res_con %>% group_by(Continent_Name,Country) %>% summarise(N=n())
    subset1 <- res_summary %>% subset(Continent_Name==input$bycontinent)

    p <- ggplot(subset1, aes(x = reorder(Country, N), y = N)) +
      geom_col(position = "dodge2", fill = "steelblue")  +
      labs(x = NULL, y = "Number of Restrictions",
           title = input$bycontinent) +
      guides(fill = FALSE) +
      theme(axis.text.y = element_text(size=14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      coord_flip()
    p

  })
  
  output$myTable1 = renderDataTable({
    return(datatable(TableCovid19Cases, rownames= FALSE))
    })
  output$myTable2 = renderDataTable({
    return(datatable(TableTrends, rownames= FALSE))
  })
  output$myTable3 = renderDataTable({
    return(datatable(TableFlightsMap, rownames= FALSE))
  })
  output$myTable4 = renderDataTable({
    return(datatable(TableTravelers, rownames= FALSE))
  })
  output$myTable5 = renderDataTable({
    return(datatable(TableRestrictions, rownames= FALSE))
  })
  output$myTable6 = renderDataTable({
    return(datatable(TableFlightTravelers, rownames= FALSE))
  })
}


shinyApp(ui = ui, server = server)
