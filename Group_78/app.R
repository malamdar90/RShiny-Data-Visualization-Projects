# Dashboard url: 
# https://dannycarrot.shinyapps.io/Final_project-F1-101/

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(leaflet)
library(maps)
library(plotly)
library(ggrepel)
library(readr)
library(shinydashboard) 
library(DT)
# library(bit64)    run it if you don't have, required to run "sqldf"
# library(memoise)  run it if you don't have, required to run "sqldf"
library(sqldf)

ui <- dashboardPage(
  dashboardHeader(title = "F1:101"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("book")),
      menuItem("Historical Standings", tabName = "page2", icon = icon("line-chart")),
      menuItem("Constructor Championship", tabName = "page3", icon = icon("car")),
      menuItem("Grand Prix Tracking Map", tabName = "page4", icon = icon("map-o")),
      menuItem('Driver Championship',tabName='page5',icon=icon('book')),
      menuItem('About Us',tabName='page6',icon=icon('address-card'))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "page1",
              h1('Introduction for Descriptive Data Visulization'),
              h3('Project Purpose:'),
              textOutput('introText1'), 
              h3('Overview and Dataset Sources:'), 
              textOutput('introText2'), 
              h2('About the Tabs'),
              h3('Season standings visualization:'), 
              textOutput('introText3'),
              h3('Data for Constructors:'),
              textOutput('introText4'), 
              h3('Track map overview:'), 
              textOutput('introText5'),
              h3('Driver championship statistics:'),
              textOutput('introText6')    
              ),
      tabItem(tabName = "page2",
              h1("Historical Drivers' Standings and Races"),
              h2('Standings'),
              selectInput(inputId = "Season",label = "Season", choices = c(1950:2021), selected = 2020),
              sliderInput(inputId = "Round", label = "After Round#:", 1, 8, 1, animate = animationOptions(interval = 1000, loop = FALSE)),
              plotOutput("standingPlot"), 
              h2('Race Scoreboard'),
              dataTableOutput("raceScoreboard")
      ),
      tabItem(tabName = "page3",
              sliderInput("constructorYear", "Season:", min = 1950, max = 2021, value = 2020, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              plotOutput("plot4"),
              dataTableOutput('constructorTable')
      ),
      tabItem(tabName = "page4",
              h1('Matching history'),
              sliderInput(
                "year",
                "Year:",
                min = 1950,
                max = 2021,
                value = 2000,
                step = 1,
                animate = animationOptions(interval = 1000, loop = FALSE)
              ),
              leafletOutput("myMap", width="50%"), 
              dataTableOutput("mapScoreboard")
      ),
      tabItem(tabName = "page5",
              h1("Historical Drivers' Championship number"),
              mainPanel(
                plotlyOutput("championPlot", height = 750)), 
                dataTableOutput("championScoreboard")
      ),
      tabItem(tabName = "page6",
                       h1('Course: Data Visualization'),
                       h1('Course Instructor: Mohammad Ali Alamdar Yazdi'),
              h3('Reference:'),
              textOutput('introText7')
                      )
              )
      ))




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Danny's part
  circuits = read_csv("circuits.csv")
  races = read_csv("races.csv")
  results = read_csv("results.csv")
  drivers = read_csv("drivers.csv")
  
  
  circuits = circuits %>% select(-9)
  
  races = races %>% select(-7,-8)
  
  results = results %>% select(1,2,3,5,7)
  
  drivers = drivers %>% select(1,3,5,6)
  
  comb1 = circuits %>% left_join(races, by = 'circuitId')
  
  comb2 = comb1 %>% left_join(results, by='raceId')
  
  data = comb2 %>% left_join(drivers, by='driverId')
  
  data= data %>% mutate(podium=paste0(forename,' ',surname))
  
  filter1=data %>% filter(position==1) %>% select(9,17,21) %>% rename(first=podium)
  filter2=data %>% filter(position==2) %>% select(9,17,21) %>% rename(second=podium)
  filter3=data %>% filter(position==3) %>% select(9,17,21) %>% rename(third=podium)
  
  filter4= filter1 %>% left_join(filter2, by='raceId') %>% left_join(filter3,by='raceId')
  
  fulldata= data %>% left_join(filter4, by='raceId') %>% distinct(raceId,.keep_all=TRUE) %>% select(3,4,5,6,7,10,11,12,13,23,25,27)
  
  output$myMap <- renderLeaflet({
    # Copy and paste part d below
    thisYear = input$year
    filterdata = fulldata %>% filter(year == thisYear) 
    
    leaflet(data = filterdata) %>% addTiles() %>% addMarkers(
      lng =  ~ lng,
      lat =  ~ lat,
      label =  ~ round,
      labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,textsize = "15px"),
      popup = (paste(sep = "<br/>",filterdata$name.y, 
                     paste('Country:',filterdata$country),
                     filterdata$date,
                     paste('First Place:', filterdata$first), 
                     paste('Second Place:',filterdata$second),
                     paste('Third Place:',filterdata$third))
      ))
    
  })
  
  # datatable
  output$mapScoreboard = renderDataTable({
    thisYear = input$year
    filterdata = fulldata%>%
      filter(year == thisYear)%>%
      select(-4, -5, -6, -7)
    return(datatable(filterdata, rownames= FALSE))
  })
  
  output$introText1 <- renderText({
    "
    We made this web application to provide comprehensive visualization and statistical data of the team, 
    drivers, and standings for the audience of Formula One introductory cars.
    We will show how the Formula One Grand Prix has become popular and loved by people over the past 70 years. 
    This sport originated in Europe and developed in all parts of the world. 
    Its high visibility and popularity have created a good business environment and brought many investments from sponsors and even the government."
  })
  output$introText2 <- renderText({
    "Formula One, also known as Formula 1 or F1, is a race of the highest level organized by the International Automobile Federation with the participation of 20 drivers. 
    The official name of F1 is 'International Automobile Federation World Formula One Championship.' 
    The 'formula in the title refers to a set of rules that all participating vehicles must abide by. 
    The F1 season includes a series of competitions, and the venues of these so-called 'Grand Prix'' (Grand Prix, from French, meaning Great Prizes) are fully enclosed unique tracks or temporarily closed ordinary roads. 
    The result of each race is counted into the points system to determine two annual world championships: one awarded to the driver, and one awarded to the manufacturer.
    If you are new to Formula One and fast cars fascinate you, we will bring you up to speed in this History Introduction to Formula 1."
  })
  output$introText3 <- renderText({
    "The standings tab provides a dynamic and straight-forward way to see how the drivers standings change throughout any season since 1950. 
    The audience could check whether the competition in a season is a blow-out or an intense game. 
This tab also provides a concise table that shows the result of any game since 1950. 
    The audience could check the winner, runner-up, race time, etc., to gather a brief overview of the game. "
  })
  output$introText4 <- renderText({
    "This part is about constructors. Famous constructors include Mercedes, Ferrari, Red Bull and McLaren. 
    Their reputation has been forged by the championships they won again and again. 
    The bar chart on the top of this page shows the total points of each constructor obtained in each season. 
    At the meantime, we can tell who obtained the most points and who is the champion in this season. 
    The data frame on the below shows drivers information from top 3 constructors in each season. 
    For example, Mercedes, Ferrari and Williams are the top 3 constructors in 2015. 
    We can also tell from the below data frame that how much points did Lewis Hamilton and Nico Rosberg contribute to Mercedes championship. "
  })
  
  output$introText5 <- renderText({
    "Unlike soccer games and basketball games, which take place in one country or a few countries each season, 
    F-1 or Formula one is an exciting sport that travels around the world each season to bring the excitement to the fans. 
    Also, as a sport that has a history for more than 80 years, it has been developing its influence around the world step by step. 
    I believe the data visualization using map can clearly demonstrate how F-1 has been developed across the years. 
    For people who would love to look deeper into this sport and want to know who the best drivers around the world in different seasons are, 
    this map can help them to see which drivers have stood on the podiums.  "
  })
  
  output$introText6 <- renderText({
    "This part will visually show the list of world champions of F1 drivers ranked by the number of championships they got, 
    which a histogram will display. The attribution of this award depends on the driver's performance in each race, 
    and it is reflected in the driver's standings by the scoring system. Combined with the year information, 
    you can easily determine the peak period of the driver's career. 
    Through the attached table, the drivers are classified in several dimensions, 
    which is convenient for you to make comprehensive judgments from different angles. "
  })
  
  output$introText7 <- renderText({
    'https://www.kaggle.com/rohanrao/formula-1-world-championship-1950-2020?select=results.csv'
  })
  
  
  # Wiz's part
  # import dataset
  wiz_drivers <- read_csv("drivers.csv")
  wiz_driver_standings <- read_csv("driver_standings.csv")
  wiz_races <- read_csv("races.csv")
  wiz_races = wiz_races[-1036,]
  wiz_results <- read_csv("results.csv")
  wiz_constructors <- read_csv("constructors.csv")
  wiz_constructor_standings <- read_csv("constructor_standings.csv")
  wiz_circuits <- read_csv("circuits.csv")
  
  # update slider input
  observe({
    roundsInSeason = wiz_races%>%filter(year==input$Season)%>%select(round)%>%max()
    updateSliderInput(session = session, inputId = 'Round', min = 1, max = roundsInSeason, value = 1, step = 1)
  })
  
  # data preparation
  # Add a colomn indicating drivers' fullname
  wiz_drivers$fullname = paste(wiz_drivers$forename, wiz_drivers$surname, sep = ' ')
  # Create a df to show drivers' constructor in each race
  wiz_driver_constructor = wiz_results%>%select(2, 3, 4)%>%left_join(select(wiz_constructors, constructorId, name), by = 'constructorId')
  colnames(wiz_driver_constructor)[4] = 'constructorName'
  # change constructor_standings colname
  colnames(wiz_constructor_standings)[5] = 'constructorPosition'
  # Merge data
  standings_merged = wiz_driver_standings%>%
    left_join(select(wiz_drivers, driverId, fullname), by = 'driverId')%>%
    left_join(select(wiz_races, raceId, year, round), by = 'raceId')%>%
    left_join(wiz_driver_constructor, by = c('raceId', 'driverId'))%>%
    left_join(select(wiz_constructor_standings, raceId, constructorId, constructorPosition), by = c('raceId', 'constructorId'))%>%
    distinct()
  # number of wins
  standings_merged = standings_merged%>%
    mutate(winsLevel = ifelse(wins==0, 0, ifelse(wins==1,1, ifelse(wins==2,2, '3+'))))
  standings_merged
  
  # plot
  output$standingPlot <- renderPlot({ 
    selectedYear=input$Season
    selectedRound=input$Round
    selectedRace=wiz_races%>%filter(year==selectedYear & round==selectedRound)%>%select(raceId)%>%as.numeric()
    selectedCircuit = wiz_races%>%filter(raceId==selectedRace)%>%select(circuitId)%>%as.numeric()
    wiz_circuits%>%filter(circuitId==selectedCircuit)%>%select(name)%>%as.character()
    standings_merged%>%
      filter(year == selectedYear & round == selectedRound)%>%
      arrange(desc(points))%>%
      slice(1:15)%>%
      ggplot(aes(x=reorder(fullname, points), y=points, fill=winsLevel))+
      scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Oranges"),labels = c('0', '1', '2', '3+'))+
      geom_col()+
      coord_flip()+
      geom_text(aes(label = points), size = 3, hjust = -0.1)+
      theme(axis.text.x=element_blank(), 
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.058))+
      xlab(label = 'Driver')+
      ylab(label = 'Points')+
      labs(title = paste('Drivers Standings after Race', 
                         selectedRound, 
                         '(', 
                         races%>%filter(raceId==selectedRace)%>%select(name)%>%as.character(), 
                         'in', 
                         paste(circuits%>%filter(circuitId==selectedCircuit)%>%select(location)%>%as.character(), 
                               circuits%>%filter(circuitId==selectedCircuit)%>%select(country)%>%as.character(), 
                               sep = ','), 
                         ')'), 
           fill = 'Number of Wins')
  })
  
  # datatable
  output$raceScoreboard = renderDataTable({
    # retrieve raceId
    selectedYear=input$Season
    selectedRound=input$Round
    selectedRace=wiz_races%>%filter(year==selectedYear & round==selectedRound)%>%select(raceId)%>%as.numeric()
    # result table
    selectedWinnerTime = wiz_results%>%filter(raceId==selectedRace & position==1)%>%select(milliseconds)%>%as.numeric()
    scoreboardTable = wiz_results%>%
      filter(raceId==selectedRace)%>%
      left_join(select(wiz_drivers, driverId, fullname), by = 'driverId')%>%
      left_join(select(wiz_driver_constructor, raceId, driverId, constructorName), by = c('raceId', 'driverId'))%>%
      select(position, fullname, constructorName, points, time, fastestLapTime)%>%
      mutate(time=ifelse(position==1, format(as.POSIXct(selectedWinnerTime/1000, origin = "1970-01-01", tz = 'GMT'), "%H:%M:%OS3"), 
                         ifelse(substr(time,1,1)=='+', time, paste('+', time, sep = ''))))%>%
      rename(Position=position, Driver=fullname, Constructor=constructorName, Points=points, Time=time, 'Fastest Lap Time'=fastestLapTime)
    
    return(datatable(scoreboardTable, rownames= FALSE))
  })
  
  # Steve's part
  #read in data
  constructors = read.csv("constructors.csv", encoding = "UTF-8")
  drivers = read.csv("drivers.csv", encoding = "UTF-8")
  results = read.csv("results.csv", encoding = "UTF-8")
  races = read.csv("races.csv", encoding = "UTF-8")
  #merge 3 datasets
  df = merge(constructors, results, by = "constructorId")
  df = merge(df, drivers, by = "driverId")
  df = merge(df, races, by = "raceId")
  
  #select useful information for plot
  df1 = df %>%
    select(constructorId, name.x, nationality.x, forename, surname, driverId, 
           nationality.y, year, points, rank, raceId) %>%
    unite(driver_name, forename, surname, sep = " ") %>%
    rename(constructor = name.x, cons_nationality=nationality.x, driver_nationality = nationality.y) %>%
    distinct()
  
  
  ##
  output$plot4 = renderPlot({
    
    # Constructors Championship by season
    filt_year = input$constructorYear
    
    filtered_data = df1 %>% filter(year == filt_year) %>% 
      group_by(constructor) %>% 
      summarise(Points = sum(points)) %>%
      arrange(-Points) %>% slice(1:10)
    
    ggplot(data = filtered_data)+
      aes(x = reorder(constructor, -Points), y = Points) +
      geom_bar(stat="identity", fill = "gray60", color="snow4") +
      annotate("text", label = filt_year, size=20, x=8, y=(as.numeric(filtered_data[1,"Points"]))/2, color = "grey80")+
      labs(title = 'Constructors Championship by Season')+
      xlab('Constructors')+
      theme_wsj()+
      theme(plot.title = element_text(size = rel(1),hjust = 0.5), 
            axis.ticks.y = element_blank(),
            axis.title=element_text(face = "bold", size = rel(0.6)),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank())
    
  })
  output$constructorTable = renderDataTable({
    filt_year = input$constructorYear
    #select useful information for dataframe
    filtered_data_2 = df1 %>% filter(year == filt_year) %>% 
      group_by(constructor) %>% 
      summarise(Points = sum(points)) %>%
      arrange(-Points) %>% slice(1:3) %>% 
      select(constructor)
    
    con = as.vector(unlist(filtered_data_2$constructor))
    
    df2 = df%>% filter(name.x %in% con) %>% filter(year == filt_year) %>%
      select(name.x, forename, surname, nationality.y, points,raceId) %>%
      unite(driver_name, forename, surname, sep = " ") %>%
      rename(constructor = name.x, driver_nationality = nationality.y) %>%
      distinct()
    
    df3 = df2%>% group_by(constructor, driver_name) %>% 
      summarise(total_points = sum(points)) %>% arrange(-total_points)
    
    return(datatable(df3, rownames= FALSE))
  })
  
  # Scarlett's part
  # import dataset
  champion <- read_csv("champion.csv")
  
  
  drivers <- read.csv("drivers.csv", encoding = "UTF-8") %>%
    drop_na() %>%
    unite(drivername, c("forename", "surname"), sep = " ") %>% 
    select(driverId, code, number, drivername, dob, nationality, url) %>% 
    rename(driver.number = number)
  
  # data preparation
  champion_number <- sqldf("select name, champion.nationality, count(*) as championship, sum(points) as points, url
                             from champion, drivers
                             where name==drivername
                             group by name 
                             order by championship 
                             desc")
  
  
  output$championPlot <- renderPlotly({ 
    

    g <- ggplot(champion_number, aes(x=reorder(name, championship), y=championship, color=championship, fill=championship))+
      geom_col()+
      coord_flip()+
      #scale_colour_gradient(low = "grey", high = "orange")+
      #geom_text(aes(label = points), size = 3, hjust = -0.1)+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.058))+
      xlab(label = 'Driver')+
      ylab(label = 'Champion numbers')+
      scale_y_continuous(breaks=seq(0,7,1))
    
    ggplotly(g)
    
  })

  # datatable
  
  
  output$championScoreboard = renderDataTable({
    champion_years <- sqldf("select name, year from champion order by name")
    names <- champion_years$name[!duplicated(champion_years$name)]
    champion_years_matrix <- matrix(NA,nrow=length(names),ncol=2)
    colnames(champion_years_matrix) = c('name', 'champ_year')
  
      for (i in 1:length(names)) {
      a <- champion_years[champion_years$name==names[i],]
      champion_years_matrix[i,1] <- a[1,1]
      champion_years_matrix[i,2] <- paste(a[,2],collapse = ",")
    }

    champion_years_matrix <- as.data.frame(champion_years_matrix)
    champion_number = champion_number %>% left_join(champion_years_matrix, by = "name")
    
    return(datatable(champion_number, rownames= FALSE))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)