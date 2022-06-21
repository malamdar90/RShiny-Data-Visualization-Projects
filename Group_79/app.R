library(tidyverse)
library(plotly)
library(scales)
library(ggthemes)
library(ggrepel)
library(ggplot2)
library(shiny)
library(maps)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
Sys.setlocale("LC_ALL", "English")
#####################################
covid = read_csv("owid-covid-data.csv")
map = read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv")

covid$date = as.Date(covid$date, "%Y/%m/%d")

colnames(map)[4] = "location"
map = map %>%
  select(2, 3, 4)

covidAll = covid %>%
  inner_join(map, by = "location")
######################################

data_indicator = read_csv(file = "cyx_refined_data.csv")
# this data is originated from the owid data which summarizes the data for each country or district
# eliminates countries and districts with NA total positive cases
data_indicator$smoke = data_indicator$female_smokers + data_indicator$male_smokers
# add up for the total share of smokers in each location
data_indicator$log_total_positive_cases_per_million = log(data_indicator$total_cases_per_million)
# take the log format of the total cases reported to simplify the y-axis
data_indicator_final = subset(data_indicator,
                              select = c(2, 3, 11, 33, 14, 17, 19, 21, 23, 24, 27:30, 32))
# create the final dataset for investigating the impacts of indicator

#######################################

data1 <- read.csv("owid-covid-data.csv")
sub_data <- data1[, c(1:5, 8, 35:37, 45)]
sub_data$date =  as.Date(sub_data$date, "%Y/%m/%d")
data <- sub_data %>%
  mutate(
    vaccination_rate = as.numeric(
      if_else(
        people_fully_vaccinated / population * 100 < 100,
        round(people_fully_vaccinated / population * 100, digits = 2),
        99.9900
      )
    ),
    year = as.numeric(substr(sub_data$date, 1, 4)),
    month = as.numeric(substr(sub_data$date, 6, 7))
  )
data[is.na(data)] <- 0

#######################################
header <- dashboardHeader(title = tags$strong("Directory    "))
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Project Description",
    tabName = "ProjectDescription",
    icon = icon("info-circle")
  ),
  menuItem(
    "Covid-19 Overview",
    tabName = "Covid-19Map",
    icon = icon("map")
  ),
  menuItem(
    "Potential Indicators",
    tabName = "Indicators",
    icon = icon("question-circle")
  ),
  menuItem(
    "Recovery Status",
    tabName = "Recoverystatus",
    icon = icon("first-aid")
  ),
  menuItem("Data", tabName = "datatable", icon = icon("table"))
))
body <- dashboardBody(tabItems(
  tabItem(
    "ProjectDescription",
    fluidRow(
      column(12,
             h1(
               strong("The Global Evolution of COVID-19 Cases")
             )),
      box(
        title = "Case Overview",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        collapsible = TRUE,
        height = "50%",
        column(12,
               tags$div(tags$span(
                 tags$h5(
                   "Coronavirus disease 2019 (COVID-19) is a contagious disease caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2).
                            The first known case was identified in Wuhan, China, in December 2019.
                            The disease has since spread worldwide, leading to an ongoing pandemic. " ,
                   br(),
                   br(),
                   "In this project, we plan to use data analytic techniques to visualize an animated data-driven documentary about the evolution
                            of covid-19 cases since the beginning of the pandemic.

                                 ",
                   br(),
                   br(),
                   style = "color:Black"
                 ),
                 HTML(
                   '<iframe width="70%" height="350" src="https://www.youtube.com/embed/rD5qZkULSaU"
                                             frameborder="0" allowfullscreen></iframe>'
                 )
               ),))
      )# End for box
    ),
    # End for fluidRow - Case Overview
    fluidRow(
      box(
        title = "Date Source",
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        collapsible = TRUE,
        tags$h5(
          "Our data about confirmed cases and deaths come from the COVID-19 Data Repository
                         by the Center for Systems Science and Engineering (CSSE) at John Hopkins University.
                         The CSSE is a research collective housed within the Department of Civil and Systems
                         Engineering (CaSE) at Johns Hopkins University (JHU). It utilizes the expertise of r
                         esearchers from the schools of Medicine, Public Health, Nursing, Arts and Sciences,
                         Business, and Education; and from JHU's Applied Physics Laboratory, already one of
                         the nation's leading centers of systems engineering.",
          style = "color:Black"
        ),
        fluidRow(column(
          6, tags$img(
            src = "photo2.jpg",
            height = 240,
            width = 350
          )
        ),
        column(
          6, tags$img(
            src = "photo1.png",
            height = 240,
            width = 350
          )
        ))
      )
      
      
    ),
    # End for fluidRow - Date Source'
    
    
    fluidRow(
      box(
        title = "  Research Question & Method Analysis",
        solidHeader = TRUE,
        status = "danger",
        width = 12,
        collapsible = TRUE,
        tags$h5(
          "In this project, we will build a web application the provides a
                       visual summary of a series of variables, average daily cases, average
                       positive test rate, fatality rate, total cases, and uncertainty interval. ",
          style = "color:Black"
        ),
        tags$li(
          "Question 1: Check the number of Covid-19 new cases and vaccination rate in different countries and note the trend"
        ),
        tags$li(
          "Question 2: Explore how potential indicators that we are
                      interestedd would affect COVID-19 positive rate and death
                      rate around the world"
        ),
        tags$li(
          "Question 3: Explore trends in vaccination rates in countries around the world, as well as across continents and globally."
        ),
        br(),
        h5(strong("Methodology and Analysis")),
        h5(
          "To analyze data and explore keys to our research questions, we firstly use a leaflet map to display an overview of Covid-19 new cases and vaccination across the globe on a specifc date.
                Line plots are also used if users are intereted in looking at the trend for a certain country. In the Potential Indicator tab, we use plotly to draw an interactive scatter plot for users' interested x-asis (indicator) and y-axis (outcome).
                Users may also explore how distinct indicators would affect positive cases/deaths by selecting wanted x and y variables. In the Recovery State tab, We made an animation over time to show how vaccination rates have changed in each country.
                Also, users can see the area chart that shows relative changes in vaccination trends across continents and the line chart that shows cumulative number growth globally.
                   In the last tab, a data table is also provided for viewers to see the exact number of confirmed case and death in each country at different time period."
        )
        
      )
    ),
    # End for fluidRow - Research Question'
    
    
    fluidRow(
      box(
        title = "  Reference",
        solidHeader = TRUE,
        status = "info",
        width = 12,
        collapsible = TRUE,
        
        tags$a(href = "https://www.arcgis.com/apps/dashboards/bda7594740fd40299423467b48e9ecf6",
               "COVID-19 Dashboard by the Center for CSSE at JHU"),
        br(),
        tags$a(href = "https://www.mayoclinic.org/coronavirus-covid-19/map?mc_id=google&campaign=12619887048&geo=9007915&kw=covid%2019%20cases&ad=509662149064&network=g&sitetarget=&adgroup=120372571336&extension=&target=kwd-890476467331&matchtype=p&device=c&account=7470347919&placementsite=enterprise&gclid=CjwKCAjwieuGBhAsEiwA1Ly_nQlVZwKxf1fDUEeV79ggBXP90mGoSbMqMGz8VZAW0wv10UBWb0xM-xoCxrEQAvD_BwE",
               "U.S. coronavirus map: What do the trends mean for you?"),
        br(),
        tags$a(href = "https://github.com/owid/covid-19-data",
               "Our World in Data (OWID)/covid -19-data"),
        br(),
        tags$a(href = "https://www.kaggle.com/paultimothymooney/latitude-and-longitude-for-every-country-and-state",
               "Latitude and Longitude for Every Country and State")
        
        
      )
    ),
    # End for fluidRow - Reference'
    fluidRow(
      box(
        title = "  Team Members",
        solidHeader = TRUE,
        status = "success",
        width = 12,
        collapsible = TRUE,
        tags$h5(
          strong(
            "Johns Hopkins Carey Business School MS Business Analytics &
                         Risk Management Candidates "
          ),
          style = "color:Black"
        )
      )
      
    ) # End for Team Members
    
  ),
  
  tabItem(
    "Covid-19Map",
    h2("Overview of Covid-19 New Cases and Vaccination Status"),
    fluidRow(
      box(
        title = "How to use",
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        collapsible = TRUE,
        h5(
          strong("The World Map"),
          "gives an overview of Covid-19 new cases and vaccination status at different countries on a specific date. You can interact with the map by choosing different dates, and check the impact of the pandemic at different locations on that day."
        ),
        h5(
          "Also, the 2 checkboxes below give you options to show Top 5 countries with most daily new cases, or to show Top 5 countries with least people vaccinated."
        ),
        h5(
          strong("The three Line Plots"),
          "display the number of new cases/death rate/vaccination rate for a chosen country. You may interact with the line plots by choosing a continent >>> then a country, and check the trends for these 3 rates for this particular country."
        )
      )
    ),
    fluidRow(
      box(
        title = "Map",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        collapsible = TRUE,
        column(4,
               fluidRow(
                 dateInput(
                   "date1",
                   "Date:",
                   min = "2020-01-01",
                   max = "2021-06-20",
                   value = "2021-05-05"
                 )
               )),
        column(8,
               fluidRow(
                 leafletOutput("map1", height = 350, width = 645),
                 checkboxInput("MostNew", label = "Show Top5 Countries with most new cases", value = FALSE),
                 checkboxInput("LeastVac", label = "Show Top5 Countries with least people vaccinated", value = FALSE)
               ))
      ),
      fluidRow(
        box(
          title = "Lines",
          solidHeader = TRUE,
          status = "danger",
          width = 12,
          collapsible = TRUE,
          fluidRow(column(2),
                   column(4,
                          fluidRow(
                            selectInput(
                              "selectcontinent",
                              "Continent:",
                              choices = unique(covidAll$continent)
                            )
                          )),
                   column(4,
                          fluidRow(
                            selectInput("selectcountry", "Country:", choices = unique(covidAll$location))
                          ))),
          fluidRow(
            column(4, plotlyOutput("line1")),
            column(4, plotlyOutput("line2")),
            column(4, plotlyOutput("line3"))
          )
        )
      ),
      
      tags$head(
        tags$style(
          ".myRow1{height:300px;}.myRow2{height:100px;}.myRow3{height:300px;}"
        )
      )
    )
  ),
  
  
  
  
  tabItem(
    "Indicators",
    h2(
      "Explore How Potential Indicators Affect COVID Positive Rate & Death Rate Around the World"
    ),
    fluidRow(
      box(
        title = "How to use",
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        collapsible = TRUE,
        h5(
          strong("The Sidebar Panel: "),
          "Users could choose one outcome and one indicator from
                            the bottom left panel at a time to observe their relationship in the figure"
        ),
        h5(strong("The Figure: ")),
        h5("1. Hover around to see dots' information"),
        h5(
          "2. Click on each continent's name in the legend to filter dots based on continents"
        ),
        h5(
          "3. Select and zoom in and out for specific areas that you are interested"
        ),
        h5("4. Explore more functions in the toolbox on the right head corner")
      )
    ),
    fluidRow(
      box(
        title = "Figure",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        collapsible = TRUE,
        plotlyOutput("plot_indicator")
      )
    ),
    
    fluidRow(
      box(
        title = "Panel & Conclusion",
        solidHeader = TRUE,
        status = "danger",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(
            4,
            h4(strong("Choose your indicator & outcome")),
            selectInput(
              inputId = "Outcome",
              label = "Outcome",
              choices = colnames(data_indicator_final)[c(4, 5)]
            ),
            h6(
              "* log_total_positive_cases_per_million indicates the logarithm
              of the actual number of total positive cases per million in order
              to fit all data properly in the same figure"
            ),
            selectInput(
              inputId = "Indicator",
              label = "Indicator",
              choices = colnames(data_indicator_final)[c(6:15)]
            )
          ),
          
          column(
            7,
            h4(strong("Conclusion:")),
            h6(
              "1. This interactive figure is trying to provide some insights for
              exploring potential impacts from the 10 indicators on COVID-19 positive
              rate and COVID-19 death rate around the world. It includes data from nearly
              200 different countries and districts between January 2020 to June 2021."
            ),
            h6(
              "2. We expect to identify the potential relationship between death rate from
              cardiovascular disease (diabetes prevalence, smoking behavior)
              and COVID positive rate (COVID death rate) but there are no clear
              correlations based on the graphs. And many indicators do not show clear
              global effects but rather intercontinental effects."
            ),
            h6(
              "3. Regardless of other factors, when population density increases,
              total positive cases per million and total deaths per million show clear
              growths in Asia and Europe."
            ),
            h6(
              "4. Regardless of other factors, when the share of aging population
              (age >=65) increases,  total positive cases per million and total
              deaths per million would increase on average. "
            ),
            h6(
              "5. Regardless of other factors, when GDP per capita increases,
              total positive cases per million and total deaths per million would
              increase in Africa and North America."
            ),
            h6(
              "6. There is a positive correlation between hospital beds per thousand
           people (share of population with basic handwashing facilities, life
           expectancy, Human Development Index) and total positive cases per million.
           However, it seems ambiguous since these indicators imply better public
           health conditions. If people who are interested in this topic,
           they could try on controlling for more variables and make observations."
            )
          )
        )
      )
    )
  ),
  
  tabItem(
    "Recoverystatus",
    h2("Overview of Covid-19 Vaccination Status"),
    fluidRow(
      box(
        title = "How to use",
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        collapsible = TRUE,
        h5(strong("The Exhibition: ")),
        h5(
          "1.",
          strong("Changes in vaccination rates over time in different location"),
          "shows the number of vaccination in some countries with the highest vaccination rates between January 2020 and June 2021.
                            ( Two-time vaccination to be considered fully vaccination.)"
        ),
        h5(
          "2.",
          strong("COVID-19 vaccine doses administered by continent"),
          "shows the relative proportion of vaccinated people by continent between January 2020 and June 2021."
        ),
        h5(strong("The Panel: ")),
        h5(
          "1.The Slider panel allows you to control the date shown in the left image and the animation switch."
        ),
        h5(
          "2.Users can choose the number of countries to display by adjusting the number on the panel."
        ),
        h5(
          "3.Relative checkbox below the right figure is used to determine whether the figure shows the Relative proportion or the cumulative number"
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Exhibition",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        collapsible = TRUE,
        column(6,
               plotOutput("plot_Recovery1")),
        column(
          6,
          plotOutput("plot_Recovery2"),
          checkboxInput("Relative", label = "Relative", value = TRUE)
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Panel & Obsevation",
        solidHeader = TRUE,
        status = "danger",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(
            4,
            h4(strong("Start animation")),
            sliderInput(
              "Time",
              "Time",
              min = as.Date("2020-01-01"),
              max = as.Date("2021-06-01"),
              value = as.Date("2021-01-01"),
              timeFormat = "%b %Y",
              step = 31,
              animate = animationOptions(interval = 1000, loop = FALSE)
            ),
            h6(""),
            numericInput(
              "num",
              "Number of countries to show:",
              value = 10,
              min = 5,
              max = 20,
              step = 1
            )
          ),
          
          column(
            7,
            h4(strong("Observation:")),
            h5(
              "1. Global vaccination began in January 2021, and is gradually recovering from the pandemic."
            ),
            h5(
              "2. European countries have been relatively quick to respond to the Covid-19, starting vaccination first and fastest, followed by Asia and North America. "
            ),
            h5(
              "3. By June 2020, nearly 500 million people had been vaccinated worldwide, including nearly 200 million in North America and more than 100 million in Europe and Asia."
            ),
            h5(
              "4. While other countries are in a rapid recovery phase, countries in Africa and Oceania are still recovering slowly, with only a small percentage of people getting vaccinated. "
            ),
            h5(
              "5. Most countries with vaccination rates greater than 50 today have relatively small populations, making it easier to complete vaccination.
                              However, in countries with a large population and the highest vaccination rate, such as the United Kingdom (45.79%) and the United States (45.05%), less than half of the population has completed two vaccinations. "
            )
          )
        )
      )
    )
    
  ),
  tabItem(
    "datatable",
    dataTableOutput("t1"),
    a("Data Source", href = "https://ourworldindata.org/covid-vaccinations?country=USA", target =
        "_blank")
  )
))

ui = dashboardPage(header, sidebar, body, skin = "blue")
server <- function(input, output, session) {
  # Shiyi's part
  countryname = reactive({
    covidAll %>% filter(continent == input$selectcontinent) %>% distinct(location)
  })
  observe({
    updateSelectInput(session, "selectcountry", choices = countryname())
  })
  
  
  filterDate = reactive({
    covidAll %>%
      filter(date == input$date1)
  })
  
  filterLoca = reactive({
    covidAll %>%
      filter(location == input$selectcountry)
  })
  
  
  output$map1 <- renderLeaflet({
    pal <- colorNumeric(
      palette = c("YlOrRd"),
      domain = filterDate()$people_vaccinated_per_hundred,
      reverse = TRUE
    )
    
    addLegendCustom <-
      function(map, colors, labels, sizes, opacity = 0.5) {
        colorAdditions <-
          paste0(colors,
                 "; border-radius:50%; width:",
                 sizes,
                 "px; height:",
                 sizes,
                 "px")
        labelAdditions <-
          paste0(
            "<div style='display: inline-block;height: ",
            sizes,
            "px;margin-top: 5px;line-height: ",
            sizes,
            "px;'>",
            labels,
            "</div>"
          )
        return(
          addLegend(
            map,
            colors = colorAdditions,
            labels = labelAdditions,
            opacity = opacity
          )
        )
      }
    
    
    mostnew = filterDate() %>%
      arrange(-new_cases_smoothed_per_million) %>%
      head(5)
    leastvac = filterDate() %>%
      arrange(people_vaccinated_per_hundred) %>%
      head(5)
    
    vacIcons <- icons(
      "https://www.pngrepo.com/png/129412/512/vaccine.png",
      iconWidth = 30,
      iconHeight = 30
    )
    
    virusIcons <- icons(
      "https://cdn.pixabay.com/photo/2020/04/29/07/54/coronavirus-5107715_1280.png",
      iconWidth = 30,
      iconHeight = 30
    )
    
    m = leaflet(data = filterDate()) %>%
      setView(lat = 40,
              lng = 15,
              zoom = 1.5) %>%
      addTiles() %>%
      addCircles(
        lat =  ~ latitude,
        lng =  ~ longitude,
        weight = 3,
        radius =  ~ new_cases_smoothed_per_million * 500,
        color =  ~ pal(people_vaccinated_per_hundred),
        popup =  ~ location
      ) %>%
      addLegend(
        "topright",
        pal = pal,
        values =  ~ people_vaccinated_per_hundred,
        title = "% People Vaccinated",
        na.label = "No data"
      ) %>%
      addLegendCustom(
        colors = c("white", "black", "black", "black"),
        labels = c("New Cases", "0", "500", "1000"),
        sizes = c(0, 6, 15, 25)
      )
    
    if (input$MostNew == TRUE) {
      m = m %>%
        addMarkers(
          data = mostnew,
          lat =  ~ latitude,
          lng =  ~ longitude,
          label =  ~ location,
          icon = virusIcons
        )
    }
    m
    
    if (input$LeastVac == TRUE) {
      m = m %>%
        addMarkers(
          data = leastvac,
          lat =  ~ latitude,
          lng =  ~ longitude,
          label =  ~ location,
          icon = vacIcons
        )
    }
    m
    
  })
  
  
  output$line1 <- renderPlotly({
    line1 = ggplot(data = filterLoca()) +
      geom_line(mapping = aes(x = date, y = new_cases_smoothed_per_million)) +
      labs(x = "Date", y = "New cases per million", title = "New Cases") +
      scale_x_date(date_labels = "%Y-%b-%d", date_breaks = "5 months")
    line1 + theme(panel.background = element_rect(fill = "lightyellow"))
  })
  
  
  output$line3 <- renderPlotly({
    line2 = ggplot(data = filterLoca()) +
      geom_line(mapping = aes(x = date, y = people_vaccinated_per_hundred)) +
      labs(x = "Date", y = "People vaccinated per hundred", title =
             "Vaccination Rate") +
      scale_x_date(date_labels = "%Y-%b-%d", date_breaks = "5 months")
    line2 + theme(panel.background = element_rect(fill = "lightyellow"))
  })
  
  
  output$line2 <- renderPlotly({
    line3 = ggplot(data = filterLoca()) +
      geom_line(mapping = aes(x = date, y = new_deaths_smoothed_per_million)) +
      labs(x = "Date", y = "New deaths per million", title = "Death Rate") +
      scale_x_date(date_labels = "%Y-%b-%d", date_breaks = "5 months")
    line3 + theme(panel.background = element_rect(fill = "lightyellow"))
  })
  
  
  # Yixin's part
  output$plot_indicator = renderPlotly({
    p_indicator = data_indicator_final %>% drop_na(input$Outcome) %>% drop_na(input$Indicator) %>%
      ggplot(aes_string(x = input$Indicator, y = input$Outcome)) +
      geom_point(aes(color = continent)) +
      theme_wsj() +
      theme(
        plot.title = element_text(size = rel(0.8), hjust = 0.5),
        legend.title = element_text(size = rel(0.5), face = "bold"),
        text = element_text(family = "Helvetica Neue"),
        axis.title = element_text(face = "bold", size = rel(0.6)),
        panel.grid.major = element_line(
          size = 0.1,
          linetype = 'dashed',
          colour = "white"
        ),
        legend.background = element_rect(
          fill = "white",
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      labs(color = "Continent:  ") +
      guides(color = guide_legend(title = "Continent: "))
    ggplotly(p_indicator)
    
  })
  
  #Ziming's part
  
  
  
  
  output$plot_Recovery1 <- renderPlot({
    thisYear <- as.numeric(substr(input$Time, 1, 4))
    thisMonth <- as.numeric(substr(input$Time, 6, 7))
    data$location <- reorder(data$location, -data$vaccination_rate)
    newdata <-
      aggregate(vaccination_rate ~ month + year + location,
                data = data,
                FUN = "max")
    newdata <- na.omit(newdata)
    filterData <- newdata %>%
      filter(newdata$year == thisYear, newdata$month == thisMonth)
    filterData$location <-
      reorder(filterData$location, filterData$vaccination_rate)
    
    cnum = input$num
    LocationToShow = filterData %>%
      arrange(-vaccination_rate) %>%
      pull(location) %>%
      head(cnum)
    
    p1 <- filterData %>%
      filter(location %in% LocationToShow) %>%
      ggplot(aes(location, vaccination_rate, fill = location)) +
      geom_bar(stat = "identity") +
      geom_text(aes(
        label = vaccination_rate,
        hjust = 1.2,
        fontface = "bold"
      )) +
      annotate(
        "text",
        x = 5,
        y = 85,
        size = 20,
        color = "grey80",
        label = month.abb[thisMonth]
      ) +
      labs(title = "Changes in vaccination rates over time by location",
           caption = "Source:National government reports",
           y = "vaccination rate(%)") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      coord_flip() +
      theme_get() +
      theme(
        plot.title = element_text(
          size = 22,
          face = "bold",
          hjust = 0.5
        ),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(size = 10)
      )
    
    p1
    
    
    
  })
  output$plot_Recovery2 <- renderPlot({
    continent_name <-
      c("Africa",
        "Asia",
        "Europe",
        "North America",
        "Oceania",
        "South America")
    data_c <- data %>%
      filter(continent %in% continent_name)
    cum <-
      aggregate(people_fully_vaccinated ~ date + continent,
                data = data_c,
                FUN = sum)
    sum <-
      aggregate(people_fully_vaccinated ~ date,
                data = data_c,
                FUN = sum)
    data_con <- inner_join(cum, sum, by = "date")
    data_con <- data_con %>%
      mutate(rate = if_else(
        people_fully_vaccinated.y != 0,
        as.numeric(people_fully_vaccinated.x) / as.numeric(people_fully_vaccinated.y),
        0
      ))
    
    if (input$Relative == TRUE) {
      p2 <- data_con %>%
        ggplot(aes(x = date, y = rate, fill = continent)) +
        geom_area() +
        scale_x_date(limits = c(as.Date("2021-01-01"), as.Date("2021-07-01"))) +
        scale_y_continuous(labels =  percent) +
        theme_get() +
        theme(
          plot.title = element_text(
            size = 22,
            face = "bold",
            hjust = 0.5
          ),
          axis.title = element_text(face = "bold", size = 12),
          legend.title = element_text(size = 10, face = "bold"),
          plot.caption = element_text(size = 10)
        ) +
        labs(
          title = "COVID-19 vaccine administered by continent",
          color = "",
          fill = "",
          caption = "Source: Our World in Data",
          y = "",
          x = "Date"
        )
      p2
    }
    else{
      data_cum <- data_con %>%
        mutate(
          people_fully_vaccinated.x = people_fully_vaccinated.x / 1000000,
          people_fully_vaccinated.y = people_fully_vaccinated.y /
            1000000
        )
      p3 <- ggplot(data_cum) +
        geom_smooth(aes(
          x = date,
          y = people_fully_vaccinated.x,
          group = continent,
          color = continent
        )) +
        geom_smooth(aes(x = date, y = people_fully_vaccinated.y), color =
                      "grey80") +
        annotate(
          "text",
          x = as.Date("2021-04-01"),
          y = 400,
          size = 10,
          color = "grey80",
          label = "Overall"
        ) +
        scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2021-07-01"))) +
        labs(
          title = "COVID-19 vaccine doses administered by continent",
          color = "",
          caption = "Source: Our World in Data",
          y = "Number of fully vaccinated (million)",
          x = "Date"
        ) +
        theme_get() +
        theme(
          plot.title = element_text(
            size = 22,
            face = "bold",
            hjust = 0.5
          ),
          axis.title = element_text(face = "bold", size = 12),
          legend.title = element_text(size = 10, face = "bold"),
          plot.caption = element_text(size = 10)
        )
      
      
      
      
      p3
    }
  })
  
  # Data table
  output$t1 = renderDataTable(datatable(covidAll, options = list(scrollX =
                                                                   TRUE)))
  
  
}

shinyApp(ui = ui, server = server)
