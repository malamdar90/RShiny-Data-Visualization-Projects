library(shiny)
library(plotly)
library(shinydashboard) 
library(shinydashboardPlus) 
library(leaflet)
library(DT)
library(lattice)
library(grid)
library(chron)
library(httr)
library(tidyverse)
library(scales)
library(lubridate)
library(openintro)
library(gapminder)
library(gganimate)
library(sf)
library(tmap)
library(spData)
library(viridis)
library(dashboardthemes)

ui <- dashboardPage(
  header = dashboardHeader(title = tags$strong("PROJECT 007")),
  skin="black",
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("hospital")),
      menuItem("COVID-19 Cases Status", tabName = "page2", icon = icon("medkit"),
               menuSubItem(
                 "Overview", tabName = "page8", icon = icon("hospital-o")
               ),
               menuSubItem(
                 "COVID-19 Trend", tabName = "page3", icon = icon("line-chart")
               ),
               menuSubItem(
                 "Cases In States", tabName = "page4", icon = icon("map")
               )
      ),
      # 
      menuItem("COVID-19 And Air Quality", tabName = "page5", icon = icon("thermometer-3"),
               menuSubItem(
                 "Overview", tabName = "page10", icon = icon("hospital-o")
               ),
               menuSubItem(
                 "Air Quality Trend", tabName = "page7", icon = icon("line-chart")
               ),
               menuSubItem(
                 "Air Quality Map", tabName = "page6", icon = icon("map")
               )
      )
    )
  ),
  
  body = dashboardBody(
    #change the color to the theme color
    tags$style(HTML(".box.box-solid.box-info>.box-header {color:#fff;background:#2d3c42}
                .box.box-solid.box-info{
                border-bottom-color:#ecf0f5;
                border-left-color:#ecf0f5;
                border-right-color:#ecf0f5;
                border-top-color:#ecf0f5;}"
    )),
    tabItems(
      tabItem(
        tabName = "home",
        box(
          title = "Motivation",status="info",solidHeader = TRUE, width = 12,
          #item 1
          "This year, we faced an unprecedented challenge that interrupts everyone’s life from Coronavirus (COVID-19). We have heard in daily news that the number of confirmed cases is soaring. 
                    Starting from one case in Washington State, the number of COVID-19 cases in the United States currently ranks the highest around the globe. Moreover, there has been a dynamic change in 
                    the severity of the spread among the states. Based on this background, in this project, we aim to visualize the growth trend of the total number of cases and the dynamic change of the severity of 
                    the spread of the diseases among the states.",
          br(), 
          br(),
          #item 2 motivation
          "Except for exploring the COVID-19 cases, we also want to focus on effects of COVID-19 on a specific aspect of our live.
                    Since the early days of COVID-19, scientists and civilians have witnessed improvement in air quality,
                    especially over quarantined regions. United States has enacted curfew and shut down a few businesses to
                    prevent pandemic from expansion. It's like a environmental experiment and we want to visually show how air condition change during the shut-down in the United States.",
          collapsible = TRUE
        ),
        box(
          title = "Research Questions", status="info",solidHeader = TRUE, width = 12,
          tags$strong("COVID-19 Cases Status: "),
          br(),
          "How does the number of COVID-19 cases (New/Death/Current/Confirmed) in the United States and in each state change over time?", 
          br(), 
          tags$strong("COVID-19 And Air Quality: "),
          br(),
          "How does the shutdown during COVID-19 affect the air quality in the United States?",
          collapsible = TRUE
        ),
        box(title="References and Further Readings",status="info",solidHeader = TRUE,width = 12,
            #References 
            tags$strong("References:"),
            br(),
            uiOutput("reference"),
            
            #Further Readings
            tags$strong("Further Readings:"),
            br(),
            uiOutput("reading"),
            
            collapsible = TRUE)
       
      ),
      
      tabItem(
        tabName = "page8",
        box(
          title = "Research Question 1", status="info",solidHeader = TRUE, width = 12,
          tags$strong("How does the number of COVID-19 cases (New/Death/Current/Confirmed) in the United States and in each state change over time?"),
          collapsible = TRUE
        ),
        box(
          title = "Description of Datasets", status = "info", solidHeader = TRUE, width = 12,
          fluidPage(
            uiOutput("covidsummary"),
            br(),
            DT::dataTableOutput("tablecountry")
          ),
          collapsible = TRUE
        ),
        box(
          title = "Data Cleaning", status = "info", solidHeader = TRUE, width = 12,
          uiOutput("timeseries"),
          collapsible = TRUE
          #item 1
          
        ),
        box(
          title = "Methodology", status = "info", solidHeader = TRUE, width = 12,
          uiOutput("method1"),
          collapsible = TRUE
          
        ),
        box(
          title = "Analysis", status = "info", solidHeader = TRUE, width = 12,
          uiOutput("q1analysis"),
          collapsible = TRUE
        )
      ),
      tabItem(
        tabName = "page2"
      ),
      tabItem(
        tabName = "page3",
        fluidPage(
          fluidRow(
            box(
              selectInput(inputId = "range","Scope",
                          choices=c("Country","State"),selected="Country"),
              conditionalPanel(
                condition = "input.range =='State'",
                selectInput("region1","State",
                            choices=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                                      "Colorado", "Connecticut","Delaware", "Florida", "Georgia", 
                                      "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",  
                                      "Kansas", "Kentucky", "Louisiana","Maine", "Maryland",  
                                      "Massachusetts", "Michigan", "Minnesota", "Mississippi","Missouri",  
                                      "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                      "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",  
                                      "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",  
                                      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",  
                                      "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),selected="Maryland")
              ),
              width=12
            ),
            tags$style(HTML(".small_icon_test { font-size: 40px; }")),
            valueBoxOutput("confirmbox",width=3),
            valueBoxOutput("deathbox",width=3),
            valueBoxOutput("recoveredbox",width=3),
            valueBoxOutput("currentbox",width=3),
            
            box(title="Confirmed Cases",status="primary",solidHeader = TRUE,collapsible = TRUE,
                fluidPage(
                  selectInput(inputId = "input5","Scale",
                              choices=c("Value","Log")),
                  plotlyOutput("plot1",height = 400))),
            box(title="Death Cases",status="danger",solidHeader = TRUE,collapsible = TRUE,
                fluidPage(
                  selectInput(inputId = "input6","Scale",
                              choices=c("Value","Log")),
                  plotlyOutput("plot2",height = 400))),
            box(title="Recovered Cases",status="success",solidHeader = TRUE,collapsible = TRUE,
                fluidPage(
                  selectInput(inputId = "input7","Scale",
                              choices=c("Value","Log")),
                  plotlyOutput("plot3",height = 400))),
            box(title="Current Cases",status="warning",solidHeader = TRUE,collapsible = TRUE,
                fluidPage(
                  selectInput(inputId = "input8","Scale",
                              choices=c("Value","Log")),
                  plotlyOutput("plot4",height = 400)))
          )
        )
      ),
      tabItem(
        tabName = "page10",
        box(
          title = "Research Question 2", status="info",solidHeader = TRUE, width = 12,
          tags$strong("How does the shutdown during COVID-19 affect the air quality in the United States?"),
          collapsible = TRUE
        ),
        
        box(
          title = "Description of Datasets", status = "info", solidHeader = TRUE, width = 12,
          #item 2
          uiOutput("epa"),
          collapsible = TRUE
        ),
        box(
          title = "Data Cleaning", status = "info", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          #item 2 content
          uiOutput("airqualitydata"),
          uiOutput("dct2"),
          box(
            title = "US Air Quality Data Variables", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE,collapsed = TRUE,
            uiOutput("dataintro2")
            #introduction of data sets
          )
        ),
        box(
          title = "Methodology", status = "info", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          #item 2 content
          uiOutput("method2")
        ),
        box(
          title = "Analysis", status = "info", solidHeader = TRUE, width = 12,
          collapsible = TRUE,
          uiOutput("q2analysis")
        )
      ),
      tabItem(
        tabName = "page4",
        fluidPage(
          dateInput("startdate","Date",value="2020-07-14"),
          br("Data available from 2020-01-22 to 2020-07-14"),
          br(),
          leafletOutput("changeplot"),
          br(),
          fluidRow(
            box(
              title = "COVID-19 cases developement in U.S.", status = "info", 
              solidHeader = TRUE, 
              collapsible = TRUE,
              HTML('<iframe width="450" height="300" 
                             src="https://www.youtube.com/embed/anIaYCwPlC0" 
                             frameborder="0" allowfullscreen></iframe>')
            ),
            box(
              title = "Confirmed cases in each state (Top 10)", status = "info", 
              solidHeader = TRUE, 
              collapsible = TRUE,
              HTML('<iframe width="450" height="300" 
                             src="https://www.youtube.com/embed/q5k8ZCn3_A8" 
                             frameborder="0" allowfullscreen></iframe>')
            )
          )
        )
      ),
      tabItem(
        tabName = "page5"
      ),
      tabItem(
        tabName = "page6",
        dateInput("oneday", label = h3("Select a date"), value = "2020-01-01"),
        br("Data available from 2020-01-01 to 2020-07-04"),
        br(),
        leafletOutput("plot9", height=500)
      ),
      tabItem(
        tabName = "page7",
        fluidPage(
          title = "PM 2.5 Daily AQI", status = "primary", 
          solidHeader = TRUE, collapsible = TRUE,
          selectInput(inputId = "input9", label = "Pollutant",
                      choices = c("PM2.5","CO","NO2","PM10","SO2")),
          selectInput(inputId = "region5", label = "States",
                      choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                                  "Colorado", "Connecticut","Delaware", "Florida", "Georgia", 
                                  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",  
                                  "Kansas", "Kentucky", "Louisiana","Maine", "Maryland",  
                                  "Massachusetts", "Michigan", "Minnesota", "Mississippi","Missouri",  
                                  "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",  
                                  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",  
                                  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",  
                                  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")),
          plotlyOutput("plot10",height = 200),
          br(),
          br(),
          plotlyOutput("plot11",height = 500)
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  options(tigris_use_cache = TRUE)
  #references & further readings
  output$reference<-renderUI(
    tags$ul(
      tags$li(a("Global COVID-19 Tracker & Interactive Charts ", href="https://coronavirus.1point3acres.com/en/test")),
      tags$li(a("Tmap guideline ", href="https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html")),
      tags$li(a("Plotly and subplot ", href="https://plotly.com/r/subplots/")),
      tags$li(a("Shiny Dashboard Structure ", href="https://rstudio.github.io/shinydashboard/structure.html#valuebox")),
      tags$li(a("Top 100 R sources on Novel COVID-19 Coronavirus ", href="https://towardsdatascience.com/top-5-r-resources-on-covid-19-coronavirus-1d4c8df6d85f")),
      tags$li(a("Leaflet for R", href="https://rstudio.github.io/leaflet/shapes.html")),
      tags$li(a("Air Quality Index", href="https://en.wikipedia.org/wiki/Air_quality_index")),
      tags$li(a("United States Environmental Protection Agency", href="https://www.epa.gov")),
      tags$li(a("Calendar Heatmap-Visualise time series data in calendar style", href="https://dominikkoch.github.io/Calendar-Heatmap/"))
    ))
  
  output$reading<- renderUI(
    tags$ul(
      tags$li(a("Pandemic’s Cleaner Air Could Reshape What We Know About the Atmosphere", href="https://www.nytimes.com/2020/06/25/climate/coronavirus-clean-air.html")),
      tags$li(a("Has the coronavirus shutdown improved air quality? Is our air really cleaner?", href="https://whyy.org/segments/has-the-coronavirus-shutdown-improved-air-quality/"))
    ))
  
  ######url link for dataset source & introduction text
  
  #item1
  url3<-a("Johns Hopkins University Center for Systems Science (organized version)", href="https://github.com/CSSEGISandData/COVID-19")
  url4<-a("The COVID Tracking Project", href="https://covidtracking.com/data/download")
  
  output$covidsummary <- renderUI({
    tagList(tags$strong("COVID-19 Data Summary: "),
            br(),
            "We collected two datasets, one from ",url3," and the other from ",url4,".",
            br(),
            br(),
            "The first dataset carefully recorded confirmed cases by day for every county of every state from January 22, 2020, 
                when the first case was diagnosed, to July 14, 2020. The dataset in total contains 188 variables, the majority of which are date variables from January to July.",
            br(),
            br(),
            "The second dataset documented death/recovered/current cases for the country and each state during the same period. The dataset contains 25 variables, recording 
                information of test-positive cases, test-negative cases, test-pending cases, hospitalized cases, patient-on-ventilator cases, recovered cases, death cases and 
                daily-positive-increase cases for each state. The datasets also recorded cases from Grand Princess Cruise and other regions, which we do not take into consideration.",
            
    )})
  ## item2
  url1 <- a("United States Environmental Protection Agency", href="https://www.epa.gov/outdoor-air-quality-data")
  output$epa <- renderUI({
    tagList(tags$strong("Air Quality Data: "),"The data for the US air quality is from", url1,".",
            br(),
            "The datasets are open source that can be downloaded by selecting different state and pollutants. We download and clean 5 pollutant data (PM2.5, CO, NO2, PM10, SO2) 
                from each state during the time period from 2020-01-01 to 2020-07-04.",
            br(),
            br(),
            tags$strong("US States Map Data: "),"The state map data which used to draw polygons is imported from a R library named “tigris”."
            
            
    )})
  
  #####data cleanining text
  
  #item 1
  #dataset link
  url5 <- a("Country", href="https://drive.google.com/file/d/1Ee9NT4qFwdfTo6O53T8w5Vd8_yYtyx9b/view?usp=sharing")
  url6 <- a("State", href="https://drive.google.com/file/d/1h0ZwtE1_Q4xT0F0NO1mifbHnw1hc0tln/view?usp=sharing")
  url7 <- a("State Comparison Data", href="https://drive.google.com/file/d/1UkBMQit0vWf43rZjkiyNayb9sYzh0har/view?usp=sharing")
  
  #description
  output$timeseries<-renderUI({
    tagList(tags$strong("Time Series Data "),"(",url5,"&",url6,")",
            br(),
            tags$ul(
              tags$li("We extracted only date, test-positive cases, recovered cases, and death cases for each state and compute current cases by subtracting recovered and death cases from test-positive cases."),
              tags$li("We also created subset of the dataset for the latest date of each state."),
              tags$li("There are many NA values in the dataset and we turned them into zero for computation of current cases.")
              
            ),
            url7,
            tags$ul(
              tags$li("Firstly, we only extracted the state columns and all the date columns. "),
              tags$li("Secondly, since each county takes up a row, we used the summarise and group_by function to get the num of confirmed cases in each state. "),
              tags$li("Thirdly, since we would need the dataset in long format, we used the pivot_longer function to convert the date columns into date rows."),
              tags$li("Fourthly, since there are case records from instance such as Grand Princess Cruise, we cross-referred to the us_states dataset to obtain the final dataset.")
              
            )
            
    )})
  
  
  
  #variables of dataset
  output$dataintro2<-renderUI({
    tagList(tags$strong("Variables: "),
            br(),
            tags$ul(
              tags$li("Date: Day of Measuring Air Quality"),
              tags$li("State: The State of United States"),
              tags$li("PM2.5: Daily Mean PM2.5 Concentration"),
              tags$li("CO: Daily Max 8-hour CO Concentration"),
              tags$li("NO2: Daily Max 1-hour NO2 Concentration"),
              tags$li("PM10: Daily Mean PM10 Concentration"),
              tags$li("SO2: Daily Max 1-hour SO2 Concentration"),
              tags$li("AQ_PM2.5:Level of Air Quality Based on PM2.5"),
              tags$li("AQ_CO: Level of Air Quality Based on CO"),
              tags$li("AQ_NO2: Level of Air Quality Based on NO2"),
              tags$li("AQ_PM10: Level of Air Quality Based on PM10"),
              tags$li("AQ_SO2: Level of Air Quality Based on SO2"),
            )
            
    )})
  
  #item2
  #dataset link
  url2 <- a("US Air Quality Data", href="https://docs.google.com/spreadsheets/d/1DxCWvOkMiHs8DZ8rvmfcv18aUp3DUZ7dnailWdZ5UPo/edit#gid=1000836268
")
  output$airqualitydata <- renderUI(url2)
  #description
  output$dct2 <- renderUI(HTML("<ul>
    <li>Since there are more than one monitor sites in each states, we count the number of days each monitor site has and slice the monitor site that has most monitoring days.Then we merge states and pollutants’ together, ending up with a file with 8 variables.</li>
    <li>Merge states and pollutants’ together, ending up with a file with 8 variables.</li>
    <li>From the color theory, we believe it will be better to use categorical data to represent air quality.
        According to official air quality index color, we cut the pollutants’ number based on EPA’s table of breakpoints. 
        By using mutate( ), we create 5 more columns representing the air quality.</li>
    <li>Considering we need to draw polygons on the US states map, we need to join states map data with air quality data, which ending up in a datasets with 22 variables.</li></ul>"))
  
  ######Methodology
  
  #item1: description of methodology
  output$method1<- renderUI(HTML("<ul>
    <li><b>Plotly:</b> We used plotly package to visualize the time series documentation of confirmed, death, recovered, and current cases so that the data for each day could be easily read. </li>
    <li><b>Tmap:</b> We used tmap package to visualize the comparison among states. We used tmap_animation and ImageMagick tool to create a dynamic visualization of the change of the number of cases of each state.</li>  
    <li><b>ggplot2:</b> We use ggplot2 package to generate all bar graphs with confirmed cases in each state (Top 10) and corresponding numbers by date together.</li>  
    <li><b>gganimate:</b> We use gganimate package to render the dynamic bar graph, which requires transition_state() to split the data into multiple dates and specify the relative length of the transition as well as the relative length of the pause at the date. Finally we use animation() to render it into an animation and to control the display speed.</li>  
</ul>"))
  #item2: description of the methodlogy
  output$method2 <- renderUI(HTML("<ul>
    <li><b>Leaflet:</b> Create a function and its only input is the date. By filtering the dataset by date, we can generate different pollutant datasets of whole US states only for a one day. Then we use addPolygons() and fill in colors representing levels of air quality. (Except for PM2.5, other pollutants are not representative enough in the map, therefore we only use air quality index for PM2.5 to show the color.) When we click on the state, there is a popup showing the state’s name and values of pollutants.
</li>
     <li><b>ggplot2:</b> We used ggplot to draw the calendar heat map and line graph to show the trend. In the calendar heat map, we choose gradient colors instead of categorical colors because we found that the air quality of most days locates between level“good”and level“moderate”. The colors of those two levels are hard to tell. So I use gradient colors so that there are more dark colors are involved.
    </li></ul>"))
  
  ####### analysis
  #item1
  
  output$q1analysis<-renderUI(HTML("<ul>
    <li><b>COVID19 Trend: </b>In this section, we can select the different scope, country or state, to monitor the trend. The colored rectangles (valueboxes) show the total confirmed cases, total death cases, total recovered cases, and total current cases by July 14th, 2020. The following line graphs demonstrate the time series trend of each dimension from Jan 22nd to July 14th, and if we want to see when the rate of increase has slowed, we can also scale the graph by selecting logarithm.  </li>
    <li><b>Cases in States: </b>By selecting a date on the top right of the main page, you will see the allocation of confirmed cases in each state and you can also see the detailed information of each state by zooming the map and clicking on the state. The two videos below literally display the change of COVID-19 cases development in U.S and confirmed cases in each state (Top 10) from Jan to July.    </li>
    <li><b>Overall: </b>Since January, novel coronavirus has spread to nearly every state and territory. New York has reported the most cases (403,175) so far and 24,994 deaths till July 14th. Every state is following a different coronavirus trajectory. New York and Washington were hit hard during the pandemic’s early days, for example. But cases and deaths have risen more slowly in other states, some of which haven’t yet hit their “peaks”. Though growth in new cases slowed in late spring, by mid-June new cases began to trend upward nationally. To sum up, the situation of coronavirus in U.S. is still severe and we highly recommend everyone to stay home or put their mask on when going out to protect themselves and slow the spread.</li>                                
                                     </ul>"))
  
  #item2
  output$q2analysis <- renderUI(HTML("
    <div align='center'>
        <img src = 'https://camo.githubusercontent.com/d5b489ed1feefeb5d3af8b6e82e3ea70de264252/68747470733a2f2f696d6167652e6962622e636f2f6776724663352f323031375f30375f32305f31355f32325f32312e706e67'
    width = 80%/>
    <br>,<br>
    </div>
    <ul>
      <li> Here are pollutant concentration breakpoints, which represent the level of air quality.</li>
      <li> It can be obviously seen through the trend that after the middle of March (when every state announced to recommend to shut down all the offline business), the air quality improved a lot, which might be due to the reduction of the use of cars and other transportations. Also, the shutdown of many factories contributed to the better environment.</li>
      <li> However, in May, to recover economics, some states gradually reopened the business and factories. Besides, there were some activities in the late May. So after May, the air quality became even worse than the period before shutdown.</li>
      <li> Therefore, we can conclude that human activities do have great impact on the environment, especially the air quality.</li>
    </ul>"))
  
  
  
  
  
  ##### page3 #####
  country<-read_csv("daily us.csv")
  country$date<- as.Date(as.character(country$date),format="%Y%m%d")
  state<-read_csv("daily state.csv")
  state$date<- as.Date(as.character(state$date),format="%Y%m%d")
  state$state<-abbr2state(state$state)
  
  country<- subset(country,select=c("date","positive","death","recovered","positiveIncrease","deathIncrease"))
  country$recovered[is.na(country$recovered)] <- 0
  country$death[is.na(country$death)]<-0
  country$current <- country$positive - country$recovered - country$death
  
  state<-subset(state,select=c("date","state","positive","death","recovered","positiveIncrease","deathIncrease"))
  state$recovered[is.na(state$recovered)] <- 0
  state$death[is.na(state$death)] <- 0
  state$current <- state$positive - state$recovered - state$death
  
  Countryviz<-function(scale,type,color){
    if(type=="recovered") {Cases=country$recovered}
    else if(type=="current") {Cases=country$current}
    p = ggplot(data=country,mapping=aes(x=date,y=Cases)) + 
      geom_line(color=color)+
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(scale=="Value"){
      q=p + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                               breaks = scales::pretty_breaks(n = 5)) 
    }
    else if(scale=="Log"){
      q=p + scale_y_log10(labels = unit_format(unit = "k", scale = 1e-3))
    }
    
  }
  
  Countryviz2<-function(scale,type,color){
    if(type=="positive") {Total=country$positive}
    else if(type=="death") {Total=country$death}
    if(type=="positive") {DailyIncrease=country$positiveIncrease}
    else if(type=="death") {DailyIncrease=country$deathIncrease}
    
    p = ggplot(data=country,mapping=aes(x=date,y=Total)) + 
      geom_line(color=color)+
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(scale=="Value"){
      q=p + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                               breaks = scales::pretty_breaks(n = 5)) 
    }
    else if(scale=="Log"){
      q=p + scale_y_log10(labels = unit_format(unit = "k", scale = 1e-3))
    }
    
    p2 = ggplot(data=country,mapping=aes(x=date,y=DailyIncrease)) + 
      geom_line(color=color)+
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(scale=="Value"){
      q2=p2 + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                                 breaks = scales::pretty_breaks(n = 5))
    }
    else if(scale=="Log"){
      q2=p2 + scale_y_log10(labels = unit_format(unit = "k", scale = 1e-3))
    }
    
    u<- subplot(q,q2,nrows = 2,shareX = TRUE, titleY=TRUE)
    return(u)
  }
  
  Regionalviz<- function(place,method,type,color){
    filterdata<-filter(state,state==place)
    if(type=="recovered") {Cases=filterdata$recovered}
    else if(type=="current") {Cases=filterdata$current}
    n= ggplot(data=filterdata,mapping=aes(x=date,y=Cases)) +
      geom_line(color=color) + 
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(method=="Value"){
      m=n + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                               breaks = scales::pretty_breaks(n = 5)) 
    }
    else if(method=="Log"){
      m=n + scale_y_log10(labels = unit_format(unit = "k", scale = 1e-3))
    }
    return(m)
  }
  
  Regionalviz2<- function(place,method,type,color){
    filterdata<-filter(state,state==place)
    if(type=="positive") {Total=filterdata$positive}
    else if(type=="death") {Total=filterdata$death}
    
    if(type=="positive") {DailyIncrease=filterdata$positiveIncrease}
    else if(type=="death") {DailyIncrease=filterdata$deathIncrease}
    n= ggplot(data=filterdata,mapping=aes(x=date,y=Total)) +
      geom_line(color=color) + 
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(method=="Value"){
      m=n + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                               breaks = scales::pretty_breaks(n = 5)) 
    }
    else if(method=="Log"){
      m=n + scale_y_log10(labels = unit_format(unit = "k", scale = 1e-3))
    }
    
    
    n2= ggplot(data=filterdata,mapping=aes(x=date,y=DailyIncrease)) +
      geom_line(color=color) + 
      scale_x_date(date_labels="%B") +
      theme(axis.title.x=element_blank(),
            panel.grid = element_line(color="grey90"),
            panel.background = element_blank())
    if(method=="Value"){
      m2=n2 + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 
    }
    else if(method=="Log"){
      m2=n2 + scale_y_log10()
    }
    
    u1<- subplot(m,m2,nrows = 2,shareX = TRUE,titleY = TRUE)
    return(u1)
    
  }
  
  output$plot1<-renderPlotly({
    if(input$range=="Country"){
      Countryviz2(input$input5,"positive","blue")
    }
    else if(input$range=="State"){
      Regionalviz2(input$region1,input$input5,"positive","blue")
    }
  })
  
  output$plot2<-renderPlotly({
    if(input$range=="Country"){
      Countryviz2(input$input6,"death","red")
    }
    else if(input$range=="State"){
      Regionalviz2(input$region1,input$input6,"death","red")
    }
  })
  
  output$plot3<-renderPlotly({
    if(input$range=="Country"){
      Countryviz(input$input7,"recovered","green")
    }
    else if(input$range=="State"){
      Regionalviz(input$region1,input$input7,"recovered","green")
    }
  })
  
  output$plot4<-renderPlotly({
    if(input$range=="Country"){
      Countryviz(input$input8,"current","#ffbc03")
    }
    else if(input$range=="State"){
      Regionalviz(input$region1,input$input8,"current","#ffbc03")
    }
  })
  
  confirmstatevalue<-function(location,situation){
    dataconfirm<-filter(state,date=="2020-07-14")%>%
      filter(state==location)
    if(situation=="positive") {y=dataconfirm$positive}
    else if(situation=="death") {y=dataconfirm$death}
    else if(situation=="recovered") {y=dataconfirm$recovered}
    else if(situation=="current") {y=dataconfirm$current}
    return(y)
  }
  
  confirmcountryvalue<-function(situation){
    dataconfirm<-filter(country,date=="2020-07-14")
    if(situation=="positive") {y=dataconfirm$positive}
    else if(situation=="death") {y=dataconfirm$death}
    else if(situation=="recovered") {y=dataconfirm$recovered}
    else if(situation=="current") {y=dataconfirm$current}
    return(y)
  }
  
  output$confirmbox <- renderValueBox({
    confirm<- if(input$range=="Country"){confirmcountryvalue("positive")}
    else if(input$range=="State"){confirmstatevalue(input$region1,"positive")}
    valueBox(
      value=tags$p(scales::comma(confirm),style="font-size:50%;"),
      subtitle="Total confirmed cases",
      color="blue",icon = icon("hospital-o",class="small_icon_test"))
  })
  output$deathbox <- renderValueBox({
    death <- if(input$range=="Country"){confirmcountryvalue("death")}
    else if(input$range=="State"){confirmstatevalue(input$region1,"death")}
    valueBox(
      value=tags$p(scales::comma(death),style="font-size:50%;"),
      subtitle="Total death cases",
      color="red",icon = icon("plus",class="small_icon_test"))
  })
  output$recoveredbox <- renderValueBox({
    recover <- if(input$range=="Country"){confirmcountryvalue("recovered")}
    else if(input$range=="State"){confirmstatevalue(input$region1,"recovered")}
    valueBox(
      value=tags$p(scales::comma(recover),style="font-size:50%;"),
      subtitle="Total recovered cases",
      color="green",icon = icon("heart-o",class="small_icon_test"))
  })
  output$currentbox <- renderValueBox({
    current <- if(input$range=="Country"){confirmcountryvalue("current")}
    else if(input$range=="State"){confirmstatevalue(input$region1,"current")}
    valueBox(
      value=tags$p(scales::comma(current),style="font-size:50%;"),
      subtitle="Total current cases",
      color="yellow",icon=icon("heartbeat",class="small_icon_test"))
  })
  
  ##### page4 #####
  Confirmed<-read_csv("COVID191.csv")
  data<-dplyr::select(Confirmed, `Province_State`, '1/22/20':'7/14/20')
  
  new<- data %>%
    group_by(Province_State) %>%
    summarise_all(sum)
  
  Date <- colnames(new)[2:176]
  US_ByDate <- new %>% 
    pivot_longer(Date, names_to = "Date", values_to = "Cases")
  
  US_ByDate<- US_ByDate %>% 
    dplyr::rename(NAME = Province_State)
  
  US_ByDate$Date<-as.Date(US_ByDate$Date,format="%m/%d/%y")
  
  data(us_states)
  cov_US <- full_join(us_states, US_ByDate, by="NAME")
  cov_US$abb<-state2abbr(cov_US$NAME)
  cov_US<- cov_US[,-1]
  
  output$changeplot <- renderLeaflet({
    
    datetoshow<-filter(cov_US,Date==input$startdate)
    
    breaks <- c(0, 1,10, 100, 1000, 10000,50000,100000,200000,400000)
    
    map1<- tm_shape(datetoshow) +
      tm_polygons(border.col="white", 
                  lwd=1, 
                  col="Cases",
                  breaks=breaks, 
                  title="Confirmed cases", 
                  palette=plasma(n=6, direction=-1)) +
      tm_text("abb",remove.overlap = TRUE,col="black")+
      tm_layout(bg.color = "white", legend.position=c("right", "bottom")) 
    
    tmap_leaflet(map1,mode="view",show=TRUE)
    
    
  })
  
  #dataset output
  
  
  output$tablecountry = DT::renderDataTable({
    return(datatable(country,rownames=FALSE))
  })
  
  #item 2 datasets and clean data
  #setwd("/Users/cybillsu/Desktop/Summer/Data Visualization/Group Project/Data")
  library(tigris)
  states<-states(cb=T)
  all<-read.csv("item2_all.csv")
  all=all[,-1]
  states$NAME<-tolower(states$NAME)
  #merge state data with air quality data by states
  states_merge_air<-left_join(states,all,by=c("NAME"="state"))
  # cleaned data for item2 part2
  data <- read.csv("item2.csv") %>% select(-1)
  data$date <- as.Date(data$date)
  
  
  #air quality map visualization
  AQmapviz<-function(oneday){
    filteredData<-subset(states_merge_air, as.Date(date)==as.Date(oneday))
    AQI_color<- c("#01e400", "#ffff00", "#ff7e00")
    pal_discrete<-colorFactor(AQI_color,filteredData$AQ_PM2.5)
    
    popup_all_pollutant <- paste0("<strong>", as.character(toupper(filteredData$NAME)) ,
                                  "</strong><br />PM2.5: ",as.character(filteredData$PM2.5)," μg/m3",
                                  "<br />CO: ", as.character(filteredData$CO)," ppm",
                                  "<br />NO2: ", as.character(filteredData$NO2), " ppb",
                                  "<br />PM10: ", as.character(filteredData$PM10)," μg/m3",
                                  "<br />SO2: ", as.character(filteredData$SO2), " ppb")
    
    map<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addPolygons(data = filteredData , 
                  fillColor = pal_discrete(filteredData$AQ_PM2.5), 
                  fillOpacity = 0.9, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~popup_all_pollutant) %>%
      addLegend(pal = pal_discrete, 
                values = filteredData$AQ_PM2.5, 
                position = "bottomright",
                title = paste0("Air Quality<br />on ",oneday))
    
    map
  }
  
  output$plot9 <- renderLeaflet({
    AQmapviz(input$oneday)
    
  })
  ### AQI trend
  # A function by Paul Bleicher (Copyright 2009 Humedica.)
  # Since we customize some parameters, I put the function here.
  calendarHeat <- function(dates, values, title = "", subtitle = "", legendtitle = ""){
    
    # Parameter checks
    if(missing(dates)){
      stop("Need to specify a dates vector.")
    }
    if(missing(values)){
      stop("Need to specify a values vector.")
    }
    if(!is.Date(dates)){
      stop("dates vector need to be in Date format.")
    }
    if(length(dates) != length(values)){
      stop("dates and values need to have the same length.")
    }
    
    
    # load required packages
    require(ggplot2)
    
    my_theme <- function() {
      
      # Colors
      color.background = "white"
      color.text = "#22211d"
      
      # Begin construction of chart
      theme_bw(base_size=15) +
        
        # Format background colors
        theme(panel.background = element_rect(fill=color.background, color=color.background)) +
        theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
        theme(panel.border     = element_rect(color=color.background)) +
        theme(strip.background = element_rect(fill=color.background, color=color.background)) +
        
        # Format the grid
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(axis.ticks       = element_blank()) +
        
        # Format the legend
        theme(legend.position = "bottom") +
        theme(legend.text = element_text(size = 8, color = color.text)) +
        theme(legend.title = element_text(size = 10, face = "bold", color = color.text)) +
        
        # Format title and axis labels
        theme(plot.title       = element_text(color = color.text, size=10, face = "bold", hjust = 0.5)) +
        theme(plot.subtitle    = element_text(color = color.text, size = 5, hjust = 0.5))+
        theme(axis.text.x      = element_text(size=12, color="black")) +
        theme(axis.text.y      = element_text(size=12, color="black")) +
        theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
        theme(axis.title.y     = element_text(size=14, color="black", vjust=1.25)) +
        theme(axis.text.x      = element_text(size=10, hjust = 0, color = color.text)) +
        theme(axis.text.y      = element_text(size=10, color = color.text)) +
        theme(strip.text       = element_text(face = "bold")) + 
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
    }
    
    # create empty calendar
    min.date <- as.Date(paste(format(min(dates), "%Y"),"-1-1",sep = ""))
    max.date <- as.Date(paste(format(max(dates), "%Y"),"-12-31", sep = ""))
    df <- data.frame(date = seq(min.date, max.date, by="days"), value = NA)
    
    # fill in values
    df$value[match(dates, df$date)] <- values
    
    df$year  <-  as.factor(format(df$date, "%Y"))
    df$month <- as.numeric(format(df$date, "%m"))
    df$doy   <- as.numeric(format(df$date, "%j"))
    #df$dow  <- as.numeric(format(df$date, "%u"))
    #df$woy  <- as.numeric(format(df$date, "%W"))
    df$dow <- as.numeric(format(df$date, "%w"))
    df$woy <- as.numeric(format(df$date, "%U")) + 1
    
    df$dowmapped <- ordered(df$dow, levels = 6:0)
    levels(df$dowmapped) <- rev(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
    
    g <- ggplot(df, aes(woy, dowmapped, fill = value)) + 
      geom_tile(colour = "darkgrey") + 
      facet_wrap(~year, ncol = 1) + # Facet for years
      coord_equal(xlim = c(2.5,54)) + # square tiles
      scale_x_continuous(breaks = 53/12*(1:12)-1.5, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
      my_theme() +
      scale_fill_gradientn(colours = c("#01e400", "#ffff00", "#ff7e00", "#ff0000", "#903f97", "#7e0024"), na.value = "white",
                           name = legendtitle,
                           guide = guide_colorbar(
                             direction = "horizontal",
                             barheight = unit(2, units = "mm"),
                             barwidth = unit(75, units = "mm"),
                             title.position = 'top',
                             title.hjust = 0.5
                           )) +
      labs(x = NULL, 
           y = NULL, 
           title = title, 
           subtitle = subtitle)
    
    my.lines<-data.frame(x=numeric(), 
                         y=numeric(), 
                         xend=numeric(), 
                         yend=numeric(), 
                         year=character())
    
    for(years in levels(df$year)){
      df.subset <- df[df$year == years,]
      
      y.start <- df.subset$dow[1]
      x.start <- df.subset$woy[1]
      
      x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
      y.top.left <- 7.5
      x.top.right <- df.subset$woy[nrow(df.subset)] + 0.5
      y.top.right <- 7.5
      
      x.mid.left01 <- x.start - 0.5
      y.mid.left01 <- 7.5 - y.start
      x.mid.left02 <- x.start + 0.5
      y.mid.left02 <- 7.5 - y.start
      
      x.bottom.left <- x.start - 0.5
      y.bottom.left <- 0.5
      x.bottom.right <- ifelse(y.start == 6, df.subset$woy[nrow(df.subset)] + 0.5, df.subset$woy[nrow(df.subset)] - 0.5)
      y.bottom.right <- 0.5
      
      my.lines<-rbind(my.lines,
                      data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left), 
                                 y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                                 xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01), 
                                 yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01), 
                                 year = years))
      
      # lines to separate months
      for (j in 1:12)  {
        df.subset.month <- max(df.subset$doy[df.subset$month == j])
        x.month <- df.subset$woy[df.subset.month]
        y.month <- df.subset$dow[df.subset.month]
        
        x.top.mid <- x.month + 0.5
        y.top.mid <- 7.5
        
        x.mid.mid01 <- x.month - 0.5
        y.mid.mid01 <- 7.5 - y.month - 1
        x.mid.mid02 <- x.month + 0.5
        y.mid.mid02 <- 7.5 - y.month - 1
        
        x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5)
        y.bottom.mid <- 0.5
        
        my.lines<-rbind(my.lines,
                        data.frame(x    = c(x.top.mid, x.mid.mid01, x.mid.mid01), 
                                   y    = c(y.top.mid, y.mid.mid01, y.mid.mid01),
                                   xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid), 
                                   yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid), 
                                   year = years))
        
      }
      
    }
    
    # add lines
    g <- g + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)
    
    return(g)
  }
  
  
  # calanderHeatMap
  calanderHeatMap <- function(pollutant, states){
    
    filteredData <- data %>% 
      filter(state == states) 
    
    vars <- c("date", "state", pollutant)
    filteredData <- filteredData[vars]
    
    p <- calendarHeat(dates = filteredData$date,
                      values = filteredData[,3],
                      title = paste(pollutant, " Daily AQI Values - ", states, sep = ""), 
                      subtitle = "", 
                      legendtitle = "")
    return(p)
  }
  
  output$plot10 <- renderPlotly({
    calanderHeatMap(input$input9,input$region5)
  })
  
  
  
  
  # 
  lineGraph <- function(pollutant,states){
    filteredData <- data %>% filter(state == states)
    p <- ggplot(filteredData) +
      aes(x = date, y = get(pollutant)) +
      geom_line(mapping = aes(group = 1),color = "#5080B0",size = 1) +
      labs(title = paste(pollutant, " Daily AQI Values in 2020 - ", states, sep = ""),
           x = "",
           y = NULL) +
      scale_x_date(date_breaks = "1 month",date_labels = "%b") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(face = "bold", color = "#595959", size = 20,hjust = 0.5),
            plot.margin = unit(c(1, 0.2, 0.3, 0.35), "cm"))
    if (pollutant == "PM2.5") {
      p <- p + geom_hline(yintercept = 12, color = "#01e400", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 35.5, color = "#ffff00", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 55.5, color = "#ff7e00", alpha = 0.4, size = 1) +
        annotate(geom = "text", x = median(filteredData$date), y = 6, colour = "#01e400", label = "GOOD", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 23.75, colour = "#ffff00", label = "MODERATE", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 45.5, colour = "#ff7e00", label = "UNHEALTHY FOR 
SENSITIVE GROUPS", size = 10, alpha = 0.4,fontface = "bold") +
        scale_y_continuous(breaks = c(12,35.5,55.5))
    }
    if (pollutant == "CO") {
      p <- p + geom_hline(yintercept = 4.5, color = "#01e400", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 9.5, color = "#ffff00", alpha = 0.4, size = 1) +
        annotate(geom = "text", x = median(filteredData$date), y = 2.25, colour = "#01e400", label = "GOOD", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 7, colour = "#ffff00", label = "MODERATE", size = 10, alpha = 0.4,fontface = "bold") +
        scale_y_continuous(breaks = c(4.5,9.5))
    }
    if (pollutant == "NO2") {
      p <- p + geom_hline(yintercept = 54, color = "#01e400", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 101, color = "#ffff00", alpha = 0.4, size = 1) +
        annotate(geom = "text", x = median(filteredData$date), y = 27, colour = "#01e400", label = "GOOD", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 77.5, colour = "#ffff00", label = "MODERATE", size = 10, alpha = 0.4,fontface = "bold") +
        scale_y_continuous(breaks = c(54,101))
    }
    if (pollutant == "PM10") {
      p <- p + geom_hline(yintercept = 55, color = "#01e400", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 155, color = "#ffff00", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 255, color = "#ff7e00", alpha = 0.4, size = 1) +
        annotate(geom = "text", x = median(filteredData$date), y = 27.5, colour = "#01e400", label = "GOOD", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 105, colour = "#ffff00", label = "MODERATE", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 205, colour = "#ff7e00", label = "UNHEALTHY FOR 
SENSITIVE GROUPS", size = 10, alpha = 0.4,fontface = "bold") +
        scale_y_continuous(breaks = c(55,155,255))
    }
    if (pollutant == "SO2") {
      p <- p + geom_hline(yintercept = 36, color = "#01e400", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 76, color = "#ffff00", alpha = 0.4, size = 1) +
        geom_hline(yintercept = 186, color = "#ff7e00", alpha = 0.4, size = 1) +
        annotate(geom = "text", x = median(filteredData$date), y = 18, colour = "#01e400", label = "GOOD", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 56, colour = "#ffff00", label = "MODERATE", size = 10, alpha = 0.4,fontface = "bold") +
        annotate(geom = "text", x = median(filteredData$date), y = 131, colour = "#ff7e00", label = "UNHEALTHY FOR 
SENSITIVE GROUPS", size = 10, alpha = 0.4,fontface = "bold") +
        scale_y_continuous(breaks = c(36,76,186))
    }
    return(p)
  }
  output$plot11 <- renderPlotly({
    lineGraph(input$input9,input$region5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
