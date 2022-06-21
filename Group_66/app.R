library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(ggthemes)
#library(scales)
library(ggrepel)



ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Index"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1", icon = icon("list-alt")),
      menuItem("Life Expentancy vs. GDP", tabName = "page2", icon = icon("location-arrow")),
      menuItem("Freedom vs. Generosity", tabName = "page3", icon = icon("chart-line")),
      menuItem("Trust vs. Happiness Score", tabName = "page4", icon = icon("map-o")),
      menuItem("World Happiness Map", tabName = "page5", icon = icon("map-marker-alt")),
      menuItem("Rank", tabName = "page6", icon = icon("map-o")),
      menuItem("Data Explorer", tabName = "page7", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("page1",
              fluidRow(
                box(
                  title = "About the Application", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             "This is the shiny dashboard application designed to explore the happiness index for countries all over the world with", tags$strong("5"), "factors, including:", style = "font-size:16px"),
                           br(), br(),
                           fluidRow(column(6, tags$li("Economy, GDP per capita"), tags$li("Social support"), tags$li("Generosity")), 
                                    column(6, tags$li("Freedom to make life changes"), tags$li("Corruption Perception"))),
                           br(),
                           fluidRow(tags$i(tags$i(" "))
                           )
                         )
                  )
                )
              ),
              fluidRow (
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("This dataset is about the bliss score for different countries in the world. The score are based on answers to the most life evaluation address inquired within the Gallup World Survey, and are from broadly agent tests for the a long time 2013-2016 and utilize the Gallup weights to create the gauges agent.
                                    Five variables – Economy, Social support, Freedom to change, Corruption, and Generosity – contribute to making life assessments higher in each nation than they are in Dystopia, a theoretical nation that has values rise to to the world’s least national midpoints for each of the six variables. "),
                           br(), br(),
                           tags$li(tags$strong("Source: https://www.kaggle.com/mathurinache/world-happiness-report-20152021 "),tags$a(href = "https://www.kaggle.com/mathurinache/world-happiness-report-20152021")),
                           tags$li("The filtered dataset for this application contains total",tags$strong("935"), "cases (in ", tags$strong("8"), "columns) from 2015 to 2020.")
                         )
                  )
                )
              ),
              fluidRow (
                box(
                  title = "About Us", solidHeader = TRUE, 
                  status = "info", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           fluidRow (
                             column(8, tags$div("Team members are all from Carey Business School, Johns Hopkins University:", style = "font-size:16px")
                             )
                           )
                         ),
                         br(),
                  )
                )
              )
      ),
      tabItem(tabName = "page2",
              sliderInput("thisyear", "Year:", min = 2015, max = 2020, value = 2015, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              numericInput("num", "Number of countries to show:",
                           value = 5, min = 0, max = 15, step = 1),
              plotOutput("plot1")
      ),
      tabItem(tabName = "page3",
              sliderInput("thisyear1", "Year:", min = 2015, max = 2020, value = 2015, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              numericInput("num1", "Number of countries to show:",
                           value = 5, min = 0, max = 15, step = 1),
              plotOutput("plot2")
      ),
      tabItem(tabName = "page4",
              sliderInput("thisyear2", "Year:", min = 2015, max = 2020, value = 2015, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              numericInput("num2", "Number of countries to show:",
                           value = 5, min = 0, max = 15, step = 1),
              plotOutput("plot3")
      ),
      tabItem(tabName = "page5",
              sliderInput("year","Year:", min=2015, max=2020, value = 2015, step=1, animate = animationOptions(interval = 2000, loop = FALSE)),
              leafletOutput("myMap", width="100%")
              
      ),
      tabItem(tabName = "page6", 
              sliderInput("theyear", "Year:", min = 2015, max = 2020, value = 2015, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              numericInput("thenum", "Number of countries to show:",
                           value = 5, min = 0, max = 15, step = 1),
              plotOutput("plot4")
      ),
      tabItem(tabName = "page7",dataTableOutput("myTable"),
              (a("Data Source",href='https://www.kaggle.com/mathurinache/world-happiness-report?select=2015.csv', target="_blank"))
              
      )
    )
  )
)




server <- function(input, output, session) {
  
  data <- read_csv("happi_data_15to20.csv")
  Datafull <- read.csv("happi_data_15to201.csv")
  
  output$plot1 = renderPlot({
    
    Datafull$thisYear = input$thisyear
    filteredData= Datafull %>% filter(year==input$thisyear)
    p <- ggplot(data=filteredData,aes(x=Health_.Life_Expectancy., y=Economy_.GDP_per_Capita.)) +
      geom_point(alpha=0.6,aes(color =Region, size = Happiness_Score))+
      scale_size(range=c(.1,6))+
      annotate("text",x=0.3,y=1.3,colour="grey80",size=18,label= filteredData$thisYear) +
      theme_fivethirtyeight()+
      theme(plot.title = element_text(size = rel(1)),
            legend.title = element_text(size = rel(0.5)),
            axis.title=element_text(face = "bold", size = rel(0.6)))+
      labs(title="Life Expectancy Vs. GDP", colour="Region",size="Happiness_Score")
    cnum=input$num
    countriesToShow= filteredData%>% arrange(-Happiness_Score) %>%
      pull(Country) %>% head(cnum)
    p <-  p + geom_text_repel(data= filteredData %>% filter(Country %in% countriesToShow),
                              mapping = aes(label = Country),   family = "Poppins",
                              size = 5,
                              min.segment.length = 0, 
                              seed = 42, 
                              box.padding = 0.5,
                              max.overlaps = Inf,
                              arrow = arrow(length = unit(0.010, "npc")),
                              nudge_x = .15,
                              nudge_y = .5,
                              colour="grey28") 
    p })
  
  output$plot2 <- renderPlot({
    Datafull$thisYear = input$thisyear1
    filteredData= Datafull %>% filter(year==input$thisyear1)
    
    p2 <- ggplot(data=filteredData,aes(x=Freedom, y=Generosity)) +
      geom_point(alpha=0.6,aes(color =Region, size = Happiness_Score))+
      scale_size(range=c(.1,6))+
      annotate("text",x=0.3,y=0.5,colour="grey80",size=14,label= filteredData$thisYear) +
      theme_fivethirtyeight()+
      theme(plot.title = element_text(size = rel(1)),
            legend.title = element_text(size = rel(0.5)),
            axis.title=element_text(face = "bold", size = rel(0.6)))+
      labs(title="Freedom Vs. Generosity", colour="Region",size="Happiness_Score")
    cnum=input$num1
    countriesToShow= filteredData%>% arrange(-Happiness_Score) %>%
      pull(Country) %>% head(cnum)
    p2 <-  p2 + geom_text_repel(data= filteredData %>% filter(Country %in% countriesToShow),
                                mapping = aes(label = Country),   family = "Poppins",
                                size = 5,
                                min.segment.length = 0, 
                                seed = 42, 
                                box.padding = 0.5,
                                max.overlaps = Inf,
                                arrow = arrow(length = unit(0.010, "npc")),
                                nudge_x = .8,
                                nudge_y = .2,
                                colour="grey28") 
    p2
  })
  output$plot3 <- renderPlot({
    Datafull$thisYear = input$thisyear2
    filteredData= Datafull %>% filter(year==input$thisyear2)
    filteredData$Trust_.Government_Corruption.<-as.numeric(as.character(filteredData$Trust_.Government_Corruption.))
    p3 <- ggplot(data=filteredData,aes(x=Trust_.Government_Corruption., y=Happiness_Score)) +
      geom_point(aes(color =Region),size=3)+
      annotate("text",x=0.4,y=5,colour="grey80",size=14,label= filteredData$thisYear) +
      theme_fivethirtyeight()+
      theme(plot.title = element_text(size = rel(1)),
            legend.title = element_text(size = rel(0.5)),
            axis.title=element_text(face = "bold", size = rel(0.6)))+
      labs(title="Trust vs. Happiness Score", colour="Region",size="Happiness_Score")
    cnum=input$num2
    countriesToShow= filteredData%>% arrange(-Happiness_Score) %>%
      pull(Country) %>% head(cnum)
    p3 <-  p3 + geom_text_repel(data= filteredData %>% filter(Country %in% countriesToShow),
                                mapping = aes(label = Country),   family = "Poppins",
                                size = 5,
                                min.segment.length = 0, 
                                seed = 42, 
                                box.padding = 1,
                                max.overlaps = Inf,
                                arrow = arrow(length = unit(0.010, "npc")),
                                nudge_x = .3,
                                nudge_y = .8,
                                colour="grey28") 
    p3
  })
  
  output$myMap = renderLeaflet({
    loc_data <- data%>% 
      mutate(lng=round(Long,3),lat=round(Lat,3))
    loc_data <-  loc_data%>%
      mutate(popusText=paste("In", year, ", the Happiness Score of", `Country`, "is", `Happiness_Score`,
                             ", its rank is", `Happiness_Rank`),
             rank=1/Happiness_Rank)
    m=loc_data %>%
      filter(year==input$year) %>%
      leaflet() %>% 
      addTiles()%>%
      setView(46.82,30.22, zoom=2)%>%
      addCircleMarkers(lng = ~lng, 
                       lat = ~lat, 
                       fillOpacity = ~rank*3, 
                       fillColor = "red", 
                       label = ~Country,
                       popup = ~popusText)
    m
    
  })
  
  output$plot4 = renderPlot({
    n <- data %>%
      mutate(score=round(Happiness_Score,3)) %>%
      filter(year==input$theyear) %>% 
      filter(Happiness_Rank<=10) 
    nm <- ggplot(n, aes(x=Country, y=reorder(score,Happiness_Rank), 
                        fill= Country)) + 
      geom_bar(stat="identity")+
      labs(x="Country", y="Happiness Score",
           title=paste("Happiness Rank in",input$theyear))+
      theme_bw() +
      coord_flip()
    nm
    
  })
  
  output$myTable = renderDataTable({
    return(datatable(data, rownames= FALSE))
  })
}

shinyApp(ui = ui, server = server)
