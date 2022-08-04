library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(directlabels)
library(ggrepel)
library(sitools)
library(tools)
library(grid)
library(googleVis)
library(ggvis)
library(ggthemes)
library(scales)
library(patchwork)
library(ggplot2) 
library(reshape2)
library(shinyWidgets)


test = read.csv("covid_data.csv")
test<-test%>%select(3,4,6,7,14)
test<-na.omit(test)
country=test%>%distinct(location)%>%pull(location)
first_show_country = c("World","India","Africa","United States","Brazil","Germany")


ui <- dashboardPage(
    dashboardHeader(title = "Covid-19 Vaccination"),
    dashboardSidebar(
      width = 300,
        sidebarMenu(
          menuItem("About", tabName = "page0", icon = icon("fas fa-book")),
            menuItem("Covid-19 Vaccination Line Chart", tabName = "page1", icon = icon("line-chart")),
            menuItem("Covid-19 Vaccination Bar Chart", tabName = "page2", icon = icon("area-chart")),
            menuItem("Covid-19 Vaccination Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Covid-19 Vaccination Data", tabName = "page4", icon = icon("layer-group"))
        )
    ),
    dashboardBody(
      tags$head( 
        tags$style(HTML(".main-sidebar { font-size: 16px; }"))
      ),
        tabItems(
          
          tabItem(tabName = "page0",
                  fluidRow(
                    box(
                      title = "About the Application", solidHeader = TRUE,
                      status = "success", width = 12, collapsible = TRUE,
                      column(12, 
                             fluidRow(column(12,"Initial ideas include a linear graph that displays the number of people who get vaccinated across countries against time; Geographical interactive map."), style = "font-size:16px"),
                             br(),
                              fluidRow(column(12,"The geographical interactive map that can show the percentage of people who have been vaccinated is segmented by country. If possible, users could click on the map and look into the details of a specific country if needed."), style = "font-size:16px"),
                              br(),
                               fluidRow(column(12,"Filters would be considered as a user could zoom in if they'd like to focus on a specific country. Filter would also be applied to adjust the timeline."), style = "font-size:16px")
                            
                      )#cloumn12
                    )
                  ),#fluidrow
                  
                  
                  fluidRow(
                    box(
                      title = "About the Dataset", solidHeader = TRUE,
                      status = "primary", width = 12, collapsible = TRUE,
                      column(12, 
                             fluidRow(column(12,"The vaccination dataset uses the most recent official numbers from governments and health ministries worldwide. The population estimates we use to calculate per-capita metrics are all based on the last revision of the United Nations World Population Prospects. "), style = "font-size:16px")
                             
                      )#cloumn12
                    )
                  ),#fluidrow
                  conditionalPanel("plot_click!=null",
                                   h4(textOutput("nametext")),
                                   HTML('<iframe width="1200" height="600" src="https://www.youtube.com/embed/eVDh86MIZeQ" frameborder="0" allowfullscreen></iframe>')),
                  
                  a("Video Source", href="https://www.youtube.com/watch?v=eVDh86MIZeQ&t=11s",
                    target="_blank"),
                  fluidRow(
                    box(
                      title = "About Us", solidHeader = TRUE, 
                      status = "info", width = 12, collapsible = TRUE,
                      column(12, 
                             tags$div(
                               fluidRow(
                                 column(12, tags$div("Team members are all from BARM program at Carey Business School, Johns Hopkins University:", style = "font-size:16px")
                                 )
                               )
                             ),
                             br(),
                             tags$li("If you have any suggestion, question, or review for this app, comments are open! 
                       Please send an email to ", tags$a(href = "jshao10@jhu.edu", "jshao10@jhu.edu"), "and refer to this Shiny app.")
                      )
                    ),
                    br()
                  )
                      
                  ),

                  

            tabItem(tabName = "page1",
                    h2("Share of the population vaccinated against COVID-19"),
                    fluidRow(
                      box(
                        title = "How to Use", solidHeader = TRUE,
                        status = "warning", width = 12, collapsible = TRUE, #collapsed = TRUE,
                        column(12, 
                        h4("* Thank you for your patience, it may take seconds to load the plots"),
                        fluidRow(column(12,"This 'Share of the population vaccinated against COVID-19' panel has two plots. In the Metric checkbox, select the 'People vaccinated', you will see the plot that shows the share of the total population that received at least one vaccine dose. Select the 'People fully vaccinated', you will see the plot that shows the share of the total population that have received all doses prescribed by the vaccination protocol. On the left side, you can also choose which countries you want to show in the plot."), style = "font-size:16px"),
                        fluidRow(tags$mark(tags$i("   * This data is only available for countries which report the breakdown of doses administered by first and second doses.")))
                      ))
                    ),
                    fluidRow(
                      box(
                        title = "Analysis", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               fluidRow(column(12,"The purpose of these two plots is to visualize the trend of the share of the population that fully or partially vaccinated against COVID-19 until July 2021. You can also see the comparison of the trends among countries you are interested in."), style = "font-size:16px")
                               
                        )#cloumn12
                      )
                    ),#fluidrow
                    fluidRow(
                      
                      column(3, 
                             selectInput(inputId = 'metric',
                                         label = 'Metric', 
                                         choices = c("People vaccinated" ="people_vaccinated_per_hundred", 
                                                     "People fully vaccinated" ="people_fully_vaccinated_per_hundred"),
                                         multiple = FALSE),
                             br(),
                             h6("The selected countries are in the right column."),
                             multiInput(
                               inputId = "location1", label = "Countries :",
                               choices = country,
                               selected = first_show_country, width = 250
                              )
                          
                        ),
                      column(9,  plotOutput("plot1"))
                    )
                    ),
            tabItem(tabName = "page2",
                    h2("Share of people vaccinated against COVID-19"),
                    fluidRow(
                      box(
                        title = "How to Use", solidHeader = TRUE,
                        status = "warning", width = 12, collapsible = TRUE, #collapsed = TRUE,
                        column(12, 
                               h4("Below shows the share of people partially or fully vaccinated worldwide. 
                                  The total percentage is the percentage of people vaccinated in that country relative to its population.")
                        ))
                    ),
                    fluidRow(
                      box(
                        title = "Analysis", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               fluidRow(column(12,"The purpose of bar chart is to visualize the share of the population that fully or partially vaccinated against COVID-19 until July 2021. You can also see the comparison of shares among countries you are interested in."), style = "font-size:16px")
                               
                        )#cloumn12
                      )
                    ),#fluidrow
                    fluidRow(column(11,
                                    sliderInput("DatesMerge",
                                                "Date:",
                                                min = as.Date("01/01/2021","%m/%d/%Y"),
                                                max = as.Date("07/11/2021","%m/%d/%Y"),
                                                value=as.Date("07/11/2021","%m/%d/%Y"),
                                                ticks=FALSE,
                                                width = "100%"
                                    ))),
                    
                    fluidRow(
                      
                      column(3, 
                             
                             h6("The selected countries are in the right column."),
                             multiInput(
                               inputId = "location", label = "Countries :",
                               choices = country,
                               selected = first_show_country, width = 250
                             )   
                             
                      ),
                      column(9,  plotOutput("plot2"))
                    ) 
            ),
                    
            tabItem(tabName = "page3",
                    h2("Percentage of people vaccinated against COVID-19"),
                    fluidRow(
                      box(
                        title = "How to Use", solidHeader = TRUE,
                        status = "warning", width = 12, collapsible = TRUE, #collapsed = TRUE,
                        column(12, 
                               h4("The Map and table can show the specific date data about the percentage of people vaccinated or the percentage of people fully vaccinated."),
                               fluidRow(tags$mark(tags$i("   *Some countries have more than 100% people vaccinated because of foreigner ")))
                        ))
                    ),
                    
                    fluidRow(
                      box(
                        title = "Analysis", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               fluidRow(column(12,"The purpose of the world map is to see the distribution of percentage of people vaccinated."), style = "font-size:16px")
                               
                        )#cloumn12
                      )
                    ),#fluidrow
                    fluidRow(
                      column(9, 
                             sliderInput("DatesMerge1",
                                         "Date:",
                                         min = as.Date("01/01/2021","%m/%d/%Y"),
                                         max = as.Date("07/11/2021","%m/%d/%Y"),
                                         value=as.Date("07/11/2021","%m/%d/%Y"),
                                         ticks=FALSE,
                                         width = "100%")
                      ),
                      column(3,
                             radioButtons("mapvalue1", "Map Value:",
                                          c("People Vaccinated" = "People_Vaccinated",
                                            "People Fully Vaccinated" = "People_Fully_Vaccinated"), inline = T)
                      ),
                      
                    ),
                    fluidRow(
                             box(
                               title = "Vaccinated Map", solidHeader = TRUE,
                               collapsible = TRUE,
                               htmlOutput("myMap",width="100%") 
                             ),
                             box(
                               title = "Data Table for Vaccinated Map", solidHeader = TRUE,
                               collapsible = TRUE,
                               DT::dataTableOutput("mapData")
                             ),
                             
                      )
            ),

            tabItem(tabName = "page4",
                    dataTableOutput("myTable"),
                    a("Data Source", href="https://ourworldindata.org/covid-vaccinations",
                      target="_blank")
            )
        )
    )
)


server <- function(input, output, session) {
  
  data1 <- read_csv("covid_data.csv")
  data1$date <- as.Date(data1$date, format = "%m/%d/%Y")
  
  
  vaccine = read.csv("covid_data.csv")
  data <- vaccine%>%select(3,4,6,7,14)
  data<-na.omit(data)
  
  data<-data%>%
    mutate(one_dose_only=people_vaccinated - people_fully_vaccinated)
  
  one<-data%>%select(1,2,5,6)
  one$type = "one_dose"
  one<-one%>%
    mutate(value=one_dose_only/population)
  one<-one%>%select(1,2,5,6)
  
  full<-data%>%select(1,2,4,5)
  full$type = "fully_vaccinated"
  full<-full%>%
    mutate(value=people_fully_vaccinated/population)
  full<-full%>%select(1,2,5,6)
  
  whole<-rbind(one,full)
  whole$date<-as.Date(whole$date, format="%m/%d/%Y")
  
  
  
  vaccine2 = read.csv("covid_data.csv")
  data2 <- vaccine%>%select(3,4,6,7,14)
  data2<-na.omit(data2)
  data2$date<-as.Date(data2$date, format="%m/%d/%Y")
  
  data2<-data2%>%
    mutate(PCT_people_fully_vaccinated=people_fully_vaccinated/population*100) %>% 
    mutate(PCT_people_vaccinated=people_vaccinated/population*100)
    

  
    output$plot1 = renderPlot({
      
        showed_country=input$location1
        filteredData <- data1%>%filter(location %in% showed_country)
        
        if (input$metric == "people_fully_vaccinated_per_hundred"){
          filteredData <- filteredData[!is.na(filteredData$people_fully_vaccinated_per_hundred),]
          
          
          minDate <- min((filteredData %>% filter(!is.na(people_fully_vaccinated_per_hundred)))$date)
          maxDate <- max((filteredData %>% filter(!is.na(people_fully_vaccinated_per_hundred)))$date)
          
          
          q = ggplot(filteredData) + 
            geom_line(aes(x = date, y = people_fully_vaccinated_per_hundred/100,
                          group = location, colour = location)) + 
            geom_point( aes(x = date, y = people_fully_vaccinated_per_hundred/100,
                            group = location, colour = location),size=0.5)+
            geom_text(data = subset(filteredData, date == maxDate), 
                      aes(label = location, colour = location, x = maxDate,
                          y = people_fully_vaccinated_per_hundred/100), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +  
            labs(
              title = "Share of the population fully vaccinated against COVID-19"
            ) +
            coord_cartesian(xlim = as.Date(c(minDate, maxDate), format="%d/%m/%Y"),
                            ylim = c(0,NA)) +
            scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
            scale_x_date(expand = c(0, 0), date_breaks = "2 months", date_labels = "%b %d,%Y") +
            theme(plot.margin = unit(c(1,6,1,1), "lines"),
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey", linetype="dashed"),
                  panel.background = element_blank(), axis.line.x = element_line(colour = "grey"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())+theme(legend.position = "none")
          
          gt <- ggplotGrob(q)
          gt$layout$clip[gt$layout$name == "panel"] <- "off"
          grid.draw(gt)
        }else{
          filteredData <- filteredData[!is.na(filteredData$people_vaccinated_per_hundred),]
          
          
          minDate <- min((filteredData %>% filter(!is.na(people_vaccinated_per_hundred)))$date)
          maxDate <- max((filteredData %>% filter(!is.na(people_vaccinated_per_hundred)))$date)
          
          
          q = ggplot(filteredData) + 
            geom_line(aes(x = date, y = people_vaccinated_per_hundred/100,
                          group = location, colour = location)) + 
            geom_point( aes(x = date, y = people_vaccinated_per_hundred/100,
                            group = location, colour = location),size=0.5)+
            geom_text(data = subset(filteredData, date == maxDate), 
                      aes(label = location, colour = location, x = maxDate,
                          y = people_vaccinated_per_hundred/100), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +  
            labs(
              title = "Share of people who received at least one dose of COVID-19 vaccine"
            ) +
            coord_cartesian(xlim = as.Date(c(minDate, maxDate), format="%d/%m/%Y"),
                            ylim = c(0,NA)) +
            scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
            scale_x_date(expand = c(0, 0), date_breaks = "2 months", date_labels = "%b %d,%Y") +
            theme(plot.margin = unit(c(1,6,1,1), "lines"),
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey", linetype="dashed"),
                  panel.background = element_blank(), axis.line.x = element_line(colour = "grey"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())+theme(legend.position = "none")
          
          gt <- ggplotGrob(q)
          gt$layout$clip[gt$layout$name == "panel"] <- "off"
          grid.draw(gt)
        }
        
        
       
      
      
    })
    
    output$plot2 = renderPlot({
      
      choose_date = input$DatesMerge
      filtered_whole<-whole%>%filter(date==choose_date)
      
      
      showed_country=input$location
      filtered_whole<-filtered_whole%>%filter(location%in%showed_country)
      
      
      ggplot(filtered_whole, aes(fill=type, y=value, x=location))+ 
        geom_bar(position='stack',stat="identity")+
        coord_flip()+
        geom_text(aes(label = percent(value,2)), color = "white", size = 3, position = position_stack(vjust = 0.5))+
        scale_y_continuous(labels=percent)+
        theme(panel.background = element_blank())+
        ggtitle(paste("Share of people vaccinated against COVID-19, ",choose_date))+
        theme(axis.title.y=element_blank())+theme(legend.title = element_blank())+
        scale_fill_discrete(labels = c("Share of people fully vaccinated", "Share of people only received one-dose"))+
        theme(plot.title = element_text(hjust = 0.5))
     
                
        
      
    })
    
    output$myMap = renderGvis({
      choose_date = input$DatesMerge1
      datafiltered<-data2%>%filter(date==choose_date)
      
      if(input$mapvalue1 == "People_Vaccinated"){
        gvisGeoChart(datafiltered, locationvar= "location", colorvar="PCT_people_vaccinated",
                     options = list(backgroundColor="gray",
                                    width="100%"))
      } else {
        gvisGeoChart(datafiltered, locationvar= "location", colorvar="PCT_people_fully_vaccinated",
                     options = list(backgroundColor="gray",
                                    width="100%"))
      }
      
      
      
       
        
    })
    output$mapData = DT::renderDataTable(DT::datatable({
      choose_date = input$DatesMerge1
      datafiltered<-data2%>%filter(date==choose_date)
      
      datafiltered[, c(1,2,6,7)]
     
    }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
      deferRender = TRUE,  
      searching = F,
      dom = 'RC<"clear">lfrtip',
      buttons = I('colvis'),
      lengthMenu = list(c(5, 10, 15, 25, 25, 50, 100), c('5', '10', '15', '20', '25','50','100')),
      pageLength =  10,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
        "}")
    ) 
    ))
    
    output$myTable = renderDataTable(
        return(datatable(data, rownames= FALSE))
    )
    

}

shinyApp(ui = ui, server = server)
