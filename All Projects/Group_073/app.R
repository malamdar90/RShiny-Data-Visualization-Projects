library(shiny)
library(maps)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(shinydashboardPlus)

#URL: https://tianyao.shinyapps.io/ProjectDeath/


data <- read_csv("annual-number-of-deaths-by-cause.csv")
data <- data %>% gather(key=cause, value=value, -c(Entity, Code, Year)) %>%
    mutate(cause=gsub("Deaths \\- ", "", cause)) %>%
    mutate(cause=gsub(" \\- Sex: Both \\- Age: All Ages \\(Number\\)", "", cause))
data$cause[data$cause=="Number of executions (Amnesty International)"] <- "Number of executions"

data1 = read_csv("causes_of_death_under_5.csv")
data1 = gather(data1, key=Cause, value=Value, -c(Entity, Code, Year))
data1 = mutate(data1, Cause=gsub("Deaths \\- ", "", Cause))
data1 = mutate(data1, Cause=gsub(" \\- Sex: Both \\- Age: Under 5 \\(Number\\)", "", Cause))
data2 = read_csv("causes_of_death_5_14.csv")
data2 = gather(data2, key=Cause, value=Value, -c(Entity, Code, Year))
data2 = mutate(data2, Cause=gsub("Deaths \\- ", "", Cause))
data2 = mutate(data2, Cause=gsub(" \\- Sex: Both \\- Age: 5\\-14 years \\(Number\\)", "", Cause))
data3 = read_csv("causes_of_death_15_49.csv")
data3 = gather(data3, key=Cause, value=Value, -c(Entity, Code, Year))
data3 = mutate(data3, Cause=gsub("Deaths \\- ", "", Cause))
data3 = mutate(data3, Cause=gsub(" \\- Sex: Both \\- Age: 15\\-49 years \\(Number\\)", "", Cause))
data4 = read_csv("causes_of_death_50_69.csv")
data4 = gather(data4, key=Cause, value=Value, -c(Entity, Code, Year))
data4 = mutate(data4, Cause=gsub("Deaths \\- ", "", Cause))
data4 = mutate(data4, Cause=gsub(" \\- Sex: Both \\- Age: 50\\-69 years \\(Number\\)", "", Cause))
data5 = read_csv("causes_of_death_in_70.csv")
data5 = gather(data5, key=Cause, value=Value, -c(Entity, Code, Year))
data5 = mutate(data5, Cause=gsub("Deaths \\- ", "", Cause))
data5 = mutate(data5, Cause=gsub(" \\- Sex: Both \\- Age: 70\\+ years \\(Number\\)", "", Cause))

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = strong("Death Cause")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("newspaper")),
            menuItem("World Map", tabName = "wmap", icon = icon("globe")),
            menuItem("Cause of Death", tabName = "cod", icon = icon("bars"),
                     menuSubItem("ALL Age", tabName = "subitem1", icon = icon("bar-chart")),
                     menuSubItem("Age from 0 to 4", tabName = "subitem2", icon = icon("bar-chart")),
                     menuSubItem("Age from 5 to 14", tabName = "subitem3", icon = icon("bar-chart")),
                     menuSubItem("Age from 15 to 49", tabName = "subitem4", icon = icon("bar-chart")),
                     menuSubItem("Age from 50 to 69", tabName = "subitem5", icon = icon("bar-chart")),
                     menuSubItem("Age start from 70", tabName = "subitem6", icon = icon("bar-chart"))
                     ),
            menuItem("Conclusion", tabName = "conclusion", icon = icon("newspaper")),
            menuItem("Reference", tabName = "reference1", icon = icon("search"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "about", h1("About"), fluidRow(box(title = strong("Motivation"),width = 12,
                                                        "Millions of people died each year, but what is behind the staggering numbers of deaths? We will find the relationships between the cause of death and geographic region, economic level, type of diseases, age, etc. We hope the accurate cause-of-death information can be helpful for the public health community in evaluating and improving the health of all citizens, and for the individuals as significant guidance to keep healthy and prolong life now and in the future.",
                                                        solidHeader = TRUE, collapsible = TRUE)),
                    fluidRow(box(title = strong("About the Project"),collapsed = TRUE,width = 12,
                        "The Research questions that this application is trying to address is “What are the top death results in different countries and different times, and who are the main victims (age) of these causes.” To demonstrate this problem, we will divide the APP into two major sections:",br(),
                        strong("Number of Death Global Map Tab"),br(),
                        "•	On the map, we see the number of death across the world. As the darker(steel blue) the color, the more deaths there are in that area.",br(), 
                        "•	The name of the country or region, and the total death on the annual number of deaths for that region will automatically be shown when users put their mouses in the corresponding position.",br(), 
                        "•	The timeline at the left of the graph explores how the number of deaths has changed globally over time. The Year Slider allows you to view information respective to that selected year [1900 - 2017]",br(),
                        
                        strong("Causes of Death Tab: "),br(),
                        "•	The bar chart will show the annual number of deaths by cause worldwide in a given year, with the y-axis will be the specific causes of death in number order and the x-axis be the number of people dead. ",br(),
                        "•	The bar charts will also display the number of deaths in different age groups. We divide the age groups into five. By clicking the Tab, users can select different age groups according to their interests ",br(),
                        "•	The users would choose the countries they are interested in simply by clicking the “change country” button in the graph. ",br(),
                        "•	The timeline at the top left of the graph explores how deaths by cause have changed over time. The Year Slider allows you to view information respective to that selected year [1900 - 2017]",
                        solidHeader = TRUE, collapsible = TRUE)),
                    fluidRow(box(title = strong("Initial Sketch"), collapsed = TRUE,width = 12,
                                 solidHeader = TRUE, collapsible = TRUE, img(src = "Initialsketch.jpg", alt = "Initial Sketch"))
                             ),
                    fluidRow(box(title = strong("About the Method"),collapsed = TRUE,width = 12,
                                 "During the building of this project, the primary methods used are:",br(),
                                 strong("Data Collection & Processing"),br(),
                                 "•	Data Cleaning: ", br(),
                                 "When making the world map, we found that the original data table did not contain the longitude and latitude of each country. Without such an important measurement, we were unable to draw the world map. Therefore, we download a package containing latitude and longitude, and then combined the data of the two tables according to the same country name through the Left Join Function. From the complete data, a map of the deaths of the world was finally drawn. The specific steps and corresponding codes are as follows:",br(),
                                 "Step 1：Read and store the map package and the “total number of death by cause.csv”",br(),
                                 img(src = "step1.png", alt = "Step1"),
                                 br(),
                                 
                                 "Step 2: Combine death data and map data, clean the row in which year is 0. Notice that we lost 50 (231-181) entities in this step because of the difference of names in two packages. For example, some entities are called 'world', 'Central Europe, Eastern Europe, and East Asia., and so on. In this situation, the entity East Asia will be partially overlapped with the entity “China” because technically, China is a part of East Asia. Therefore, the 50 data loss is not very influential since we still have 181 countries and regions, which cover almost every corner of the earth, for the users to pick.",br(),
                                 img(src = "step1.png", alt = "Step2"),
                                 br(),
                                 
                                 "Step 3: Draw the map. ",br(),
                                 img(src = "step1.png", alt = "Step3"),
                                 br(),
                                 strong("Data Visualization: "),br(),
                                 "•	World Map: “Maps present information about the world in a most simple, visual way.” They demonstrate the size and shape of countries, the locations of landmarks, and the geographic distances between places (National Geographic Society, 2012). A world map can show distributions of things over Earth, such as the number of deaths across the regions in this project. For this project, the world map could easily picture the severity of the death toll by region since the darker (steel blue) colors represented more deaths.", br(),
                                 "Reference: National Geographic Society. “Map.” National Geographic Society, 9 Oct. 2012, www.nationalgeographic.org/encyclopedia/map/. ", br(),br(),
                                 "•	Bar Chart: Bar Charts show a distribution of data points or compare metric values across different subgroups of the data. From a bar chart, we can examine which groups are highest or most common and how other groups compare against the others (A Complete Guide to Bar Charts, 2019). In this visualization project, the Bar chart compares different categories (cause of death). The length of each bar is proportional to a specific aggregation. Thus, the three most common causes of death will always stand out because their bars are the longest and at the top of the picture. ",br(),
                                 "Reference: Yi, Mike. “A Complete Guide to Bar Charts.” Chartio, 2019, chartio.com/learn/charts/bar-chart-complete-guide/. ",
                                 
                                 solidHeader = TRUE, collapsible = TRUE)),
                    fluidRow(box(title = strong("About the Dataset"), collapsed = TRUE, width = 12,
                                 "The dataset used in this visualization project is from the Global Burden of Disease (GBD) Results Tool developed by the Institute of Health Metrics and Evaluation (IHME). The database includes death rates and numbers across all risk factors and causes in all age groups across all regions and countries from 1990 onwards.",br(),
                                 "Source:  Global Burden of Disease",
                                 a("http://ghdx.healthdata.org/gbd-results-tool", href = "http://ghdx.healthdata.org/gbd-results-tool",  target="_blank"),
                                 dataTableOutput("myTable"),
                                 solidHeader = TRUE, collapsible = TRUE))
                    ),
            
            
            #jiamin
            tabItem(tabName = "wmap", h2("Death around the World"),
                    sliderInput("Year","Number of year:",min = 1990,max = 2017,value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotlyOutput("map", width = 800)),
            
            
            #keyi
            tabItem(tabName = "subitem1", sliderInput("year","Year:",min = min(data$Year),max = max(data$Year),
                                                      value = 1990, step=1,
                                                      animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country", "Country:",
                                choices=unique(data$Entity), selected=unique(data$Entity)[1]),
                    plotOutput("distPlot")),
            
            #zehao
            tabItem(tabName = "subitem2", sliderInput("year1", "Year:", min = min(data1$Year), max = max(data1$Year), 
                                                      value = 1990, step = 1, 
                                                      animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country1", "Country:",
                                choices=unique(data1$Entity), selected=unique(data1$Entity)[1]),
                    plotOutput("plot1")),
            tabItem(tabName = "subitem3", sliderInput("year2", "Year:", min = min(data2$Year), max = max(data2$Year), 
                                                      value = 1990, step = 1, 
                                                      animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country2", "Country:",
                                choices=unique(data2$Entity), selected=unique(data2$Entity)[1]),
                    plotOutput("plot2")),
            tabItem(tabName = "subitem4", sliderInput("year3", "Year:", min = min(data3$Year), max = max(data3$Year), 
                                              value = 1990, step = 1, 
                                              animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country3", "Country:",
                                choices=unique(data3$Entity), selected=unique(data3$Entity)[1]),
                    plotOutput("plot3")),
            tabItem(tabName = "subitem5",sliderInput("year4", "Year:", min = min(data4$Year), max = max(data4$Year), 
                                                     value = 1990, step = 1, 
                                                     animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country4", "Country:",
                                choices=unique(data4$Entity), selected=unique(data4$Entity)[1]),
                    plotOutput("plot4")),
            tabItem(tabName = "subitem6",sliderInput("year5", "Year:", min = min(data5$Year), max = max(data5$Year), 
                                                     value = 1990, step = 1, 
                                                     animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("country5", "Country:",
                                choices=unique(data5$Entity), selected=unique(data5$Entity)[1]),
                    plotOutput("plot5")),
            tabItem(tabName = "conclusion", fluidRow(box(title = strong("Conclusion"), width = 12,
                                                         "•	Globally, three non-communicable diseases(NCDS)— Cardiovascular diseases, Cancer, and Respiratory diseases are the top 3 leading causes of death in 2017. ",br(),br(),
                                                         "•	Causes of death vary significantly between countries, especially when we divide countries into developed and developing countries. The causes of death in developed countries tend to follow the global pattern, with non-communicable diseases dominating, whereas infectious diseases are prevalent in low-income countries. For example, In the United States, the top five causes of death are all non-communicable diseases. Meanwhile, Tuberculosis is the leading cause of death in Burundi, one of the world's poorest countries and located south of the equator in east-central Africa. However, Tuberculosis only ranked 28th in the United States in 2017. ",br(),br(),
                                                         "•	Over time, fewer and fewer people die at a young age. As it comes to the year 2017, nearly half of people who die are over 70. Specifically, lower respiratory infections are the most common death in children under five, while the leading causes globally in 5–14 year olds are road accidents, cancers, and malaria in 2017. In the 15 to 69 years old category, we see that non-communicable diseases (NCDs) begin to become strongly dominant, except for HIV/AIDS, which some developing countries climb into the top causes. Noncommunicable diseases (NCDs) continue to be the leading cause of mortality in the oldest age group (70 years and above), although other causes of death, such as Alzheimer's/dementias and diarrheal diseases, are also becoming more prevalent.",br(),
                                                         solidHeader = TRUE, collapsible = TRUE))),
            tabItem(tabName = "reference1", fluidRow(box(title = strong("Reference and Sources"), width = 12,
                                                                 "National Geographic Society. “Map.” National Geographic Society, 9 Oct. 2012, www.nationalgeographic.org/encyclopedia/map/. ",br(),br(),
                                                         "Yi, Mike. “A Complete Guide to Bar Charts.” Chartio, 2019, chartio.com/learn/charts/bar-chart-complete-guide/. ",
                                                         "Data source: http://ghdx.healthdata.org/gbd-results-tool",
                                                                 solidHeader = TRUE, collapsible = TRUE))
            )
            
        )
    )
)

server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        df <- data[data$Year==as.integer(input$year) & data$Entity==input$country,]
        keyi <- ggplot(df, aes(x=value, y=reorder(cause, value), fill=cause)) + geom_bar(stat="identity") +
            labs(x="Number of deaths", y="Cause", fill="Cause", 
                 title=paste0("Number of deaths by cause,", input$country, " in ", input$year))
        keyi
    })
    
    output$map <- renderPlotly({
        world_map <- map_data("world")
        Death = read_csv('total-number-of-deaths-by-cause.csv')
        world_map %>% 
            left_join(Death, by = c("region" = "Entity")) -> act_world_map
        act_world_map=act_world_map[!is.na(act_world_map$Year),]
        
        thisYear = input$Year
        mp1=act_world_map %>% 
            filter(as.numeric(act_world_map$Year)==thisYear) 
        
        mp= ggplot(mp1, mapping=aes(x =as.numeric(long), y = as.numeric(lat), group = group,text=region)) +
            geom_polygon(aes(fill= totalDeath), colour = "grey50") +
            scale_fill_gradient(low = "pink", high="steel blue") +
            labs(title="Death around the World")+
            theme_map()
        
        mp=ggplotly(mp,tooltip=c('fill','text'))
        mp
    })
    
    output$plot1 = renderPlot({
        
        thisYear1 = input$year1
        thisEntity1 = input$country1
        filteredData1 = filter(data1, data1$Year==thisYear1 & data1$Entity == thisEntity1)
        
        p1 = ggplot(data = filteredData1, mapping = aes(x = Value, y=reorder(Cause, Value), fill = Cause))+
            geom_bar(stat="identity")+
            ylab("Causes")+
            xlab("Total Deaths")
        p1
        
        
    })
    
    output$plot2 = renderPlot({
        
        thisYear2 = input$year2
        thisEntity2 = input$country2
        filteredData2 = filter(data2, data2$Year==thisYear2 & data2$Entity == thisEntity2)
        
        p2 = ggplot(data = filteredData2, mapping = aes(x = Value, y=reorder(Cause, Value), fill = Cause))+
            geom_bar(stat="identity")+
            ylab("Causes")+
            xlab("Total Deaths")
        p2
        
        
    })
    
    output$plot3 = renderPlot({
        
        thisYear3 = input$year3
        thisEntity3 = input$country3
        filteredData3 = filter(data3, data3$Year==thisYear3 & data3$Entity == thisEntity3)
        
        p3 = ggplot(data = filteredData3, mapping = aes(x = Value, y=reorder(Cause, Value), fill = Cause))+
            geom_bar(stat="identity")+
            ylab("Causes")+
            xlab("Total Deaths")
        p3
        
        
    })
    
    output$plot4 = renderPlot({
        
        thisYear4 = input$year4
        thisEntity4 = input$country4
        filteredData4 = filter(data4, data4$Year==thisYear4 & data4$Entity == thisEntity4)
        
        p4 = ggplot(data = filteredData4, mapping = aes(x = Value, y=reorder(Cause, Value), fill = Cause))+
            geom_bar(stat="identity")+
            ylab("Causes")+
            xlab("Total Deaths")
        p4
        
        
    })
    
    output$plot5 = renderPlot({
        
        thisYear5 = input$year5
        thisEntity5 = input$country5
        filteredData5 = filter(data5, data5$Year==thisYear5 & data5$Entity == thisEntity5)
        
        p5 = ggplot(data = filteredData5, mapping = aes(x = Value, y=reorder(Cause, Value), fill = Cause))+
            geom_bar(stat="identity")+
            ylab("Causes")+
            xlab("Total Deaths")
        p5
        
        
    })
    
    output$initialsketch <- renderImage({
        filename <- normalizePath(file.path('./image',paste('Initialsketch.jpg', sep='')))
        list(src = filename)
        }, deleteFile = FALSE)
    
    output$myTable = renderDataTable({
        return(datatable(data, rownames= FALSE))
    }
    
    )

}

shinyApp(ui = ui, server = server)
