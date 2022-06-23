library(dplyr)
library(ggplot2)
library(plotly)
library(googleVis)
library(ggvis)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)
library(tidyverse)
library(leaflet)
library(viridis)
library(hrbrthemes)

# Reference 1: Zip Code Boundaries: https://nam02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fdata.cityofnewyork.us%2FBusiness%2FZip-Code-Boundaries%2Fi8iw-xf4u%2Fdata%3Fno_mobile%3Dtrue&data=04%7C01%7Cche24%40jhu.edu%7C69abf94e14904aeac3ae08d94b1ccebd%7C9fa4f438b1e6473b803f86f8aedf0dec%7C0%7C0%7C637623409358859496%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&sdata=x351d%2FG8sSIHSmFD55CqlCRHfR65lUD31x1M1jmJGwQ%3D&reserved=0
# Reference 2: GeoNames: http://download.geonames.org/export/zip/
# Reference 3: Original dataset from Kaggle: https://www.kaggle.com/new-york-city/nyc-inspections

ui <- dashboardPage(
  dashboardHeader(title = "NYC Restaurant Inspections", titleWidth = 280),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "page1", icon = icon("info-circle")),
      menuItem("Cuisine By Borough", tabName = "page2", icon = icon("chart-bar")),
      menuItem("Cuisine By Zipcode & Grade", tabName = "page3", icon = icon("map-o")),
      menuItem("Data", tabName = "page4", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    #tags$head(tags$style(HTML('.content-wrapper {header-color: #419454;}'))),
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                box(
                  title = "About the project", solidHead = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12,
                         tags$strong("NYC Restaurant Inspections"),
                         br(),
                         tags$div(
                           tags$span(
                             "In this project we have attempted to understand the 
                             distributions of Restaurants in New York City. We have 
                             utilised data from the"),
                           tags$b("NYC health Department"), 
                           tags$span("detailing 
                             the results of NYC restaurant inspections over a period 
                             of 7 years from 2010 to 2017. We have used indicators 
                             such as the ratings each restaurant received, the cuisine 
                             of each restaurant, and the location of the restaurant 
                             (including the zipcode and borough) in NYC. Our motivation 
                             in doing this project was to understand what are the different 
                             types of restaurants in NYC and whether the location or 
                             the cuisine affect the evaluation of the restaurants."
                                       ),
                           br(), br(),
                           tags$span(
                             tags$strong(
                             "Data Understanding & Preparation"
                             )
                             ),
                           br(),
                           tags$span(
                            "The data used to address our business problem is called 
                            “NYC Restaurant Inspection”. The original dataset consisted 
                            of 399,918 rows (restaurants) and 18 columns (variables). 
                            After an initial analysis, it was obvious that our target 
                            variable was"
                            ), 
                          tags$b("Critical Flag"), 
                          tags$span(", indicating whether a restaurant 
                            was cited with a critical violation or not.
                            The original dataset has a lot of categorical data columns 
                            including Dates, Street, Phone, Building, etc., having 
                            hundreds of unique values. Upon converting all the columns 
                            in the original dataset into factors, there were around 
                            450 columns generated. To make classification easier, 
                            we decided to trim the dataset by deleting columns such as 
                            Building, Street, Date, Violation Description, etc., because 
                            they were either repetitive or had too many unique values 
                            for us to work with. With that, we were down to 6 rows."
                                    ),
                          br(),br(),
                          tags$span("Because we wanted to predict whether a restaurant would be cited 
                            with a critical flag or not, the restaurant’s Grade would 
                            be an important predictor. There were about 210,000 rows 
                            where the Grades were P, Z, Not Graded, or Blanks. Upon 
                            more research, we discovered that the grade assigned 
                            by the NYC inspections were based on the Score received. 
                            So, to make Grade more meaningful, we converted all the 
                            grades into A, B, C, or Z based on the score. A score of 
                            under 13 warranted A, under 28 warranted B, under 35 warranted 
                            C, and the rest were Z. Since the Grade column reflected the 
                            Score received, we deleted the Score column. The column 
                            of Cuisine had over 87 distinct values. Some were repetitive 
                            values, for e.g., pizza, Italian, and pasta. We reduced the 
                            number of unique values by using continents as Cuisine Origin 
                            instead of Cuisine."
                                    ), 
                          br(), br(),
                          tags$span("We realized that there were a significant 
                            number of rows that were missing Zip codes, so we removed 
                            those for expediency. There were also several rows with blank 
                            actions. We removed those as well. Then were rows where there 
                            was a violation was recorded in the “Action” column, but no 
                            corresponding violation codes were present in the “Violation Code” 
                            column. So, we removed the rows where there were actions, but no 
                            violations recorded. However, we wanted to preserve rows where 
                            the violations in “Actions” were “No violations recorded,” so 
                            we added another code “AAA” to indicate that even though the 
                            “Violation Code” column was blank, the blank was meaningful, 
                            in that it indicated a positive scenario where no violations 
                            were recorded. This preprocessing left us with a final clean 
                            dataset of 6 columns and over 399,000 rows. Using this cleaned 
                            dataset, we performed attempted to understand the data using 
                            visualizations as described below."
                                    )
                          ),
                         br(),
                         fluidRow(
                             tags$mark(tags$i("*Please note that the data is retrieved from", 
                                              tags$b("Kaggle.com"), 
                                              tags$i(", an online community of data 
                                                     scientists and machine learning practitioners."
                                                     )
                                              )
                                       )
                             )
                         )
                  )
                ),
              fluidRow(
                box(
                  title = "About us",
                  column(12,
                         tags$div(
                           tags$span(
                           "Team members are all from the MSIS program at Johns Hopkins 
                           University Carey Business School."
                           )
                           ),
                         br(), br()
                         )),
                box(
                  title = "Reference",
                  column(12,
                         tags$div(
                           a("NYC Zip Code Boundaries", href = "https://nam02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fdata.cityofnewyork.us%2FBusiness%2FZip-Code-Boundaries%2Fi8iw-xf4u%2Fdata%3Fno_mobile%3Dtrue&data=04%7C01%7Cche24%40jhu.edu%7C69abf94e14904aeac3ae08d94b1ccebd%7C9fa4f438b1e6473b803f86f8aedf0dec%7C0%7C0%7C637623409358859496%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&sdata=x351d%2FG8sSIHSmFD55CqlCRHfR65lUD31x1M1jmJGwQ%3D&reserved=0", 
                             target ="_blank"),
                           br(),
                           a("GeoNames", href = "http://download.geonames.org/export/zip/",
                             target = "_blank"),
                           br(),
                           a("Data Source", href = "https://www.kaggle.com/new-york-city/nyc-inspections", 
                             target="_blank")
                           )
                         )
                  )
                )
              ),
    
      tabItem(tabName = "page2",
              fluidRow(box(
                title = "Cuisine by Borough", solidHead = TRUE,
                status = "success", width = 12, collapsible = TRUE,
                column(12, 
                       tags$div(
                         tags$span(
                         "This interactive graph shows the concentration of each 
                         cuisine origin in each of the main neighborhoods/ Boroughs 
                         in New York City."
                         )
                         )
                       )
                ),
                ),
              selectInput(inputId = "BoroCuisine",label = "Borough",
                          choices = c("All", "QUEENS","BRONX", "BROOKLYN", "MANHATTAN","STATENISLAND")),
              plotOutput("plot1"),
              br(),
              fluidRow(
                valueBox("North America", "All", icon = icon("utensils"), color = "orange"),
                valueBox("East Asia", "QUEENS", icon = icon("utensils"), col = "fuchsia"),
                valueBox("Europe", "BRONX", icon = icon("utensils"), col = "red"),
                valueBox("North America", "BROOKLYN", icon = icon("utensils"), col = "orange"),
                valueBox("North America", "MANHATTAN", icon = icon("utensils"), col = "orange"),
                valueBox("North America", "STATENISLAND", icon = icon("utensils"), col = "orange")
              )
      ),
    
      tabItem(tabName = "page3",
              fluidRow(box(
                title = "Cuisine by Zipcode", solidHead = TRUE,
                status = "success", width = 12, collapsible = TRUE,
                column(12, 
                       tags$div(
                         tags$span(
                           "This interactive map allows a user to understand the 
                           concentration of restaurants having a specific cuisine and 
                           receiving a specific grade in NYC. Each block on the map is 
                           indicative of the zipcode of that area. Further, the color 
                           opacity indicates the number of restaurants meeting the criteria. 
                           The more opaque the color, the more restaurants in that area 
                           meet the user’s requirements."
                           )
                         )
                       )
                ),
                ),
              fluidRow(
                column(2,
                       selectInput(inputId = "Cuisine",label = "Cuisine",
                                   choices = c("All", "Africa", "American", 
                                                 "Central America", "Australia", 
                                                 "East Asia", "Europe", "Middle East", 
                                                 "Other", "South America", "South Asia"
                                                 )
                                     )
                       ),
                column(2, 
                       selectInput(inputId = "Grade",label = "Grade", 
                                   choices = c("All", "A", "B", "C", "Z")
                                   )
                       )
                ),
              leafletOutput("myMap", width="100%")
              ),
    
      tabItem(tabName = "page4",
              dataTableOutput("myTable"),
              br(),
              fluidRow(
                tags$mark(
                  tags$i(
                    "*Above is the cleaned dataset that we came up with for this 
                    application, you can access the original full dataset by clicking 
                    on the link below."
                    )
                  ),
                br(),
                a("Data Source", href = "https://www.kaggle.com/new-york-city/nyc-inspections", target="_blank")
                ),
              )
      )
    )
  )


server <- function(input, output, session) {
  
  # tab 2
  data <- read_csv("Book4.csv") # read data
  
  output$plot1 <- renderPlot({ 
    if(input$BoroCuisine!="All"){
      data = data %>% filter(BORO == input$BoroCuisine) # filter data 
    }
    data = data %>% group_by(CUISINEORIGIN) %>% summarise(N = n()) # Group by cusine origin 
    
    p <- data %>%
      ggplot(aes(x = CUISINEORIGIN, y = N, fill = CUISINEORIGIN)) + 
      geom_col() +
      geom_text(aes(label = N), position = position_dodge(width = 0.9), vjust = -0.25) +
      labs(title = "Concentrations of Cuisines by Boroughs", # add title and labels for x and y axis
           y = "Count",
           x = "Cuisine Origin",
           fill = "Cuisine Origin") +
      scale_fill_brewer(palette = "Paired") +
      theme(plot.title = element_text(size = 25, face = 'bold')) +
      theme(axis.title.x = element_text(size = 15, face = 'bold')) +
      theme(axis.title.y = element_text(size = 15, face = 'bold')) +
      theme(axis.text = element_text(size = 15)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15, face = 'bold'))
    
    p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Remove background theme 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  
  # tab 3
  nycdata <- read.csv("nycRestData.csv")
  nycdata <- nycdata[-c(8,9,10)]
  
  #Finding Unique zipcodes in NYC
  zipcodecount <- unique(nycdata$ZIPCODE)
  
  #count of unique zipcodes
  zipcodeunique <- nycdata %>% 
    group_by(ZIPCODE) %>% 
    summarise(RESTNUM = n())
  
  #Add Average Lat Long for unique zipcodes 
  uszipcodes <- read.csv("uszipcodelatlong.csv")
  uszipcodes <- uszipcodes %>% 
    rename(LAT = X54.143, LONG = X.165.7854, ZIPCODE = X99553)
  
  # Perform Left join to add all the lat longs to the appropriate zipcodesin the zipcodeunique list
  zipcodeuniquetest <- merge(x = zipcodeunique, 
                             y=uszipcodes, 
                             by = "ZIPCODE", 
                             all.x = TRUE)
  
  zipcodeunique <- zipcodeuniquetest[c(1, 2, 11, 12)]
  
  #table with zipcode and lat long 
  
  #perform left join to add all lat longs to the main NYC dataset with critical flags
  nycfinal <- merge(x=nycdata, 
                    y=zipcodeunique, 
                    by = "ZIPCODE", 
                    all.x = TRUE)
  
  #Final NYC dataset with zipcode lat long in all
  
  output$myMap = renderLeaflet({
    #Plot map with zipcode rectangle filtered by grade and cuisine
    thiscuisine = input$Cuisine
    thisgrade = input$Grade
    #I want to get number of "East Asia" rest in specific zipcodes for heatmap
    if(thiscuisine != "All")
    { 
      cuisinesubset = subset(nycfinal, CUISINE.ORIGIN == thiscuisine)
    }
    if(thiscuisine =="All")
      cuisinesubset = nycfinal

    #I want to get the number of "A" grade rests. in specific ZIPCODE for heatmap
    #make a subset with only grade A
    if(thisgrade != "All")
    {
      cuisinesubset = subset(cuisinesubset, GRADE == thisgrade)
    }
    
    #frequency of each rectangle for a specific grade
    #arrange based on zipcode and count the density in a specific zipcode
    loc_cuisine = cuisinesubset %>% group_by(ZIPCODE) %>% 
      summarise(CUISINECOUNT = n()) 
    loc_cuisine = full_join(loc_cuisine, zipcodeunique, by = "ZIPCODE")
    ## table with rest info on rest with grade A
    
    # mutate to include limitations if coordinates
    loc_cuisine <- loc_cuisine %>% mutate(LATL = LAT-0.003, LATH = LAT+0.003,
                                          LONGL = LONG-0.003, LONGH = LONG+0.003)
    
    #Leaflet Layers on Map
    #Leaflet layers on Map
    color.fill <- switch (thisgrade,
      "A" = "blue",
      "B" = "red",
      "C" = "purple",
      "Z" = "green",
      "All" = "yellow"
    )
    c = loc_cuisine %>% leaflet() %>% 
      addTiles( group = "OSM(default)") %>%
      setView(-74.01, 40.71, zoom=12) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
      addLayersControl(baseGroups = c("OSM", "Toner"),
                       options = layersControlOptions(collapsed = FALSE)
                       ) %>%
      addRectangles(
        lng1 = ~LONGL, lat1 = ~LATL,
        lng2 = ~LONGH, lat2 = ~LATH,
        fillOpacity = ~CUISINECOUNT/500, 
        opacity = 0, 
        fillColor = color.fill,
        label = ~CUISINECOUNT
        )
    c
    
  })
  
  # tab 4
  output$myTable = renderDataTable({
    return(datatable(nycdata, rownames = FALSE))
  })
  
}

shinyApp(ui = ui, server = server)