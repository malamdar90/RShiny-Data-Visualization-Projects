library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(DT)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "DC Real Estate"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Statistics",
        icon = icon("server"),
        menuSubItem("Price Histogram", tabName = "pg1"),
        menuSubItem("Price Change", tabName = "pg2"),
        menuSubItem("Heat Map", tabName = "pg4")
      ),
      menuItem("Map", tabName = "map", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "intro",
        column(width = 12, img(src = "X-1.jpg", width = 600), align = "center"),
        #              img(src = "X-1.jpg", width = 700),
        h2("General Description"),
        p("Using the information on houses sold in Washington D.C., we created different charts and visualizations (histogram, boxplot, heatmap, data table) to better understand the price trend over years as well as the intrinsic relationship between variables. From our analysis, we find that housing price has increased significantly from 2005 to 2016 with average price from 560K to 776K. We also find that whether or not a house has AC has huge impact on the price. Moreover, the most expensive neighborhood’s in DC is Georgetown."),
        a(href = "https://www.kaggle.com/christophercorrea/dc-residential-properties", "Data Source"),
        h2("Research Questions"),
        p("• How does residential property value change in DC area over years?"),
        p("• How does number of bedroom/bathroom/AC can impact sale price and by how much?"),
        p("• What are some most expensive neighborhoods in DC?"),
        p("• What are the statistics (min, max, average) of different variables?"),
        p("• How do I find my dream house?"),
        h2("Data Cleaning"),
        p("• Convert to dataframe"),
        p("• Select only residential properties from SOURCE variable"),
        p("• Create ADDRESS variable by combing full address, city, state, and zip code"),
        p("• Create YR variable by selecting the maximum value for AYB, YR_RMDL, and EYB"),
        p("• Combine variable STORIES and STYLE to STORIES only"),
        p("• Remove missing value in PRICE, BATHRM, ROOMS, BEDRM, NUM_UNITS, KITCHENS, and LANDAREA"),
        p("• Remove useless variables STORIES,USECODE,AYB,YR_RMDL,EYB,X.1,SQUARE,FULLADDRESS, CITY, STATE, ZIPCODE, NATIONALGRID,WARD,X,Y,CMPLX_NUM, LIVING_GBA, SOURCE, CENSUS_TRACT, CENSUS_BLOCK, BLDG_NUM, GIS_LAST_MOD_DTTM"),
        p("• Save cleaned dataset to cleanedDC.csv"),
        h2("Data Exploration"),
        p("Histogram: shows the trend of the most concerned variable price change over years."),
        p("Heatmap: shows area of neighborhoods in DC based on their sales price. Red color indicates expensive area while green indicates cheap area. Georgetown, Dupont circle, Logan circle are among the most expensive areas."),
        p("Data table: shows all data points with filter of price range, quadrant location, number of bedrooms, number of bathrooms, and whether has AC or not. It is a very useful tool for people who are looking for their dream house.")
      ),
      tabItem(
        tabName = "pg1",
        h2("Distribution of Price Over Years"),
        br(),
        column(
          width = 4,
          sliderInput("year1", "Year:",
            min = 2005, max = 2018, value = 2000, step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)
          ),
          uiOutput("text1")
        ),
        column(
          width = 8,
          plotOutput("plot1", height = 280)
        )
      ),
      tabItem(
        tabName = "pg2",
        h2("Price Impact Elements"),
        br(),
        column(
          width = 4,
          sliderInput("bedrm", "Bedroom Number:",
            min = 1, max = 7, value = 4, step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)
          ),
          br(),
          sliderInput("bathrm", "Bathroom Number:",
            min = 1, max = 5, value = 3, step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)
          ),
          br(),
          radioButtons("ac", "With or without AC:", c("With AC" = "Y", "Without AC" = "N")),
          p("")
        ),
        column(
          width = 8,
          plotOutput("plot2")
        )
      ),
      tabItem(
        tabName = "pg4",
        h2("Price Heatmap"),
        br(),
        column(
          width = 2,
          radioButtons("radio2", "Location:", choices = list(
            "NW" = "NW", "SW" = "SW",
            "NE" = "NE", "SE" = "SE"
          ))
        ),
        column(
          width = 10,
          leafletOutput("plot4")
        ),
        br(),
        column(
          width = 12,
          br(),
          img(src = "box.jpg", width = 600), align = "center"
        )
      ),
      tabItem(
        tabName = "map",
        h2("Datatable"),
        br(),
        column(
          width = 4,
          sliderInput("range", "Price Between:", min = 100000, max = 1000000, value = c(200000, 400000)),
          sliderInput("bedrm2", "Bedroom Number:", min = 1, max = 8, value = 4),
          sliderInput("bathrm2", "Bathroom Number:", min = 1, max = 5, value = 3),
          radioButtons("ac2", "Air Conditioner:", c("With AC" = "Y", "Without AC" = "N")),
          radioButtons("radio", "Location:", choices = list(
            "NW" = "NW", "SW" = "SW",
            "NE" = "NE", "SE" = "SE"
          ))
        ),
        column(
          width = 8,
          leafletOutput("plot5")
        ),
        br(),
        dataTableOutput("table")
      )
    )
  )
)





server <- function(input, output) {
  data <- read.csv("cleanedDC.csv")

  subdata1 <- reactive({
    data[data$YEAR == input$year1, ]
  })
  output$plot1 <- renderPlot({
    ggplot(data = subdata1(), aes(PRICE)) +
      geom_histogram(binwidth = 100000, fill = "steelblue") +
      xlim(0, 2500000) +
      ylim(0, 500) +
      geom_vline(aes(xintercept = mean(subdata1()$PRICE, na.rm = TRUE)), linetype = 2, col = "gray30", size = 1) +
      scale_y_continuous(name = "COUNT")
  })

  textMessages <- c(
    "In 2005, the average price of DC real estate was $559,180, total number of sale was 2,241.",
    "In 2006, average price was $564,875, total sale number was 2,040.",
    "In 2007, average price was $608,817, total sale number was 2,130.",
    "In 2008, impacted by the financial crisis, the average price dropped to $587,412, and the total number of sale was 1,782.",
    "In 2009, DC real estate market was still under the impact of financial crisis, the average price kept going down to $552,507, but the total sale number warmed up to 2,205.",
    "In 2010, the market showed signs to warm up, the average price raised up to $588,479, total sale number was 2,364.",
    "In 2011, average price was $600,840, total sale number was 2,512.",
    "In 2012, average price was $641,417, total sale number was 2,788.",
    "In 2013, the average price raised up by 12%, reached to $719,976, total sale number was 3,302.",
    "In 2014, the average price kept up raising, reached to a level of $722,249. What’s more surprising, the total sale number increase 203%, to 10,029",
    "In 2015, the average price was $766,750, total sale number was 13,636.",
    "In 2016, the average price was $775,656, which was a little bit lower than the previous year, but the total sale number kept increase to 15,308.",
    "In 2017, it is a flourishing year of DC real estate market, the average price reached to $801,955, which is the highest level in the past decade. The total sale number also sharply raised up to 19,100.",
    "In 2018, the price and sale number fell back, the average price was $775,037, total sale number was 11,150. In the past 13 years, DC real estate market average price has increased by 38.6%, the total sale number increased by 397%, which is almost 4 times than in 2005."
  )

  output$text1 <- renderUI({
    wellPanel(p(textMessages[input$year1 - 2004]))
  })

  subdata2 <- reactive({
    data[data$BEDRM == input$bedrm & data$BATHRM == input$bathrm & data$AC == input$ac, ]
  })
  output$plot2 <- renderPlot({
    ggplot(data = subdata2(), aes(x = YEAR, y = PRICE)) +
      geom_point() +
      ylim(0, 2000000) +
      geom_hline(aes(yintercept = mean(subdata2()$PRICE)), linetype = 5, col = "gray30") +
      geom_smooth() +
      scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015))
  })


  subdata4 <- reactive({
    data[data$QUADRANT == input$radio2, ]
  })
  output$plot4 <- renderLeaflet({
    leaflet(data = subdata4()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addWebGLHeatmap(
        lng = ~ subdata4()$LONGITUDE, lat = ~ subdata4()$LATITUDE,
        intensity = ~ subdata4()$PRICE / subdata4()$GBA / 12500, size = 240
      )
  })


  # Price Range: input$range[1], input$range[2]
  # District: radio
  # Bedroom num: bedrm2
  # Bathroom num: bathrm2
  # AC (Y/N): ac2

  subdata5 <- reactive({
    data[data$QUADRANT == input$radio & data$PRICE <= input$range[2] & data$PRICE >= input$range[1] &
      data$BEDRM == input$bedrm2 & data$BATHRM == input$bathrm2 & data$AC == input$ac2, ]
  })
  output$table <- renderDataTable(datatable(subdata5()[, c("ADDRESS", "YR", "BEDRM", "BATHRM", "KITCHENS", "GRADE", "PRICE")]))

  output$plot5 <- renderLeaflet({
    leaflet(data = subdata5()) %>%
      addTiles() %>%
      addMarkers(
        lng = ~ subdata5()$LONGITUDE, lat = ~ subdata5()$LATITUDE,
        popup = ~ paste0(
          "Price: $", subdata5()$PRICE, br(),
          "Bedroom Number: ", subdata5()$BEDRM, br(),
          "Bathroom Number: ", subdata5()$BATHRM, br(),
          "Kitchen Number: ", subdata5()$KITCHENS, br(),
          "Grade: ", subdata5()$GRADE, br(),
          "Address: ", subdata5()$ADDRESS
        ),
        clusterOptions = markerClusterOptions()
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
