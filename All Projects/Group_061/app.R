library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggplot2)
library(googleVis)
library(tidyverse)
library(DT)
library(leaflet)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)
library(shinyWidgets)

options(scipen = 999)

# loading dfs
location = read.csv("data/location.csv", stringsAsFactors = F)
province = geojsonio::geojson_read("data/china.json", what = 'sp')
date = read.csv("data/date.csv", stringsAsFactors = F)
product_category = read.csv("data/category.csv", stringsAsFactors = F)
category_date = read.csv("data/category_date.csv", stringsAsFactors = F)
merge = read.csv("data/merge.csv", stringsAsFactors = F)

category_date$purchase_date <- as.Date(category_date$purchase_date)
date$purchase_date <- as.Date(date$purchase_date)

#China Province
CNprovince = sort(as.vector(t(merge %>% distinct(s_province)))[1:20])
as.vector(t(merge %>% distinct(delivered_year) %>% arrange(delivered_year))) -> Years
as.vector(t(merge %>% distinct(product_category))) -> category

k.max <- 10
withinss <-
  c(440000,
    300000,
    200000,
    105000,
    55000,
    35000,
    23000,
    15000,
    8000,
    5000)

merge %>%
  select(c_lat, c_lng) %>%
  na.omit() -> clusters_data

# Assigning variables
map_select = list(
  "Gross Merchandise Volume" = names(location)[[2]],
  "Order Value (Avg)" = names(location)[[3]],
  "Shipping Cost (Avg)" = names(location)[[4]],
  "Delivery Days (Avg)" = names(location)[[6]],
  "Review Score (Avg)" = names(location)[[7]]
)

select_xaxis = list(
  "Order Value (Avg)" = names(location)[[3]],
  "Shipping Cost (Avg)" = names(location)[[4]],
  "Delivery Days (Avg)" = names(location)[[6]]
)

select_yaxis = list(
  "Review Score (Avg)" = names(location)[[7]],
  "Actual Delivery Time" = names(location)[[8]]
)

prod_ctg = sort(colnames(category_date)[2:13])

prod_measures = list(
  "Gross Merchandise Value" = names(product_category)[[2]],
  "Sales per order" = names(product_category)[[3]],
  "Review Score (Avg)" = names(product_category)[[4]]
)

rankings = sort(product_category$category[order(product_category$total_sales, decreasing =
                                                  TRUE)[1:8]])

merge %>%
  select(
    'delivered_week',
    'delivered_year',
    'OrderConfirmationDuration',
    'SellerToCarrier',
    'CarrierToCustomer',
    's_province'
  ) %>%
  gather(
    'OrderConfirmationDuration',
    'SellerToCarrier',
    'CarrierToCustomer',
    key = 'DeliveryTime',
    value = days
  ) %>%
  na.omit()  -> DeliveryTime2
names(DeliveryTime2) = c("dw", "dy", "sprovince", "dt", "days")



ui <- fluidPage(theme = "style.css",
                shinyUI(
                  dashboardPage(
                    skin = "yellow",
                    dashboardHeader(title = "Kwai Live Commerce"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(
                          "Introduction",
                          tabName = "introduction",
                          icon = icon("align-justify")
                        ),
                        menuItem("Geolocation", tabName = "map", icon = icon("map")),
                        menuItem(
                          "Sales Trend",
                          tabName = "STrend",
                          icon = icon("line-chart")
                        ),
                        menuItem(
                          "Product Ranking",
                          tabName = "PRank",
                          icon = icon("bar-chart")
                        ),
                        menuItem("Tracks",
                                 tabName = "Track",
                                 icon = icon("dashboard")),
                        menuItem(
                          "Warehouse Location",
                          tabName = "Insights",
                          icon = icon("globe-americas")
                        ),
                        menuItem(
                          "References",
                          tabName = "Ref",
                          icon = icon("send", lib = 'glyphicon')
                        )
                      )
                    ),
                    dashboardBody(
                      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                      tabItems(
                        tabItem(tabName = "introduction",
                                fluidRow(column(
                                  width = 12,
                                  box(
                                    title = "Introduction",
                                    solidHeader = T,
                                    width = NULL,
                                    #status = "primary",
                                    id = "intro",
                                    tags$h5(tags$strong("Project Description")),
                                    tags$h5(
                                      "With the huge increase in traffic in the e-commerce industry, more and more people start to sale their products via live streaming, blogs of expert or celebrities and lifestyle stories from short videos and photos.
                                                                                Nevertheless, the sales and operations part of the business is still very important in the retail industry. They can be greatly improved, and has huge potential, just like demand forecasting.
                                                                                The supply chain network as a whole is an extremely long and complex process, and saving transportation or inventory costs is a big deal for many companies, especially for Kwai Inc.
                                                                                It can be seen from the reduction of physical stores that e-commerce is an inevitable choice for retail enterprises. One of the biggest differences between Amazon and other online retailers is the speed of delivery.
                                                                                This visual dashboard provides insights into the health and performance of real-time business services, with different priorities and responsibilities for different purposes from different perspectives."
                                    ),
                                    tags$h5(
                                      tags$i(
                                        "Background knowledge from KDD Cup 2020 Challenges for Modern E-Commerce Platform "
                                      )
                                    ),
                                    tags$h5(tags$strong("Research Questions")),
                                    tags$h5("Which candidate products are most popular among customers?"),
                                    tags$h5(
                                      "Which is the most important factor to measure customer satisfaction?"
                                    ),
                                    tags$h5("What is the average product delivery time and how can we improve it?"),
                                    
                                    tags$h5(tags$strong("What is Kwai App?")),
                                    HTML(
                                      '<iframe width="560" height="315" src="https://www.youtube.com/embed/prbUkLykzaE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
                                    )
                                  )
                                  
                                )),
                                fluidRow(column(
                                  width = 12,
                                  box(
                                    title = "About the Dataset",
                                    solidHeader = T,
                                    width = NULL,
                                    collapsible = TRUE,
                                    #status = "success",
                                    id = 'dataset',
                                    tags$h5(
                                      "The dataset comes from the real-scenario multimodal data of Kwai's E-commerce."
                                    ),
                                    tags$h5(tags$strong("Order Dataset:")),
                                    tags$h5(
                                      "1) order_id 2) user_id 3) order_status 4) order_submit_timestamp 5) order_to_carrier_date 6) order_to_customer_date 7) estimated_delivery_date"
                                    ),
                                    tags$h5(tags$strong("Reviews Dataset:")),
                                    tags$h5("1) review_id 2) order_id 3) review_score"),
                                    tags$h5(tags$strong("Customers Dataset:")),
                                    tags$h5(
                                      "1) customer_id 2) customer_zip_code 3) customer_city 5) customer_province"
                                    ),
                                    tags$h5(tags$strong("Sellers Dataset:")),
                                    tags$h5("1) product_id 2) product_category_name 3) product_name"),
                                    tags$h5(tags$strong("Geography Dataset:")),
                                    tags$h5(
                                      "1) geolocation_zip_code_2) geolocation_lat 3) geolocation_long 4) geolocation_city 5) geolocation_province"
                                    ),
                                    
                                    tags$h5("Source: Kwai App")
                                    
                                  )
                                  
                                ))),
                        tabItem(tabName = "map",
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      title = "Select to Plot",
                                      solidHeader = F,
                                      width = NULL,
                                      selectizeInput("geoin", label = NULL, map_select)
                                    )
                                  ),
                                  
                                  column(
                                    width = 6,
                                    box(
                                      title = "Data",
                                      solidHeader = F,
                                      collapsible = T,
                                      collapsed = TRUE,
                                      width = NULL,
                                      tableOutput({
                                        "table"
                                      })
                                    )
                                  ),
                                  column(
                                    width = 12,
                                    box(
                                      title = "Map",
                                      solidHeader = F,
                                      #status = "info",
                                      leafletOutput("geo", height = 800),
                                      width = NULL,
                                      height = "auto"
                                    )
                                  ),
                                  column(
                                    width = 12,
                                    box(
                                      id = "scatter",
                                      title = "Scatter Plot",
                                      solidHeader = F,
                                      collapsible = T,
                                      width = NULL,
                                      tags$h4("Measures"),
                                      selectInput(
                                        inputId = 'xcol',
                                        label = 'X Variable',
                                        choices = select_xaxis,
                                        selected = select_xaxis[[1]]
                                      ),
                                      selectInput(
                                        inputId = 'ycol',
                                        label = 'Y Variable',
                                        choices = select_yaxis,
                                        selected = select_yaxis[[1]]
                                      ),
                                      
                                      htmlOutput("geoscat"),
                                      tags$h5(textOutput("cor"))
                                    )
                                  )
                                  
                                )),
                        
                        tabItem(
                          tabName = "STrend",
                          fluidRow(
                            column(
                              width = 6,
                              box(
                                title = "Date Range Input",
                                solidHeader = F,
                                width = NULL,
                                dateRangeInput(
                                  "datein",
                                  label = NULL,
                                  start = head("2020-01-01", 1),
                                  end = tail(date$purchase_date, 1),
                                  min = head(date$purchase_date, 1),
                                  max = tail(date$purchase_date, 1)
                                )
                              )
                            ),
                            column(
                              width = 6,
                              box(
                                title = "Categories To Plot",
                                solidHeader = F,
                                width = NULL,
                                pickerInput(
                                  inputId = "product_category",
                                  choices = prod_ctg,
                                  selected = prod_ctg[12],
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                                )
                              )
                            ),
                            column(
                              width = 12,
                              box(
                                title = "Trends",
                                solidHeader = F,
                                width = NULL,
                                height = 600,
                                #status = "info",
                                htmlOutput("tim")
                              )
                            ),
                            column(
                              width = 12,
                              box(
                                title = "Category Input",
                                solidHeader = F,
                                width = NULL,
                                selectInput(
                                  "catsfortable",
                                  label = NULL,
                                  choices = prod_ctg,
                                  selected = prod_ctg[12],
                                  multiple = T
                                )
                              )
                            ),
                            column(
                              width = 12,
                              box(
                                title = "Data",
                                solidHeader = F,
                                width = NULL,
                                DT::dataTableOutput({
                                  "ctgtable"
                                })
                              )
                            ),
                            column(
                              width = 12,
                              box(
                                title = "Insights",
                                solidHeader = F,
                                width = NULL,
                                tags$h5(
                                  "From July 2019, to June 2020, the overall product sales has increased cumulatively over the years.
                                                                                However, if transform the data into logarithmic scale, it can be observed that the growth was the fastest in the initial stages, with a slight boost in June 2020 but then,
                                                                                the rate of growth slowed down. Although such trend is regular while business keep growing, it still worth to take note and try to continuously increase its conversion and sales."
                                )
                              )
                            )
                          )
                        ),
                        tabItem(tabName = "PRank",
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      title = "Bar Chart",
                                      solidHeader = F,
                                      width = NULL,
                                      height = 500,
                                      status = "danger",
                                      htmlOutput("PRank")
                                    )
                                  ),
                                  column(
                                    width = 4,
                                    box(
                                      title = "Measures",
                                      solidHeader = F,
                                      width = NULL,
                                      status = "danger",
                                      selectInput(
                                        "catvalue",
                                        label = NULL,
                                        choices = prod_measures,
                                        selected = "total_sales"
                                      )
                                    )
                                  ),
                                  column(
                                    width = 4,
                                    box(
                                      title = "Product Categories",
                                      solidHeader = F,
                                      width = NULL,
                                      status = "danger",
                                      pickerInput(
                                        inputId = "cats",
                                        label = NULL,
                                        choices = sort(product_category$category),
                                        selected = rankings,
                                        options = list(`actions-box` = TRUE),
                                        multiple = TRUE
                                      )
                                    )
                                  ),
                                  column(
                                    width = 4,
                                    box(
                                      title = "Data",
                                      solidHeader = F,
                                      collapsible = T,
                                      collapsed = T,
                                      width = NULL,
                                      status = "danger",
                                      tableOutput({
                                        "cattable"
                                      })
                                    )
                                  ),
                                  column(
                                    width = 12,
                                    box(
                                      title = "Insights",
                                      solidHeader = F,
                                      width = NULL,
                                      status = "danger",
                                      tags$h5(
                                        "Household are the best-selling products (in terms of sales) for Kwai's live-commerce site. Therefore, it is reasonable for Kwai to focus on further growing and milking the sales of products from this category.
                                                                                One such way would be to increase the diversity of household products and sellers that are focused on this category. Engaging in any promotion in price might not be useful as this is already a mature category where willing users are less sensitive to price."
                                      ),
                                      
                                      tags$h5(
                                        "On the other hand, categories such as clothing, DIY goods and services seem to lag behind in sales. Should Kwai's ecommerce want to try and balance their sales portfolio more, marketing strategies targeted at boosting sales in these categories should be developed.
                                                                                For example, it could consider cross selling or promotions such as free shipping vouchers which, according to industry knowledge, is the most attractive offer to users."
                                      )
                                    )
                                  )
                                  
                                  
                                  
                                  
                                )),
                        tabItem(tabName = "Track",
                                
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      title = 'Delivery Time',
                                      width = NULL,
                                      height = 600,
                                      plotOutput(outputId = "DeliveryTime_plot")
                                    )
                                  ),
                                  column(width = 6,
                                         box(
                                           #title="List of Input",
                                           width = NULL,
                                           pickerInput(
                                             inputId = "locYear",
                                             "Year",
                                             choices = Years,
                                             selected = Years[2],
                                             options = list(`actions-box` = TRUE),
                                             multiple = F
                                           ),
                                           pickerInput(
                                             inputId = "locInput",
                                             "Location",
                                             choices = CNprovince,
                                             selected = CNprovince[1],
                                             options = list(`actions-box` = TRUE),
                                             multiple = T
                                           )
                                         )),
                                  column(width = 6,
                                         box(
                                           title = "Insights",
                                           width = NULL,
                                           tags$h5(
                                             "Most orders are fulfilled within a week which is quite a fair delivery time for ecommerce orders.
                                                                                        However, it should also be noted that some orders take up to or more than two weeks to be fulfilled.
                                                                                        These would negatively affect the user experience and efforts should be carried out to improve this."
                                           ),
                                           tags$h6(
                                             tags$i(
                                               "CarrierToCustomer: Time from Carrier to Customer",
                                               br(),
                                               "OrderConfirmationDuration: Order Confirmation time (fraud detection)",
                                               br(),
                                               "SellerToCarrier: Time from Seller to Carrier"
                                             )
                                           )
                                         ))
                                  
                                  
                                )),
                        
                        
                        tabItem(tabName = "Insights",
                                fluidRow(
                                  column(
                                    width = 9,
                                    box(
                                      title = "Choosing Warehouse Location",
                                      solidHeader = F,
                                      width = NULL,
                                      tags$h4(
                                        "Which cities in China are the best locations for warehouses to improve order delivery time?"
                                      ),
                                      tags$h5(
                                        "We used the elbow method to determine which value is the best for K of K-means:",
                                        plotOutput(outputId = 'k_op')
                                        
                                      )
                                    )
                                    
                                  ),
                                  column(
                                    width = 3,
                                    box(
                                      title = "Notes",
                                      solidHeader = T,
                                      width = NULL,
                                      #status="info",
                                      tags$h5(
                                        "Within-cluster variation measures how tightly grouped the clusters are. It is calculated by computing the Euclidean distance between the cluster
                                                                               centroid and the data points. The trend in shown above illustrates how such variation decreases when the number of clusters K increases."
                                      )
                                    )
                                    
                                  ),
                                  
                                  column(
                                    width = 12,
                                    box(
                                      title = 'Best Locations',
                                      solidHeader = T,
                                      status = "danger",
                                      width = NULL,
                                      tags$h5(
                                        "By selecting the location of the transportation warehouse through K-mean, the average delivery time can be reduced most efficiently, thus improving user satisfaction."
                                      ),
                                      tags$h5(
                                        "The best locations in cluster 1 is Changsha from Hunan",
                                        br(),
                                        br(),
                                        "The best location in cluster 2 is Kunming from Yunnan",
                                        br(),
                                        br(),
                                        "The best location in cluster 3 is Hefei from Anhui",
                                        br(),
                                        br(),
                                        "The best location in cluster 4 is Jinan from Shandong",
                                        br(),
                                        br(),
                                        "The best location in cluster 5 are Xining from Qinghai and Huhehaote from Inner Mongolia",
                                        br()
                                        
                                      )
                                    )
                                  )
                                  
                                )),
                        tabItem(tabName = "Ref",
                                fluidRow (column(
                                  width = 12,
                                  box(
                                    title = "References",
                                    solidHeader = T,
                                    width = NULL,
                                    status = "danger",
                                    tags$h5(
                                      "1. Perform K-means Clustering in R" ,
                                      br(),
                                      "https://towardsdatascience.com/k-means-clustering-in-r-feb4a4740aa",
                                      br(),
                                      br(),
                                      "2. Interactive choropleth map with R and leaflet" ,
                                      br(),
                                      "https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html",
                                      br(),
                                      br(),
                                      "3. Building Interactive World Maps in Shiny" ,
                                      br(),
                                      "https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/",
                                      br(),
                                      br(),
                                      "4. Introduction to googleVis" ,
                                      br(),
                                      "https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_intro.html",
                                      br(),
                                      br(),
                                      "5. renderGvis: Use a googleVis Chart as Shiny Output" ,
                                      br(),
                                      "https://rdrr.io/github/kleanthisk10/google-motion-charts-with-r/man/renderGvis.html",
                                      br(),
                                      br(),
                                      "6. gvisBarChart: Google Bar Chart with R barchart" ,
                                      br(),
                                      "https://www.rdocumentation.org/packages/googleVis/versions/0.6.0/topics/gvisBarChart",
                                      br(),
                                      br(),
                                      "7. GoogleVis scatter plot with Shiny" ,
                                      br(),
                                      "https://community.rstudio.com/t/googlevis-scatter-plot-with-shiny/9423",
                                      br(),
                                      br(),
                                      "8. KDD Cup 2020 Challenges for Modern E-Commerce Platform: Debiasing" ,
                                      br(),
                                      "https://tianchi.aliyun.com/competition/entrance/231785/introduction",
                                      br(),
                                      br(),
                                      "9. Shiny Dashboard" ,
                                      br(),
                                      "https://rstudio.github.io/shinydashboard/index.html",
                                      br(),
                                      br(),
                                      "10. Build An R Shiny Dashboard to Monitor Your Model Performance" ,
                                      br(),
                                      "https://medium.com/dataman-in-ai/build-an-r-shiny-dashboard-to-monitor-your-model-performance-1b47c2fef997",
                                      br()
                                    )
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                  )
                                )))
                      )
                    )
                  )
                ))


server <- function(input, output, session) {
  bins = reactiveValues()
  labtxt = reactiveValues()
  
  
  # Reactive Data For scatter Plot
  location_scat = reactive({
    req(input$xcol, input$ycol)
    
    location %>%
      select(input$xcol, input$ycol)
    
  })
  
  # Reactive Data For Correlation computing
  geo_corx = reactive({
    req(input$xcol)
    
    location %>%
      select(input$xcol)
  })
  geo_cory = reactive({
    req(input$ycol)
    
    location %>%
      select(input$ycol)
  })
  
  # Reactive Data For geo Table
  location_table = reactive({
    req(input$geoin)
    location %>%
      select(province, value = input$geoin) %>%
      arrange(desc(value))
  })
  
  # Reactive Data For Line chart
  category_date_line = reactive({
    req(input$datein)
    req(input$product_category)
    
    category_date %>%
      select(purchase_date, input$product_category) %>%
      filter(purchase_date >= input$datein[1] &
               purchase_date <= input$datein[2])
    
  })
  
  # Reactive Data for Categories Trend table
  cat_time_table = reactive({
    req(input$datein)
    req(input$catsfortable)
    
    
    category_date %>%
      filter(purchase_date >= input$datein[1] &
               purchase_date <= input$datein[2]) %>%
      select(Date = purchase_date, input$catsfortable)
  })
  
  # Reactive Data For Bar Chart and Table
  product_category_bar = reactive({
    req(input$catvalue)
    
    product_category %>%
      select(category, value = input$catvalue) %>%
      filter(category %in% input$cats) %>%
      arrange(., desc(value))
    
    
  })
  
  # Switching labels for map
  observe({
    if (input$geoin == "sales") {
      labtxt$x = "<strong>%s</strong><br/><strong>GMV:</strong> $%g"
      bins$y = c(0,
                 50000,
                 100000,
                 200000,
                 300000,
                 400000,
                 1000000,
                 2000000,
                 5000000,
                 Inf)
    } else if (input$geoin == "shipcost/unitsalesratio") {
      labtxt$x = "<strong>%s</strong><br/><strong>Ratio:</strong> %g"
      bins$y = 6
    } else if (input$geoin == "satisfaction") {
      labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %g"
      bins$y = 6
    } else if (input$geoin %in% c("deliverduration", "est_deliverduration")) {
      labtxt$x = "<strong>%s</strong><br/>%g Days"
      bins$y = 6
    } else {
      labtxt$x = "<strong>%s</strong><br/>$%g US"
      bins$y = 6
    }
  })
  
  
  #Graph for Map
  output$geo = renderLeaflet({
    pal = colorBin("YlOrBr",
                   location[, input$geoin],
                   bins = bins$y,
                   pretty = F)
    
    labels = sprintf(labtxt$x,
                     province$name,
                     location[, input$geoin]) %>% lapply(htmltools::HTML)
    
    geo = leaflet(province) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(location[, input$geoin]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
    geo %>%
      addLegend(
        pal = pal,
        values = location[, input$geoin],
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
  })
  
  #Geo scatter plot
  output$geoscat = renderGvis({
    gvisScatterChart(location_scat(),
                     options = list(
                       width = "justify",
                       height = "400px",
                       legend = "none"
                     ))
  })
  
  # Printing correlation
  output$cor = renderText({
    paste("Correlation:", round(cor(geo_corx(), geo_cory())[[1]], 2), sep = " ")
  })
  
  # Geo Data Output
  output$table = renderTable({
    head(location_table(), 6)
  },
  striped = T,
  spacing = 'l',
  width = '100%',
  colnames = F,
  digits = 2)
  
  
  # Trend Line Chart
  output$tim = renderGvis({
    gvisLineChart(
      category_date_line(),
      options = list(
        width = "automatic",
        height = "500px",
        vAxis = "{title: 'Sales (in $US)', format: 'short'}",
        hAxis = "{title: 'Date'}",
        animation = "{startup: true}"
      )
    )
  })
  
  # Trend table
  output$ctgtable = DT::renderDataTable({
    datatable(cat_time_table(), rownames = T)
  })
  
  
  # Categories Bar Chart
  output$PRank = renderGvis(gvisColumnChart(
    product_category_bar(),
    options = list(
      width = "automatic",
      height = "400px",
      bar = "{groupWidth: '60%'}",
      vAxis = "{title:'Sales (in $US)', format: 'short'}",
      hAxis = "{title:'Categories'}",
      animation = "{startup: true}",
      legend = "none"
    )
  ))
  
  # Categories Table
  output$cattable = renderTable({
    head(product_category_bar(), 10)
  },
  striped = T,
  spacing = 'l',
  width = '100%',
  colnames = F)
  
  
  output$DeliveryTime_plot <- renderPlot({
    dt_df =  DeliveryTime2 %>%
      filter(dy %in% input$locYear) %>%
      filter(sprovince %in% input$locInput) %>%
      group_by(dy, dw, dt) %>%
      summarise(mean2 = mean(as.numeric(days))) %>%
      ungroup()
    dD_df = merge %>%
      na.omit %>%
      filter(delivered_year %in% input$locYear) %>%
      group_by(delivered_year, delivered_week) %>%
      summarise(
        m_OrderSubmitToRDD = median(as.numeric(OrderSubmitToRDD)),
        m_POtoDO = mean(
          as.numeric(OrderConfirmationDuration) + as.numeric(SellToDeliver)
        )
      )
    
    ggplot() +
      geom_line(
        data = dD_df,
        aes(x = delivered_week, y = as.numeric(m_OrderSubmitToRDD)),
        linetype = "dashed",
        size = 1,
        label = 'OrderSubmitToRDD'
      ) +
      geom_col(data = dt_df, aes(
        x = dw,
        y = as.numeric(mean2),
        fill = dt
      )) +
      theme_bw() +
      labs(
        title = 'Time Takes from Order Purchase to Delivery',
        x = 'N-th Weeks',
        y = 'Average Delivery Days',
        color = 'Year'
      ) +
      scale_fill_manual(values = c("orange", "grey", "coral")) +
      theme(
        legend.key = element_blank(),
        plot.title = element_text(
          size = 15,
          face = 'bold',
          hjust = 0.5
        )
      ) +
      coord_cartesian()
  }, height = 500)
  
  output$k_op <- renderPlot({
    plot(
      1:k.max,
      withinss,
      type = "b",
      pch = 19,
      frame = FALSE,
      xlab = "Number of clusters K",
      ylab = "Total within-clusters sum of squares"
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
