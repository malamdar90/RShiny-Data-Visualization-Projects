library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggrepel)
library(DT)
library(RColorBrewer)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title = "Netflix Analysis"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = 'Home',
        tabName = 'page1',
        icon = icon('home')
      ),
      menuItem(
        'Dashboard',
        tabName = 'page2',
        icon = icon('dashboard')
      ),
      menuItem(
        'Revenue Analysis',
        tabName = 'page3',
        icon = icon('money')
      ),
      menuItem(
        'Subscribers Analysis',
        tabName = 'page4',
        icon = icon('user-friends')
      ),
      menuItem('Source', tabName = 'page5', icon = icon('poll-h'))
    )
  ),
  skin = 'red',
  body = dashboardBody(
    tags$style(
      HTML(
        "

            p {
            line-height: 2;
            }

            .content-wrapper{
            background-color:white;
            }


          "
      )
    ),
    tabItems(
      # Home page content
      tabItem(
        tabName = 'page1',
        h1(strong(
          'Netflix Revenue and Subscription Analysis'
        )),
        br(),
        br(),
        img(src = 'logo', alt = 'Netflix Logo', width = 300),
        h2('About Netflix'),
        br(),
        h4(
          p(
            "Netflix is one of the world's leading entertainment services with 208 million paid memberships in over 190 countries, including TV series, documentaries and feature films across a wide variety of categories and languages."
          ),
          p(
            "As a member, they can watch as many as they want, at anytime, anywhere, as long as they have an internet-connected screen. In addition, members can play, pause and resume watching, all these actions are without commercials or commitments. What’s more, Netflix provides game-streaming services as well."
          ),
          p(
            "Moreover, the company's primary business is a subscription-based streaming service offering online streaming from a library of films and television series, including those produced in-house. Here, we have Netflix’s data about the number of subscribers."
          )
        ),
        br(),
        h2('About Data'),
        br(),
        h4(
          p(
            "The data was categorized by four different regions: Asia-Pacific, Europe, Middle East, and Africa, Latin America, United States and Canada over the last 2.5 years. We used data to show how Netflix’s subscription figures and Netflix's revenue($) have grown and to visualize the different number of subscribers by region to see which region has the most subscribers."
          )
        ),
        br()
      ),
      
      # Dashboard page content
      tabItem(
        tabName = 'page2',
        h1('Overview'),
        br(),
        plotOutput('plotRO'),
        br(),
        h4(
          "From the chart, we can clearly see a pattern that the revenue is increasing from 2018 to 2020, quarter by quarter.
                       In North America and Europe, revenue increased significantly and in Asia and Latin America, revenue increased comparatively slower.
                       The increasing trend in the different regions is proportional to the quantity of revenue by region."
        ),
        br(),
        plotOutput('plotSO'),
        br(),
        h4(
          "The numbers of subscribers are also increasing, regardless of which region.
                       However, the number of subscribers in Europe increased the most , indicating that Europe is a new booming hub of Netflix.
                       We take a look from the whole picture, we can see that North America still takes the biggest portion in revenue and number of subscribers."
        ),
        br(),
        plotOutput('plotPR'),
        plotOutput('plotPS')
      ),
      # Revenue analysis content
      tabItem(
        tabName = 'page3',
        sliderInput(
          "yearR",
          "Year:",
          min = 2018,
          max = 2020,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 2000, loop = FALSE)
        ),
        plotOutput('plotRev')
      ),
      # Subscribers analysis content
      tabItem(
        tabName = 'page4',
        sliderInput(
          "yearS",
          "Year:",
          min = 2018,
          max = 2020,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 2000, loop = FALSE)
        ),
        plotOutput('plotSub')
      ),
      # Source content
      tabItem(
        tabName = 'page5',
        dataTableOutput('mytable'),
        tags$a(href = "https://www.kaggle.com/pariaagharabi/netflix2020",
               'Data Source', target = '_blank')
      )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  ######################################## Data preparation #######################################
  # Read the data sets
  revenue <- read.csv('Revenue2020.csv')
  subscrib <- read.csv('Subscriber2020.csv')
  
  # Merge 2 datasets
  netflix <- revenue %>%
    left_join(subscrib, by = c('Area', 'Years'))
  # Separate the Years column in to Quarter and Year
  netflix <- mutate(netflix, byQuater = Years)
  netflix <-
    separate(
      netflix,
      col = Years,
      into = c('Quarter', 'Year'),
      sep = '-'
    )
  # Change Revenue and Subscriber columns to smaller decimal
  netflix <-
    mutate(netflix,
           Revenue = Revenue / 1000000,
           Subscribers = Subscribers / 1000000)
  # Change Area column to factor
  netflix$Area <- as.factor(netflix$Area)
  netflix$byQuater <- as.factor(netflix$byQuater)
  netflix$Year <- as.numeric(netflix$Year)
  
  
  
  ############################################# Visualization #########################################
  ########### Dashbaord page
  # Creating Revenue overview line plot
  output$plotRO = renderPlot({
    plotRO <- netflix %>%
      ggplot(aes(
        x = byQuater,
        y = Revenue,
        color = Area,
        group = Area
      )) +
      geom_point() +
      geom_line()
    
    plotRO <-
      plotRO + labs(title = 'Revenue Overview from 2018 to 2020',
                    color = 'Region',
                    x = 'Quarter') +
      theme_tufte() +
      scale_color_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    plotRO
  })
  
  # Creating Subscribers overview line plot
  output$plotSO <- renderPlot({
    plotSO <- netflix %>%
      ggplot(aes(
        x = byQuater,
        y = Subscribers,
        color = Area,
        group = Area
      )) +
      geom_point() +
      geom_line() +
      labs(title = 'Subscribers Overview from 2018 to 2020',
           color = 'Region',
           x = 'Quarter') +
      theme_tufte() +
      scale_color_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    
    plotSO
  })
  
  # Create a pie chart for revenue and subscriber
  output$plotPR <- renderPlot({
    plotPR <- ggplot(netflix, aes(x = '', y = Revenue, fill = Area)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = 'Revenue Distribution by Region',
           fill = 'Region',
           x = ' ') +
      theme_tufte() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
      ) +
      scale_fill_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    plotPR
  })
  
  output$plotPS <- renderPlot({
    plotPS <- ggplot(netflix, aes(x = '', y = Subscribers, fill = Area)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = 'Subscribers Distribution by Region',
           fill = 'Region',
           x = ' ') +
      theme_tufte() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
      ) +
      scale_fill_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    
    plotPS
  })
  
  ############ Revenue analysis page
  # Create a bar chart of revenue by year
  output$plotRev <- renderPlot({
    data_2 = netflix %>%
      filter(Year == input$yearR)
    
    max_q = length(unique(data_2$Quarter))
    
    plotRev <- data_2 %>%
      ggplot(aes(x = Quarter, y = Revenue, fill = Area)) +
      geom_bar(stat = 'identity', position = "dodge2") +
      labs(title = 'Netflix Revenue by Quarter in each year') +
      theme_tufte() +
      scale_fill_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    plotRev <- plotRev + annotate(
      "text",
      x = max_q / 2 + 0.5,
      y = 1300,
      color = "grey20",
      size = 30,
      label = input$yearR,
      alpha = 0.3
    )
    plotRev
  })
  
  
  ############ Subscription analysis page
  output$plotSub <- renderPlot({
    data_2 = netflix %>%
      filter(Year == input$yearS)
    max_q = length(unique(data_2$Quarter))
    
    plotSub <- data_2 %>%
      ggplot(aes(x = Quarter, y = Subscribers, fill = Area)) +
      geom_bar(position = "dodge2", stat = "identity") +
      labs(title = 'Netflix Subscribers by Quarter in each year') +
      theme_tufte() +
      scale_fill_brewer(type = 'seq', palette = 'RdGy') +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    plotSub <- plotSub + annotate(
      "text",
      x = max_q / 2 + 0.5,
      y = 40,
      color = "grey20",
      size = 30,
      label = input$yearS,
      alpha = 0.3
    )
    plotSub
  })
  
  
  
  ############ Data table page
  output$mytable <- renderDataTable({
    return(datatable(netflix))
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
