library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(leaflet)
library(scales)
library(tm)
library(syuzhet)
library(wordcloud)
library(ggplot2)
library(maps)
ui <- dashboardPage(  
  skin = "blue", 
  header = dashboardHeader(
    title = "Google's Ban on Huawei"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Home", 
        tabName = "page1", 
        icon = icon("home")
      ),
      
      hr() ,
      
      menuItem(
        text = "Topic Modeling ", 
        tabName = "page2",
        icon = icon("list-ul")
      ),
      menuItem(
        text = "Sentiment Analysis", 
        tabName = "page3",
        icon = icon("smile-wink")
      ),
      menuItem(
        text = "Wordcloud", 
        tabName = "page4",
        icon = icon("cloud")
      ),
      menuItem(
        text = "Users' Location", 
        tabName = "page5",
        icon = icon("location-arrow")
      ),
      menuItem(
        text = "Clustering", 
        tabName = "page6",
        icon = icon("globe-americas")
      ),
      hr() 
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "page1",
        h1(
          a(icon("home",lib = "font-awesome"), strong("Home")),
          align= "center"),
        fluidRow(
          column(1), 
          column(10,
          hr(),
        
        h3(strong("1.Project Description")),
        br(),
        
        h4(strong("(1).Huawei")),
        
        box(
          width=12,
          column(
            width = 10, 
            p("As the leading Chinese tech giant, Huawei is famous for its quality electronic devices around the world. 
              To take smartphone as an example, Huawei sold 206 million smartphones in 2018, accounting for 14.6% of the 
              whole market volume, only next to Samsung(20.8%) and Apple(14.9%). However, Huawei was added to the U.S 
              entity list which bans Huawei from doing business with all American corporations in May 15, 2019. 
              After that, a series of American companies follows this order and stopped their business with Huawei. 
              Below is a brief summary of what happened within the first two weeks of this issue:")
            ),
          column(
            width = 2, 
            img(src='123.jpg', align = "right", width = "100%", height = "100%"))
            ),
        
        br(),
        h4(strong("(2).Huawei Ban")),
        box(
          width=12,
          column(
            width=8,
            br(),
            tags$ul(
              tags$li("Wednesday, May 15:  The U.S. government added Huawei to the Entity List via executive order, thereby blacklisting this company as far as U.S. corporations are concerned."),
              br(),
              tags$li(strong("Sunday, May 19:     Google publicly stated it will obey the this administration order. Huawei was only be allowed to use the public version of Android and would not be able to get access to proprietary apps and services from Google.")),
              br(),
              tags$li("Monday, May 20:     Intel and Qualcomm joined Google: Neither company issued a statement, but sources cited by Bloomberg said the companies would comply with the order."),
              br(),
              tags$li("Wednesday, May 22:  British chip designer Arm told its employees to halt conducting business with Huawei."),
              br(),
              tags$li("Friday, May 24:     Huawei was barred from SD card organization.")
            )
          ),
          column(
            width=4,
            br(),
            br(),
            img(src='456.jpg',  align = "right", width = "100%", height = "100%")
          )
        ),
        br(),
        
        h3(strong("2.Research Questions")),
        p(
          tags$span("What is people's opinions on"), 
          a(href="https://www.androidauthority.com/huawei-google-android-ban-988382/", target="_blank", "Huawei ban"),
          tags$span("and what is their attitude toward"),
          a(href="https://www.xda-developers.com/google-revoke-huawei-android-ban-blacklist/", target="_blank", "Google's ban on Huawei"),
          tags$span("from using Android in its new devices?")
        ),
        br(), 
        
        h3(strong("3.Methods")),
        p("The main methods in this project are Tweeter Scrapping, Topic Modeling, Sentiment Analysis, Wordcloud Visualization and Location Mapping"),
        br(),
        
        h4(em("(1).Twitter Scrapping")),
        p("We use Twitter API collect more than 2000 tweets about this issue. The test tweets are all bewteen May 15 and May 26."),
        br(),
        
        h4(em("(2).Topic Modeling")),
        p("Using Topic Modeling methods, we refine the tweets we collect and then extract 4 main topics from the filtered group so that we know what topcis people around the world mainly talked about before and after this issue."),
        br(),
        
        h4(em("(3).Sentiment Analysis")),
        p("We use Sentiment Analysis to analyze tweeter users' sentiment change about Huawei and the issue of Google's ban on it from the beginning of U.S. ban to 1 week after Google followed the government's order."),
        br(),
        
        h4(em("(4).Wordcloud Visualization")),
        p("We will explore which specific topics tweeter users mainly talked about in each day."),
        br(),
        
        h4(em("(5).Location Mapping")),
        p("We use location mapping to get and mark the coordinates of selected twitter users that participated in the discussion."),
        br(),
        
        h3(strong("4.Dataset Description")),
        tags$ul(
          tags$li("Source: Twitter API"),
          tags$li("Key Words: Google, Huawei, Android" ),
          tags$li("Scraping Data: May 15 - May 26"),
          tags$li("Sample Size: 2203")
        ),
        DT::dataTableOutput("DataSample"),
        br(),
        
        h3(strong("5.Team Description")),
        box(
          width=12,
          column(
            width = 8, 
            br(),
            p(strong("WeFaries Team")),
            p("Johns Hopkins Carey Business School MS Marketing Candidates"),
            p("BU.520.650.81.SU19 Data Visualization")
          )
        ),
        br(),
        hr(), 
        h3("Made in China",align= "center")
        )
        )
        ),
      
      tabItem(
        tabName = "page2",
        titlePanel(
          h1(
            a(icon("list-ul",lib = "font-awesome"), strong("Topic Modeling")),
            align= "center")
        ),
        br(),
        tabBox(
          width = 12,
          tabPanel(
            title = "Topic Heat Each Day",
            fluidRow(
              column(
                width = 9,
                plotOutput("plot_topic", height = 550)
              ),
              column(
                width = 3,
                sliderInput(
                  inputId = "input_topic", 
                  label = "Date",
                  #pre = "2019-5-",
                  min = as.Date("2019-05-15", "%Y-%m-%d"), max = as.Date("2019-05-26", "%Y-%m-%d"),
                  value = as.Date("2019-05-15", "%Y-%m-%d"),
                  timeFormat = "%m/%d",
                  ticks = T,
                  animate = animationOptions(interval = 1000, loop = TRUE)),
                hr(),
                p(strong("1. Trade War:")),
                p("trade war (countries, world)/ US government action (trump, adminstration, government)"),
                p(strong("2. Ecosystem")),
                p("ios, app, store, platform, developer, opensource"),
                p(strong("3. Industry")),
                p("industry (suppliers)/ competition (brands)"),
                p(strong("4. Huawei Facts")),
                p("Huawei action/ Google action (Google, iron grip, remove, update)"))
            )
          ),
          tabPanel(
            title = "Topic  Heat Overview",
            width = 12,
            plotOutput("plot_topic2", height = 550))
        ),
        p("Powered by Shiny from RStudio", align = "right")
      ),
      
      tabItem(
        tabName = "page3",
        titlePanel(
          h1(
            a(icon("smile-wink",lib = "font-awesome"), strong("Sentiment Analysis")),
            align= "center")
        ),
        br(),
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              plotOutput("plot_sentiment", height = 500)
            )
          ),
          column(
            width = 3,
            radioButtons(
              inputId = "input_sentiment", 
              label = "Topic",
              choiceNames = c("Trade War", "Operation System", "Phone Industry", "Huawei Facts"),
              choiceValues = c("1", "2", "3", "4"))
          )
        ),
        box(
          width = 12,
          h3(strong("Sample of Tweets from the Selected Topic")),
          br(),
          DTOutput("sentiment_table", width = "100%", height = "auto")
        ),
        p("Powered by Shiny from RStudio", align = "right")
      ),
      
      tabItem(
        tabName = "page4",
        titlePanel(
          h1(
            a(icon("cloud",lib = "font-awesome"), strong("Wordcloud")),
            align= "center")
        ),
        br(),
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              plotOutput("plot_wordcloud", height = 550))
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "input_wordcloud", 
              label = "Date",
              #pre = "2019-5-",
              min = as.Date("2019-05-15", "%Y-%m-%d"), max = as.Date("2019-05-26", "%Y-%m-%d"),
              value = as.Date("2019-05-15", "%Y-%m-%d"),
              timeFormat = "%m/%d",
              ticks = T,
              animate = animationOptions(interval = 2000, loop = TRUE)),
            hr(),
            sliderInput(inputId = "num_word", "Number of words:", 5, 70, 20),
            hr(),
            div("Red Color = Negative Sentiment", style = "color:red"),
            div("Green Color = Positive Sentiment", style = "color:green")
          )
        ),
        p("Powered by Shiny from RStudio", align = "right")
      ),
      
      tabItem(
        tabName = "page5",
        titlePanel(
          h1(
            a(icon("location-arrow",lib = "font-awesome"), strong("Users' Location")),
            align= "center")
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              plotOutput("plot_world", width = "100%", height = 500))
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "input_world", 
              label = "Date",
              #pre = "2019-5-",
              min = as.Date("2019-05-15", "%Y-%m-%d"), max = as.Date("2019-05-26", "%Y-%m-%d"),
              value = as.Date("2019-05-15", "%Y-%m-%d"),
              timeFormat = "%m/%d",
              ticks = T,
              animate = animationOptions(interval = 10000, loop = TRUE))
          )
        ),
        p("Powered by Shiny from RStudio", align = "right")
      ),
      
      tabItem(
        tabName = "page6",
        titlePanel(
          h1(
            a(icon("globe-americas",lib = "font-awesome"), strong("Location Clustering")),
            align= "center")
        ),
        br(),
        titlePanel(h2("Interactive Heat Map", align= "center")),
        div(p("(zoom in and check the locations)"),align = "center"),
        fluidRow(
          column(1),
          column(10, leafletOutput("plot_cluster", width = "100%", height = 600))
        ),
        br(),
        p("Powered by Shiny from RStudio", align = "right")
      )
      )
      ),

footer = dashboardFooter(
  left = p(
    icon("user", lib="glyphicon"),
    strong("WeFairies")
  ),right = p(
    icon("graduation-cap"),
    tags$span("Tutored by"),
    strong("Professor Mohammad Ali Alamdar Yazdi")
  )
)
)