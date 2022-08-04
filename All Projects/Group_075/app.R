library(readr)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tigris)
library(shiny)
library(tidyverse) 
library(wordcloud) 
library(Rcpp)
library(rvest)
library(ggthemes)
library(lubridate)
library(plotly)



ui <- fluidPage(
   
    
    dashboardPage(
        
        skin = "green",
        
        dashboardHeader(title="Shopping Behavior", titleWidth = 300),
        
        dashboardSidebar(width = 300,
                         sidebarMenu(
                             img(src = 'pngfind.com-background-png-849853.png', width = 186, 
                                 style="display: block; margin-left: auto; margin-right: auto;"),   
                             menuItem("Home", tabName = "page1", icon = icon("home")),
                             menuItem("Date and Time", tabName = "page2", icon = icon("clock")),
                             menuItem("Customer Behavior", tabName = "page3", icon = icon("user")),
                             menuItem("Data and Conclusion", tabName = "page4", icon = icon("database")),
                             menuItem("Word Cloud", tabName = "page5", icon = icon("cloud"))
                             )
                         
        ), 
        
        dashboardBody(
            
            tabItems(
                
                tabItem(tabName = "page1",
                        
                        h1("Online Shopping Behavior Analysis", align = 'center', 
                           style = "font-size:45px;"),
                        br(),
                        h2(icon('home'), 'Project Description', style = "font-size:30px;"),
                        p(h3("Online shopping has become trending in recent years. It provides convinience
                        for both the shopper and the seller as they can fully utilize the advantages of e-commerce
                        platforms. As the demand of online shopping increases, more and more people decide to open 
                        up online shops on C2C platform such as Taobao and Ebay. Therefore, it is important for them
                        to learn about shopper's behaviors and understand the entire flow-process behind every 
                        item sold", style = "font-size:20px;")),
                        p(h3("This interactive web application is built to explore online shopping data and discover
                          potential relationships between items sold and multiple other factors, including timing of 
                          transaction and different user behaviors. The second and thrid page features interactive 
                          visulizations that supports the analysis. The fourth page includes the conclusion we draw from
                          the analysis while also exhibits the main dataset we are using. The last page features a wordcloud 
                          that exhibits some commonly searched items on Ebay." , style = "font-size:20px;")),
                        br(),
                        h2(icon('question'), 'Research Questions', style = "font-size:30px;"),
                        p(h3("1. What is the relationship between quantity of items sold and the date & time when items are sold?
                          Is day to day quantity sold stable?", style = "font-size:20px;")),
                        p(h3("2. What is the relationship between quantity of items sold and the shopper's behaviors?", style = "font-size:20px;")),
                        br(),
                        h2(icon('database'), 'About the Dataset', style = "font-size:30px;"),
                        p(h3('The main dataset we used is the shopper behavior data from Taobao.com. You can access the original dataset',
                          a('here', href = 'https://tianchi.aliyun.com/dataset/dataDetail?dataId=649'), '. The original dataset has more than
                          100,000,000 rows and 5 columns. In order to perform analysis in R, we truncated the dataset and kept the first 100,000
                          columns for analysis. We then changed the time stamp column to standardized time format. To view the dataset, please navigate
                          to the Conclusion & Data page.', style = "font-size:20px;")),
                        p(h3('Another dataset we used is the keyword query dataset from Ebay. You can access the original dataset',
                          a('here', href = 'https://www.kaggle.com/awsaf49/ecommerce-search-result-relevane-by-crowdflower/version/1'), '. 
                          This dataset is not used for analysis purpose, rather, it is used to exhibit what items are being searched most often on Ebay
                          Since the orginal Taobao dataset only shows item id instead of what the item is.', style = "font-size:20px;")),
                        fluidRow(
                            column(6, img(src = 'Taobao.jpg', height = 400, width = 500)),
                            column(6, img(src = 'Ebay.jpg', height = 400, width = 500))
                        ),
                        
                        br(),
                        h2(icon('youtube'), 'Video Presentation', style = "font-size:30px;"),
                        p('Here is a video presentation that covers how to use this web application, click ', a('here', href = 'https://youtu.be/iMK_Y3Crttk'),
                          'to watch it on Youtube, or click the video below to watch it here.', style = "font-size:20px;"),
                        HTML('<iframe width="50%" height="300" src ="https://www.youtube.com/embed/iMK_Y3Crttk" frameborder="0" allowfullscreen ></iframe>'),
                        br(),
                        h2(icon('users'), 'Our Team', style = "font-size:30px;"),
                        p(h3('Team members:', br(),  'Johns Hopkins University Carey Business School BARM Candidates',
                          br(), 'BU.520.650.X2.SU21 Data Visualization', style = "font-size:20px;"))
                        
                ),
                
                tabItem(tabName = "page2",
                        
                        h2("Relationship between sales and date & time"),
                        br(),
                        fluidRow(box(
                            h4("Relationship between sales and date"),
                            plotlyOutput("plot1",height=350,width = "90%"),
                            br(h5("From the bar chart, we can see that sales volume on Nov.24 is much lower than other days, which may be caused by missing data. Ignore data of this day, sales volume on Dec.02 is highest and sales volume on Nov.25 is lowest. There are no obvious pattern among sales from Monday to Friday. The high sales volume on Dec. 02 is more likely to be caused by promotional activities.")),
                            width = 10
                            )),
                        br(),
                        fluidRow(
                        box(
                            h4("Relationship between sales and time"),
                            sliderInput("slider1", label = h4("Time Range"), min = 0, max = 24, value = c(0,24),width=350),
                            br(plotOutput("plot2",height=300,width = "90%")),
                            br(h5(" In the morning, the sales volumne fluctuates and gradually increases. The peak of the average sales volume occurs at 14:00, and then the sales volumne decreases steadily until 20:00.")),
                            width = 10
                            )
                        )
                ),
                
                tabItem(tabName = "page3",

                        h2("Relationship between customer behavior and sales"),
                        br(),
                        h4("In this page, we will explore the relationship between users behavior and sale with a funnel plot. we will analyze some 
                        important behaviors like favorating items, adding items into carts, and also buying products. You can find how many people
                          actually buy one item after putting it into the shopping cart.", style = "font-size:20px;"),
                        br(),
                        
                        fluidRow(box(
                            h4("Funnel plot of behavior type"),
                            plotlyOutput("plot3_1",height=350,width = "90%"),
                            width=10
                        )),
                        br(),
                        h4("In the first figure, user's size decreased through the process of Clicking -> Favoriting -> Purchasing. Clicks accounted 
                          for 89.7% of the total number of behaviors, adding to favorite list accounted for 2.75%, and final purchases accounted for
                          only 2.1%. The detail information from date level is shown as followed:", style = "font-size:20px;"),
                        br(),
                        fluidRow(
                            box(
                                h4("Analysis of different behaviors from date level"),
                                br(plotlyOutput("plot3_2",height=300,width = "90%")),
                                width = 10
                            )
                        )
                        
                ),
                
                tabItem(tabName = "page4",
                        
                        h2("Conclusion",style = "font-size:30px;"),
                        p(h3("1. Sales are related to the time of day. Shoppers mainly shopping around 2pm, and sales gradually decline after 3pm. Therefore, sellers can choose to carry out promotion and activities in this time period to boost shopper's spending. Moreover, It's a good idea to do promotions on weekends so sellers can have a large number of active users to drive sales. ", style = "font-size:20px;")),
                        p(h3("2. The number of shoppers will decrease in the process of clicking -> Favorites -> Buy. Therefore, based on this flow of user behavior, sellers can choose to add personalized reminder through chat box to improve the conversion rate of orders.",style = "font-size:20px;")),
                        p(h3("3. Since few users switch from clicking to buying, sellers can create memberships to boost sales and increase customer loyalty.",style = "font-size:20px;")),
                        br(),
                        h2("User Behavior Data",style = "font-size:30px;"),
                        dataTableOutput("UserBehaviorTable")
                ),       
                
                tabItem(tabName = "page5", 
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("color", "Color Pallet:",
                                            choices = c("Dark2","Paired","Set1")),
                                radioButtons("method","Scale Method" ,c("Square"="square",
                                                                        "Sqrt"="sqrt",
                                                                        "None"="none")),
                                sliderInput("max",
                                            "Maximum Number of Words:",
                                            min = 1,  max = 263,  value = 60)
                            ),
                            
                            mainPanel(
                                plotOutput("plot4",height = 500,width = "100%"),
                                br(),
                                br(),
                                a("Data Source",href="https://www.kaggle.com/awsaf49/ecommerce-search-result-relevane-by-crowdflower/version/1",target="_blank")
                            )
                        )
                        
                )
                
            )
            
        ) 
        
    )
    

)


server <- function(input, output, session) {
    UserBehavior <- read_csv("UserBehavior_Clean.csv")
    

    data1<-UserBehavior
    data1$date <- as.Date(data1$date)
    data1<-data1%>%
        mutate(weekday=wday(date, label = T))%>%
        filter(behavior_type=="buy")%>%
        arrange(date)
    Sys.setlocale("LC_TIME", "English")
    data1$weekday=wday(as.Date(data1$date,"%Y-%m-%d"),label=TRUE)

    data1$hour=as.POSIXlt(data1$time)
    data1$hour=hour(round_date(data1$hour,unit="hour"))
    
    output$plot1 <- renderPlotly({
        p1=data1%>%
            ggplot(aes(x=date,fill=weekday))+
            geom_histogram(binwidth = 0.5)+
            scale_fill_brewer(palette = "Greys", direction = 1)+
            labs(x="Date",
                 y="Sales")+
            theme_wsj()+
            theme(legend.position = "right",legend.direction = "vertical",legend.title = element_blank())
            
        
        ggplotly(p1)
    })
    output$plot2 <- renderPlot({
        data_hr=summarise(data1%>%group_by(hour),num=n())%>%
            filter(min(input$slider1)<=hour,hour<=max(input$slider1))
        p2=data_hr%>%
            ggplot(aes(x = hour, y = num)) +
            geom_line(size=1.0) +
            geom_text(aes(label= num,y=num+15),data=data_hr)+
            labs(x="Time in a day",
                 y="Sales")+
            theme_wsj()
        
        plot(p2)
    })
  
    
    data_hu<-UserBehavior %>%
        group_by(behavior_type) %>%
        summarise(counts=n()) %>%
        arrange(-counts)
    
    output$plot3_1 <- renderPlotly({
        
        
        p_hu <- ggplot(data_hu,mapping=aes(x = behavior_type,y=counts))+
            geom_histogram(stat ="identity",width = 0.4) +
            labs(x="Behavior_type",y="Counts")+
            coord_flip()+
            theme_wsj()
        ggplotly(p_hu)
    })
    
    output$plot3_2 <- renderPlotly({
        
        data_hu2<-UserBehavior%>%
            group_by(behavior_type,date)%>%
            filter(date %in% c('2017/11/24','2017/11/25','2017/11/26','2017/11/27','2017/11/28','2017/11/29','2017/11/30'))%>%
            summarise(counts=n())
        
        p_hu2 <- ggplot(data_hu2,mapping = aes(x = date,y=counts,fill=behavior_type)) +
            geom_bar(stat="identity",width=0.4) +
            theme_wsj()+
            theme(legend.position = "right",legend.direction = "vertical",legend.title = element_text(size = 9))+
            labs(fill = "Behavior Type")
        ggplotly(p_hu2)
    })   
      
 
    data2=read_csv("kaggle_ecommerce_complete_Rank_Crowd_AGG_Descriptions_ALL_ROWS.csv")
    data2$query=tolower(data2$query)
    query=data2%>%
        group_by(query)%>%
        summarise(N=n())
    
    output$plot4 <- renderPlot({
        cal=switch(input$method,
                   square=(query$N)^2,
                   sqrt=sqrt(query$N),
                   none=query$N
        )
        frequency=round(cal,0)
        set.seed(10)
        wordcloud(words = query$query, 
                  freq = frequency,
                  min.freq = 10,
                  max.words=input$max,
                  colors=brewer.pal(8, input$color),
                  scale = c(3,0.5),
                  random.order=F)
    })
    
    output$UserBehaviorTable = renderDataTable(UserBehavior)
}
   

shinyApp(ui = ui, server = server)
