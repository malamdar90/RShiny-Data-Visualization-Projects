library(shiny)
library(lessR)
library(reader)
library(plyr)
library(ggplot2)
library(dplyr)
library(corrgram)

source("data.R")

choices <- names(d)[2:(length(d) - 3)]


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "superhero"),
    navlistPanel(
    id = "tabset",
    tabPanel(
        "introduction",
        h1("Twtich.com Exploratory data analysis"),
        h2("We would like to conduct an exploratory data analysis on the trending streaming platform Twitch.com. Proving visualizations on performance data and the impact of those top streamers. The website aims to include the overall performance (Viewers, followers, stream time) of top streamers")
        ),
    tabPanel(
        "data description",
        h3("data description :"),
        p("Data description:  Watch time: Total amount of time the audience spend time watching a specific channel "),
        p("Stream time: Total amount of time streamer was on the platform the sum of the stream\'s durations."),
        p("Peak Viewers: Maximum number of viewers simultaneously watching a channel for a specific period. Also known as max viewers"),
        p("Average viewers: The average number of concurrent viewers on live streams in the selected period."),
        p("Followers gained: Number of followers gained by a channel within a specific period. The difference between last and first followers count during a selected period."),
        imageOutput("image") 
        ),
    tabPanel(
        "overall",
            #plotOutput("hot"),
            verbatimTextOutput("sum")
        ),
    tabPanel(
        "language relation",
        sidebarLayout(
            sidebarPanel(
                selectInput("tar", "target", choices)
            ),
            mainPanel(
                plotOutput("langrel")
            )
        )
        ),
    tabPanel(
        "any relation",
        sidebarLayout(
            sidebarPanel(
                selectInput("x", "relation x", choices),
                selectInput("y", "relation y", choices)
            ),
            mainPanel(
                plotOutput("relation"),
                textOutput("rel_q"),
                textOutput("rel_a")
            )
        )
        ),
    tabPanel(
        "top channel",
        sidebarLayout(
            sidebarPanel(
                selectInput("order", "order by", c("Hours.Watched", "Peak.Viewers", "Average.Viewers", "Airtime", "Followers.Gain")),
            ),
            mainPanel(
                tableOutput("channel")
            )
        )
        ),
    tabPanel(
        "conclusion",
        h1("In conclusion:"),
        h2("To increase views and followers, the streamers should focus on grabbing audience attention and increase their watch time rather than simply spending long-time streaming. To do that, streamers can have more inactive activities with the audience and reduce the number of advertisements. "),
        ),
    tabPanel(
        "reference",
        h1("https://www.kaggle.com/aayushmishra1512/twitchdata/tasks?taskId=1760"),
        )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dff <- eventReactive(input$tar, {
        d[input$tar]
    })
    
    output$image <- renderImage({
      list(src="data.png", contentType= "image/png", width = 800, height = 650)
    }, deleteFile = FALSE)
    
    output$sum <- renderPrint({
        summary(sd)
    })
    
    output$langrel <- renderPlot({
        
        if (input$tar == "WatchTime") {
         
            bc(Language, WatchTime, stat = "mean", sort = "-", horiz = TRUE)
        } 
        else if (input$tar == "StreamTime") {
            bc(Language, StreamTime, stat = "mean", sort = "-", horiz=TRUE)
        } 
        else if (input$tar == "Peak.viewers") {
            bc(Language, Peak.viewers, stat = "mean", sort = "-", horiz=TRUE)
        } 
        else if (input$tar == "Average.viewers") {
            bc(Language, Average.viewers, stat = "mean", sort = "-", horiz= TRUE)
        } 
        else if (input$tar == "Followers") {
            bc(Language, Followers, stat = "mean", sort = "-", horiz=TRUE)
        } 
        else if (input$tar == "Followers.gained") {
            bc(Language, Followers.gained, stat = "mean", sort = "-", horiz= TRUE)
        } 
        else if (input$tar == "Views.gained") {
            bc(Language, Views.gained, stat = "mean", sort = "-", horiz= TRUE)
        } 
        
    })
    
    output$rel_q <- renderText({
      if (input$x == "WatchTime" && input$y == "Followers") {
        "Is there any correlation between watch time, stream time, average viewers, followers, and gains?"
      } 
      else if (input$x == "WatchTime" && input$y == "Followers.gained") {
        "Is there any correlation between watch time, stream time, average viewers, followers, and gains?"
      } 
      else if (input$x == "WatchTime" && input$y == "Peak.viewers") {
        "Is there any correlation between watch time, stream time, average viewers, followers, and gains?"
      } 
      else if (input$x == "StreamTime" && input$y == "Average.viewers") {
        "Is there any correlation between watch time, stream time, average viewers, followers, and gains?"
      } 
    })
    
    output$rel_a <- renderText({
      if (input$x == "WatchTime" && input$y == "Followers") {
        "Watch time vs Followers: we see a positive trend between watch time and followers. The longer people watch a stream, the more followers are observed on the channel."
      } 
      else if (input$x == "WatchTime" && input$y == "Followers.gained") {
        "Watch time vs Followers gained: As people spending more time watching Twitch, more people will follow the platform."
      } 
      else if (input$x == "WatchTime" && input$y == "Peak.viewers") {
        "Watch time vs Peak viewers: The longer people spending their time watch Twitch, the higher number of peak viewers there are."
      } 
      else if (input$x == "StreamTime" && input$y == "Average.viewers") {
        "Stream time vs Average viewers: As shown in the graph, streamers stream time does not equal more viewers. Stream time vs Followers gained: As shown in the graph, the data is all over the place with no clear relationship."
      } 
    })
    
    output$relation <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) + geom_point()
    })
    
    output$channel <- renderTable({
               if (input$order == "Hours.Watched") {
                    tt[order(tt$Hours.Watched, decreasing = TRUE),]         
               } 
                else if (input$order  =="Peak.Viewers") {
                    tt[order(tt$Peak.Viewers, decreasing = TRUE),]         
                } else if (input$order == "Average.Viewers")  {
                    tt[order(tt$Average.Viewers, decreasing = TRUE),]         
                }
                else if (input$order == "Airtime") {
                    tt[order(tt$Airtime, decreasing = TRUE),]         
                } else if (input$order == "Followers.Gain") {
                    tt[order(tt$Follwers.Gain, decreasing = TRUE),]         
                }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
