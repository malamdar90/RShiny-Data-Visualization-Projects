server <- function(input, output) {
  
  # import dataset
  hw <- read.csv("hw.csv")
  
  # data: topic modeling 
  hw_topic_aggr <- aggregate(hw$topic, by = hw[,c("topic","date")],  FUN = "length")
  hw_topic_aggr$date <- as.Date(hw_topic_aggr$date, format("%Y-%m-%d"))
  hw_topic_aggr$topic <- as.character(hw_topic_aggr$topic)
  topic_by_date <- reactive({hw_topic_aggr[hw_topic_aggr$date == input$input_topic,]})  
  
  # data: sentiment analysis
  hw$date <- as.Date(hw$date, format("%Y-%m-%d"))
  hw$topic <- as.character(hw$topic)
  sentiment_by_topic <- reactive({hw[hw$topic == input$input_sentiment,]}) 
  
  # data: word cloud with sentiment score
  wordcloud_by_date <- reactive({hw[hw$date == input$input_wordcloud,]})  
  ## buid customized stop words
  myStopWords <- c("may", "now", "also", "many", "packer", 
                   "dhabi", "mtj", "arab", "removal", "typically",
                   "given", "will", "furniture", "dubai", "abu",
                   "via", "can", "often", "pdf", "mover", "house", 
                   "fujairah", "emirate", "mhj", "just", "emiratesn", 
                   "say", "even", "early", "immediately", "etc", "umm", 
                   "quawain", "quwain", "ain", "maj", "aensharjah",
                   "home", "dhabimhjmovers", "packersampfurniture", 
                   "packersfurniture", "packersmoving", "mfr",
                   "zamzam", 
                   tolower(month.name))
  # data: mapping
  hw_map <- read.csv("da.csv")
  hw_map$date <- as.Date(hw_map$date, format("%Y/%m/%d"))
  map_by_date <- reactive({hw_map[hw_map$date == input$input_world,]})  
  
  
  # function: topic modeling
  topicbydate <- function(date){
    topic1 <- ggplot(data = topic_by_date(), mapping = aes(x = topic_by_date()$topic, 
                                                           y = topic_by_date()$x)) +
      theme(panel.background = element_rect(fill = "white"), 
            legend.position = "bottom", 
            axis.line = element_line(linetype = 1, size = 0.6), 
            axis.ticks = element_line(linetype = 1, size = 0.6),
            text = element_text(size = 20)) +
      scale_x_discrete(breaks = c("1", "2", "3", "4"), labels = c("1" = "Trade War", "2" = "Operation System", "3" = "Phone Industry", "4" = "Huawei Facts")) +
      ggtitle(paste0("# Tweets from Each Topic in ", date)) + # title
      labs(x = "Topic", y = "# Tweets") + # label of xy-axis
      ylim(0,155) # fix the scale of y-axis in all plots
    
    topic1 <- topic1 + geom_col(colour = c("#696969", "#27aa80", "#00ccff", "#ff6666"),
                                fill = c("#696969", "#27aa80", "#00ccff", "#ff6666")) +
      geom_text(aes(label = topic_by_date()$x), vjust = -0.5, size = 5, colour = c("#696969", "#32CD32", "#00ccff", "#ff6666")) 
    topic1
  }
  
  
  # function: sentiment
  sentimentbytopic <- function(topic){
    sentiment1 <- ggplot(data = sentiment_by_topic(),
                         mapping = aes(x = sentiment_by_topic()$date,
                                       y = sentiment_by_topic()$sentiment, 
                                       group = sentiment_by_topic()$date)) +
      theme(panel.background = element_rect(fill = "white"), 
            legend.position = "bottom", 
            axis.line = element_line(linetype = 1, size = 0.6), 
            axis.ticks = element_line(linetype = 1, size = 0.6),
            text = element_text(size = 20)) +
      scale_x_date(labels = date_format("%m/%d")) +
      ylim(-0.75, 0.82) +
      ggtitle("Sentiment Score Each Day") +
      labs(x = "Date", y = "Score")
    sentiment1 <- sentiment1 + geom_boxplot() +
      geom_hline(yintercept = 0, linetype = 2, color = "red", size = 1)
    sentiment1
  }
  
  # function: Word Cloud with sentiment score
  wordcloudbydate <- function(date, num){
    
    wordscleaning <- 
      iconv(wordcloud_by_date()$text, "WINDOWS-1252", "UTF-8") %>%
      VectorSource() %>%
      Corpus() %>%
      tm_map(., content_transformer(tolower)) %>%
      tm_map(., removeWords, stopwords("en")) %>%
      tm_map(., removePunctuation) %>%
      tm_map(., removeNumbers) %>%
      tm_map(., stripWhitespace) %>%
      tm_map(., removeWords, myStopWords)
    
    wordcloud <- TermDocumentMatrix(wordscleaning) 
    wordcloud <- removeSparseTerms(wordcloud, sparse = .9996) 
    wordcloud <- as.matrix(wordcloud)
    wordcloud <- sort(rowSums(wordcloud),decreasing=TRUE)
    wordcloud <- data.frame(word = names(wordcloud),freq=round(sqrt(wordcloud)))
    
    sentRes <- get_sentiment(as.character(wordcloud$word))
    dWSe <- data.frame(wordcloud,sentRes=sentRes, color=rep("black",length(sentRes)), 
                       stringsAsFactors = FALSE)
    dWSe[dWSe$sentRes<0,4] <- "red"
    dWSe[dWSe$sentRes>0,4] <- "green"
    NegativeWords <- dWSe[dWSe$sentRes<0,]
    PositiveWords <- dWSe[dWSe$sentRes>0,]
    NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]
    NeutralWords <- dWSe[dWSe$sentRes==0,]
    
    weighted.mean(NoneNeutralWords$sentRes,NoneNeutralWords$freq)
    dWSe <- data.frame(wordcloud,sentRes=sentRes, color=rep("#00000000",length(sentRes)), 
                       stringsAsFactors = FALSE)
    for (i in 1:length(dWSe$word)) {
      curRate=dWSe$sentRes[i]
      if(curRate<0){
        dWSe$color[i]=rgb(red = 179/255,green = 0,blue = 12/255,alpha = -curRate)
      }
      if(curRate>0){
        dWSe$color[i]=rgb(red = 13/255,green = 89/255,blue = 1/255,alpha = curRate)
      }
      
      NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]}
    
    set.seed(5)
    wordcloud(words = NoneNeutralWords$word, freq = NoneNeutralWords$freq, min.freq = 1,
              max.words=num, random.order=FALSE, rot.per=0.35, 
              colors=NoneNeutralWords$color,
              ordered.colors=TRUE) 
  }
  
  #function: World Map 
  locationsbydate <- function(date){
    
    world <- map_data("world")
    
    labs <- data.frame(
      long = map_by_date()$lng,
      lat = map_by_date()$lat,
      stringsAsFactors = FALSE
    )  
    
    gg2 <- ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group)) + 
      borders("world", colour = "white", fill="grey") +
      coord_fixed(1.3) +
      guides(fill=FALSE) +
      theme(panel.background = element_rect(fill = "white"), 
            axis.line = element_line(linetype = 0, size = 0.6), 
            axis.ticks = element_line(linetype = 0, size = 0.6))
    
    World<- gg2 + 
      geom_point(data = labs, aes(x = long, y = lat), color = "red", size = 1) +
      geom_point(data = labs, aes(x = long, y = lat), color = "red", size = 0.5)
    World
  }
  
  #function: Cluster Map 
  ClusterMap <- function(MapData) {
    MapData <- hw_map
    leaflet(MapData) %>% 
      addTiles() %>% 
      addMarkers(
        clusterOptions = markerClusterOptions()
      )
  }
  
  
  # output: topic modeling
  output$plot_topic <- renderPlot({topicbydate(paste0(input$input_topic))})
  output$plot_topic2 <- renderPlot({t2 <- ggplot(data = hw_topic_aggr,
                                                 mapping = aes(x = hw_topic_aggr$date,
                                                               y = hw_topic_aggr$x,
                                                               group = hw_topic_aggr$topic,
                                                               fill = hw_topic_aggr$topic)) +
    theme(panel.background = element_rect(fill = "white"), 
          legend.position = "bottom", 
          axis.line = element_line(linetype = 1, size = 0.6), 
          axis.ticks = element_line(linetype = 1, size = 0.6),
          text = element_text(size = 20)) + 
    scale_fill_discrete(labels=c("Trade War", "Operation System", "Phone Industry", "Huawei Facts")) +
    scale_x_date(date_labels = format("%m/%d"), breaks = as.Date(hw_topic_aggr$date)) +
    ggtitle("# Tweets by Date") +
    labs(x = "Date", y = "# Tweets", fill = "Topic") + # set xy-axis & legend label
    guides(fill = "legend")
  
  t2 <- t2 + geom_col()
  t2})
  # output: sentiment analysis
  output$plot_sentiment <- renderPlot({sentimentbytopic(input$input_sentiment)})
  output$sentiment_table <- renderDT({datatable(sentiment_by_topic()[,c("date", "text", "sentiment")], rownames=FALSE, options = list(pageLength = 5, searching = F))})
  # output: wordcloud
  output$plot_wordcloud <- renderPlot({wordcloudbydate(input$input_wordcloud, input$num_word)})
  # output: World Map
  output$plot_world <- renderPlot({locationsbydate(input$input_world)})
  # output: Cluster Map
  output$plot_cluster <- renderLeaflet({ClusterMap(MapData)})
  # output: datatable
  DTData <- datatable(hw_map,rownames=FALSE, options = list(pageLength = 5, searching = F))
  output$DataSample <- DT::renderDataTable(DTData)
}