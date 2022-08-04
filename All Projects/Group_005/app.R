library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rvest)
library(tm)
library(dplyr)
library(plotly)
library(xml2)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(NLP)
library(syuzhet)
library(ggplot2)
library(maps)

ui <- dashboardPage(
  skin = "blue-light",
  header = dashboardHeader(title = "FIFA 19 Player Tool"),
  sidebar = dashboardSidebar(sidebarMenu(
    menuItem(
      text = "App Description",
      tabName = "page1",
      icon = icon("futbol")
    ),
    menuItem(
      text = "Player Profile",
      tabName = "page2",
      icon = icon("user")
    ),
    menuItem(
      text = "Player Comparison",
      tabName = "page3",
      icon = icon("balance-scale", lib = "font-awesome")
    ),
    menuItem(
      text = "Player Comments",
      tabName = "page4",
      icon = icon("comments", lib = "font-awesome")
    ),
    hr()
  )),
  body = dashboardBody(tabItems(
    tabItem(
      tabName = "page1",
      column(width = 3,
             img(
               src = 'FIFA19.JPG',
               align = "left",
               height = 200
             )),
      column(
        width = 9,
        # img1.JPG should be placed in the www folder),
        h3("App Description"),
        p(
          tags$span(
            "This App is intended for users of the video game FIFA 19 to have an overview of football players they would like to select. We conducted visualizations with 18k+ FIFA players(every player registered in the latest edition of FIFA 19), ~40 attributes extracted from"
          ),
          a(href = "https://sofifa.com/", target =
              "_blank", "the SOFIFA website")
        ),
        br(),
        h3("Research Target"),
        p(
          "To present a straighforward search tool and performance measurement for FIFA 19 players"
        )
      ),
      column(
        12,
        h3("Methods"),
        p(
          "The main methods in this small project are Web Scrapping, Radar Plot Visualization and Sentiment Analysis. We also use ggplot to locate home country of players on the world map."
        ),
        br(),
        h4("Web Scrapping"),
        p(
          "We use webscrapping methods to collect football players data from SOFIFA website and display the data in a user-friendly table."
        ),
        br(),
        h4("Radar Plot Visualization"),
        p(
          "Using radar plots, we enable users to compare skills of two football players in a clear way."
        ),
        br(),
        h4("Sentiment Analysis"),
        p(
          "Using sentiment analysis on players background stated in Wikipedia, we enable users to have a brief idea of the competence of the player."
        ),
        br(),
        h3("Video demonstration of the app"),
        HTML(
          '<iframe width="100%" height="300" src="https://www.youtube.com/embed/5zv8NBlKM_Y" frameborder="0" allowfullscreen></iframe>'
        )
      )
    ),
    tabItem(height = 2200,
            tabName = "page2",
            fluidRow(column(
              width = 12,
              tabBox(
                width = 12,
                title = "Player Search",
                id = "tabset1",
                tabPanel(
                  height = "500px",
                  title = "Search by player",
                  textInput(
                    inputId = "input1",
                    label = "Player",
                    value = "lionel_messi"
                  )
                ),
                fluidRow(column(width = 1.2,
                                DT::dataTableOutput('table')))
                
                
              )
            ))),
    tabItem(tabName = "page3",
            column(
              width = 4,
              tabBox(
                width = 12,
                title = "Player Comparison",
                id = "tabset1",
                height = "250px",
                tabPanel(
                  title = "Search by Name",
                  textInput(
                    inputId = "player1",
                    label = "Player1",
                    value = "Alexandr_Golovin"
                  ),
                  textInput(
                    inputId = "player2",
                    label = "Player2",
                    value = "Neymar_Da_Silva_Santos_Jr"
                  )
                ),
                tabPanel(
                  title = "Comparison setting",
                  radioButtons(
                    "set",
                    label = h3("Skillset"),
                    choices = list("Attacking", "Skill", "Movement", "Power", "Mentality"),
                    selected = "Attacking"
                  )
                )
              )
            ),
            column(
              width = 8,
              box(
                width = 12,
                plotlyOutput(outputId = "radarplot",
                             height = 250),
                plotOutput(outputId = "map",
                           height = 350)
              )
            )),
    tabItem(tabName = "page4",
            column(
              width = 4,
              tabBox(
                width = 12,
                title = "Player Comments",
                id = "tabset1",
                height = "100px",
                tabPanel(
                  title = "Search by Name",
                  textInput(
                    inputId = "input4",
                    label = "Player",
                    value = "Neymar"
                  )
                )
              )
            ),
            column(width = 8,
                   box(
                     width = 12,
                     plotOutput(outputId = "Wordcloud",
                                height = 500)
                   )))
    
  ))
)


server <- function(input, output) {
  #page 2
  playerScraper <- function(uInput) {
    options(list(pageLength = 10, stringsAsFactors = FALSE))
    
    url <-
      paste0("https://sofifa.com/players?keyword=", uInput)
    tbl <- data.frame()
    tbl_page <- url %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="content-target"]/table') %>%
      html_table()
    names(tbl_page[[1]]) <- names(tbl)
    tbl <- bind_rows(tbl, tbl_page[[1]])
    
    tbl <- tbl[colSums(!is.na(tbl)) > 0]
    
    header.true <- function(t) {
      names(t) <- as.character(unlist(t[1,]))
      t[-1,]
    }
    tbl <- header.true(tbl)
    
    row.names(tbl) <- NULL
    
    return(tbl)
  }
  
  
  
  output$table <- DT::renderDataTable(DT::datatable(
    playerScraper(input$input1),
    style = "bootstrap",
    options = list(pageLength = 5)
  ))
  
  #page 3
  idScraper <- function(x) {
    options(stringsAsFactors = FALSE)
    
    url <-
      paste0("https://sofifa.com/players?keyword=", x)
    url <- paste0(url, "&showCol%5B%5D=pi")
    tbl2 <- data.frame()
    
    tbl2_page <- url %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="content-target"]/table') %>%
      html_table(fill = TRUE)
    names(tbl2_page[[1]]) <- names(tbl2)
    tbl2 <- bind_rows(tbl2, tbl2_page[[1]])
    
    tbl2 <- tbl2[colSums(!is.na(tbl2)) > 0]
    
    header.true <- function(t) {
      names(t) <- as.character(unlist(t[1,]))
      t[-1,]
    }
    tbl2 <- header.true(tbl2)
    
    row.names(tbl2) <- NULL
    tbl2[, 3]
  }
  
  SkillScraper <- function(x) {
    url <- paste0("https://sofifa.com/player/", idScraper(x))
    doc <- read_html(url)
    d <- html_nodes(doc, "span.bp3-tag") %>%
      html_attr('class') %>%
      as.data.frame()
    d$score <- str_sub(d$.,-2,-1)
    d <- d[c(2:3, (nrow(d) - 33):nrow(d)),]
    d$skill <-
      c(
        "Overall Rating",
        "Potential",
        "Crossing",
        "Finishing",
        "Heading Accuracy",
        "Short Passing",
        "Volleys",
        "Dribbling",
        "Curve",
        "FK Accuracy",
        "Long Passing",
        "Ball Control",
        "Acceleration",
        "Sprint Speed",
        "Agility",
        "Reactions",
        "Balance",
        "Shot Power",
        "Jumping",
        "Stamina",
        "Strength",
        "Long Shots",
        "Aggression",
        "Interceptions",
        "Positioning",
        "Vision",
        "Penalties",
        "Composure",
        "Marking",
        "Standing Tackle",
        "Sliding Tackle",
        "GK Diving",
        "GK Handling",
        "GK Kicking",
        "GK Positioning",
        "GK Reflexes"
      )
    d <- d[-1]
    d
  }
  
  radarcompare <- function(player1, player2, set) {
    name1 = player1
    name2 = player2
    if (set == "Attacking") {
      data1 = SkillScraper(name1)$score[5:9]
      data2 = SkillScraper(name2)$score[5:9]
      category1 = SkillScraper(name1)$skill[5:9]
      category2 = category1
    }
    else if (set == "Skill") {
      data1 = SkillScraper(name1)$score[10:14]
      data2 = SkillScraper(name2)$score[10:14]
      category1 = SkillScraper(name1)$skill[10:14]
      category2 = category1
    }
    else if (set == "Movement") {
      data1 = SkillScraper(name1)$score[15:19]
      data2 = SkillScraper(name2)$score[15:19]
      category1 = SkillScraper(name1)$skill[15:19]
      category2 = category1
    }
    else if (set == "Power") {
      data1 = SkillScraper(name1)$score[20:24]
      data2 = SkillScraper(name2)$score[20:24]
      category1 = SkillScraper(name1)$skill[20:24]
      category2 = category1
    }
    else if (set == "Mentality") {
      data1 = SkillScraper(name1)$score[25:30]
      data2 = SkillScraper(name2)$score[25:30]
      category1 = SkillScraper(name1)$skill[25:30]
      category2 = category1
    }
    
    plot_ly(mode = "lines",
            type = 'scatterpolar',
            fill = 'toself') %>%
      add_trace(r = data1,
                theta = category1,
                name = name1) %>%
      add_trace(r = data2,
                theta = category2,
                name = name2) %>%
      layout(polar = list(radialaxis = list(
        visible = T,
        range = c(0, 100)
      )))
    
  }
  
  Maptool <- function(Player1, Player2) {
    url <- paste0("https://sofifa.com/player/", idScraper(Player1))
    doc1 <- read_html(url) %>%
      html_nodes("a") %>%
      html_attr("title")
    country1 <- doc1[51]
    url <-
      paste0("https://sofifa.com/player/", idScraper(Player2))
    doc2 <- read_html(url) %>%
      html_nodes("a") %>%
      html_attr("title")
    country2 <- doc2[51]
    country <- c(country1, country2)
    
    plotmap <- function(country) {
      WorldData <- map_data('world')
      head(WorldData, 100)
      df <- data.frame(region = country)
      
      Total <-
        WorldData[WorldData$region %in% df$region, ]
      
      ggplot(Total, aes(
        x = long,
        y = lat,
        group = group,
        fill = 5
      )) +
        geom_map(
          data = WorldData,
          map = WorldData,
          aes(
            x = long,
            y = lat,
            group = group,
            map_id = region
          ),
          fill = "white",
          colour = "#7f7f7f",
          size = 0.5
        ) +
        geom_polygon(colour = "white") +
        theme_bw()  +
        #labs(fill = "legend" ,title = "Title", x="", y="") +
        #scale_y_continuous(breaks=c()) +
        #scale_x_continuous(breaks=c()) +
        theme(panel.border =  element_blank())
    }
    plotmap(country)
  }
  
  output$radarplot <-
    renderPlotly(radarcompare(input$player1, input$player2, input$set))
  
  output$map <- renderPlot(Maptool(input$player1, input$player2))
  
  #page 4
  SentimentScraper <- function(x) {
    myStopWords <-
      c(
        "may",
        "now",
        "also",
        "many",
        "use",
        "used",
        "typically",
        "given",
        "like",
        "will",
        "can",
        "often",
        "see",
        "one",
        "pdf",
        "issn",
        "journal",
        "player",
        "forward",
        "dead",
        "league",
        "including",
        tolower(month.name)
      )
    
    v <-
      paste0("https://en.wikipedia.org/wiki/", x) %>%
      read_html %>% html_nodes("#bodyContent") %>% html_text %>%
      VectorSource %>% Corpus %>% tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>% tm_map(removeWords, stopwords("english")) %>%
      tm_map(removeWords, myStopWords) %>% tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>% TermDocumentMatrix %>% as.matrix %>%
      rowSums %>% sort(decreasing = TRUE)
    
    d <- data.frame(word = names(v), freq = round(sqrt(v)))
    
    d$word = str_replace_all(d$word, "[^[:graph:]]", " ")
    
    sentRes <- get_sentiment(paste0(d$word))
    
    dWSe <-
      data.frame(
        d,
        sentRes = sentRes,
        color = rep("#00000000", length(sentRes)),
        stringsAsFactors = FALSE
      )
    
    NegativeWords <- dWSe[dWSe$sentRes < 0,]
    PositiveWords <- dWSe[dWSe$sentRes > 0,]
    NoneNeutralWords <- dWSe[dWSe$sentRes != 0,]
    NeutralWords <- dWSe[dWSe$sentRes == 0,]
    
    weighted.mean(NoneNeutralWords$sentRes, NoneNeutralWords$freq)
    
    for (i in 1:length(dWSe$word)) {
      curRate = dWSe$sentRes[i]
      if (curRate < 0) {
        dWSe$color[i] = rgb(
          red = 10 / 255,
          green = 100 / 255,
          blue = 100 / 255,
          alpha = -curRate
        )
      }
      if (curRate > 0) {
        dWSe$color[i] = rgb(
          red = 180 / 255,
          green = 0,
          blue = 20 / 255,
          alpha = curRate
        )
      }
    }
    
    NoneNeutralWords <- dWSe[dWSe$sentRes != 0,]
    
    set.seed(5)
    wordcloud(
      words = NoneNeutralWords$word,
      freq = NoneNeutralWords$freq,
      min.freq = 1,
      max.words = 70,
      random.order = FALSE,
      rot.per = 0.35,
      colors = NoneNeutralWords$color,
      ordered.colors = TRUE
    )
  }
  
  output$Wordcloud <- renderPlot({
    SentimentScraper(input$input4)
  })
  
}



shinyApp(ui = ui, server = server)
