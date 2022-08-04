


library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(viridisLite)
library(heatmaply)
library(Rcpp)
library(dashboardthemes)
library(ggthemes)


champions <- read.csv("riot_champion.csv")
events <- read.csv("Worlds 2020 Main Event - Player Stats - OraclesElixir.csv")
champHis <- read.csv("champ_history.csv")
champSelection <- read.csv("champ_selection.csv")
team_status <- read_csv("Worlds 2020 Main Event - Team Stats - OraclesElixir.csv")
champ_status <- read_csv("Worlds 2020 Main Event - Champion Stats - OraclesElixir.csv")


ui <- dashboardPage(
  dashboardHeader(title="League of Legends"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HomePage", tabName = "home", icon = icon("dashboard")),
      menuItem("Esports", tabName = "winner", icon = icon("globe")),
      menuItem("Champions Comparison", tabName = "hero", icon = icon("rocket")),
      menuItem("Players", tabName = "player", icon = icon("user-o")),
      menuItem("DataSource", tabName = "datasource", icon = icon("database")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Introduction", solidHeader = TRUE, 
                  status = "info", collapsible = TRUE, width = 12,
                  div(
                  tags$iframe(src="https://www.youtube.com/embed/0uyLRPmmYPk",
                              width="80%",
                              height="500",
                              frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
                  style="text-align:center",
                  p(a("Video Link", href = "https://www.youtube.com/watch?v=0uyLRPmmYPk&t=1s"))),
                  br(p("League of Legends, commonly referred to as League, is a 2009 multiplayer online battle arena video game developed and published by Riot Games. Inspired by Defense of the Ancients, a custom map for Warcraft III, Riot's founders sought to develop a stand-alone game in the same genre. Since its release in October 2009, the game has been free-to-play and is monetized through purchasable character customization. The game is available for Microsoft Windows and macOS."),
                  p("In the game, two teams of five players battle in player versus player combat, each team occupying and defending their half of the map. Each of the ten players controls a character, known as a 'champion', with unique abilities and differing styles of play. During a match, champions become more powerful by collecting experience points, earning gold, and purchasing items to defeat the opposing team. In the game's main mode, Summoner's Rift, a team wins by pushing through to the enemy base and destroying their 'Nexus', a large structure located within."),
                  p("League of Legends received generally positive reviews; critics highlighted its accessibility, character designs, and production value. The game's long lifespan has resulted in a critical reappraisal, with reviews trending positively. The negative and abusive in-game behavior of its players, criticized since early in the game's lifetime, persists despite attempts by Riot to fix the problem. In 2019, the game regularly peaked at eight million concurrent players, and its popularity has led to tie-ins such as music videos, comic books, short stories, and an upcoming animated series. Its success has also spawned several spin-off video games, including a mobile version and a digital collectible card game. A massively multiplayer online role-playing game based on the property is in development."),
                  p("The game is often cited as the world's largest esport, with an international competitive scene composed of 12 leagues. The domestic leagues culminate in the annual League of Legends World Championship. The 2019 championship had over 100 million unique viewers, peaking at a concurrent viewership of 44 million. Domestic and international events have been broadcast on livestreaming websites such as Twitch, YouTube, Bilibili, as well as cable television sports channel ESPN. (Wikimedia Foundation. 2021, July 19.)"),
                  )
              
                )
              ),

      fluidRow(
        box(
          title = "Teams", solidHeader = TRUE, 
          status = "info", width = 12, collapsible = TRUE,
          column(12, 
                 tags$div(
                   fluidRow(
                     column(4, tags$img(src="Carey.jpg", height=158, width=280, align ="center")),
                     column(8, tags$div("All team members are MSBARM candidates at Carey Business School, Johns Hopkins University:", style = "font-size:16px")
                     )
                   )
                 ),
                 br(),
                 tags$li("If you have any suggestion, question, or review for this app, comments are open! 
                       Please send an email to leagueoflegends@riot.com and refer to this Shiny app.")
          )
        ),
        br()
      )
    ),
      # Second tab content
      tabItem(tabName = "winner",
              mainPanel(
                tabsetPanel(
                  tabPanel("Highest KP", 
                           br(),
                           p("Kill participation: Percentage of team’s kills in which player earned a kill or assist. 
Generally saying, we conclude that kill participation goes up with the proficiency of players, with the increase of the level of league, the skill gap between players will gradually narrow, while the power of teamwork infinitely magnified. 
"),
                           p("By analyzing the average KP of players on each team and finding the five teams with the highest KP, we wanted to find out whether this general rule holds true in the top competitions."),
                           br(),
                           br(),
                           plotlyOutput("plot1",width = '100%')),
                  tabPanel("Aggressive Players", 
                           br(),
                           p("Damage: average damage to champions per minute."),
                           p("Damage statistics in League of Legends are a tricky thing. Dealing damage is a big part of the game, but how much is inflicted depends on many factors. Some of those within the control of the players, some not."),
                           p("In general, players with high DMG numbers will play a more important role in the team, and other players will either choose to protect him or support him in the team. The ADC will generally be the character with the highest DMG on the team. We tried to analyze the data to find the top ten players."),
                           p("Timbolt. 2016, September 24."),
                           br(),
                           br(),
                           plotlyOutput("plot2",width = '100%')),
                  tabPanel("Historic Prize Pool", 
                           br(),
                           p("World Championship Prize"),
                           p("The League of Legends World Championship (commonly abbreviated as Worlds) is the annual professional League of Legends world championship tournament hosted by Riot Games and is the culmination of each season. Teams compete for the champion title, the 70-pound (32-kilogram) Summoner's Cup, and a multi-million-dollar championship prize. In 2018, the final was watched by 99.6 million people, breaking 2017's final's viewer record. The tournament has been praised for its ceremonial performances, while receiving attention worldwide due to its dramatic and emotional nature."),
                           p("The Prize of world championship changes from year to year. The tournament now has a starting prize of $2.225 million and is expected to grow with the sales of in-game items.  We want to show the prize money of the last few years to give you a basic idea of how big a tournament it is."),
                           p("Wikimedia Foundation. 2021, July 12."),
                           br(),
                           br(),
                           plotlyOutput("plot3",width = '100%')),
                  tabPanel("Regression Analysis", 
                           br(),
                           p("Regression Analysis shows the kill per game of 16 teams to determine whether League of Legends is a game where the more people killed, the better to win."),
                           br(),
                           br(),
                           plotlyOutput("plot5",width = '100%'),
                           tags$li("Regression of Kill per game and Winning"),
                           br(),
                           br(),
                           plotlyOutput("plot9",width = '100%'),
                           tags$li("Regression of Total Game Wined and KDA of Champions"),
                           br(),
                           br(),
                           plotlyOutput("plot10",width = '100%'),
                           tags$li("Regression of Total Game Wined and KDA of Players"),
                           br(),
                           br()
                           ),
                  tabPanel("Correlation Heatmap", 
                           br(),
                           p("Correlation heatmap shows the Correlation between each value in the data set."),
                           br(),
                           br(),
                           plotlyOutput("plot6",width="800",
                                        height="645"),
                           tags$li("Correlation of Teams' Statistical data"),
                           br(),
                           br(),
                           plotlyOutput("plot7", width="800",
                                        height="645"),
                           tags$li("Correlation of Champions' Statistical data"),
                           br(),
                           br(),
                           plotlyOutput("plot8", width="800",
                                        height="645"),
                           tags$li("Correlation of Players' Statistical data"),
                           br(),
                           br())
                  
                  
                  
                ))),
      
      tabItem(tabName = "hero",
              h2("Compare Heros"),
              fluidRow(
                column(6,
                       selectInput("hero1", "Select Hero:", choices = unique(champions$id), selected = champions$id[1])),
                column(6,
                       selectInput("hero2", "Select Hero:", choices = unique(champions$id), selected = champions$id[1])),
                br(),
                br(),
                box(
                       column(1,),
                       column(9,uiOutput("hero1")),
                       column(2,),
                       br(),
                       br(),
                       ),
                box(
                      column(1,),
                      column(9,uiOutput("hero2"),),
                      column(2,),
                       br(),
                       br()
                       ),
                fluidRow(
                  box(
                    column(1,)
                    ,column(11, plotlyOutput("plot4_1",width = '90%',height = "400")),
                  ),
                  box(
                    column(1,),
                    column(11, plotlyOutput("plot4_2",width = '90%',height = "400")),
                  )
                ),
                fluidRow(
                  box(
                    uiOutput("vid1", width = '500%',height = "300"),
                  ),
                  box(
                    uiOutput("vid2", width = '500%',height = "300")),
                )
                
                
                
                
                
                
                
                )
         
      ),
      
      tabItem(tabName = "player",
              fluidRow(
                h2("Player Data"),
                
                box(column(4,
                           selectInput('position','Position', choices = c("All",unique(events$Pos)))
                ),
                column(4,
                       selectInput('team','Team', choices = c("All",unique(events$Team)))
                ),
                column(4,
                       sliderInput("w","Win Percent", min=0, value=0.1, max=1),
                ), background = "light-blue", width = 12),
                
                
    
                column(12,
                       dataTableOutput("FilteredPlayers"))
              )
      ),
      tabItem(tabName = "datasource",
              fluidRow(
                  column(12,
                         selectInput('dataset','Choose Data Set', choices = c('Worlds 2020 Main Event', "Championship History","Champion Selection of Each Game","Team Stats",'Champion Stats',"Champion Data"), width = "200%")
                  ),
                  column(12,
                         dataTableOutput('dataset')
                  )
                         
                  
              )),
              
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12,
                  box(title = "Description", solidHeader = TRUE, 
                      status = "info", collapsible = TRUE, width = 12,
                      p("Our project aims to visualize the League of Legends matches, teams and players statistics based on the 2020 League of Legends World Championship dataset. 
League of Legends is a popular online multiplayer game. We created our website with 6 tabs."),
                      p("The first tab is the Home Page, which contains a complete guide for beginners, and an general introduction about the game. "),
                      p("The second tab is esports, we tried to analysis the dataset with some of the major performance indicator for the game to tell which team is the best team and who is the best player in this series from multiple perspective. "),
                      p("The third tab visualizes each champion’s attributes, including attack, defense, magic and difficulty. The viewers can use the drop-down menu & search bar to find the champions they are interested in. Besides, we also make comparison of two champions available, by selecting two champions from the select box, you may tell the difference between them. "),
                      p("Next, we have a players tab showing all of the 70 players who participated in the 2020 world championship series. Viewers may filter players by positions and teams they were in. We also enables the slider to filter win rate of each player. "),
                      p("Next, we have a data source page where all used datasets are presented. "),
                      p("Finally, it is the About Page, and this is the page where we discuss the project holistically.")),
                      box(title = "Motivation", solidHeader = TRUE, 
                      status = "info", collapsible = TRUE, width = 12,
                  p("The purpose of our project is to visualize the gaming stats and information to help new players better understand the game so as to have a better gaming experience. Many new players have no clue about the champions, and the official website of the Riot only has explanations about the champions' abilities. We provided video guides that were created by many YouTubers with real gaming experiences, showing viewers how to properly use the abilities to have more fun playing that specific champion. Hence, we believe our website is a more comprehensive and better guide for beginner players and old players.")),
                  box(title = "Initial Sketches", solidHeader = TRUE, 
                      status = "info", collapsible = TRUE, width = 12,
                  p("Here is the inital sketches"),
                  column(4, tags$img(src="sketch1.png", width = '100%',align ="center")),
                  column(4, tags$img(src="sketch2.png", width = '100%',align ="center")),
                  column(4, tags$img(src="sketch3.png", width = '100%',align ="center"))),
                  
                  box(title = "References", solidHeader = TRUE, 
                      status = "info", collapsible = TRUE, width = 12,
                      p(a("Timbolt. (2016, September 24). Improving on DPM - Introducing Average Damage per Minute Difference. League of Analytics.", href="http://league-analytics.com/2016/09/02/improving-dpm-introducing-average-damage-per-minute-difference/")),
                      p(a("Wikimedia Foundation. (2021, July 12). League of Legends World Championship. Wikipedia.", href="https://en.wikipedia.org/wiki/League_of_Legends_World_Championship")),
                      p(a("Wikimedia Foundation. (2021, July 19). League of Legends. Wikipedia.", href="https://en.wikipedia.org/wiki/League_of_Legends#:~:text=League%20of%20Legends%20(LoL)%2C,game%20in%20the%20same%20genre")),
                      p(a("Moules, J. (2016, May 29). Research round-up: how misconduct makes colleagues work harder. Financial Times.", href="https://www.ft.com/content/71685e80-1e91-11e6-a7bc-ee846770ec15")),
                      p("For all the images and videos used, reference links can be found in our posted dataset, or you can view it through DataSource tab.")
                      )
                  )
              )
              )
    
)))

########server##################

server <- function(input, output) {
  output$hero1 <- renderUI({
    dat <- champions[champions$id==input$hero1,]
    tags$div(
      h3(dat$title),
      img(src=dat$image, width="600", height="315"), style="text-align:center")
    
  })
  
  output$hero2 <- renderUI({
    dat <- champions[champions$id==input$hero2,]
    tags$div(
      h3(dat$title),
      img(src=dat$image, width="600", height="315"), style="text-align:center")
    
  })
  
  
  
  output$plot1 <- renderPlotly({
    events %>% group_by(Team) %>% summarise(KP=mean(KP)) %>%
      arrange(desc(KP)) %>% head(5) %>% 
      ggplot(aes(x=reorder(Team, -KP), y=KP, fill=Team)) + 
      geom_col() +
      labs(title="Top 5 teams with highest average KP", x="Team", y="Kill Participation")+
      theme_economist() + scale_fill_economist()
  })
  
  output$plot2 <- renderPlotly({
    events %>% filter(Pos=="ADC") %>% 
      arrange(desc(DMG)) %>% head(8) %>%
      ggplot(aes(x=reorder(Player, DMG), y=DMG, fill=Player)) + geom_col() +
      labs(x="Player") + coord_flip()+
      theme_economist() + scale_fill_economist()
  })
  
  output$plot3 <- renderPlotly({
    dat <- read.csv("champ_history.csv", fileEncoding = "UTF-8-BOM")
    ggplot(data=dat, aes(x=Year, y=Prize, label=Team)) + geom_point() + geom_line()+
      xlim(NA,2021)+
      theme_economist() + scale_fill_economist()
      
  })
  
  output$plot4_1 <- renderPlotly({
    dat <- champions[champions$id==input$hero1,]
    df <- data.frame(attribute=c("Attack", "Defense", "Magic", "Difficulty"),
                     value=c(dat$attack, dat$defense, dat$magic, dat$difficulty))
    ggplot(df, aes(y=value, x=attribute, fill=attribute)) + geom_bar(stat="identity") +
      coord_flip()+
      theme_economist() + scale_fill_economist()
  })
  
  output$plot4_2 <- renderPlotly({
    dat <- champions[champions$id==input$hero2,]
    df <- data.frame(attribute=c("Attack", "Defense", "Magic", "Difficulty"),
                     value=c(dat$attack, dat$defense, dat$magic, dat$difficulty))
    ggplot(df, aes(y=value, x=attribute, fill=attribute)) + geom_bar(stat="identity") +
      coord_flip()+
      theme_economist() + scale_fill_economist()
  })
  
 
  output$plot5 <- renderPlotly({
    team_status <- read_csv("Worlds 2020 Main Event - Team Stats - OraclesElixir.csv")
    team_status$KPG <- team_status$K/team_status$GP
    team_status$lg_W <- round(log(team_status$W),5)
    team_status$lg_KPG <- round(log(team_status$KPG),5)
    ggplot(team_status,aes(x=lg_W,y=lg_KPG),)+ggtitle("Regression of Kill per game and Winning")+
      xlab("Winning")+ylab("Kill Per Game") +geom_point(size=1,shape=15)+geom_smooth(method=lm)+
      theme_economist() + scale_fill_economist()
  })
  output$plot6 <- renderPlotly({
    team_status <- read_csv("Worlds 2020 Main Event - Team Stats - OraclesElixir.csv")
    team_status$KPG <- team_status$K/team_status$GP
    team_status$lg_W <- round(log(team_status$W),5)
    team_status$lg_KPG <- round(log(team_status$KPG),5)
    team_status1 <-  team_status[,-c(1,21,30,31)]
    data <- as.matrix(team_status1)
    heatmaply_cor(cor(team_status1), xlab="Variables", ylab="Variables", k_col = NA,
                  k_row = NA,main = "Teams' Statistical data")
  })
  output$plot7 <- renderPlotly({
    champ_status <- read_csv("Worlds 2020 Main Event - Champion Stats - OraclesElixir.csv")
    champ_status <- na.omit(champ_status)
    summary(champ_status)
    champ_status$W <- as.numeric(champ_status$W)
    champ_status$CTR <- as.numeric(champ_status$CTR)
    champ_status$K <- as.numeric(champ_status$K)
    champ_status$D <- as.numeric(champ_status$D)
    champ_status$A <- as.numeric(champ_status$A)
    champ_status$KDA <- as.numeric(champ_status$KDA)
    champ_status$KP<- as.numeric(champ_status$KP)
    champ_status$DTH <- as.numeric(champ_status$DTH)
    champ_status$FB <- as.numeric(champ_status$FB)
    champ_status$GD10 <- as.numeric(champ_status$GD10)
    champ_status$XPD10 <- as.numeric(champ_status$XPD10)
    champ_status$CSD10 <- as.numeric(champ_status$CSD10)
    champ_status$CSPM <- as.numeric(champ_status$CSPM)
    champ_status$CSP15 <- as.numeric(champ_status$CSP15)
    champ_status$DPM <- as.numeric(champ_status$DPM)
    champ_status$DMG <- as.numeric(champ_status$DMG)
    champ_status$GOLD <- as.numeric(champ_status$GOLD)
    champ_status$WPM <- as.numeric(champ_status$WPM)
    champ_status$WCPM <- as.numeric(champ_status$WCPM)
    
    champ_status$TGW <- round(champ_status$W*champ_status$GP,0)
    
    champ_status1 <- champ_status[,-c(1,2)]
    data2 <- as.matrix(champ_status1)
    heatmaply_cor(cor(data2), xlab="Variables", ylab="Variables",   k_col = NA,
                  k_row = NA,main = "Champions' Statistical data")
  })
  output$plot8 <- renderPlotly({
    player_status <- read_csv("Worlds 2020 Main Event - Player Stats - OraclesElixir.csv")
    summary(player_status)
    
    player_status$GW <- round(player_status$GP*player_status$W,0)
    player_status1 <- player_status[,-c(1,2,3)]
    data3 <- as.matrix(player_status1)
    heatmaply_cor(cor(data3), xlab="Variables", ylab="Variables",   k_col = NA,
                  k_row = NA,main = "Players' Statistical data")
  })  
  
  output$plot9 <- renderPlotly({
    champ_status <- read_csv("Worlds 2020 Main Event - Champion Stats - OraclesElixir.csv")
    champ_status <- na.omit(champ_status)
    champ_status$W <- as.numeric(champ_status$W)
    champ_status$CTR <- as.numeric(champ_status$CTR)
    champ_status$K <- as.numeric(champ_status$K)
    champ_status$D <- as.numeric(champ_status$D)
    champ_status$A <- as.numeric(champ_status$A)
    champ_status$KDA <- as.numeric(champ_status$KDA)
    champ_status$KP<- as.numeric(champ_status$KP)
    champ_status$DTH <- as.numeric(champ_status$DTH)
    champ_status$FB <- as.numeric(champ_status$FB)
    champ_status$GD10 <- as.numeric(champ_status$GD10)
    champ_status$XPD10 <- as.numeric(champ_status$XPD10)
    champ_status$CSD10 <- as.numeric(champ_status$CSD10)
    champ_status$CSPM <- as.numeric(champ_status$CSPM)
    champ_status$CSP15 <- as.numeric(champ_status$CSP15)
    champ_status$DPM <- as.numeric(champ_status$DPM)
    champ_status$DMG <- as.numeric(champ_status$DMG)
    champ_status$GOLD <- as.numeric(champ_status$GOLD)
    champ_status$WPM <- as.numeric(champ_status$WPM)
    champ_status$WCPM <- as.numeric(champ_status$WCPM)
    champ_status$TGW <- round(champ_status$W*champ_status$GP,0)
    ggplot(champ_status,aes(x=TGW,y=KDA),)+ ggtitle("Regression of Total Games Winning and KDA of Champions")+
      xlab("Total Games Winning")+ylab("KDA")+geom_point(size=1,shape=15)+geom_smooth(method=lm)+
      theme_economist() + scale_fill_economist()
    
  })  
  
  output$plot10 <- renderPlotly({
    player_status <- read_csv("Worlds 2020 Main Event - Player Stats - OraclesElixir.csv")
    player_status$GW <- round(player_status$GP*player_status$W,0)
    ggplot(player_status,aes(x=W,y=KDA),)+ ggtitle("Regression of Total Games Winning and KDA of Players")+
      xlab("Total Games Winning")+ylab("KDA")+geom_point(size=1,shape=15)+geom_smooth(method=lm)+
      theme_economist() + scale_fill_economist()
  })
    
  output$vid1 <- renderUI({
    dat <- champions[champions$id==input$hero1,]
    tags$div(
      div(
        tags$iframe(width="100%",
                    height="550", src= gsub("watch\\?v=", "embed/",dat$video), 
                    frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"),style="text-align:center")
    )
  })
  
  output$vid2 <- renderUI({
    dat <- champions[champions$id==input$hero2,]
    tags$div(
      div(
        tags$iframe(width="100%",
                    height="550", src= gsub("watch\\?v=", "embed/",dat$video), 
                    frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"),style="text-align:center")
    )
  })
  
  
  
  output$FilteredPlayers <- renderDataTable({
    datatable({
      if (input$position!='All'){
        events <- events[events$Pos==input$position,]
      }
      if(input$team!="All"){
        events <- events[events$Team==input$team,]
      }
      events<- events %>% select(c(1:14)) %>% filter(events$W>input$w)
      events
      
    })
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           'Worlds 2020 Main Event'=events,
           "Champion Data"=champions,
           "Championship History"=champHis,
           "Champion Selection of Each Game"=champSelection,
           "Team Stats"=team_status,
          "Champion Stats"=champ_status)
  })
  
  output$dataset <- renderDataTable({
    datasetInput()
  })
  
  
  
 
}

shinyApp(ui, server)