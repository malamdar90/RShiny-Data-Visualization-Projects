# UI
library(shiny)
library(shinydashboard)
library(shinyjs)
# Data
library(DT)
library(tidyverse)
library(scales)
library(lubridate)
library(FNN)
library(stats)
library(formattable)
# Pic
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(wordcloud)
# Model
library(class)
# Crawling
library(rvest)

# Read Data
main <- read_csv("./Dataset/main.csv")
genres <- read_csv("./Dataset/genres.csv")
keywords <- read_csv("./Dataset/keywords.csv") %>% na.omit()
production_companies <- read_csv("./Dataset/production_companies.csv") %>% na.omit()
production_countries <- read_csv("./Dataset/production_countries.csv") %>% na.omit()
spoken_languages <- read_csv("./Dataset/spoken_languages.csv") %>% na.omit()
cast <- read_csv("./Dataset/cast.csv") %>% na.omit()
crew <- read_csv("./Dataset/crew.csv") %>% na.omit()

main_szy <- readRDS("./Dataset/main.RDS")
movies_szy <- main_szy %>% select(id, title, original_language, popularity,
                                  release_date, runtime, status, vote_count, vote_average,
                                  budget, revenue)
# home
ddw <- main %>% select(title, original_language, release_date, vote_count, vote_average)

# prediciton
data <- main %>%
  select(id, title, release_date, runtime, vote_average, budget, revenue) %>%
  na.omit()
# feature of genre
genreRev <- genres %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(genre) %>%
  summarise(avggenreRev = mean(revenue)) %>%
  filter(avggenreRev > 0) %>%
  arrange(-avggenreRev)
genreRev.join <- data %>%
  left_join(genres, by = "id") %>%
  left_join(genreRev, by = "genre") %>%
  group_by(id) %>%
  summarise(avggenreRev = max(avggenreRev)) %>%
  na.omit()
# feature of keyword
keywordRev <- keywords %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(keywords) %>%
  summarise(avgkwRev = mean(revenue)) %>%
  filter(avgkwRev > 0) %>%
  arrange(-avgkwRev)
keywordRev.join <- data %>%
  left_join(keywords, by = "id") %>%
  left_join(keywordRev, by = "keywords") %>%
  group_by(id) %>%
  summarise(avgkwRev = max(avgkwRev)) %>%
  na.omit()
# feature of production_companies
pdtcompaniesRev <- production_companies %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(production_companies) %>%
  summarise(avgpdtcompaniesRev = mean(revenue)) %>%
  filter(avgpdtcompaniesRev > 0) %>%
  arrange(-avgpdtcompaniesRev)
pdtcompaniesRev.join <- data %>%
  left_join(production_companies, by = "id") %>%
  left_join(pdtcompaniesRev, by = "production_companies") %>%
  group_by(id) %>%
  summarise(avgpdtcompaniesRev = max(avgpdtcompaniesRev)) %>%
  na.omit()
# feature of production_countries
pdtcountriesRev <- production_countries %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(production_countries) %>%
  summarise(avgpdtcountriesRev = mean(revenue)) %>%
  filter(avgpdtcountriesRev > 0) %>%
  arrange(-avgpdtcountriesRev)
pdtcountriesRev.join <- data %>%
  left_join(production_countries, by = "id") %>%
  left_join(pdtcountriesRev, by = "production_countries") %>%
  group_by(id) %>%
  summarise(avgpdtcountriesRev = max(avgpdtcountriesRev)) %>%
  na.omit()
# feature of spoken_languages
spokenlangRev <- spoken_languages %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(SL) %>%
  summarise(avgspokenlangRev = mean(revenue)) %>%
  filter(avgspokenlangRev > 0) %>%
  arrange(-avgspokenlangRev)
spokenlangRev.join <- data %>%
  left_join(spoken_languages, by = "id") %>%
  left_join(spokenlangRev, by = "SL") %>%
  group_by(id) %>%
  summarise(avgspokenlangRev = max(avgspokenlangRev)) %>%
  na.omit()
# feature of cast
castRev <- cast %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(cast_name) %>%
  summarise(avgcastRev = mean(revenue)) %>%
  filter(avgcastRev > 0) %>%
  arrange(-avgcastRev)
castRev.join <- data %>%
  left_join(cast, by = "id") %>%
  left_join(castRev, by = "cast_name") %>%
  group_by(id) %>%
  summarise(avgcastRev = max(avgcastRev)) %>%
  na.omit()
# feature of crew
crewRev <- crew %>%
  left_join(data, by = "id") %>%
  na.omit() %>%
  group_by(crew_name) %>%
  summarise(avgcrewRev = mean(revenue)) %>%
  filter(avgcrewRev > 0) %>%
  arrange(-avgcrewRev)
crewRev.join <- data %>%
  left_join(crew, by = "id") %>%
  left_join(crewRev, by = "crew_name") %>%
  group_by(id) %>%
  summarise(avgcrewRev = max(avgcrewRev)) %>%
  na.omit()
# generate features
data.features <- data %>%
  left_join(genreRev.join, by = "id") %>%
  left_join(keywordRev.join, by = "id") %>%
  left_join(pdtcompaniesRev.join, by = "id") %>%
  left_join(pdtcountriesRev.join, by = "id") %>%
  left_join(spokenlangRev.join, by = "id") %>%
  left_join(castRev.join, by = "id") %>%
  left_join(crewRev.join, by = "id") %>%
  na.omit() %>%
  mutate(release_date = year(release_date)) %>%
  arrange(-revenue)

# function of getting photolink and videolink
# because the links keep changing, we put the real-time crawling function in the code
getPic <- function(title){
  movie <- read_html(paste0("https://www.imdb.com/find?q=", URLencode(title)))
  results = movie %>%
    html_nodes(".findList .primary_photo a img") %>% 
    html_attr('src')
  
  result_link=""
  if(length(results)>0){
    result_link=results[1]
    print(result_link)
    lastCh = str_locate_all(pattern ="@",result_link)[[1]][1]-1
    v = unlist(strsplit(result_link, "\\."))
    v <- v[length(v)]
    finalLink = paste0(str_sub(result_link,1,lastCh),"@.", v)
    return(finalLink)
  }
  return(NULL)
}

getVideo <- function(title){
  movie <- read_html(paste0("https://www.imdb.com/find?q=",URLencode(title)))
  # print(paste0("https://www.imdb.com/find?q=",URLencode(title)))
  jump_url <- movie %>%
    html_node("td.result_text") %>%
    html_node("a") %>%
    html_attr("href")
  jump_url <- paste0("https://www.imdb.com", jump_url)
  page <- read_html(jump_url)
  url <- str_extract(unlist(str_extract_all(as.character(page), '<a class="ipc-lockup-overlay.*" href="/video/.*" .*>'))[1], 'href="(.*?)"')
  url <- gsub('href', "", url)
  url <- gsub('"', "", url)
  url <- gsub('=', "", url)
  if(is.na(url)){
    return("")
  }
  url <- paste0("https://www.imdb.com", url)
  page <- read_html(url)
  video_url <- unlist(str_extract_all(as.character(page), '"https://imdb-video.*?\\.mp4.*?"'))[2]
  video_url <-  gsub('"', "", video_url)
  video_url <-  gsub("\\\\", "", video_url)
  video_url
}
# ==============================================================================================================================
# ==============================================================================================================================
# ui
# ==============================================================================================================================

# ==============================================================================================================================


ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Movie Hub"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "myTabForIntroduction", icon = icon("fas fa-book")),
      menuItem("Dataset", tabName = "Home", icon = icon("table")),
      menuItem("Movie", tabName = "movie", icon = icon("tv")),
      menuItem("Analysis",
               tabName = "Analysis", icon = icon("chart-pie"), selected = T,
               menuItem("Genre Trend", tabName = "Analysis1", icon = icon("chart-line")),
               menuItem("Other Trends", tabName = "Analysis2", icon = icon("chart-area")),
               menuItem("Detailed Relationships", tabName = "Analysis3", icon = icon("chart-bar"))
      ),
      menuItem("Prediction", tabName = "prediction", icon = icon("compress-arrows-alt"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # 1st: Intro
      tabItem("myTabForIntroduction",
              fluidRow(
                box(
                  title = "About the Project", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("This app,",tags$strong("Movie Hub,"), 
                               "is designed to provide movie lovers and movie producers with the valuable information they need."),
                             p("If you are the", tags$strong("movie lover"), 
                               ", here, you can have a glance at total of 20 genres and up to 5000 movies according to your taste. You can also gain insight into the history of film development, all the way back to the beginning of the 20th century."),
                             p("If you are the", tags$strong("movie producer"), 
                               ", here, you can have an idea of the public preferences trend and the relationships between the key factors of making a move. What's more, you can even simulated making your own movie on our platform, and based on the quantitative analysis, our system will tell you how much you are likely to earn and how to make an improvements in your movie.")
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "About the Motivation", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("The rapid growth of data collection has led to a new era of information. Data is being used to create more systems that enables us to do the analysis more effectively and efficiently."),
                             p("We have noticed that there are still some pain points in the present movie analysis platforms, like, not only movie lovers but also movie makers would need such system, but few of them are designed for both of them, and many systems are not flexible enough."),
                             p("So, we are motivated to create our Movie Hub to better serve more users.")
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("Using data from online forums and social media sources, the film is rated according to the types, likeability and relevant comments, so as to help the film company or film watchers keep track of consumer comments and preferences at any time."),
                           br(),
                           tags$li(tags$strong("Source: "),tags$a(href = "https://datasets.imdbws.com/", "IMDb Data")),
                           tags$li("The filtered dataset for this application contains total",tags$strong("4,802"), "rows (in ", tags$strong("23"), "columns) ."),
                           br(),
                           tags$span(
                             "This app,",tags$strong("Movie Hub,"), 
                             "is the shiny dashboard application designed to explore and compare movie data among", tags$strong("8"), "factors, including:"),
                           br(),
                           fluidRow(column(6, tags$li("Cast"), tags$li("Crew"), tags$li("Genres"), tags$li("Keywords")), 
                                    column(6, tags$li("Production Companies"), tags$li("Production Countries"), tags$li("Production Year"), tags$li("Spoken Languages"))),
                         )
                  )
                )
              )
      ), 
      tabItem(
        tabName = "Home",
        fluidRow(
          box(
            title = "Dataset Description", solidHeader = TRUE,
            status = "success", width = 12, collapsible = TRUE,
            tags$div(
              tags$span(
                p("This is the Final Group Project of BU.520.650.X2 Data Visualization class taught by Professor Mohammad Ali Alamdar Yazdi"),
                p("There's no one who doesn't like watching movies, isn’t it? We are looking forward to designing a film data analysis platform for film lovers to enjoy. "),
              )
            ),
          ),
        ),
        dataTableOutput(outputId = "tbl"),
        fluidRow(
          box(
            title = "Method and Analysis & Initial Sketch", solidHeader = TRUE,
            status = "primary", width = 12, collapsible = TRUE,
            tags$div(
              tags$span(
                fluidRow(
                  column(6,
                         p(tags$strong("First"), ", our Movie Hub show the detailed information of 5000 films. Readers can search the films by genre and title of the movie, and take a quick look at the posters and short videos of their favorite movies."),
                  ),
                  column(6,img(src="Movie_Sketch.jpg",width="50%"))
                ),
                fluidRow(
                  column(6,
                         p(tags$strong("Second"), ", we embed the analysis system in Movie Hub, such as 1) the trend of number of movies, total tickets sold and average ratings per year. and many other possible ideas; 2) word cloud illustration that users can choose a specific year to watch the movie genre frequency of that year; 3) the relationship between variables, for example, the relationship between movie budget and number of tickets sold, the relationship between movie ratings and tickets sold…"),
                  ),
                  column(3,img(src="Genre1.jpg",width="100%")),
                  column(3,img(src="Trend.jpg",width="100%"))
                ),
                fluidRow(
                  column(6,
                         p(tags$strong("Third"), ", we build the prediction model to make it is possible for users to simulate producing his own movie on our website. Here the user fills in the budget, genre, their ideal cast and crew, estimated release date, producer and other information of the movie he wants to produce. After submission, the page displays the predicted revenue, predicted rating and even more will show the user how to improve the profitability of his/her movie.")
                  ),
                  column(3,img(src="Prediction.jpg",width="100%")),
                  column(3,img(src="Movie Battle.jpg",width="100%"))
                ),
              )
            ),
          ),
        ),
        fluidRow(
          box(
            title = "Research Questions", solidHeader = TRUE,
            status = "info", width = 12, collapsible = TRUE,
            tags$div(
              tags$span(
                p("With our Movie Hub, you may definitely have an idea on the following questions:"),
                tags$li("How did population taste of genre change over time?"),
                tags$li("Which genres catch audience' eyes most?"),
                tags$li("How is the relationship any two of the variables? e.g. budget&revenue, budget&popularity"),
                tags$li("How do the facotrs contribute to movie revenue?"),
                tags$li("Which movie is better when two movies battle?")
              )
            ),
          ),
        ),
      ),
      # ==============================================================================================================================
      # movie
      # ==============================================================================================================================
      
      # Second tab content
      tabItem(tabName = "movie",
              box(
                width="100%",
                tags$div(
                  img(src="movie_background2.jpg",width="80%"),
                  style="text-align:center"
                ),
                fluidRow(
                  column(6,
                         selectInput("genre", "Genre",
                                     choices=unique(genres$genre),
                                     selected = unique(genres$genre)[1])
                  ),
                  column(6,uiOutput("movieOut"))
                ),
                box(
                  width="100%",
                  uiOutput("info")
                ),
              )
      ),
      
      
      # ==============================================================================================================================
      # analysis
      # ==============================================================================================================================
      
      tabItem(
        tabName = "Analysis1",
        titlePanel("First Glimpse-Number of genres during years:"),
        br(),br(),
        tabsetPanel(
          tabPanel(
            "Dynamic Trend",
            sliderInput("Plot3Input", "Select a year to show the number of genres:",
                        min = 1937, max = 2017,
                        value = 1937, step = 1, width = "100%",
                        animate = animationOptions(interval = 1000, loop = FALSE)
            ),
            plotOutput("plot3", width="100%")
          ),
          tabPanel(
            "Word Cloud",
            fluidRow(
              mainPanel(
                sliderInput("cyear_B", "Select analyse start year:",
                            min = 1916, max = 2017,
                            value = 1916, step = 1, width = "100%"
                ),
                sliderInput("cyear_E", "Select analyse end year:",
                            min = 1937, max = 2017,
                            value = 2017, step = 1, width = "100%"
                ),
                br(),br(),
                plotOutput("plot4")
              )
            ),
            fluidRow(br(),br(),br(),br(),br(),br(),
                     mainPanel(br())
            )
          )
        )
      ),
      tabItem(
        tabName = "Analysis2",
        h2("Go Ahead-Analyse different trends during years:"),
        sliderInput("year_Begin", "Select analyse begin year:",
                    min = 1916, max = 2017,
                    value = 1916, step = 1, width = "100%"
        ),
        sliderInput("year_End", "Select analyse end year:",
                    min = 1937, max = 2017,
                    value = 2017, step = 1, width = "100%"
        ),
        selectInput("select",
                    label = "Choose a characteristic to analyse ", width = "100%",
                    choices = list("number of movies" = "num_movie", "average popularity" = "ave_popularity", "average runtime" = "ave_runtime", "average vote" = "ave_vote", "average budget in million" = "ave_budget", "average revenue in million" = "ave_revenue"),
                    selected = 1
        ),
        plotOutput("plot1"),
      ),
      tabItem(
        tabName = "Analysis3",
        h2("Go Futher-Analyse relationship between characteristics:"),
        fluidRow(
          column(6,
                 selectInput("select1",
                             label = "Choose character of x-axis:", width = "100%",
                             choices = list("popularity" = "popularity", "runtime" = "runtime", "average vote" = "vote_average", "average budget in million" = "budget_in_million", "average revenue in million" = "revenue_in_million"),
                             selected = 1
                 ),
          ),
          column(6,
                 selectInput("select2",
                             label = "Choose character of y-axis:", width = "100%",
                             choices = list("popularity" = "popularity", "runtime" = "runtime", "average vote" = "vote_average", "average budget in million" = "budget_in_million", "average revenue in million" = "revenue_in_million"),
                             selected = 1
                 ),
          )
        ),
        selectInput("select3",
                    label = "Choose character that represents colors:", width = "100%",
                    choices = list("average vote" = "vote_average", "popularity" = "popularity", "runtime" = "runtime", "average budget in million" = "budget_in_million", "average revenue in million" = "revenue_in_million"),
                    selected = 1
        ),
        plotOutput("plot2")
      ),
      #==============================================================================================================================
      # Make your own movie
      #==============================================================================================================================
      
      tabItem(tabName = "prediction",
              titlePanel("Make Your Own Movie"),
              # Sidebar with a slider input for number of bins
              sidebarLayout(
                sidebarPanel(
                  textInput("Mtitle", label = "Type your Movie Title:"),
                  dateInput("Mreleasedate", "Select your Movie Released Date:"),
                  sliderInput("Mruntime",
                              label = "Select your Movie Runtime (min):",
                              min = 0, max = 180, value = 0),
                  sliderInput("Mbudget",
                              label = "Select your Movie Budget (million dollars):",
                              min = 0, max = 380, value = 0),
                  selectInput(inputId = "Mgenre",
                              label = "Choose your Movie Genre:",
                              choices = genreRev$genre),
                  # selectInput(inputId = "Mkeyword",
                  #             label = "Choose your Movie Keyword:",
                  #             choices = keywordRev$keywords[1:1000]),
                  # selectInput(inputId = "McastName",
                  #             label = "Choose your ideal Actor/Actress:",
                  #             choices = castRev$cast_name[1:1000]),
                  # selectInput(inputId = "McrewName",
                  #             label = "Choose your ideal Crew:",
                  #             choices = crewRev$crew_name[1:1000]),
                  # selectInput(inputId = "Mpdtcompany",
                  #             label = "Choose your Production Company:",
                  #             choices = pdtcompaniesRev$production_companies[1:1000]),  
                  selectizeInput(inputId = "Mkeyword",
                                 label = "Choose your Movie Keyword:",
                                 choices = keywordRev$keywords[1:1000],
                                 selected = NULL,
                                 multiple = FALSE,
                                 options = list(maxOptions = 10, create = FALSE)
                  ),
                  selectizeInput(inputId = "McastName",
                                 label = "Choose your ideal Actor/Actress:",
                                 choices = castRev$cast_name[1:1000],
                                 selected = NULL,
                                 multiple = FALSE,
                                 options = list(maxOptions = 10, create = FALSE)
                  ),
                  selectizeInput(inputId = "McrewName",
                                 label = "Choose your ideal Crew:",
                                 choices = crewRev$crew_name[1:1000],
                                 selected = NULL,
                                 multiple = FALSE,
                                 options = list(maxOptions = 10, create = FALSE)
                  ),
                  selectizeInput(inputId = "Mpdtcompany",
                                 label = "Choose your Production Company:",
                                 choices = pdtcompaniesRev$production_companies[1:1000],
                                 selected = NULL,
                                 multiple = FALSE,
                                 options = list(maxOptions = 10, create = FALSE)
                  ),
                  selectInput(inputId = "Mpdtcountry",
                              label = "Choose your Production Country:",
                              choices = pdtcountriesRev$production_countries),
                  actionButton(inputId = "goButton",
                               label="Go!",
                               icon=icon('play-circle'))
                ),

                mainPanel(
                  fluidRow(
                    column(3,h4("Your Revenue:"),align="right"),
                    column(4,h4(textOutput("Mrev"), style = "background-color:#FFCC33"),align="center"),
                    column(3,h4("Your Vote:"),align="right"),
                    column(2,h4(textOutput("Mvote"), style = "background-color:#FFCC33"),align="center")
                  ),
                  br(),
                  fluidRow(
                    column(1),
                    column(4,h3("Your Movie beats"),align="right"),
                    column(2,h4(textOutput("Mbeat"), style = "background-color:#FFCC33"),align="center"),
                    column(5,h3("movies in the market"),align="left")
                  ),
                  br(),
                  br(),
                  tabsetPanel(
                    tabPanel(
                      "Profitability Analysis",
                      plotOutput("BudgetandProfit"),
                      br(),
                      plotOutput("ROI")
                    ),
                    tabPanel(
                      "Movie Battle",
                      div(
                        style="background-color: #FFF",
                        br(),
                        selectInput(inputId = "McomparedMovie",
                                    label = "Select the movie you want to compared with:",
                                    choices = data.features$title),
                        fluidRow(
                          column(5,h2(textOutput("selectedMovie"),face="bold"),align="center"),
                          column(2,img(src="vs.png",width="100%")),
                          column(5,h2(textOutput("yourMovie"),face="bold"),align="center")
                        ),
                        fluidRow(
                          column(5,plotOutput("ComparedMovie",height=300)),
                          column(7,plotOutput("YourMovie",height=300))
                        ),
                        p("  * The x-axis represents how the movie performs compared to the industry average:  '1' equals to the industry level")
                      )
                    )
                  )
                )
              )
      )
    )
  )
)

# =================================================================================================================================
# server
# =================================================================================================================================
# =================================================================================================================================
# movie
# =================================================================================================================================

server <- function(input, output) {
  # ==============================================================================================================================
  # home
  # ==============================================================================================================================
  
  output$tbl <- renderDataTable(ddw,
                                caption = ("MOVIE DATA"),
                                options = list(pageLength = 10), # 展示的行数
                                # rownames=FALSE,
                                # colnames = c(LETTERS[1:6])) %>%
                                colnames = c(
                                  "title" = 2, "original_language" = 3, "release_date" = 4,
                                  "vote_count" = 5, "vote_average" = 6
                                )
  )
  
  # ==============================================================================================================================
  # movie
  # ==============================================================================================================================
  
  values <- reactiveValues(titles = NULL)
  
  observeEvent(input$genre, {
    if (!is.null(input$genre)) {
      titles <- genres[genres$genre==input$genre,]$title
      values$titles <- titles
      output$movieOut <- renderUI({
        selectInput(
          inputId = "movieInput",
          label = "Movie",
          choices =  c("",values$titles),
          selected=""
        )
      })
    }
  })
  
  output$tbl = renderDataTable(movies_szy,
                               caption = ('MOVIE DATA'),
                               options = list(pageLength = 10),
                               colnames = c('title'=2,'original_language'=3,'release_date'=4,
                                            'vote_count'=5,'vote_average'=6))
  output$info <- renderUI({
    req(input$movieInput)
    if(input$movieInput==""){
      tags$div(
        img(src="all_genre_alpa.jpg",height =860, width = "80%",style='margin:0 auto'),
        style="text-align:center"
      )
    }else{
      movie_id <- genres$id[genres$title==input$movieInput][1]
      movie <- main_szy[main_szy$id==movie_id, ]
      # pic_url <- getPic(input$movieInput)
      video_url <- getVideo(input$movieInput)
      tags$div(
        fluidRow(
          column(6,
                 h2(input$movieInput),
                 h4(movie$tagline),
                 p(span(movie$status, class="label label-danger"), span(movie$release_date),
                   span("runtime", class="label label-danger"),span(movie$runtime)),
                 p(span("votes", class="label label-success"), span(movie$vote_count),
                   span("popularity", class="label label-success"), round(movie$popularity,0)),
                 p(span("Budget", class="label label-info"), prettyNum(movie$budget, big.mark = ","),
                   span("Revenue", class="label label-info"), prettyNum(movie$revenue, big.mark = ",")),
                 p(span("Language", class="label label-warning"), movie$original_language),
                 p(span("Homepage", class="label label-warning"), a(href=movie$homepage, movie$homepage)),
                 h3("Overview"),
                 p(movie$overview)
          ),
          div(
            column(6, tags$div(img(src=movie$pic_url, width="80%"))),
            style="text-align:center;margin-top:20px"
          )
        ),
        fluidRow(
          div(
            tags$video(src=video_url, width="80%", height="500", controls="controls"),
            style="text-align:center;margin-top:20px"
          )
        ),
        p("Video and Picture Source: https://www.imdb.com/"),
        style="margin:0 auto; width:90%"
      )
    }
  })
  
  # ==============================================================================================================================
  # analysis
  # ==============================================================================================================================
  
  main0 <- read_csv("./Dataset/mainzyf.csv")
  main1 <- main0 %>%
    select(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  main1$Year <- substr(main1$release_date, 1, 4)
  main1 <- na.omit(main1)
  genres1 <- genres %>% select(2, 4)
  genres1 <- na.omit(genres1)
  main1$id <- as.character(main1$id)
  genres1$id <- as.character(genres1$id)
  joindata <- main1 %>%
    inner_join(genres1, by = c("id"))
  joindata <- na.omit(joindata)
  joindata$budget_in_million <- joindata$budget / 1000000
  joindata$revenue_in_million <- joindata$revenue / 1000000
  
  output$plot1 <- renderPlot({
    Year_Begin <- input$year_Begin
    Year_End <- input$year_End
    caldata <- joindata %>%
      filter(Year >= Year_Begin) %>%
      filter(Year <= Year_End) %>%
      group_by(Year) %>%
      summarise(
        "num_movie" = n(),
        "ave_popularity" = mean(popularity),
        "ave_runtime" = mean(runtime),
        "ave_vote" = mean(vote_average),
        "ave_budget" = mean(budget_in_million),
        "ave_revenue" = mean(revenue_in_million)
      )
    
    # plot1
    if (Year_Begin > Year_End) {
      ggplot(NULL) +
        annotate("text",
                 label = "The end year is less than the begin year",
                 x = 1, y = 1, size = 6
        ) +
        theme_bw() +
        theme(
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
    } else {
      index <- input$select
      xlable <- seq(Year_Begin, Year_End, by = 5)
      ggplot(data = caldata) +
        aes_string("Year", index, group = 1) +
        geom_point(color="steelblue",size=2) +
        geom_line(color="steelblue") +
        xlab("Years") +
        ylab(index) +
        scale_x_discrete(breaks = xlable)+
        theme_classic()
    }
  })
  
  # plot 2
  output$plot2 <- renderPlot({
    index1 <- input$select1
    index2 <- input$select2
    ggplot(data = joindata) +
      aes_string(index1, index2, group = 1, color = input$select3) +
      geom_point() +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(face = "bold",size = 15),
        legend.title = element_text(face = "bold",size = 10),
        axis.title=element_text(face = "bold", size = 10)
      )
  })
  # plot3
  output$plot3 <- renderPlot({
    index3 <- input$Plot3Input
    caldata1 <- main1 %>%
      inner_join(genres1, by = c("id")) %>%
      select(id,release_date,genre) %>%
      mutate(year=year(as.Date(release_date))) %>%
      group_by(genre,year) %>%
      summarise(cnt=n()) %>%
      filter(year<=index3) %>%
      group_by(genre) %>%
      summarise(n=sum(cnt))
    if (nrow(caldata1) == 0) {
      ggplot(NULL) +
        annotate("text",
                 label = "Data of this year is incomplete.",
                 x = 1, y = 1, size = 10
        ) +
        theme_bw() +
        theme(
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
    } else {
      ggplot(data=caldata1,aes(x=reorder(genre,n), y=n, fill=genre)) +
          geom_bar(stat = "identity", alpha = .6, width = 0.7) +
          geom_text(aes(label=n), hjust=-0.4) +
          coord_flip() +
          theme_bw() +
          theme(plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.text=element_text(size=10, face="bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none") +
          theme(plot.margin=unit(c(0.9,0.9,0.9,0.9),"cm"))
    }
  })
  
  # plot4
  output$plot4 <- renderPlot(
    {
      index41 <- input$cyear_B
      index42 <- input$cyear_E
      caldata2 <- joindata %>%
        filter(Year >= index41) %>%
        filter(Year <= index42) %>%
        group_by(genre) %>%
        summarise(n = n()) %>%
        arrange(n) %>%
        mutate(genre = factor(genre, levels = genre))
      
      if (nrow(caldata2)==0) {
        ggplot(NULL) +
          annotate("text",
                   label = "The end year is less than the begin year or this year has no movie",
                   x = 1, y = 1, size = 6
          ) +
          theme_bw() +
          theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none"
          )
      } else {
        wordcloud(caldata2$genre, caldata2$n,min.freq=1,
                  scale = c(7, 1),
                  random.order = F,
                  colors = colorRampPalette(c("blue","purple"))(nrow(caldata2))
        )
      }
    },
    height = 500
  )
  
  #==============================================================================================================================
  # make your own movie
  #==============================================================================================================================
  data.input = reactive({
    # get the input data
    # Take a dependency on input$goButton
    input$goButton
    data.frame(
      id=0,
      mtitle = isolate(input$Mtitle),
      mreleasedate = year(as.Date(isolate(input$Mreleasedate))),
      mruntime = isolate(input$Mruntime),
      mbudget = isolate(input$Mbudget)*10^6,
      
      mgenre = isolate(input$Mgenre),
      mkeyword = isolate(input$Mkeyword),
      mpdtcompany = isolate(input$Mpdtcompany),
      mpdtcountry = isolate(input$Mpdtcountry),
      mcastname = isolate(input$McastName),
      mcrewname = isolate(input$McrewName),
      
      avggenreRev = genreRev[genreRev$genre==isolate(input$Mgenre),"avggenreRev"],
      avgkwRev = keywordRev[keywordRev$keywords==isolate(input$Mkeyword),"avgkwRev"],
      avgpdtcompaniesRev = pdtcompaniesRev[pdtcompaniesRev$production_companies==isolate(input$Mpdtcompany),"avgpdtcompaniesRev"],
      avgpdtcountriesRev = pdtcountriesRev[pdtcountriesRev$production_countries==isolate(input$Mpdtcountry),"avgpdtcountriesRev"],
      avgcastRev = castRev[castRev$cast_name==isolate(input$McastName),"avgcastRev"],
      avgcrewRev = crewRev[crewRev$crew_name==isolate(input$McrewName),"avgcrewRev"]
    )
  })
  data.yourmoviedata = reactive({
    if (data.input()$mruntime!=0){
      # build model and make prediction of revenue
      rev.lm = lm(revenue~.-id-title-vote_average,data=data.features)
      coef = rev.lm$coefficients
      predRev = coef[1] +
        coef[2]*data.input()$mreleasedate +
        coef[3]*data.input()$mruntime +
        coef[4]*data.input()$mbudget +
        coef[5]*data.input()$avggenreRev +
        coef[6]*data.input()$avgkwRev +
        coef[7]*data.input()$avgpdtcompaniesRev +
        coef[8]*data.input()$avgpdtcountriesRev +
        coef[9]*data.input()$avgcastRev +
        coef[10]*data.input()$avgcrewRev
      
      # build model and make prediction of vote_average
      vote.lm = lm(vote_average~.-id-title-revenue,data=data.features)
      coef = vote.lm$coefficients
      predVote = coef[1] +
        coef[2]*data.input()$mreleasedate +
        coef[3]*data.input()$mruntime +
        coef[4]*data.input()$mbudget +
        coef[5]*data.input()$avggenreRev +
        coef[6]*data.input()$avgkwRev +
        coef[7]*data.input()$avgpdtcompaniesRev +
        coef[8]*data.input()$avgpdtcountriesRev +
        coef[9]*data.input()$avgcastRev +
        coef[10]*data.input()$avgcrewRev
      data.frame(
        id=0,
        title=data.input()$mtitle,
        budget=data.input()$mbudget,
        revenue=round(predRev,2),
        vote_average=round(predVote,1)
      )
    }
    
  })
  output$Mrev <- renderText({
    if (data.input()$mruntime!=0){
      paste(prettyNum(data.yourmoviedata()$revenue,big.mark=","))
    }else{
      paste(prettyNum(0,big.mark=","))
    }
  })
  output$Mvote <- renderText({
    if (data.input()$mruntime!=0){
      paste(data.yourmoviedata()$vote)
    }else{
      paste("0.0")
    }
  })
  output$Mbeat <- renderText({
    if (data.input()$mruntime!=0){
      percent = nrow(data.features[data.features$revenue<=data.yourmoviedata()$revenue,])/nrow(data.features)*100
      paste(round(percent,0),"%")
    } else{
      paste("0%")
    }
  })
  output$selectedMovie <- renderText({
    if (data.input()$mruntime!=0){
      input$McomparedMovie
    }
  })
  output$yourMovie <- renderText({
    if (data.input()$mruntime!=0){
      data.input()$mtitle
    }
  })
  candiOutput = reactive({
    candiOutput = data.features %>%
      select(id, title, budget, revenue, vote_average)  %>%
      arrange(abs(budget-data.yourmoviedata()$budget),desc(revenue)) %>%
      slice(1:10) %>%
      bind_rows(data.yourmoviedata()) %>%
      mutate(
        ROI = round(revenue/budget,0),
        vote = round(vote_average,1)
      ) %>%
      select(id, title, budget, revenue, ROI, vote)
    candiOutput
  })
  output$BudgetandProfit <- renderPlot({
    if (data.input()$mruntime!=0){
      candiOutput() %>%
        transmute(title=title, budget=budget, profit=revenue-budget, ROI=ROI) %>%
        pivot_longer(c("budget", "profit"),
                     names_to="type", values_to="value") %>%
        mutate(
          value=if_else(type=="profit",value,-value)
        ) %>%
        ggplot(aes(x=title, y=value)) +
          geom_bar(aes(fill=type), stat="identity") +
          scale_fill_manual(values=c(profit="#D55E00", budget="#E69F00"), name="") +
          labs(x="", y="Profitability") +
          theme(axis.ticks=element_blank(),
                axis.text.x=element_text(size=10,vjust=0.5))+
          coord_flip()
    }
    
  })
  output$ROI <- renderPlot({
    if (data.input()$mruntime!=0){
      ggplot(candiOutput(), aes(x=title,y=ROI, fill=as.factor(ROI))) +
        geom_bar(stat="identity")+
        scale_fill_brewer(palette="Oranges")+
        labs(x="", y="ROI") +
        theme(axis.ticks=element_blank(),
              legend.title=element_blank(),
              axis.text.x=element_text(size=10,vjust=0.5))+
        coord_flip()
    }
  })
  output$ComparedMovie <- renderPlot({
    if (data.input()$mruntime!=0){
      df = data.features %>%
        filter(title==input$McomparedMovie) %>%
        select(budget,avggenreRev,avgkwRev,avgpdtcompaniesRev,avgcastRev,avgcrewRev)
      colnames(df) = c("BudgetLevel","GenreLevel","KeywordLevel","PdtCompanyLevel","CastLevel","CrewLevel")
      df[,"BudgetLevel"]=df[,"BudgetLevel"]/mean(data.features$budget)
      df[,"GenreLevel"]=df[,"GenreLevel"]/mean(data.features$avggenreRev)
      df[,"KeywordLevel"]=df[,"KeywordLevel"]/mean(data.features$avgkwRev)
      df[,"PdtCompanyLevel"]=df[,"PdtCompanyLevel"]/mean(data.features$avgpdtcompaniesRev)
      df[,"CastLevel"]=df[,"CastLevel"]/mean(data.features$avgcastRev)
      df[,"CrewLevel"]=df[,"CrewLevel"]/mean(data.features$avgcrewRev)
    }else{
      df = data.frame(
        BudgetLevel=0,
        GenreLevel=0,
        KeywordLevel=0,
        PdtCompanyLevel=0,
        CastLevel=0,
        CrewLevel=0
      )
    }
    df = data.frame(t(df))
    colnames(df) = "value"
    df[,"cretier"] = rownames(df)
    ggplot(df,width=100)+
      geom_bar(mapping=aes(x=cretier,y=value),stat="identity",fill="#339999")+
      scale_y_reverse(limits=c(12,0))+
      geom_hline(yintercept = 1, color = "red") +
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            axis.text.y=element_blank())+
      coord_flip()
    
  })
  output$YourMovie <- renderPlot({
    if (data.input()$mruntime!=0){
      df = data.frame(
        BudgetLevel = data.input()$mbudget/mean(data.features$budget),
        GenreLevel = genreRev[genreRev$genre==data.input()$mgenre,"avggenreRev"]/mean(data.features$avggenreRev),
        KeywordLevel = keywordRev[keywordRev$keywords==data.input()$mkeyword,"avgkwRev"]/mean(data.features$avgkwRev),
        PdtCompanyLevel = pdtcompaniesRev[pdtcompaniesRev$production_companies==data.input()$mpdtcompany,"avgpdtcompaniesRev"]/mean(data.features$avgpdtcompaniesRev),
        CastLevel = castRev[castRev$cast_name==data.input()$mcastname,"avgcastRev"]/mean(data.features$avgcastRev),
        CrewLevel = crewRev[crewRev$crew_name==data.input()$mcrewname,"avgcrewRev"]/mean(data.features$avgcrewRev)
      )
      colnames(df) = c("BudgetLevel          ","GenreLevel          ","KeywordLevel          ","PdtCompanyLevel          ","CastLevel          ","CrewLevel          ")
    }else{
      df = data.frame(
        BudgetLevel=0,
        GenreLevel=0,
        KeywordLevel=0,
        PdtCompanyLevel=0,
        CastLevel=0,
        CrewLevel=0
      )
      colnames(df) = c("BudgetLevel          ","GenreLevel          ","KeywordLevel          ","PdtCompanyLevel          ","CastLevel          ","CrewLevel          ")
    }
    df = data.frame(t(df))
    colnames(df) = "value"
    df[,"cretier"] = rownames(df)
    ggplot(df)+
      geom_bar(mapping=aes(x=cretier,y=value),stat="identity",fill="#CC6666")+
      ylim(0,12)+
      geom_hline(yintercept = 1, color = "red") +
      coord_flip()+
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            axis.text.y=element_text(hjust=0,size=10,face="bold"))
    
  })
}

shinyApp(ui, server)
