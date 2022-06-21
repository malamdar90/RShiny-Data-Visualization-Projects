library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shinydashboard) 
library(leaflet)
library(DT)
library(RColorBrewer)
library(maps)
library(splitstackshape)
library(reshape2)
library(highcharter)
library(treemap)
library(Rcpp)

ui <- dashboardPage(
    dashboardHeader(title = "IMDB movie rating"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = 'page1', icon = icon('home')),
            menuItem("Chart", tabName = "page2", icon = icon("bar-chart")),
            menuItem("Preference", tabName = "page3", icon = icon("line-chart")),
            menuItem("Treemap", tabName = "page4", icon = icon("tree")),
            menuItem("Map", tabName = "page5", icon = icon("globe")),
            menuItem("Ranking", tabName = 'page6', icon = icon('table'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    h1(strong("Research on IMDb Movies"),align="center"),
                    column(
                        h2(icon("star"),strong("Project Overview")),
                        p('With economic development, watching movies is a more and more popular activity for urban residents in their spare time. 
                          Especially during the COVID-19 pandemic, it has been a good way of killing time.'),
                        p('The project will give some valuable business insights to movie publishers and streaming service providers such as Netflix, 
                          Amazon Prime, Hulu, Walt Disney. The project will also benefit audiences as well when they are making a decision.'),
                        h2(icon("list"),strong("About the Tabs")),
                        p('For the tabs, we have Home, Chart, Preference, Map, and Year Ranking. Chart shows some statistical analysis. Preference shows the topic change over time.
                          Map shows geographic distribution of good movies. And Year ranking directly display the table format.'),
                        h2(icon("search-plus"),strong("Research Questions")),
                        p('Our project discovers rankings based on rating or top, regions rich in good films, and preference change over time.'),
                        h2(icon("table"),strong("Data Set Info")),
                        p('The dataset gives us some information about the IMDb movies that depicts the movie plot description, Metastore ratings, 
                          critic and user ratings and reviews, release dates, and many more aspects. The data has been taken from Kaggle and sourced from IMDb website.'),
                        p('The movies dataset includes 85,855 movies with attributes such as movie description, average rating, number of votes, genre, etc. 
                          The ratings dataset includes 85,855 rating details from demographic perspective.'),
                        a(h4("Data Source: https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset"),href="https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset",target="_blank"),
                        h2(icon("user"),strong("Team Members")),
                        p('Hanqi Xu    Zefeng Li    Naina Grover'),
                        width = 12)
            ),
            tabItem(tabName = "page2",
                    fluidRow(
                        column(3),
                        column(6,highchartOutput("plot3", height = 400)),
                        column(3)
                    ),
                    fluidRow(
                        column(6,highchartOutput("plot5", height = 400)),
                        column(6,highchartOutput("plot6", height = 400))
                    )
                    
            ),
            tabItem(tabName = "page3",
                    plotlyOutput("plot2",height = 700)
            ),
            tabItem(tabName = "page4",
                    checkboxInput("topmovie", label = "Show top movies", value = FALSE),
                    plotOutput("plot7", height = 800,width="100%")
            ),
            tabItem(tabName = "page5",
                    selectizeInput("select_rank","Select ranking you want to see",
                                   choices =  c("Ratings" = "rating","Top" = "top"), 
                                   selected = NULL,  width = "200px",multiple = F,options = NULL),
                    uiOutput("ranking"),
                    plotlyOutput("plot1", height = 750)
            ),
            tabItem(tabName = "page6",
                    selectizeInput("select_year","Rank by",
                                   choices =  c("1894-1920" = 1920,"1921-1950"=1950,"1951-1960"=1960,"1961-1970"=1970,"1971-1980"=1980,"1981-1990"=1990,"1991-2000"=2000,"2001-2010"=2010,"2011-2020"=2020), 
                                   selected = NULL,  width = "200px",multiple = F,options = NULL),
                    actionButton('btn_build_year_ranking', 
                                 paste0('Build Ranking'),
                                 icon = icon('wrench')),
                    dataTableOutput("myTable")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
    world_map <- map_data("world")
    
    movie = read.csv("IMDb movies.csv")
    data = read.csv("IMDb movies.csv")    
    data_2 = read.csv("IMDb movies.csv")    
    
    ratings<- read_csv("IMDb ratings.csv")
    
    movie%>%
        distinct(country)
    movie = movie[,1:16]
    movie = movie %>%
        arrange(desc(avg_vote),desc(votes))
    movie1 = read.csv("IMDb movies 1.csv")
    movie = movie%>%
        mutate(country = movie1$country)
    movie2= movie[,c(1,2,4,6,7,8,15,16)]
    movies = read.csv("IMDb movies.csv")
    options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0)))
    country_df<- movie$country
    country_df<- data.frame(country_df)
    country_df <- country_df %>% group_by(country_df) %>% count() 
    country_df <- na.omit(country_df)
    
    title_df<- movies %>% select(imdb_title_id,title)
    rate_df <- ratings %>% select(imdb_title_id,mean_vote)
    title_rate <- merge(title_df,rate_df,by=c("imdb_title_id"))
    title_rate<- na.omit(title_rate)
    title_rate1<-title_rate %>% arrange(desc(mean_vote)) %>% head(n=20)

    rank =  eventReactive(input$btn_build_year_ranking,{
        if(input$select_year<1960){
            return(datatable(movie%>%
                                 filter(year>as.numeric(input$select_year)-30 & year<=as.numeric(input$select_year))%>%
                                 arrange(desc(avg_vote),desc(votes)),rownames = FALSE))
        }
        else{ datatable(movie%>%
                            filter(year>as.numeric(input$select_year)-10 & year<=as.numeric(input$select_year))%>%
                            arrange(desc(avg_vote),desc(votes)),rownames = FALSE)
        }
    }
    )

    output$myTable=renderDataTable({
        rank()
    })
    
    output$ranking <- renderUI({
        if (input$select_rank != "top") 
            (selectInput("rating","Search for rating",c("above 9.5" = 9.5,"above 9.2" = 9.2,"above 9.0" = 9.0,"above 8.7" = 8.7),selected = "above 9.5"))
        
        else
            selectInput("TOP", "Search for", c("TOP 5" = 5,"TOP 10" =10 ,"TOP 30"= 30, "TOP 50" = 50,"TOP 100" = 100),selected = "TOP 5")
    })
        
    output$plot1 = renderPlotly({
        if (input$select_rank != "top")
            map = movie2 %>%
                filter(avg_vote > input$rating) %>%
                right_join(world_map,by = c("country"="region"))
        else 
            map = right_join(movie2[1:input$TOP,],world_map,by = c("country"="region"))
        
            mp =  ggplot(map, aes(x = long, y = lat, group = group)) +
                geom_polygon(aes(fill=avg_vote ), colour = "white") +
                scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
                scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
                scale_fill_gradient(low = "lightblue", high="steel blue") +
                labs(title = "world ranking"
                )+
                theme_light()
            ggplotly(mp)
    })
        
    output$plot3 = renderHighchart({
        country_df %>% 
            arrange(desc(n)) %>% 
            head(n=20) %>%
            hchart('bar',hcaes(x=country_df,y=n),color="lightblue",borderColor="turquoise",borderWidth=2,pointWidth = 20) %>% 
            hc_title(text = "Countries with Top movies") %>%
            hc_xAxis(categories = country_df,
                     tickmarkPlacement = "on",
                     title = list(text = "Country Names")) %>%
            hc_yAxis(title = list(text = "Number of Movies"))
    })
    
    
    year_df<- movies$year
    year_df<-data.frame(year_df)
    year1<- year_df %>% group_by(year_df) %>% count()
    output$plot5 = renderHighchart({
        year1 %>% 
            arrange(desc(year1)) %>% 
            head(n=20) %>% 
            hchart('bar',hcaes(x=reorder(year_df,n),y=n),color="orange",borderColor="purple",borderWidth=2,pointWidth = 20) %>% 
            hc_title(text = "Movies over the years") %>%
            hc_xAxis(categories = year_df,
                     tickmarkPlacement = "on",
                     title = list(text = "Year")) %>%
            hc_yAxis(title = list(text = "Number of Movies"))
        
    })
    
    director<- movie$director
    director<- data.frame(director) 
    director1<-director %>% 
        group_by(director) %>% 
        count()
    director1<-na.omit(director1)
    director1 <- director1 %>% 
        arrange(desc(n))

    output$plot6 = renderHighchart({
        director1 %>% 
            head(n=20) %>% 
            hchart('bar',hcaes(x=reorder(director,n),y=n),color="purple",borderColor="yellow",pointWidth = 20) %>% 
            hc_title(text = "Directors with Top Movies")%>%
            hc_xAxis(categories = director,
                     tickmarkPlacement = "on",
                     title = list(text = "Director")) %>%
            hc_yAxis(title = list(text = "Number of Movies"))
        
    })


    output$plot7 = renderPlot({
        country <- cSplit(data,"country",",")
        if(input$topmovie==TRUE){
            country <- country %>% filter(avg_vote>=7)}
        stat_country_01 <- country %>% select(country_01) 
        
        stat_country_02 <- na.omit(country %>% select(country_02)) 
        stat_country_02 <- rename(stat_country_02,country_1=country_02)
        
        stat_country_03 <- na.omit(country %>% select(country_03))
        stat_country_03 <- rename(stat_country_03,country_01=country_03)
        
        stat_country_04 <- na.omit(country %>% select(country_04))
        stat_country_04 <- rename(stat_country_04,country_01=country_04)
        
        stat_country_05 <- na.omit(country %>% select(country_05))
        stat_country_05 <- rename(stat_country_05,country_01=country_05)
        
        stat_country_06 <- na.omit(country %>% select(country_06)) 
        stat_country_06 <- rename(stat_country_06,country_01=country_06)
        
        stat_country_07 <- na.omit(country %>% select(country_07))
        stat_country_07 <- rename(stat_country_07,country_01=country_07)
        
        stat_country_08 <- na.omit(country %>% select(country_08))
        stat_country_08 <- rename(stat_country_08,country_01=country_08)
        
        stat_country_09 <- na.omit(country %>% select(country_09)) 
        stat_country_09 <- rename(stat_country_09,country_01=country_09)
        
        stat_country_10 <- na.omit(country %>% select(country_10)) 
        stat_country_10 <- rename(stat_country_10,country_01=country_10)
        
        stat_country_11 <- na.omit(country %>% select(country_11))
        stat_country_11 <- rename(stat_country_11,country_01=country_11)
        
        stat_country_12 <- na.omit(country %>% select(country_12)) 
        stat_country_12 <- rename(stat_country_12,country_01=country_12)
        
        stat_country_13 <- na.omit(country %>% select(country_13)) 
        stat_country_13 <- rename(stat_country_13,country_01=country_13)
        
        stat_country_14 <- na.omit(country %>% select(country_14))
        stat_country_14 <- rename(stat_country_14,country_01=country_14)
        
        stat_country_15 <- na.omit(country %>% select(country_15))
        stat_country_15 <- rename(stat_country_15,country_01=country_15)
        
        stat_country_16 <- na.omit(country %>% select(country_16))
        stat_country_16 <- rename(stat_country_16,country_01=country_16)
        
        stat_country_17 <- na.omit(country %>% select(country_17))
        stat_country_17 <- rename(stat_country_17,country_01=country_17)
        
        stat_country_18 <- na.omit(country %>% select(country_18))
        stat_country_18 <- rename(stat_country_18,country_01=country_18)
        
        stat_country_19 <- na.omit(country %>% select(country_19))
        stat_country_19 <- rename(stat_country_19,country_01=country_19)
        
        stat_country_20 <- na.omit(country %>% select(country_20)) 
        stat_country_20 <- rename(stat_country_20,country_01=country_20)
        
        stat_country_21 <- na.omit(country %>% select(country_21))
        stat_country_21 <- rename(stat_country_21,country_01=country_21)
        
        stat_country_22 <- na.omit(country %>% select(country_22)) 
        stat_country_22 <- rename(stat_country_22,country_01=country_22)
        
        stat_country_23 <- na.omit(country %>% select(country_23))
        stat_country_23 <- rename(stat_country_23,country_01=country_23)
        
        stat_country_24 <- na.omit(country %>% select(country_24)) 
        stat_country_24 <- rename(stat_country_24,country_01=country_24)
        
        stat_country_25 <- na.omit(country %>% select(country_25))
        stat_country_25 <- rename(stat_country_25,country_01=country_25)
        
        stat_complete <- rbind(stat_country_01, 	stat_country_02, 	stat_country_03, 	stat_country_04, 	stat_country_05, 	stat_country_06, 	stat_country_07, 	stat_country_08, 	stat_country_09, 	stat_country_10, 	stat_country_11, 	stat_country_12, 	stat_country_13, 	stat_country_14, 	stat_country_15, 	stat_country_16, 	stat_country_17, 	stat_country_18, 	stat_country_19, 	stat_country_20, 	stat_country_21, 	stat_country_22, 	stat_country_23, 	stat_country_24, 	stat_country_25,fill = TRUE)
        Number<-table(stat_complete$country_01)
        dtf<-as.data.frame(Number) 
        colnames(dtf) <- c("Country_Names","Number_Of_Movies")
        
        dtf = dtf %>% arrange(-Number_Of_Movies)
        dtf2= dtf %>% filter(Number_Of_Movies>100)
        
        dtf2$label <- paste(dtf2$Country_Names, dtf2$Number_Of_Movies, sep = ", ")
        treemap(dtf2,index=c("label"),vSize="Number_Of_Movies",type = "index")
    })
    
    output$plot2 = renderPlotly({
        genre <- cSplit(data,"genre",",")
        stat_genre_1 <- genre %>% select(year,genre_1)
        stat_genre_2 <- na.omit(genre %>% select(year,genre_2))
        stat_genre_2 <- rename(stat_genre_2,genre_1=genre_2)
        stat_genre_3 <- na.omit(genre %>% select(year,genre_3))
        stat_genre_3 <- rename(stat_genre_3,genre_1=genre_3)
        stat_complete <- rbind(stat_genre_1,stat_genre_2,stat_genre_3)
        
        data2 = stat_complete %>%
            group_by(genre_1,year)%>%
            summarise(N=n())
        genre_list <- unique(stat_complete$genre_1)
        pivot_table <- dcast(stat_complete, stat_complete$year ~ stat_complete$genre_1)
        final_data <- melt(pivot_table, id="stat_complete$year")
        colnames(final_data) <- c("Year","Category","Value")
        
        final_data[final_data=="TV Movie 2019"]<-NA
        data1 = data %>%
            group_by(genre,year)%>%
            summarise(N = n())
        data3 = stat_complete %>%
            group_by(year)%>%
            summarise(N=n())%>%
            filter(N>400)
        data4 = data2%>%
            merge(data3,by = c("year"="year"))
        data4 = data4%>%
            mutate(ratio = N.x/N.y)
        
        final_data%>%
            mutate(Year = as.integer(final_data$Year))
        line_graph <- ggplot(data=data4) + 
            geom_line(aes(x=year,y=ratio,group=genre_1,color = genre_1),size = 0.5) + 
            labs(x="Year", y="Number of movies", title="Preference change over time")
        ggplotly(line_graph)
    })
} 


# Run the application 
shinyApp(ui = ui, server = server)
