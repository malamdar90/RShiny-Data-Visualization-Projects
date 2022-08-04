#library(remotes)
#remotes::install_github("hrbrmstr/ggchicklet")
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(janitor)
library(tidyverse)
library(leaflet.providers)
library(ggchicklet)
library(maps)

#Comparison page#
####################world map
ww <- map_data("world")
ww <- ww[ww$region != "Antarctica",]
setDT(ww)

####################the data 
data.file<-'DataPanelWHR2021C2.xls'
read_xls(data.file)->info
setDT(info)
names(info)<-gsub("\\s+","_",names(info))
info[,.N,year][order(year)]
######
#setdiff(info[,Country_name],ww[,region])
info[,region2:=Country_name]
info[Country_name=="United States",region2:='USA']
info[Country_name=="United Kingdom",region2:='UK']
info[Country_name=="North Cyprus",region2:='Cyprus']
info[Country_name=="North Macedonia",region2:='Macedonia']
info[Country_name=="Somaliland region",region2:='Somalia']
info[Country_name=="Palestinian Territories",region2:='Palestine']
info[Country_name=="Trinidad and Tobago",region2:='Trinidad']
info[Country_name=="Congo (Brazzaville)",region2:='Democratic Republic of the Congo']
info[Country_name=="Congo (Kinshasa)",region2:='Republic of Congo']
info[Country_name=="Taiwan Province of China",region2:='Taiwan']

ww[,region2:=region]
ww[region=='China' & subregion=='Hong Kong',region2:='Hong Kong S.A.R. of China']

info<-info[,.(region2,Positive_affect,Negative_affect,year)]
#setdiff(info[,region2],ww[,region2])
#col.palette<-scale_fill_distiller(palette='YlOrRd',direction=1,na.value='#B6B6B4',limits=c(0,1))
col.palette<-scale_fill_gradientn(colours=c(rev(brewer.pal(5,'YlGnBu')),brewer.pal(5,'YlOrRd')),na.value=NA,limits=c(0,1))
##########

#Country Analysis page#
df <- read_excel("DataPanelWHR2021C2.xls")

# map data
country_position <- map_data("world") %>%
    group_by(region) %>%
    summarise(long = mean(long),
              lat = mean(lat))

# clean columns' name and combine map data
df <- df %>%
    clean_names() %>%
    rename(region = country_name)

df <- df %>%
    mutate_at("region", ~str_replace_all(., "Congo\\ \\(Brazzaville\\)", "Republic of Congo")) %>%
    mutate_at("region", ~str_replace_all(., "Congo\\ \\(Kinshasa\\)", "Democratic Republic of the Congo")) %>%
    mutate_at("region", ~str_replace_all(., "Taiwan\\ Province\\ of\\ China", "Taiwan")) %>%
    mutate_at("region", ~str_replace_all(., "United\\ Kingdom", "UK")) %>%
    mutate_at("region", ~str_replace_all(., "United\\ States", "US")) %>%
    mutate_at("region", ~str_replace_all(., "Trinidad\\ and\\ Tobago", "Trinidad")) %>%
    mutate_at("region", ~str_replace_all(., "Palestinian\\ Territories", "Palestine"))


df <- df %>%
    left_join(country_position) %>%
    mutate_at(2:13, ~round(., 2))

df$long[df$region == "Hong Kong S.A.R. of China"] <- 114.17
df$lat[df$region == "Hong Kong S.A.R. of China"] <- 22.32

df$long[df$region == "North Cyprus"] <- 33.42
df$lat[df$region == "North Cyprus"] <- 35.12

df$long[df$region == "North Macedonia"] <- 21.74
df$lat[df$region == "North Macedonia"] <- 41.6

df$long[df$region == "Somaliland region"] <- 43
df$lat[df$region == "Somaliland region"] <- 9

#Database Page#
Pastdata= read_excel("DataPanelWHR2021C2.xls")
Data2021<- read_xls("DataForFigure2.1WHR2021C2.xls")
Data2021<- select(Data2021, c(1,3,7,8,9,10,11,12))
Data2021$year<-2021
colnames(Data2021)<-c("Country name", "Life Ladder", "Log GDP per capita","Social support", 
                      "Healthy life expectancy at birth", "Freedom to make life choices","Generosity",
                      "Perceptions of corruption","year")
data <- bind_rows(Pastdata, Data2021)




ui <- dashboardPage(
    dashboardHeader(title = "World Happiness 2021"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "page1", icon = icon("home")),
            menuItem("Overview", tabName = "page2", icon = icon("globe")),
            menuItem("Single Country Analysis", tabName = "page3", icon = icon("area-chart")),
            menuItem("Positive/Negative Affect", tabName = "page4", icon = icon("adjust")),
            menuItem("Database", tabName = "page5", icon = icon("database")),
            menuItem("Video", tabName = "page6", icon = icon("video"))
        )
    ),
    dashboardBody(
        tabItems(
            
            # Introduction page
            tabItem(tabName = "page1",
                    fluidRow(
                        box(width = 12,
                            h2("World Happiness Report",align = "center"), 
                            "The World Happiness Report is a publication of the United Nations 
                            Sustainable Development Solutions Network. It contains articles and rankings of national happiness, 
                            based on respondent ratings of their own lives, which the report also correlates with various (quality of) 
                            life factors.",
                            br(),br(),
                            "The report primarily uses data from the Gallup World Poll. Each annual report is 
                            available to the public to download on the World Happiness Report website. ",
                            style = "font-size:18px",align = "left",
                            br(),br(),
                            fluidRow(align = "right","--------",
                            a(href="https://en.wikipedia.org/wiki/World_Happiness_Report", "World Happiness Report,Wikipedia"),HTML('&emsp;')),
                            br(),
                            fluidRow(align = "center",
                            HTML('<iframe width="50%" height="300" src="https://www.youtube.com/embed/FfVrYX6dhDw" frameborder="0" allowfullscreen></iframe>')
                            )),
                        box(width = 12,solidHeader = TRUE,status = "info",style = "font-size:16px",
                            title = "About the database", 
                            "The dataset downloads from", a(href="https://worldhappiness.report/ed/2021/","World Happiness Report 2021"),"website.",br(),
                            h4("Variable interpretation:"),
                            strong("life_ladder:"), "Happiness score or subjective well-being",
                            br(),
                            strong("log_gdp_per_capital:"), 
                            "The statistics of GDP per capita (variable name gdp) in purchasing power parity (PPP) at constant 2017 international dollar prices are from the October 14, 2020 update of the World Development Indicators (WDI).",
                            br(),
                            strong("social_support:"), 
                            "The national average of the binary responses (either 0 or 1) to the GWP question “If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?”",
                            br(),
                            strong("healthy_life_expectancy_at_birth:"), 
                            "Healthy life expectancies at birth are based on the data extracted from the World Health Organization’s (WHO) Global Health Observatory data repository (Last updated: 2020-09-28).",
                            br(),
                            strong("freedom_to_make_life_choises:"), 
                            "The national average of responses to the GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?”",
                            br(),
                            strong("generosity:"),
                            "The residual of regressing national average of response to the GWP question “Have you donated money to a charity in the past month?” on GDP per capita.",
                            br(),
                            strong("perceptions_of_corruption:"), 
                            "The measure is the national average of the survey responses to two questions in the GWP: “Is corruption widespread throughout the government or not” and “Is corruption widespread within businesses or not?” ",
                            br(),
                            strong("positive_affect:"),
                            "The average of three positive affect measures in GWP: happiness, laugh and enjoyment in the Gallup World Poll waves 3-7.",
                            br(),
                            strong("negative_affect:"),
                            "The average of three negative affect measures in GWP. They are worry, sadness and anger."
                            
                        ),
                        box(
                            title = "About this project:",  width = 12, style = "font-size:16px",
                            solidHeader = TRUE, status = "primary",
                            "This project explores the data from the world happiness report 2021. World happiness report contains the report analysis and rankings of national happiness. However, the report is a final version of the analysis. We want to provide a more flexible and customizable way to research world happiness data. Let readers chose what they want to analyze and use the visualization to show the results.",
                            h4("Goal"),
                            "The goal of the application is to provide a flexible and customizable tool for exploring World happiness data and provide visualization methods to show potential relationships among different factors.",
                            h4("Research Questions"),
                            "1.	What are the trends of the happiness score for each country?",
                            br(),
                            "2.	What are potential factors for a country that have the most influence on its happiness score?",
                            h4("Method"),
                            "Leaflet: we use leaflet to do mapping and provide an overview of the data.",
                            br(),
                            "ggplot: we use ggplot package to analyze the potential relationship between variables."
                        ),
                        box(
                            title = "Reference",  width = 12, style = "font-size:16px",
                            "World Happiness Report 2021",br(),
                            a(href="https://worldhappiness.report/ed/2021/", "https://worldhappiness.report/ed/2021/"),
                            br(),
                            "World Happiness Report,Wikipedia ",br(),
                            a(href="https://en.wikipedia.org/wiki/World_Happiness_Report", "https://en.wikipedia.org/wiki/World_Happiness_Report"),
                            br(),
                            "How to Measure Happiness Around the World | National Geographic",br(),
                            a(href="https://www.youtube.com/watch?v=FfVrYX6dhDw", "https://www.youtube.com/watch?v=FfVrYX6dhDw")
                            
                        ),
                        box(
                            title = "About us",  width = 12, 
                            fluidRow( 
                                column(3,img(src ="Johns_Hopkins_Carey_Business_School's_Logo.png",height = 100, width = 230)),
                                column(6,style = "font-size:16px",
                                       "Johns Hopkins Carey Business School",
                                       br(),
                                       "Summer 2021 Data Visualization Section X3"
                                       )
                                )
                            
                            )
            
                    )
            ),
            
            #Overview page
            tabItem(tabName = "page2",
                    h2("Overview"),
                    fluidRow(
                        column(6, tabsetPanel(
                            tabPanel("Happiness Score 2021",plotlyOutput("map1")),
                            tabPanel("Happiness Score History", 
                                     selectInput(inputId = "thisyear",
                                                 label = "Please Select Year",
                                                 choices=2005:2020,selected=2019),
                                     plotlyOutput("map2")))),
                        column(6, tabsetPanel(
                            tabPanel('Happiest 2021',plotOutput("top1")),
                            tabPanel('Saddest 2021',plotOutput("top2"))
                        )))
          
            ),
            
            #Country Analysis page
            tabItem(tabName = "page3",
                    fluidRow(
                        box(title = "Variable description:",width = 12, status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,collapsed = TRUE,
                            strong("life_ladder:"), "Happiness score or subjective well-being",
                            br(),
                            strong("log_gdp_per_capital:"), 
                            "The statistics of GDP per capita (variable name gdp) in purchasing power parity (PPP) at constant 2017 international dollar prices are from the October 14, 2020 update of the World Development Indicators (WDI).",
                            br(),
                            strong("social_support:"), 
                            "The national average of the binary responses (either 0 or 1) to the GWP question “If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?”",
                            br(),
                            strong("healthy_life_expectancy_at_birth:"), 
                            "Healthy life expectancies at birth are based on the data extracted from the World Health Organization’s (WHO) Global Health Observatory data repository (Last updated: 2020-09-28).",
                            br(),
                            strong("freedom_to_make_life_choises:"), 
                            "The national average of responses to the GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?”",
                            br(),
                            strong("generosity:"),
                            "The residual of regressing national average of response to the GWP question “Have you donated money to a charity in the past month?” on GDP per capita.",
                            br(),
                            strong("perceptions_of_corruption:"), 
                            "The measure is the national average of the survey responses to two questions in the GWP: “Is corruption widespread throughout the government or not” and “Is corruption widespread within businesses or not?” ",
                            br(),
                            strong("positive_affect:"),
                            "The average of three positive affect measures in GWP: happiness, laugh and enjoyment in the Gallup World Poll waves 3-7.",
                            br(),
                            strong("negative_affect:"),
                            "The average of three negative affect measures in GWP. They are worry, sadness and anger."
                        )
                    ),
                    fluidRow(
                        column(4, leafletOutput("plot1")),
                        column(8, tabsetPanel(
                            tabPanel("Year & Life Ladder", plotOutput("plot2")),
                            tabPanel("Life Ladder & Others", 
                                     selectInput(inputId = "x",
                                                 label = "Please Select X",
                                                 choices = names(df)[3:11],
                                                 selected = "life_ladder"),
                                     selectInput(inputId = "y",
                                                 label = "Please Select Y",
                                                 choices = names(df)[3:11],
                                                 selected = "log_gdp_per_capita"),
                                     plotlyOutput("plot3")),
                            tabPanel("Data", dataTableOutput("table")))
                        )
                    )
            ),
            
            #Comparison page
            tabItem(tabName = "page4",
                    fluidRow(
                        box(title = "Variable description:",width = 12, status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,collapsed = TRUE,
                            strong("life_ladder:"), "Happiness score or subjective well-being",
                            br(),
                            strong("log_gdp_per_capital:"), 
                            "The statistics of GDP per capita (variable name gdp) in purchasing power parity (PPP) at constant 2017 international dollar prices are from the October 14, 2020 update of the World Development Indicators (WDI).",
                            br(),
                            strong("social_support:"), 
                            "The national average of the binary responses (either 0 or 1) to the GWP question “If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?”",
                            br(),
                            strong("healthy_life_expectancy_at_birth:"), 
                            "Healthy life expectancies at birth are based on the data extracted from the World Health Organization’s (WHO) Global Health Observatory data repository (Last updated: 2020-09-28).",
                            br(),
                            strong("freedom_to_make_life_choises:"), 
                            "The national average of responses to the GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?”",
                            br(),
                            strong("generosity:"),
                            "The residual of regressing national average of response to the GWP question “Have you donated money to a charity in the past month?” on GDP per capita.",
                            br(),
                            strong("perceptions_of_corruption:"), 
                            "The measure is the national average of the survey responses to two questions in the GWP: “Is corruption widespread throughout the government or not” and “Is corruption widespread within businesses or not?” ",
                            br(),
                            strong("positive_affect:"),
                            "The average of three positive affect measures in GWP: happiness, laugh and enjoyment in the Gallup World Poll waves 3-7.",
                            br(),
                            strong("negative_affect:"),
                            "The average of three negative affect measures in GWP. They are worry, sadness and anger."
                        )
                    ),
                    fluidRow(
                        column(6,
                               selectizeInput('year1','Choose the 1st year:',choices=2005:2020,selected=2018),
                               radioButtons('col1.value','Map Value:',choices=c('Positive'='Positive_affect','Negative'='Negative_affect'),selected='Negative_affect',inline=TRUE),
                               plotOutput('plot.left'),
                               DTOutput('table.left')
                        ),
                        
                        column(6,
                               selectizeInput('year2','Choose the 2nd year:',choices=2005:2020,selected=2019),
                               radioButtons('col2.value','Map Value:',choices=c('Positive'='Positive_affect','Negative'='Negative_affect'),selected='Positive_affect',inline=TRUE),
                               plotOutput('plot.right'),
                               DTOutput('table.right')
                        )
                    )
            ),
            
            #Database page
            tabItem(tabName = "page5",
                    fluidRow(
                        box(title = "Variable description:",width = 12, status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,collapsed = TRUE,
                            strong("life_ladder:"), "Happiness score or subjective well-being",
                            br(),
                            strong("log_gdp_per_capital:"), 
                            "The statistics of GDP per capita (variable name gdp) in purchasing power parity (PPP) at constant 2017 international dollar prices are from the October 14, 2020 update of the World Development Indicators (WDI).",
                            br(),
                            strong("social_support:"), 
                            "The national average of the binary responses (either 0 or 1) to the GWP question “If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?”",
                            br(),
                            strong("healthy_life_expectancy_at_birth:"), 
                            "Healthy life expectancies at birth are based on the data extracted from the World Health Organization’s (WHO) Global Health Observatory data repository (Last updated: 2020-09-28).",
                            br(),
                            strong("freedom_to_make_life_choises:"), 
                            "The national average of responses to the GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?”",
                            br(),
                            strong("generosity:"),
                            "The residual of regressing national average of response to the GWP question “Have you donated money to a charity in the past month?” on GDP per capita.",
                            br(),
                            strong("perceptions_of_corruption:"), 
                            "The measure is the national average of the survey responses to two questions in the GWP: “Is corruption widespread throughout the government or not” and “Is corruption widespread within businesses or not?” ",
                            br(),
                            strong("positive_affect:"),
                            "The average of three positive affect measures in GWP: happiness, laugh and enjoyment in the Gallup World Poll waves 3-7.",
                            br(),
                            strong("negative_affect:"),
                            "The average of three negative affect measures in GWP. They are worry, sadness and anger."
                        )
                    ),
                    h2("Database"),
                    fluidRow(
                        box(width=12,
                        selectInput('year','year:',choices=2005:2021,selected=2018),
                        div(style = 'overflow-x: scroll', dataTableOutput ('mytable')),
                    a(href="https://worldhappiness.report/ed/2021/", "Data Source")
                    )
                    )
                    ),
            tabItem(tabName = "page6",
                    fluidRow(
                        box(width=12,align = "center",title = "Presentation Video",solidHeader = TRUE,
                            HTML('<iframe width="50%" height="300" src="https://www.youtube.com/embed/_gpUEMTQ4z4" frameborder="0" allowfullscreen></iframe>')
                            ))
                    
                    )
        )
    )
)
    
 

server <- function(input, output, session) {
    output$map1<- renderPlotly({
        df0 <- read.csv('2014_world_gdp_with_codes.csv')
        names(df0) <- c("Country", "gdp_billions", "code")
        happy2021 <- read.csv('DataPanelWHR2021C2.csv')
        colnames(happy2021)[1] <- "Country"
        df3 <- happy2021 %>% left_join(df0, by="Country")
        l <- list(color = toRGB("grey"), width = 0.2)
        g <- list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator')
        )
        
        p <- plot_geo(df3) %>%
            add_trace(
                z = ~Ladder_score, color = ~Ladder_score, colors = 'GnBu',
                text = ~Country, locations = ~code, marker = list(line = l)
            ) %>%
            colorbar(title = 'Happiness Score', limits=c(0,10), thickness=10) %>%
            layout(
                title = 'Newest World Happiness Score 2021',
                font=list(size = 20),
                geo = g
            ) %>%
            layout(autosize = T, margin=list( l = 10, r = 10, b = 20, t = 50,  pad = 4))
        
        p
    })  
    output$map2<- renderPlotly({
        df0 <- read.csv('2014_world_gdp_with_codes.csv')
        names(df0) <- c("Country", "gdp_billions", "code")
        happy2021 <- read.csv('DataPanelWHR2021C2.csv')
        colnames(happy2021)[1] <- "Country"
        df3 <- happy2021 %>% left_join(df0, by="Country")
        
        happyall <- read.csv('world-happiness-report.csv')
        colnames(happyall)[1] <- "Country"
        df4 <- happyall %>% left_join(df0, by="Country")
        
        fltdata <- df4 %>%
            filter(year==input$thisyear)
        l <- list(color = toRGB("grey"), width = 0.2)
        
        g <- list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator')
        )
        
        z <- plot_geo(fltdata) %>%
            add_trace(
                z = ~Life.Ladder, color = ~Life.Ladder, colors = 'GnBu',
                text = ~Country, locations = ~code, marker = list(line = l)
            ) %>%
            colorbar(title = 'Happiness Score', limits=c(0,10), thickness=7,fontsize = 1) %>%
            layout(
                title = paste('World Happiness Score', input$thisyear),
                font=list(size = 20),
                geo = g
            ) %>%
            layout(autosize = T, margin=list( l = 20, r = 20, b = 20, t = 50,  pad = 3))
        z
    })  
    output$top1<- renderPlot({
        df0 <- read.csv('2014_world_gdp_with_codes.csv')
        names(df0) <- c("Country", "gdp_billions", "code")
        happy2021 <- read.csv('DataPanelWHR2021C2.csv')
        colnames(happy2021)[1] <- "Country"
        df3 <- happy2021 %>% left_join(df0, by="Country")
        
        
        dimensions <- c('Ladder_score','Logged.GDP.per.capita','Social.support','Healthy.life.expectancy','Freedom.to.make.life.choices','Generosity','Perceptions.of.corruption')
        country_region_dict = happy2021 %>% select(country = Country, region = Regional.indicator) %>% unique()
        
        happy2021_long <- happy2021 %>% 
            select(country = Country, all_of(dimensions)) %>%
            mutate(absence_of_corruption = 1- Perceptions.of.corruption) %>%
            pivot_longer(cols = c(all_of(dimensions),'absence_of_corruption'), names_to = 'dimension', values_to = 'score') %>%
            filter(dimension != "Perceptions.of.corruption")
        
        happy2021_tranformed <- happy2021_long %>%
            group_by(dimension) %>%
            mutate(min_value = min(score),
                   max_value = max(score)) %>%
            mutate(score_pct = (score-min_value)/(max_value-min_value)) %>%
            ungroup()
        
        # get top 10
        happy2021_top10 <- happy2021_tranformed %>%
            filter(dimension == "Ladder_score") %>%
            slice_max(score, n = 10) %>%
            mutate(cat = 'top_10', 
                   country_rank = rank(-score),
                   country_label = paste0(country, ' (', country_rank, ')'))
        
        
        qq<-ggplot(happy2021_top10, aes(x = reorder(country_label, score))) + 
            geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
            geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
            geom_text(aes(y = score), label = round(happy2021_top10$score,2), nudge_y = 0.4, size = 6) + 
            scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
            scale_fill_gradient2(low = 'black', high = '#8bc5e8', mid = 'white', midpoint = 5) + 
            coord_flip() +
            labs(y="Best possible life = 10", x = '',
                 title="10 Happiest Countries in the World",
                 subtitle="Nine of the happinest countries are in Europe",
                 caption="Source: The World Happiness Report 2021") + 
            theme(plot.title = element_text(size=24),
                  plot.subtitle = element_text(size = 20),
                  plot.caption = element_text(size = 15),
                  axis.title.x = element_text(size= 15, color = '#555955'),
                  axis.text.y = element_text(size = 19, color = 'black'),
                  axis.text.x = element_blank(),
                  legend.position = 'None')
        
        qq
    })  
    output$top2<- renderPlot({
        df0 <- read.csv('2014_world_gdp_with_codes.csv')
        names(df0) <- c("Country", "gdp_billions", "code")
        happy2021 <- read.csv('DataPanelWHR2021C2.csv')
        colnames(happy2021)[1] <- "Country"
        df3 <- happy2021 %>% left_join(df0, by="Country")
        
        
        dimensions <- c('Ladder_score','Logged.GDP.per.capita','Social.support','Healthy.life.expectancy','Freedom.to.make.life.choices','Generosity','Perceptions.of.corruption')
        country_region_dict = happy2021 %>% select(country = Country, region = Regional.indicator) %>% unique()
        
        happy2021_long <- happy2021 %>% 
            select(country = Country, all_of(dimensions)) %>%
            mutate(absence_of_corruption = 1- Perceptions.of.corruption) %>%
            pivot_longer(cols = c(all_of(dimensions),'absence_of_corruption'), names_to = 'dimension', values_to = 'score') %>%
            filter(dimension != "Perceptions.of.corruption")
        
        happy2021_tranformed <- happy2021_long %>%
            group_by(dimension) %>%
            mutate(min_value = min(score),
                   max_value = max(score)) %>%
            mutate(score_pct = (score-min_value)/(max_value-min_value)) %>%
            ungroup()
        
        # get bottom 10
        happy2021_bottom10 <- happy2021_tranformed %>%
            filter(dimension == "Ladder_score") %>%
            mutate(country_rank = rank(score),
                   country_label = paste0(country, ' (', country_rank, ')')) %>%
            slice_min(score, n = 10) %>%
            mutate(cat = 'bottom_10')
        
        
        yy<-ggplot(happy2021_bottom10, aes(x = reorder(country_label, score))) + 
            geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
            geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
            geom_text(aes(y = score), label = round(happy2021_bottom10$score,2), nudge_y = 0.4, size = 6) + 
            scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
            scale_fill_gradient2(low = 'black', high = '#7FB185', mid = 'white', midpoint = 5) + 
            coord_flip() +
            labs(y="Best possible life = 10", x = '',
                 title="10 Saddest Countries in the World",
                 subtitle="Countries torn by poverty and war",
                 caption="Source: The World Happiness Report 2021") + 
            #theme_ipsum(grid = '') +
            theme(plot.title = element_text(size=24),
                  plot.subtitle = element_text(size = 20),
                  plot.caption = element_text(size = 15),
                  axis.title.x = element_text(size= 15, color = '#555955'),
                  axis.text.y = element_text(size = 19, color = 'black'),
                  axis.text.x = element_blank(),
                  legend.position = 'None')
        yy
    })  
    
    
    
    output$plot.left<-renderPlot({
        www<-left_join(ww,info[year==input$year1],by='region2')
        
        p1<-ggplot()+
            geom_polygon(data = www,
                         aes_string(x='long',y='lat',group = 'group',fill=input$col1.value),
                         colour = '#34282C',size=0.1
            )+	
            guides(fill=guide_colorbar(barwidth=unit(4,'cm')))+				 
            labs(x='',y='')+
            theme(panel.grid = element_blank(),
                  panel.background = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  legend.position='bottom')+
            col.palette+
            ggtitle(paste(input$col1.value,input$year1,sep=","))
        
        p1
    },height=250)
    
    
    output$plot.right<-renderPlot({
        www<-left_join(ww,info[year==input$year2],by='region2')
        
        p2<-ggplot()+
            geom_polygon(data = www,
                         aes_string(x='long',y='lat',group = 'group',fill=input$col2.value),
                         colour = '#34282C',size=0.1
            )+	
            guides(fill=guide_colorbar(barwidth=unit(4,'cm')))+				 
            labs(x='',y='')+
            theme(panel.grid = element_blank(),
                  panel.background = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  legend.position='bottom')+
            col.palette+
            ggtitle(paste(input$col2.value,input$year2,sep=","))
        p2
        
    },height=250)
    
    
    
    output$table.left<-renderDataTable({
        info[year==input$year1]->tableOnLeft
        tableOnLeft[,Positive_affect:=as.numeric(sprintf('%4.3f',Positive_affect))]
        tableOnLeft[,Negative_affect:=as.numeric(sprintf('%4.3f',Negative_affect))]
        c('region2','year',input$col1.value)->ok.col
        tableOnLeft<-tableOnLeft[,..ok.col]
        names(tableOnLeft)[1]<-'Country'
        datatable(tableOnLeft,rownames=F)
    })
    
    output$table.right<-renderDataTable({
        info[year==input$year2]->tableOnRight
        tableOnRight[,Positive_affect:=as.numeric(sprintf('%4.3f',Positive_affect))]
        tableOnRight[,Negative_affect:=as.numeric(sprintf('%4.3f',Negative_affect))]
        c('region2','year',input$col2.value)->ok.col
        tableOnRight<-tableOnRight[,..ok.col]
        names(tableOnRight)[1]<-'Country'
        datatable(tableOnRight,rownames=F)
    })
    
#Country Analysis page # 
    # Country Analysis page plot the map
    output$plot1 <- renderLeaflet({
        df %>%
            group_by(region) %>%
            slice(1) %>%
            leaflet()%>%
            addTiles()%>%
            addMarkers(lng = ~long, lat = ~lat, popup = ~region)
    })
    
    
    observe({
        event <- input$plot1_marker_click
        if (is.null(event))
            return()
        
        # Country Analysis page plot year and life ladder scatterplot
        output$plot2 <- renderPlot({
            df %>%
                filter(lat == event$lat) %>%
                ggplot(aes(x = year, y = life_ladder)) +
                geom_point() +
                geom_smooth()
        })     
        
        # Country Analysis page plot scatterplot between two selected variables
        output$plot3 <- renderPlotly({
            df %>%
                filter(lat == event$lat) %>%
                plot_ly(x = ~get(input$x), y = ~get(input$y), type = "scatter", mode = "markers") %>%
                layout(margin = 0,
                       yaxis = list(title = input$y),
                       xaxis = list(title = input$x))
        })
        
        # Country Analysis page show data
        output$table <- renderDataTable({
            df %>%
                filter(lat == event$lat) %>%
                datatable(rownames = F, options = list(scrollX = T))
        })
        
    })
#Database Page#
    
    output$mytable<-renderDataTable({
        mytable=filter(data,year==input$year)
        mytable
    })
} 
    


shinyApp(ui = ui, server = server) 
