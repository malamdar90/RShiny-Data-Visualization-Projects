# title: 'Group project'
# date: "7/22/2021"

library(tidyverse)
library(ggthemes)
library(shiny)
library(shinydashboard) 
library(shinyWidgets)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(olsrr)
library(Hmisc)
library(corrplot)
library(GGally)
library(gridExtra)


ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Report 2021"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Overview ", tabName = "page1", icon = icon("database")),
      menuItem("Top 10 & Bottom", tabName = "page2", icon = icon("line-chart")),
      menuItem("Interactive map", tabName = "page5", icon = icon("map-o")),
      menuItem("Historical Average Regional Score", tabName = "page3", icon = icon("line-chart")),
      menuItem("Correlation Heatmap", tabName = "page4", icon = icon("line-chart")),
      menuItem("Data",tabName = "Page_Data",icon=icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1", icon = icon("database"),
              fluidRow(
                box(
                  title = "Project Description", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             "This app,",tags$strong("World Happiness Report,"), 
                             "is the shiny dashboard application designed to explore and illustrates World Happiness Report data among",
                             tags$strong(""), 
                             style = "font-size:16px"
                           ),
                           br(), br(),
                           
                           fluidRow(column(6, tags$li("GDP per Capita"), tags$li("Social Support"), tags$li("Life Expetancy"), tags$li("Freedom")), 
                                    column(6, tags$li("Generosiy"), tags$li("Perception of Corruption"))),
                           br(),
                           fluidRow(tags$mark(tags$i("The data is from", tags$strong("The World Hapiness Report 2021"), "The Report is based on the Gallup World Poll surveys from 2005 to 2021"))
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Motivations", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("Our motivation comes from two parts, it comes from the interest of finding the reasons that can influence people's feeling of happiness. Meanwhile, we also want to study further which countries generally have other scores than others, and why.",
                                     br(),br(),
                                     "Another part is this opportunity to try to put the knowledge we learn into practical use. The techniques and tools we learned from this course enabled us to draw a deep analysis to answer our questions and interests.",
                                     style = "font-size:16px"
                           ),
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Research questions", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("",
                                     tags$li("What are the top happiest and saddest countries in the world?"),
                                     tags$li("What are the factors and their correlations to the ranking score?"),
                                     tags$li("What are the general trend of regional happiness in the past few years?"),
                                     tags$li("What conclusion can we draw?"),
                                     
                                     style = "font-size:16px"
                           ),
                         )
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "Description of Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12,
                         tags$div(
                           tags$span("The Gallup World Poll surveys adopted a well-being measure called",
                                     br(), br(),
                                     tags$strong("Cantril ladder"),
                                     br(), br(),
                                     "It asks respondents to think of a ladder, with the best possible life for them being a 10, and the worst possible life being a 0. They are then asked to rate their own current lives on that 0 to 10 scale. The rankings are from nationally representative samples, for the years 2018-2020.",
                                     br(),br(),"They are based entirely on the survey scores, using the Gallup weights to make the estimates representative.",
                                     "The dataset shows the overall happiness score for 150  countries and includes several other indicators such as life expectancy and GDP per capita. ",
                                     style = "font-size:16px"
                           ),
                         ),
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Methodology", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("In developing the report, we first used shiny to set up the application dashboard. As for the visualization process, we first filtered the data to get the top and bottom countries on happiness scores. Then we grouped different countries into different regions and filtered the five-year historical happiness score for each region.",
                                     br(),br(),
                                     "We built a correlation matrix to visualize the factors that influence the ranking score. Because we cannot verify that the factors involved have a linear relationship with the score, we used the Spearman correlation matrix. Eigenvalues and Breush Pagan chi-square tests were also performed when identifying the determining factors from the dataset.",
                                     br(),br(),
                                     "Finally, we also introduced the interactive map to show countries' ranking on happiness scores using circles with different radius. The interactive map assists us in visualizing better how countries vary in happiness geographically.",
                                     style = "font-size:16px"
                           ),
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Results and Conclusion", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$li("Due to the weighted coefficient of the world happiness report, the top three factors that influence the happiness index are GDP per_capita, life expectancy, and social support."),
                           br(),br(),
                           tags$li("The happiest region in the world is Western Europe (9 out of 10 top happiest countries are from Europe"),
                           
                           style = "font-size:16px"
                         ),
                  )
                )
                
                
              )
      ),
      
      tabItem(tabName = "page2"
              ,
              plotOutput("plot2")
      ),
      tabItem("Average Regional Scores Yearly Comparison",tabName = "page3",
              
              selectInput(inputId = "sel_Region",
                          label = "Choose Region",
                          choices=list("Southern Asia", "Western Europe","Central and Eastern Europe","Eastern Asia",
                                       "Latin America and Caribbean", "Middle East and Northern Africa", "North America",
                                       "Southeastern Asia", "Sub-Saharan Africa")
              ),
              
              plotOutput("plot3")
              
      ),
      
      tabItem(tabName = "page4",
              plotOutput("plot4")
      ),
      tabItem(tabName = "page5",
              
              sliderInput("year", "Year:", min = 2005, max = 2021, value = 1, 
                          step = 1, animate = animationOptions(interval = 1000, loop = FALSE)),
              colorSelectorInput(
                inputId = "mycolor2", label = "Pick a color :",
                choices = c("steelblue", "cornflowerblue",
                            "firebrick", "palegoldenrod",
                            "forestgreen")
              ),
              
              leafletOutput("myMap", width="100%")
      ),
      tabItem(tabName = "Page_Data",
              dataTableOutput("myTable"),
              a("Data Source",href="https://worldhappiness.report/"
                ,target="_blank")
      )
    )
  )
)


server <- function(input, output, session) {
  #####data prep
  data1<-read.csv('data_current.csv')
  data2<-read.csv('data_historical.csv')
  
  data_correl=select(data1,7:12)
  
  #################################################
  
  #################################################
  
  ##data prep for figure 1,2
  Combined_topic <- c('Ladder.score','Logged.GDP.per.capita','Social.support','Healthy.life.expectancy','Freedom.to.make.life.choices','Generosity','Perceptions.of.corruption')
  
  
  Country_region_indicator = data1 %>% select(country = Country.name, region = Regional.indicator) %>% unique()
  
  data_long <- data1 %>% 
    select(country = Country.name, Combined_topic) %>%
    mutate(absence_of_corruption = 1- Perceptions.of.corruption) %>%
    pivot_longer(cols = c(Combined_topic,'absence_of_corruption'), names_to = 'Combined_topic', values_to = 'score') %>%
    filter(Combined_topic != "Perceptions.of.corruption")
  
  data_tranformed <- data_long %>%
    group_by(Combined_topic) %>%
    mutate(min_value = min(score),
           max_value = max(score)) %>%
    mutate(score_pct = (score-min_value)/(max_value-min_value)) %>%
    ungroup()
  
  # get top 10
  data_top10 <- data_tranformed %>%
    filter(Combined_topic == "Ladder.score") %>%
    slice_max(score, n = 10) %>%
    mutate(category = 'top_10', 
           country_rank = rank(-score),
           country_label = paste0(country, ' (', country_rank, ')'))
  
  # get bottom 10
  data_bottom10 <- data_tranformed %>%
    filter(Combined_topic == "Ladder.score") %>%
    mutate(country_rank = rank(score),
           country_label = paste0(country, ' (', country_rank, ')')) %>%
    slice_min(score, n = 10) %>%
    mutate(category = 'bottom_10')
  
  
  
  ##### plot1    
  output$plot1 = renderPlot({
  })
  ##### page 2    
  output$plot2 = renderPlot({
    
    p1=ggplot(data = data_top10, aes(x=reorder(country_label,score), y=score)) + 
      geom_bar(stat="identity",width = 0.5, fill = "#378C2F") +
      geom_text(aes(label=round(score,2)), vjust=1.6, color="black", size=3.5) +
      scale_y_continuous(expand = c(0, 0.5), position = "right", limits = c(0, 8)) +
      coord_flip() +
      labs(y="Best possible life = 10", x = '',
           title="Top 10 Countries by Weighted Score",
           caption="Source: The World Happiness Report 2021")
    
    p2=ggplot(data = data_bottom10, aes(x=reorder(country_label,score), y=score)) + 
      geom_bar(stat="identity",width = 0.5, fill = "#3068f3") +
      geom_text(aes(label=round(score,2)), vjust=1.6, color="black", size=3.5) +
      scale_y_continuous(expand = c(0, 0.5), position = "right", limits = c(0, 8)) +
      coord_flip() +
      labs(y="Best possible life = 10", x = '',
           title="Bottom 10 Countries by Weighted Score",
           caption="Source: The World Happiness Report 2021")
    
    grid.arrange(p1,p2, ncol=,widths = c(2,2))
    
  })
  
  data4 = read.csv("averagehappy.csv")
  data <- reactive({ 
    req(input$sel_Region) 
    df <- data4 %>% filter(Region %in% input$sel_Region) %>% group_by(Year) %>% summarise(HappyScore = identity(HappyScore))
  })
  ##### plot3    
  output$plot3 = renderPlot({
    
    graph = ggplot(data(), aes(x = Year, y = HappyScore)) +
      
      geom_bar(stat="identity",width = 0.5, fill = "#378C2F") +
      geom_text(aes(label=round(HappyScore,2)), vjust=1.6, color="black", size=3.5) +
      scale_y_continuous(expand = c(0, 0.5), position = "right", limits = c(0, 8)) +
      labs(y="", x = '',
           title="Regional Score YoY Change",
           caption="Source: The World Happiness Report 2021")
    graph
    
  })    
  
  ##### plot4 
  #prepare correlation matrix for spearman method   
  cor_s=cor(data_correl,method="spearman") 
  output$plot4=renderPlot({
    cor=cor(data_correl)
    round(cor, 2)
    p5=corrplot(as.matrix(cor_s), title="Correlation Heatmap Spearman",tl.cex = 0.8, tl.col = "black", method = "color", tl.srt=45,
                outline = T,  order="FPC", mar=c(0,0,2,0),#BLTR
                addCoef.col = "black", number.digits = 2, number.cex = 0.5, 
                cl.pos = 'b', 
                cl.cex = 0.5,
                cl.align.text = "c",
                addrect =1, 
                rect.lwd = 1,
                diag=F)
    return(p5)
  })
  
  #### interactive map  data  
  happyscore<-data1%>%select(1,3,21,22,23)
  happyscore=happyscore%>%
    mutate(
      popusText=paste(Country.name,", Rank ",Ranking)
    )
  
  happyscore2=data2%>%select(1,2,3,12,13,14)
  happyscore2=happyscore2%>%
    mutate(
      popusText=paste(Country.name,", Rank ",Rank)
    )
  
  ####interactive map render   
  output$myMap = renderLeaflet({
    if(input$year==2021){
      
      
      map_interactive<-leaflet(happyscore)%>%addTiles()%>%
        
        addProviderTiles(providers$Stamen.Toner, group="Toner")%>%
        
        addProviderTiles(providers$Stamen.Watercolor,group="Watercolor",
                         options=providerTileOptions((opacity=0.35)))%>%
        addProviderTiles(providers$Stamen.TonerHybrid, group="Watercolor")%>%
        addLayersControl(baseGroups = c("Toner","Watercolor"),
                         options = layersControlOptions(collapsed = FALSE))%>%
        addTiles()%>%
        addCircles(lat=~latitude,lng=~longitude,weight=0,radius =~sqrt((200-Ranking)*2)*20000,fillColor = input$mycolor2,fillOpacity = 0.4,popup=~popusText)
    }
    else{
      
      happyscore2=filter(happyscore2,year==input$year)
      
      map_interactive<-leaflet(happyscore2)%>%addTiles()%>%
        
        # addProviderTiles(providers$Stamen.Toner, group="Toner")%>%
        # 
        # addProviderTiles(providers$Stamen.Watercolor,group="Watercolor",
        #                  options=providerTileOptions((opacity=0.35)))%>%
        # addProviderTiles(providers$Stamen.TonerHybrid, group="Watercolor")%>%
        # addLayersControl(baseGroups = c("Toner","Watercolor"),
        #                  options = layersControlOptions(collapsed = FALSE))%>%
        addTiles()%>%
        addCircles(lat=~latitude,lng=~longitude,weight=0,radius =~sqrt((200-Rank)*2)*20000,fillColor = input$mycolor2,fillOpacity = 0.4,popup=~popusText)
      
      
      
      
    }
    map_interactive
    ##fillColor = "#009edb"
    
  })    
  
  #### data source datatable    
  output$myTable=renderDataTable(
    return(datatable(data1, rownames= FALSE))
  )
  
}
shinyApp(ui = ui, server = server)
