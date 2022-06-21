
# DV_X3_Project_Group4

###### All Packages we need ######
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
#remotes::install_github("RinteRface/bs4Dash")
library(bs4Dash)
#bs4DashGallery()
library(tm) # for TermDocumentMatrix
library(Rcpp)
library(wordcloud)
library(wordcloud2)
#library("Rtools")
#devtools::install_github("JohnCoene/echarts4r",force = TRUE)

library(leaflet)
library(plotly)
library(ggthemes)
library(scales)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(tm)
library(Rcpp)
library(leaflet.extras)

#remotes::install_github("JohnCoene/echarts4r.assets",force = TRUE)
library(echarts4r)
library(echarts4r.assets)

#install.packages("Hmisc")
library("Hmisc")

#install.packages("shinyEffects")
#library(shinyEffects)
#shinyEffectsGallery()


###### Dataset we need ######
movieindex <- read_csv("movie.csv")
data_0= read_csv("jamesbond.csv")
#View(data_0)
girls <- read_csv("maingirls.csv")

data <- read.csv('location_frequency.csv')
data$lat <- round(data$lat,4)
data$long <- round(data$long,4)
Movie <- c("ALL",sort(unique(data$Movie)))
pal <- colorNumeric(palette = c("blue","green","red"),domain =data$number)

read_csv("jamesbond.csv") -> databox
databox$Movie <- paste(databox$Year,databox$Movie)
subset(databox,select=c(2,12,13,14,15,16,17)) ->boxoffice0
subset(databox,select=c(2,3,4,13,15,17)) ->boxoffice
subset(boxoffice0,select=c(1,3,5,7)) ->adj
colnames(adj)[3] <- " World_Adj"
adj$Budget_Adj = adj$Budget_Adj*(-1)
df.long<-melt(adj)
as.character(df.long$variable) -> df.long$variable

options(stringsAsFactors=FALSE)
df <- as.data.frame(data_0[c("Movie","Bond_Girl_Nat")] )

country_list <- strsplit(with(df,Bond_Girl_Nat),",")
count_n <- sapply(country_list,length)
Nat <- data.frame(Movie = rep(with(df,Movie),count_n),
                  Bond_Girl_Nat = unlist(country_list))

v <- Nat[,2] %>%
  VectorSource %>%
  Corpus %>%
  TermDocumentMatrix %>%
  as.matrix %>%
  rowSums %>%
  sort(decreasing=TRUE)

#girl_Nat <- data.frame(word =names(v),freq=v,stringsAsFactors = FALSE)

girl_Nat <- data.frame(word =capitalize(names(v)),freq=v,stringsAsFactors = FALSE)

Kills <- data.frame(data_0 %>%
                      group_by(Bond) %>%
                      summarise(Total_killed=sum(Kills_Bond),
                                Average_killed=round(mean(Kills_Bond),0)))

###### ui part ######

ui <- dashboardPage(
 dashboardHeader(
     title = bs4Dash::bs4DashBrand(title = HTML("<small>007 James Bond</small>"),
                                   href =  "https://en.wikipedia.org/wiki/James_Bond",
                                   image = "Pierce_Brosnan.jpg"),
     sidebarIcon = shiny::icon("bars")
 ),
    
 dashboardSidebar(
     sidebarMenu(
         menuItem("Home Page", tabName = "home", icon = icon("home")),
         menuItem("James Bond Series Intro", tabName = "brief", icon = icon("film")),
         menuItem("Box Office", tabName = "box", icon = icon("dollar-sign")),
         menuItem("Movie Location", tabName = "map", icon = icon("globe-americas")),
         menuItem("Bond Girls", tabName = 'girl', icon = icon("venus-double")),
         menuItem('Death Toll', tabName = 'kill',icon = icon("skull"))
     )
 ),
    
 dashboardBody(
     tabItems(
       
       ##### home page #####
         tabItem(tabName = "home",
                 fluidRow(
                   div(style = "position: relative;",
                     img(src = "James_Bond_007_logo.png", width = "100%",height="100%"),
                     div(style = "position: absolute;right:0;bottom:0;",
                       p("User Manual :",
                         a("Video", href = "https://www.youtube.com/watch?v=XE7p6JqlfMc", target = "_blank")),
                       p("Data Source :", 
                         a("Kaggle", href = "https://www.kaggle.com/dreb87/jamesbond", target = "_blank")),
                     ) 
                   )
                    
                 ),
                 
                 fluidRow(
                     box(collapsed = TRUE,
                         title = "James Bond Movies", width = 12, solidHeader = TRUE, background = "gray-dark",
                         p(
                             "The James Bond series focuses on a fictional British Secret Service agent created in 1953 by writer Ian Fleming, who featured him in twelve novels and two short-story collections. ",
                             "The character, also known by the code number 007, has also been adapted for television, radio, comic strip, video games and film. ",
                             "The films are the longest continually running film series of all time and have grossed over US$7.04 billion in total, making it one of the highest-grossing media franchises of all time."
                         ),
                         p(
                             "The Bond films are renowned for a number of features, including the musical accompaniment, with the theme songs having received Academy Award nominations on several occasions, and two wins. Other important elements which run through most of the films include Bond's cars, his guns, and the gadgets with which he is supplied by Q Branch. The films are also noted for Bond's relationships with various women, who are sometimes referred to as 'Bond girls'."
                         ),
                         h6(
                             "Source:",
                             a("Wikipedia.", href = "https://en.wikipedia.org/wiki/James_Bond", target = "_blank"),
                         )
                     )
                     
                 )
                 
         ),
         
         ##### brief page #####
         tabItem(tabName = "brief",
          h2("James Bond Movie Series Intro"),
                   tabBox(title = "", width = 12,
                          tabPanel("Ratings from Internet Movie Database", plotOutput("rating1")),
                          tabPanel("Ratings from Rotten Tomatoes", plotOutput("rating2"))
                   ),
                  
                            box(title="Film Profile",width = 12,
                                selectInput("Movie_brief", label = h4("Movie Selection"), 
                                            choices = list("Dr. No (1962)" = 1, 
                                                           "From Russia with Love (1963)" = 2, 
                                                           "Goldfinger (1964)" = 3,
                                                           "Thunderball (1965)" = 4, 
                                                           "You Only Live Twice (1967)" = 5, 
                                                           "On Her Majesty's Secret Service (1969)" = 6,
                                                           "Diamonds Are Forever (1971)" = 7, 
                                                           "Live and Let Die (1973)" = 8, 
                                                           "The Man with the Golden Gun (1974)" = 9,
                                                           "The Spy Who Loved Me (1977)" = 10, 
                                                           "Moonraker (1979)" = 11, 
                                                           "For Your Eyes Only (1981)" = 12,
                                                           "Octopussy (1983)" = 13, 
                                                           "A View to a Kill (1985)" = 14, 
                                                           "The Living Daylights (1987)" = 15,
                                                           "License to Kill (1989)" = 16, 
                                                           "GoldenEye (1995)" = 17, 
                                                           "Tomorrow Never Dies (1997)" = 18,
                                                           "The World Is Not Enough (1999)" = 19, 
                                                           "Die Another Day (2002)" = 20, 
                                                           "Casino Royale (2006)" = 21,
                                                           "Quantum of Solace (2008)" = 22, 
                                                           "Skyfall (2012)" = 23, 
                                                           "Spectre (2015)" = 24), 
                                            selected = 1),
                                fluidRow(
                                column(8,
                                uiOutput("introduction")
                                ),
                                column(4,
                                       uiOutput("picture",width = "100%", height = "auto"))
                                ),
                                br(),
                                uiOutput("video_cut")
                            )

                   
                 
         ),
         
         ##### box office page #####
         tabItem(tabName = "box",
           h2("Box Office"),
           box(title="Interactive Bar Chart for Boxoffice and Budget",solidHeader = TRUE,
               width = 12, 
               p('Guide: The variables end with "_Adj" refers to the value of 
                           US dollar in 2013 in thousands, this provides an adjustment based on time 
                           value of money so you can directly comapre them.',"\n",
                 "You can hover on the barchart to see the exact number."),
               column(width = 12, 
                      fixedRow( 
                        column(6,
                               selectizeInput("value","Category",c("Year","World_Adj","US_Adj","Budget_Adj"))),
                        
                        column(6,
                               selectizeInput("order","Order",c("Ascending","Descending")))
                      ),
               ),
               plotlyOutput("plot")
           ),
                     box(
                       title = "Box Office Data Table", solidHeader = TRUE,width = 12,
                       #status = "warning", 
                       p("Guide: This is an interactive data table for box office 
                         and budget of the James Bond series, you can rearrange the table 
                         by different veraibles."),
                       dataTableOutput("table1")
                     ),
           tabBox( width = 12, title = "Is there any factor affecting box office?",solidHeader = TRUE,
                   tabPanel("Budget", 
                            fluidRow(
                              column(6,radioButtons("boxofficetype", "Boxoffice in 2013 US Dollar in thousands",c('Wolrdwide','U.S.'), selected = 'Wolrdwide',inline = TRUE)),
                              column(6,radioButtons("colortype", "Group",c('None','Bond','Director'), selected = 'None',inline = TRUE))
                            ),
                            plotlyOutput("boxbudget")),
                   tabPanel("Bond Actor", plotOutput("boxbond")),
                   tabPanel("Director",plotOutput("boxdirector"))
           )
                     
                     
                 
         ),
         
         ##### map page #####
         tabItem(tabName = "map",
                 h2("Movie Location"),
                 column(width = 12, fixedRow( 
                   column(6,
                          radioButtons("Type", "Location",c('Depicted','Shooting'), selected = 'Depicted')
                   ),
                   column(6,
                          selectInput("Movie", "Movie",Movie,selected='ALL')
                   ),
                 )),
                 leafletOutput("map", width = "100%")
                
         ),
         
         ##### fun fact page #####
         tabItem(tabName = "girl",
                 h2("Bond Girls"),
                 fluidRow(
                 column(8,
                        fluidRow(
                   box(title="007 Bond Girls",background="gray-dark", solidHeader = TRUE,
                       width=12,
                       div(style = "width:100%;height:100%;position: relative;",
                           p(style = "width: 100%;position: relative;left:0;",
                             "A Bond Girl is a character who portrays a significant 
                                 supporting role or a love interest in a James Bond film, 
                                              novel or video game. "),
                           p(style = "width: 100%;position: relative;left:0;",
                             "Bond girls might be victims rescued by Bond, fellow agents 
                                 or allies, villainesses or members of an enemy organization. 
                                 Other female characters such as Judi Dench's M, 
                                 Rosa Klebb, Irma Bunt and Miss Moneypenny are not 
                                               classified as Bond girls."),
                           #img(style = "width: 40%;position: absolute;bottom:0;right:0;",src ="girl007.jpg"),
                           a("Source",href="https://jamesbond.fandom.com/wiki/Bond_girl",target="_blank"))
                   )),
                
            
                                fluidRow(box(title="Bond Girl Nationality Preference",
                                             numericInput("top", "Top", 5, min = 3, max = 10,width=200),
                                             width=12,height="auto",
                                             wordcloud2Output("wordcloud",height=300))
                                ),
                                fluidRow(box(collapsed = TRUE,
                                             title="All Bond Girl Nationalities",
                                             width=12,height=400,
                                             plotOutput("plot1",
                                                        height="100%",width="100%"))
                                ),
                                
                                fluidRow(
                                  box(width=12,background="gray-dark", solidHeader = TRUE,
                                             img(src ="bondgirl.jpg", width="100%")
                                      )
                                  ),  
                               
                        
                 ),
                        column(4,
                               img(src ="Timeline1.jpg",width="100%",align = "right")
                               
                        )
                 )
                 
                     
                 ),
                        

         tabItem(tabName = "kill", 
                 h2("Death Toll"),
                 fluidPage(
                     tabBox(title = tagList("Who killed the most"), width = 12,solidHeader = TRUE,
                            tabPanel("Total", echarts4rOutput("totalkill")),
                            tabPanel("Average", echarts4rOutput("avekill"))
                            ),
                     fluidRow(
                         column(6,box(width=12,title="Data",
                                      dataTableOutput("killtable",width = "100%", height = "auto"))
                                ),
                         column(6,box(width=12,title="Details",
                                      selectInput("bondactor", label=h6("Actor"),
                                                  choices = list("Daniel Craig" , 
                                                                 "George Lazenby" , 
                                                                 "Pierce Brosnan" ,
                                                                 "Roger Moore" , 
                                                                 "Sean Connery" , 
                                                                 "Timothy Dalton" ),
                                                  selected = "Daniel Craig"),
                                      
                                      dataTableOutput("actor",width = "100%", height = "auto")))
                        ),
                     
                     fluidRow(
                       
                       box(title = "Top 10 James Bond Kills", width=12,background="gray-dark", solidHeader = TRUE,
                           HTML('<iframe width="100%" height="400" 
                                   src="https://www.youtube.com/embed/u53CBSedcIQ"
                                   frameborder="0" allowfullscreen ></iframe>'))
                         
                     )
                 )
             )
     )
 )
)

#### server part ####

server <- function(input, output, session) {
  
  #### biref ####
  output$rating1 <- renderPlot({
    
    rating1<- ggplot(movieindex,aes(x=reorder(MovieandYear,Avg_User_IMDB),y=Avg_User_IMDB))+
      geom_bar(stat="identity",position="identity",width = 0.09,alpha=0.6)+
      xlab("")+
      geom_text(aes(label=Avg_User_IMDB), position=position_dodge(width=0), vjust=0.5,hjust=-0.5,size=3)+
      theme(axis.line = element_line(color = "grey50"),panel.background = element_rect(fill = "white", colour = "white"))+
      ylab("The average user rating from IMDB (Out of 10)")+
      coord_flip(ylim=c(4,9))
    rating1
    
   
  })
  
  
  
  output$rating2 <- renderPlot({
    
    rating2<- ggplot(movieindex,aes(x=reorder(MovieandYear,Avg_User_Rtn_Tom),y=Avg_User_Rtn_Tom))+
      geom_bar(stat="identity",position="identity",width = 0.09,alpha=0.6)+
      xlab("")+
      geom_text(aes(label=Avg_User_Rtn_Tom), position=position_dodge(width=0), vjust=0.5,hjust=-0.5,size=3)+
      theme(axis.line = element_line(color = "grey50"),panel.background = element_rect(fill = "white", colour = "white"))+
      ylab("The average user rating from Rotten Tomatoes (Out of 10)")+
      coord_flip(ylim=c(4,9))
    rating2
    
  })
  
  output$introduction <- renderUI({
    page = input$Movie_brief
    #cast = div()
    #if(
    #  !is.na(movieindex[page,17])
    #){
    #  cast = div(
    #    tags$h4("Cast"),
    #    tags$p(movieindex[page,17]))
    #}
    
    div(
      tags$h4("Synopsis"),
      tags$p(movieindex[page,13]),
      #tags$h4("Cast"),
      #cast,
      tags$h4("Cast"),
      tags$p(movieindex[page,17]),
      tags$h4("Producers"),
      tags$p(movieindex[page,18]),
      tags$h4("Director"),
      tags$p(movieindex[page,4]),
      tags$h4("Music"),
      tags$p(movieindex[page,19]),
      tags$h4("Trivia"),
      tags$li(paste0(movieindex[page,20])),
      tags$li(paste0(movieindex[page,24])),
      tags$li(paste0(movieindex[page,25])),
      tags$li(paste0(movieindex[page,26])),
      tags$li(paste0(movieindex[page,27])),
      p(),
      tags$h4("Ratings"),
      tags$li(paste0("The average user rating from IMDB: ",movieindex[page,9])),
      tags$li(paste0("The average user rating from Rotten Tomatoes: ",movieindex[page,10])),
      tags$h4("")
      
      
    )
  })
  
  output$picture <- renderUI({
    page = input$Movie_brief
    
    div(
      tags$h1(img(src =movieindex[page,14],width="100%")),
      tags$h1(img(src =movieindex[page,21],width="100%")),
      tags$h1(img(src =movieindex[page,22],width="100%")),
      tags$h1(img(src =movieindex[page,23],width="100%")),
      tags$a("Source: www.007.com/",href=movieindex[page,16],target="_blank")
    )
    
    
  })
  
  output$video_cut <- renderUI({
    page = input$Movie_brief
    div(
    tags$iframe(width="100%", height="400",
                src=movieindex[page,28], frameborder="0", 
                allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", 
                allowfullscreen=NA)
    )
  })
  
  
  #### map ####
  
  filteredData <- reactive({
    if(input$Movie=="ALL"){
      tmp <- data[data$type %in% input$Type,]
    }else{
      tmp <- data[data$type %in% input$Type & data$Movie %in% input$Movie,]
    }
    tmp2 <- NULL
    for(i in unique(tmp$Film)){
      re <- subset(tmp,Film==i)
      re$label <- paste0(re$Movie," (",re$Year,")")
      label <- paste(re$label,collapse = ';')
      re <- unique(re[,-c(4:5,8)])
      re$label <- paste0(label,"; Times:",re$number)
      tmp2 <- rbind(tmp2,re)
    }
    tmp2
  })
  
  output$map <- renderLeaflet({
    observe({
      tmp <- filteredData()
      icons <- awesomeIcons(
        markerColor = case_when(
          tmp$type == "Depicted" ~ "blue",
          tmp$type == "Shooting" ~ "red",
          TRUE ~ "white"
        ))
      if(input$Movie=="ALL"){
        leafletProxy("map", data = tmp)
      }else{
        leafletProxy("map", data = tmp) %>%
          addCircleMarkers(
            lng          = ~long,
            lat          = ~lat,
            radius       = ~10,
            stroke       = FALSE,
            fillOpacity  = 1,
            popup=~label,
            fillColor=~pal(number)
          )  %>%
          addAwesomeMarkers(
            lng          = ~long,
            lat          = ~lat,
            icon = icons,
          )
      }
      
    })
    
    leaflet(filteredData())  %>%
      clearShapes()%>% 
      addTiles() %>% 
      setView(113.58621, 22.172394, zoom = 1) %>%
    addLegend("bottomright",pal=pal,values = ~number,bins=5,title = "Popularity")%>%
      addHeatmap(lng = ~long, 
                 lat = ~lat, 
                 radius = 8)%>%
      
      addMiniMap()
  })

  
  
  #### box office ####
  p6<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,value,min),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p6<-ggplotly(p6,tooltip = c( "text"))
  
  p5<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,-value,max),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p5<-ggplotly(p5,tooltip = c( "text"))
  
  p1<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,value,max),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p1<-ggplotly(p1,tooltip = c( "text"))
  
  p3<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,value,median),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p3<-ggplotly(p3,tooltip = c( "text"))
  
  p4<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,-value,median),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p4<-ggplotly(p4,tooltip = c( "text"))
  
  p2<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=reorder(Movie,-value,min),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p2<-ggplotly(p2,tooltip = c( "text"))
  
  p7<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=Movie,y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p7<-ggplotly(p7,tooltip = c( "text"))
  
  p8<- ggplot(df.long[order(df.long$variable,decreasing=T),],
              aes(x=fct_rev(Movie),y=value,fill=variable,
                  text=paste("Year and Movie:",Movie,"\n",variable,":",value)))+
    geom_bar(stat="identity",position="identity")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
    coord_flip()+
    xlab("Movie and Released Year")+
    ylab("Boxoffice and Budget in 2013 US Dollar in thousands")+
    scale_y_continuous(labels = comma)
  p8<-ggplotly(p8,tooltip = c( "text"))
  
  output$table1=renderDataTable(
    datatable(boxoffice),options = list(scrollX = TRUE)
  )
  
  output$plot <- renderPlotly({
    
    if (input$value == "Year" & input$order == "Descending" )  return(p8)
    if (input$value == "World_Adj" & input$order == "Ascending" )  return(p1)
    if (input$value == "World_Adj" & input$order == "Descending" )  return(p2)
    if (input$value == "US_Adj" & input$order == "Ascending" )  return(p3) 
    if (input$value == "US_Adj" & input$order == "Descending" )  return(p4)
    if (input$value == "Budget_Adj" & input$order == "Ascending" )  return(p5) 
    if (input$value == "Budget_Adj" & input$order == "Descending" )  return(p6) 
    if (input$value == "Year" & input$order == "Ascending" )  return(p7)
    
    
  })
  
  output$boxbudget <- renderPlotly({
    
    pbudget<-ggplot(data_0)+aes(x=Budget_Adj,y=World_Adj)+geom_point()+
      theme(axis.line = element_line(color = "grey50"),
            panel.background = element_rect(fill = "white", colour = "white"),
            axis.title=element_text(size=13,face="bold"))+
      ylab("Worldwide Box Office")+xlab("Budget")+
      scale_y_continuous(labels = dollar)+
      scale_x_continuous(labels = dollar)
    
    pbudget2<-ggplot(data_0)+aes(x=Budget_Adj,y=US_Adj)+geom_point()+
      theme(axis.line = element_line(color = "grey50"),
            panel.background = element_rect(fill = "white", colour = "white"),
            axis.title=element_text(size=13,face="bold"))+
      ylab("U.S. Box Office")+xlab("Budget")+
      scale_y_continuous(labels = dollar)+
      scale_x_continuous(labels = dollar)
    
    if (input$boxofficetype == "Worldwide"){
      pbudget<-pbudget
      
    }
    if (input$boxofficetype == "U.S."){
      pbudget<-pbudget2
      
    }
    
    if(input$colortype=="None"){pbudget<-pbudget+geom_point(color="black",size=2)}
    if(input$colortype=="Bond"){pbudget<-pbudget+geom_point(aes(color=Bond),size=2)}
    if(input$colortype=="Director"){pbudget<-pbudget+geom_point(aes(color=Director),size=2)}
    ggplotly(pbudget)
    
  })
  
  output$boxbond <- renderPlot({
    ggplot(data_0)+aes(x=Bond,y=World_Adj)+geom_point(color="black",size=3)+
      theme(axis.line = element_line(color = "grey50"),
            panel.background = element_rect(fill = "white", colour = "white"),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15,face="bold"))+
      ylab("Worldwide Box Office")+
      scale_y_continuous(labels = dollar)
    
  })
  
  
  output$boxdirector <- renderPlot({
    ggplot(data_0)+aes(x=Director,y=World_Adj)+geom_point(color="black",size=3)+
      theme(axis.line = element_line(color = "grey50"),
            panel.background = element_rect(fill = "white", colour = "white"),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15,face="bold"))+
      scale_x_discrete(guide = guide_axis(n.dodge=2))+
      ylab("Worldwide Box Office")+
      scale_y_continuous(labels = dollar)
    
  })
  
  
  
  
#### girls ####
  
    output$plot1 = renderPlot({
        
        minFreq=1
        maxWords=25
        set.seed(5)
        wordcloud(words = girl_Nat$word,
                  freq = girl_Nat$freq,
                  min.freq = minFreq,
                  max.words=maxWords,
                  random.order=FALSE,
                  colors= brewer.pal(11,"RdGy")[c(10,5,4,3,2)])
    })
    
    output$wordcloud = renderWordcloud2({
      
      
        
      girl_Nat_top <- girl_Nat %>%
        arrange(desc(freq)) %>%
        slice(1:input$top)

        wordcloud2(data=girl_Nat_top, size =0.7, minSize = 0, gridSize =  0,
                   fontFamily = NULL, fontWeight = 'normal',shuffle=FALSE,
                   color =  brewer.pal(9,"YlOrRd")[c(9,8,7,6,5)], backgroundColor = "white",
                   minRotation = 0, maxRotation = 0, rotateRatio = 0,
                   shape="diamind", ellipticity = 1, widgetsize = NULL)

        
    })

#### "kill" ####

    output$totalkill = renderEcharts4r({
        
        Kills %>% 
            e_charts(Bond) %>% 
            e_pictorial(Total_killed, symbol = ea_icons("user"), 
                        symbolRepeat = TRUE, z = -1,
                        symbolSize = c(6.4,6.4)) %>% 
            e_theme("westeros") %>%
            e_title("The number of people Bond killed") %>% 
            e_flip_coords() %>%
            # Hide Legend
            e_legend(show = FALSE) %>%
            # Remove Gridlines
            e_x_axis(splitLine=list(show = FALSE)) %>%
            e_y_axis(splitLine=list(show = FALSE)) %>%
            # Format Label
            e_labels(fontSize = 16, fontWeight ='bold', position = "right", offset=c(10, 0))
        
        
        
    })
 
    output$avekill = renderEcharts4r({
        
        
        Kills %>% 
            e_charts(Bond) %>% 
            e_pictorial(Average_killed, symbol = ea_icons("user"), 
                        symbolRepeat = TRUE, z = -1,
                        symbolSize = c(25, 25)) %>% 
            e_theme("westeros") %>%
            e_title("The number of people Bond killed") %>% 
            e_flip_coords() %>%
            # Hide Legend
            e_legend(show = FALSE) %>%
            # Remove Gridlines
            e_x_axis(splitLine=list(show = FALSE)) %>%
            e_y_axis(splitLine=list(show = FALSE)) %>%
            # Format Label
            e_labels(fontSize = 16, fontWeight ='bold', position = "right", offset=c(10, 0))
        
        
    })
    
    output$killtable = renderDataTable({
        
     # DT::datatable(options = list(pageLength = FALSE))
        #return(datatable(Kills, rownames= FALSE))
      DT::datatable(Kills, options = list(paging = FALSE,bFilter=0))
    })
    
    output$actor = renderDataTable({
      
      actors <- data_0[,c("Bond","Movie","Year","Kills_Bond")]
      
      
      
      actorselect <- filter(actors,Bond==input$bondactor)
      
      DT::datatable(actorselect, options = list(paging = FALSE,bFilter=0))
    })

    
    
    
}

shinyApp(ui = ui, server = server)