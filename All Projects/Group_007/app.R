library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rvest)
library(wordcloud)
library(tm)

#install.packages("dplyr")
library(dplyr)

library(rvest)
library(wordcloud)
library(tm)

library(ggplot2)
library(magick)
library(purrr)

library(reshape2)
library(zoo)
library(scales)
#install.packages("extrafont")
library(extrafont)
library(grid)
library(rgdal)
library(maptools)
library(ggplot2)

#install.packages("data.table")
library("data.table")

#install.packages("magick")
#install.packages("ImageMagick")
#install.packages("purrr")

library(ggthemes)
library(jpeg)
library(ggrepel)
library(leaflet)
library(plotly)

# Data Cleasing
data1 <- read.csv('./number-of-out-of-school-children.csv',sep=',',stringsAsFactors=F)
data2 <- read.csv('./mean-years-of-schooling-1.csv',sep=',',stringsAsFactors=F)
data3 <- read.csv('./learning-outcomes-vs-gdp-per-capita.csv',sep=',',stringsAsFactors=F)

education <- na.omit(data1[-c(2,4:7)])
names(education) <- c("region","year","female","male")
pal22=(education$region)
pal22
pal33<-pal22[!duplicated(pal22)]

avgYearEdu <- na.omit(data2[-c(2)])
names(avgYearEdu) <- c("region","year","avgYearEdu")

GDP <- na.omit(data3[-c(2,4)])
names(GDP) <- c("region","year","GDPperCapita","population")

# Left join education data into map & calcualte difference#
country_map <-map_data("world")
country_full <- left_join(country_map, education)
country_full$difference <- country_full$female-country_full$male
drops <- c("subregion")
country_full<- country_full[ , !(names(country_full) %in% drops)]
country_full$year <- as.numeric(country_full$year)

# Stack graph
data0 <- na.omit(data1)
education1 <- na.omit(data0[-c(2,4:7)])
education1$newcol <- rep("Primary",nrow(education1))
names(education1) <- c("region","year","female","male", "type")

datalist1 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education1, year == i)
  amount <- sum(as.numeric(selectedYear$male))
  stack <- as.data.frame(amount)
  stack$type <- "malePrimary"
  stack$year <- i
  datalist1[[i]] <- stack
}
stack1 = do.call(rbind, datalist1)

datalist2 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education1, year == i)
  amount <- sum(as.numeric(selectedYear$female))
  stack <- as.data.frame(amount)
  stack$type <- "femalePrimary"
  stack$year <- i
  datalist2[[i]] <- stack
}
stack2 = do.call(rbind, datalist2)

education2 <- na.omit(data0[-c(2,6:9)])
education2$newcol <- rep("UpperSecondary",nrow(education2))
names(education2) <- c("region","year","female","male", "type")

datalist3 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education2, year == i)
  amount <- sum(as.numeric(selectedYear$male))
  stack <- as.data.frame(amount)
  stack$type <- "maleUpper"
  stack$year <- i
  datalist3[[i]] <- stack
}
stack3 = do.call(rbind, datalist3)

datalist4 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education2, year == i)
  amount <- sum(as.numeric(selectedYear$female))
  stack <- as.data.frame(amount)
  stack$type <- "femaleUpper"
  stack$year <- i
  datalist4[[i]] <- stack
}
stack4 = do.call(rbind, datalist4)

education3 <- na.omit(data0[-c(2,4:5,8:9)])
education3$newcol <- rep("LowerSecondary",nrow(education3))
names(education3) <- c("region","year","female","male", "type")

datalist5 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education3, year == i)
  amount <- sum(as.numeric(selectedYear$male))
  stack <- as.data.frame(amount)
  stack$type <- "maleLower"
  stack$year <- i
  datalist5[[i]] <- stack
}
stack5 = do.call(rbind, datalist5)

datalist6 = c()
for (i in 1998:2015) {
  list = c()
  selectedYear = filter(education3, year == i)
  amount <- sum(as.numeric(selectedYear$female))
  stack <- as.data.frame(amount)
  stack$type <- "femaleLower"
  stack$year <- i
  datalist6[[i]] <- stack
}
stack6 = do.call(rbind, datalist6)

educationTotal <- rbind(stack1,stack2,stack3,stack4,stack5,stack6)

ui <- dashboardPage(  
  skin = "blue",  
  header = dashboardHeader(
    title = "Global Education",controlbarIcon = icon("gears")
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Home", 
        tabName = "page1", 
        icon = icon("home")
      ),
      menuItem(
        text = "Text Visualizer", 
        tabName = "page2",
        icon = icon("text-size", lib = "glyphicon")
      ),
      menuItem(
        text = "Global Rise of Education",
        tabName = "page3",
        icon = icon("fas fa-globe-americas") 
      ),
      menuItem(
        text = "Regional Trends", ### Change your own name ###
        tabName = "page4",
        icon = icon("bar-chart-o") ### Change your own icon ###
      ),
      menuItem(
        text = "World Trend", 
        tabName = "page5",
        icon = icon("fas fa-chart-line")
      ),
      menuItem(
        text = "Dashboard", 
        tabName = "page6",
        icon = icon("dashboard")
      ),
      menuItem(
        text = "Public School", 
        tabName = "page7",
        icon = icon("fas fa-school")
      ),
      hr() 
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "page1",
        
        h3("Description"),
        p("This is the final project of BU.520.650.SU19 Data Visualization class. 
          Goal for the project is to provide a visual summary of global education. 
          We mainly focus on number of out of school children, mean years of schooling and U.S public school locations."),
        br(),
        
        h3("Research Questions"),
        tags$ul(
          tags$li("Is there any gender inequality in primary education? If there is, are there any improvements in recent years?"),
          tags$li("How many children are out of school worldwide?"),
          tags$li("Which country has the highest years of education?"),
          tags$li("How does GDP impact the number of years for education?"),
          tags$li("Where are public schools located in U.S?")
        ),
        br(), 
        h3("Methods"),
        p("The main methods in the final projects are web scrapping, map visualization, plotly, animation, text visualizer and leaflet."),
        br(),
        h4("Web Scrapping"),
        p("We used web scrapping methods to collect all the words from World Bank website and created world cloud to give audience a brief story about global education."),
        br(),
        h4("Map Visualization"),
        p("Using map visualization and year slider, we plot a world map of number of male/female/difference out of primary school. After year passing by, the gender inequality improved. There’s clear reduction for out of school numbers in both genders."),
        br(),
        h4("Plotly"),
        p("We used plotly to create a interactive ploting about number of out of school children in different country, and interactive function make it possible to show more detail information on the ploting with clear format. Within color representing different time, the ploting show if the gender equity on eduaction improving or deteriorating"),
        br(),
        h4("Animation"),
        p("Used various packages to make an animation, showing the trend from 1998 to 2013, with average year of education as horizontal axis, GDP per capita as vertical axis, population as point size, and blue/red indicates more boys/girls out of school."),
        br(),
        h4("Text Visualizer"),
        p("The text visualizers are mainly in two parts: 
          Flag visualizer shows the country flag with highest average education years. 
          The text visualizer on the right enables users to type in country names to see their historical average education time."),
        br(),
        h4("Leaflet"),
        p("This is a map showing all the public school locations for user selected state. When you put mouse on the blue circle, it can show name of the public school."),
        
        h3("Tabs"),
        h4("Text Visualizer"), 
        p("Users are able to input their own key words and see the associated results. There is also a toolbox designed in this page that helps users adjust the visualizations based on their own interest. The features avaible are as follows:"),
        # we create an unorder list using ul and li.
        tags$ul(
          tags$li("Number of words to show"),
          tags$li("Different color pallets"),
          tags$li("Scale methods")
        ),
        br(),
        
        # Add Bio and keep picture
        h3("Team"),
        box(
          width=12,# The total width a window is 12.
          column(
            width = 3, 
            img(src='Carey.JPG', align = "left", height= 100) # img1.JPG should be placed in the www folder
          ),
          column(
            width = 9, 
            p("A creative, highly self-motivated team from Carey Business School in Business Analytics program. Good at R, Python, SAS and Tableau.")
          )
        ),
        br(), 
        
        h3("How to use this app"),
        HTML('<iframe width="100%" height="300" src= "https://www.youtube.com/embed/itqYmec_utU" frameborder="0" allowfullscreen></iframe>')
      ),
      
      tabItem(
        tabName = "page2",
        column(
          width = 4,
          tabBox(
            width = 12,
            title = "Toolbox",
            id = "tabset1", height = "250px",
            tabPanel(
              title = "Key",
              textInput(
                inputId = "input1",
                label = "World Bank Searching Key", 
                value = "Education"
              )
            ),
            
            tabPanel(
              title = "Details",
              
              # Add a slider input
              sliderInput("num1", "Number of words", 5, 50, 15),
              
              # Select analysis methods
              
              radioButtons("method", "Scale Method",
                           choices = list("none", "sqrt", "log","log2")), 
              
              # Select color pallet
              selectInput("pal", "Color Pallet", 
                          choices = list("Set1", "Set2", "Set3", "Dark2", "Accent"))
            )
          )
        ),
        column(
          width = 8,
          box(width = 12,
              plotOutput(
                outputId = "plot1", 
                height=500
              )
          )
        )
      ),
      
      tabItem(
        tabName = "page3",
        column(
          width = 4,
          tabBox(
            width = 12,
            title = "Global Rise of Education",
            id = "tabset1", height = "250px",
            tabPanel(
              title = "Map Visualization",
              sliderInput("yearSlider", "Year:", 
                          min = 1970, max = 2015, value = 2000, step = 1, 
                          animate = animationOptions(interval = 2700, loop = TRUE)),
              selectInput("gender","Gender Selection",c("Male","Female", "Difference"))
            )
          )
        ),
        column(width = 8, box(width = 12,plotOutput("plot2"),height = 420)
        ) ),
      tabItem(
        tabName = "page4",
        column(
          width = 4,
          tabBox(
            width = 12,
            title = "Trend",
            id = "tabset1", height = "250px",
            tabPanel(
              title = "Out of School Children Trend with Gender",
              selectInput("region","Region Selection",as.list(pal33))
            )
          )
        ),
        column(width = 8, box(width = 12,plotlyOutput(outputId = "plot3"),height = 420)
        ) ),
      tabItem(
        tabName = "page5",
        column(
          width = 4,
          tabBox(
            width = 12,
            title = "Settings",
            id = "tabset1", height = "250px",
            tabPanel(
              title = "Details",
              
              # Add a slider input
              sliderInput("num4", "Number of Countries to Show", 1, 152, 50),
              
              # Select analysis methods
              radioButtons("countryLabel", "Show Country Label", choices = list("Show", "None"))
            )
          )
        ),
        column(
          width = 8,
          box(width = 12,
              imageOutput("plot4")
          )
        )
      ),
      
      tabItem(
        tabName = "page6",
        h1("Dashboard"),
        column(6,
               fluidRow(
                 column(width = 7, br(),br(),
                        sliderInput("yearSlider1", "Year:", 
                                    min = 1990, max = 2015, value = 2000, step = 1, 
                                    animate = animationOptions(interval = 1000, loop = TRUE))
                 ),
                 column(width = 10, uiOutput("text1")),br(),br(),
                 column(width = 10, plotOutput("plot9"))
               )
        ),
        column(6,
               fluidRow(
                 column(width = 7, br(),br(),
                        textInput(
                          inputId = "country",
                          label = "Country Key", 
                          value = "United States"
                        )
                 ),
                 column(width = 10, plotOutput("plot11"))
                 
               )
        )
      ),         
      
      tabItem(
        tabName = "page7",
        h1("US Public School Locations"),
        column(6,
               fluidRow(
                 column(width = 7,br(),br(),
                        selectInput("state","State selection",c("AL","AK", "AZ", "AR", "CA","CO","CT","DE","DC",
                                                                "FL","GA","HI", "ID","IL","IN","IA","KS","KY",
                                                                "LA","ME","MD","MA","MI","MN","MS","MO","MT",
                                                                "NE","NV","NV","NH","NJ","NM","NY","NC","ND",
                                                                "OH","OK","OR","PA","RI","SC","SD","TN","TX",
                                                                "UT","VT","VA","WA","WV","WI","WY","GU","PR","VI"))
                 ),
                 column(width = 10, leafletOutput("plot5"))
                 )
          )
        )
      
      )
  ),
  controlbar = dashboardControlbar()
)

server <- function(input, output) {
  
  country_year <- reactive({
    country_full[country_full$year==input$yearSlider,]
  })
  options(scipen=10000)
  
  limitfunc<-function(country){
    xxx<-filter(education,region==country)
    lim<-max(xxx$female,xxx$male)
    for (i in 1:20000)
      if(lim<(i)*10000 & lim>(i-1)*10000)
      {
        l=(i)*10000
      }else { 
      }
    return(l) }
  
  output$plot3 <- renderPlotly({
    datap4<-filter(education,region == input$region)
    pp4 <- plot_ly(datap4,x=~female,y=~male,text=~year,color=~year,size = 6,mode="markers",type="scatter")%>%
      layout(title = paste0("Number of Out of School Children in ",input$region), 
             xaxis = list(range = c(0,limitfunc(input$region))),
             yaxis = list(range = c(0,limitfunc(input$region))))
    
    pp4
  })
  output$plot2 <- renderPlot({
    if(input$gender=="Difference") { 
      p2 <- ggplot() + 
        geom_map(data = country_map, 
                 map = country_map, aes(x = long, y = lat, map_id = region, group = group),
                 fill = "white", color = "black", size = 0.2)+
      geom_map(data=country_year(),map = country_map,
               aes(x=long,y=lat,map_id = region,fill = difference),alpha=.5,size=0.5)+
      theme_map()+
      labs(title = paste("Gender Difference for Primary Education Out of School,",input$yearSlider) , fill = NULL)+
      scale_fill_continuous(name = "Difference in Gender")+
      scale_fill_gradient2(low = 'yellow', mid = 'yellowgreen', high = 'forestgreen')}
    else if(input$gender=="Male") { 
      p2 <- ggplot() + 
        geom_map(data = country_map, 
                 map = country_map, aes(x = long, y = lat, map_id = region, group = group),
                 fill = "white", color = "black", size = 0.1)+
        geom_map(data=country_year(),map = country_map,aes(x=long,y=lat,map_id = region,fill = male),alpha=.5,size=.25)+
        theme_map()+
        labs(title = paste("Male for Primary Education Out of School,",input$yearSlider), fill = NULL)+
        scale_fill_continuous(name = "Male Out of School")+
        scale_fill_gradient2(low = 'yellow', mid = 'yellowgreen', high = 'forestgreen')}
    else if(input$gender=="Female") { 
      p2 <- ggplot() + 
        geom_map(data = country_map, 
                 map = country_map, aes(x = long, y = lat, map_id = region, group = group),
                 fill = "white", color = "black", size = 0.1)+
        geom_map(data=country_year(),map = country_map,aes(x=long,y=lat,map_id = region,fill = female),alpha=.5,size=.25)+
        theme_map()+
        labs(title = paste("Female for Primary Education Out of School,",input$yearSlider), fill = NULL)+
        scale_fill_continuous(name = "Female Out of School")+
        scale_fill_gradient2(low = 'yellow', mid = 'yellowgreen', high = 'forestgreen')}
    p2
  })
  
  worldYearGDP <- function(num4, countryLabel) {
    
    world1 <- merge(education,avgYearEdu, by = c("region","year"))
    world <- merge(world1, GDP, by = c("region","year"))
    
    unique<-as.data.frame(table(unlist(world$region)))
    names(unique) <- c("region","frequency")
    
    unique$frequency <- as.numeric(unique$frequency)
    unique <- unique[order(-unique$frequency),] 
    unique$seq <- seq(1, 152)
    unique <- unique[!(unique$seq > num4),]
    
    world <- merge(world, unique, by = "region")
    
    n = 0
    
    for (i in 1998:2013) {
      n = n + 1
      yearData = filter(world,year==i)
      
      yearData$color[yearData$female > yearData$male] = "red"
      yearData$color[yearData$female < yearData$male] = "blue"
      yearData$color[yearData$female == yearData$male] = "yellow"
      
      if (countryLabel=="Show") {
        p4 <- yearData%>%
          ggplot(aes(x = avgYearEdu, y = GDPperCapita, size = population))+
          geom_label(aes(x = 7.5, y = 35000, label = i), 
                     col = "grey", fill = NA, label.size = NA, size = 40)+
          geom_point(color =yearData$color)+
          xlim(0,15)+
          ylim(0,70000)+
          theme(legend.position = "none")+
          geom_text_repel(data = yearData, mapping = aes(label = region))+
          labs(title = paste0("Avg year edu VS. GDP per capita in ", i, ", top ", num4))
        
      } else { 
        p4 <- yearData%>%
          ggplot(aes(x = avgYearEdu, y = GDPperCapita, size = population))+
          geom_label(aes(x = 7.5, y = 35000, label = i), 
                     col = "grey", fill = NA, label.size = NA, size =40)+
          geom_point(color =yearData$color)+
          xlim(0,15)+
          ylim(0,70000)+
          theme(legend.position = "none")+
          labs(title = paste0("Avg year edu VS. GDP per capita in ", i, ", top ", num4))
        
      }
    
    
    fname = paste0(n,".jpeg")
    jpeg(filename = fname, width = 400, height = 400, bg="white")
    print(p4)
    dev.off()
  }
    paste0(1:n,".jpeg") %>%
      map(image_read) %>%
      image_join() %>%
      image_animate(fps=1) %>%
      image_write(paste0("./www/Year.gif"))
    
    return("Year.gif")
    
  }
  
  
  output$plot4 <- renderImage({
    list(src = "./www/Year.gif")
    
  }, deleteFile = FALSE)
  
  ## Dashbosrd
  names(avgYearEdu) <- c("region","year","avgYearEdu")
  newdata <- avgYearEdu[order(avgYearEdu[,2],-avgYearEdu[,3]),]
  subData<- reactive({newdata[newdata$year==input$yearSlider1,]})
  
  textMessgaes <- c(
    "Champion in year 1990",
    "Champion in year 1991",
    "Champion in year 1992",
    "Champion in year 1993",
    "Champion in year 1994",
    "Champion in year 1995",
    "Champion in year 1996",
    "Champion in year 1997",
    "Champion in year 1998",
    "Champion in year 1999",
    "Champion in year 2000",
    "Champion in year 2001",
    "Champion in year 2002",
    "Champion in year 2003",
    "Champion in year 2004",
    "Champion in year 2005",
    "Champion in year 2006",
    "Champion in year 2007",
    "Champion in year 2008",
    "Champion in year 2009",
    "Champion in year 2010",
    "Champion in year 2011",
    "Champion in year 2012",
    "Champion in year 2013",
    "Champion in year 2014",
    "Champion in year 2015"
  )
  
  subData1<- reactive({newdata[newdata$region==input$country,]})
  
  state <- read.csv('./Public_Schools.csv',sep=',',stringsAsFactors=F)
  state <- data.frame(state)
  
  subData2<- reactive({state[state$STATE==input$state,]})
  
  #World Bank Wordcloud#
  
  WBWebScraper <- function(key,num1, method, pal) {
    myStopWords <- c("may", "now", "also", "many", "use", "used", "typically","given",
                     "like", "will", "can", "often", "see", "one", "pdf", "issn", "journal",
                     tolower(month.name))
    
    v <-  paste0("https://www.worldbank.org/en/search?q=",key) %>% 
      read_html %>% html_nodes(".tab-content") %>% html_text %>% 
      VectorSource %>% Corpus %>% tm_map(content_transformer(tolower)) %>% 
      tm_map(removeNumbers) %>% tm_map(removeWords, stopwords("english")) %>% 
      tm_map(removeWords, myStopWords) %>% tm_map(removePunctuation) %>% 
      tm_map(stripWhitespace) %>% TermDocumentMatrix %>% as.matrix %>% 
      rowSums %>% sort(decreasing=TRUE)
    
    # Select analysis method
    if (method=="none") {
      d <- data.frame(word = names(v),freq=v) 
    } else if (method=="sqrt") { 
      d <- data.frame(word = names(v),freq=round(sqrt(v))) 
    } else if (method=="log") { 
      d <- data.frame(word = names(v),freq=round(log(v))) 
    } else { 
      d <- data.frame(word = names(v),freq=round(log2(v))) 
    }
    
    # Select color pallet
    set.seed(5)
    if (pal=="Set1") {
      wordcloud(words = d$word, freq = d$freq, max.words=num1, 
                random.order=FALSE, rot.per=0.35, min.freq = 1,
                colors=brewer.pal(8, "Set1")) 
    } else if (pal=="Set2") { 
      wordcloud(words = d$word, freq = d$freq, max.words=num1, 
                random.order=FALSE, rot.per=0.35, min.freq = 1,
                colors=brewer.pal(8, "Set2")) 
    } else if (pal=="Set3") { 
      wordcloud(words = d$word, freq = d$freq, max.words=num1, 
                random.order=FALSE, rot.per=0.35, min.freq = 1,
                colors=brewer.pal(8, "Set3"))
    } else if (pal=="Dark2") { 
      wordcloud(words = d$word, freq = d$freq, max.words=num1, 
                random.order=FALSE, rot.per=0.35, min.freq = 1,
                colors=brewer.pal(8, "Dark2"))
    } else { 
      wordcloud(words = d$word, freq = d$freq, max.words=num1, 
                random.order=FALSE, rot.per=0.35, min.freq = 1,
                colors=brewer.pal(8, "Accent")) 
    }
  }
  
  output$plot1 <- renderPlot({
    WBWebScraper(input$input1,input$num1,input$method,input$pal)
  })
  
  output$text1 <- renderUI({
    wellPanel(
      p(strong("Country with Average Longest Education")),
      img(src=paste0(subData()$region[1], ".jpg"), height=100, align ="center"),
      h4(subData()$region[1]),
      br(),
      p(textMessgaes[input$yearSlider1-1989])
    )
  })
  
  output$plot11 <- renderPlot({
    p <- ggplot(data = subData1(), mapping = aes(x = year, y = avgYearEdu))
    p+geom_line(color = "orange", size = 1.5) + 
      geom_point(alpha = 0.3)+
      scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 0.5))+
      scale_x_continuous(limits = c(1970, 2017), breaks = seq(0, 2017, by = 3))+
      labs(title = paste0("Average Education Year Summary for ",subData1()$region[1]), x= "Year", y="Average Education Years")
  })
  
  output$plot9 <- renderPlot({
    ggplot(educationTotal, aes(x=year, y=amount, fill=type)) + geom_area()+ ggtitle("Out-of-school children for each level, 1998 - 2015") 
    })
  
  output$plot5 <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addCircles(subData2()$LONGITUDE,subData2()$LATITUDE,popup  = paste0("NAME: ",subData2()$NAME),label  = paste0("NAME: ", subData2()$NAME))
    m   })
  
}

shinyApp(ui = ui, server = server)
