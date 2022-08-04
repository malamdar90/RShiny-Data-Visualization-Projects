library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(countrycode)
library(scales)
library(ggthemes)
library(hrbrthemes)
library(corrplot)
library(maps)
load("data.Rdata")
countries = data%>%filter(Year == 2021)

ui<-dashboardPage(
    dashboardHeader(title = "World Happiness"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About",tabName = "Intro",icon = icon("fas fa-info-circle")),
            menuItem("Ranking", tabName = "page1",icon=icon("smile-beam")),
            menuItem("World Map",tabName = "page2",icon = icon("fas fa-globe-americas")),
            menuItem("Country Comparison 2021", tabName = "page3", icon = icon("fas fa-glass-cheers")),
            menuItem("Analysis and Results", tabName = "page4", icon = icon("fas fa-lightbulb"))
            
            )
        ),
    dashboardBody(
        tabItems(
            tabItem("Intro",
                    fluidRow(
                        box(
                            title = "About the Application", solidHeader = TRUE,
                            status = "success", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       tags$span(
                                           "This app,",tags$strong("World Happiness Report"), 
                                           "is the shiny dashboard application designed to explore the", tags$strong("6"), "factors affecting the level of happiness of people living all over the world, including:", style = "font-size:16px"),
                                       br(), br(),
                                       fluidRow(column(6, tags$li("Economy: based on logarithm of GDP per capita"), tags$li("Society: based on level of social support"), tags$li("Health: based on healthy life extectancy at birth")), 
                                                column(6, tags$li("Freedom: based on freedom to make life choices"), tags$li("Generosity: based on the amount of money donated, participation in volunteering"), tags$li("Justice: based on country CPI, corruption perception index") )),
                                       br(),
                                       fluidRow(tags$mark(tags$i("* Please note that the data is from", tags$strong("World Happiness Report 2021")))
                                       )
                                   )
                            )
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Motivation and Research Questions", solidHeader = TRUE,
                            status = "primary", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       tags$span("Money is very important and we all like money! However, is economy the
                                                 only factor that affects people's happiness? $10,000 of GDP per capita was used to divided the countries in
                                                 this world into the developed and the developing. To explore this question, we
                                                 collected the dataset from world happiness report which include countries all over the world about their happiness score and 
                                                 other six indicators that  might determine the level of happiness of the
                                                 citizens.  We are going to see which countries are the top happiest countries. How are happiness distributed around the world over time and
                                                 how are the six factors correlated with happiness score."),br(),
                                       tags$span("*The rankings of national happiness are based on a",tags$strong("Cantril ladder survey.") )
                                       
                                   )
                            )
                            
                        )
                    ),
                    fluidRow(
                      box(
                        title = "Methodology and Dataset", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                 tags$span("The original two datasets are countries' happiness score and other relevant datas such as GDP per capita from 2005 to 2020 and 2021.
                                 The data processing steps are:"),
                                 tags$li(tags$strong("Filtering: "),"Drop the irrelevant data that wouldn't be used.")),
                                 tags$li(tags$strong("Scaling:"),"Normalize all the relevant data into a score of 0 to 100"),
                                 tags$li(tags$strong("Combining:"),"Combine the data of 2005 to 2020 with the latest data from 2021."),
                               br(),
                                 tags$span("The filtered dataset for this application contains total",tags$strong("2098"), "observations (in ", tags$strong("10"), "columns) ")
                               ),
                        column(12,dataTableOutput("datashow"))
                        )
                      ),
      
                    
                    fluidRow(
                        box(
                            title = "About Us", solidHeader = TRUE, 
                            status = "info", width = 12, collapsible = TRUE,
                            column(12, 
                                   tags$div(
                                       fluidRow(
                                           column(3, tags$img(src="carey.jpg", height=120, width=300)),
                                           column(5, tags$div("Team members are all from BARM program at Carey Business School, Johns Hopkins University", style = "font-size:16px")
                                           )
                                           )
                                   )
                            )
                        ),
                        br(),
                        box(
                          title = "Reference", solidHeader = TRUE,
                          status = "info",width = 12, collapsible = TRUE,
                          column(12,tags$li("1. Dataset from Kaggle: https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021"),
                                 tags$li("2. World Happiness Report: https://worldhappiness.report/"),
                                 tags$li("3. WELLBY Approach: https://blogs.lse.ac.uk/businessreview/2021/03/19/world-happiness-report-living-long-and-living-well/"),
                                 tags$li("4. Happiest Country: https://www.cnn.com/travel/article/worlds-happiest-countries-2021/index.html"),
                                 tags$li("5. Does Corruption Hurt Happiness?: http://www.congressi.unisi.it/happinesshealthconference/wp-content/uploads/sites/67/2018/03/rahman_stavropoulos_burger_ianchovichina.pdf"))
                        ),
                        br(),
                        box(
                          title = "Presentation Video",solidHeader = TRUE,
                          status = "info",width = 12,collapsible = TRUE,collapsed = TRUE,
                          column(12,HTML('<iframe width="1280" height="720" src="https://www.youtube.com/embed/7ikG0RmhARc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                        )
                    )
            ),
            tabItem(tabName = "page1",
                    h2("Country Ranking"),
                    fluidRow(
                        box(
                            title = "Guide", solidHeader = TRUE,
                            status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                            h4("This panel comprises of two bar charts displaying the top happiest and saddest countries based on Happiness Score in the 2005-2021 period. You can select the number of countries to show and the Year.")
                        ),
                        box(
                          title = "Findings", solidHeader = TRUE,
                          status = "info", width = 12, collapsible = TRUE, collapsed = TRUE,
                          column(12, 
                                 tags$li("For the sixth year running, Finland has come out on the top happiest country in the annual list with Denmark, Switzerland, Iceland and Netherlands following in second, third, fourth and fifth position respectively. The United States dropped from 14th to 19th place. Although there have been some changes in the top 10, the rankings were strikingly similar to the previous year for the most part, which is viewed as a positive sign."),
                                 tags$li("Afghanistan was ranked the most unhappy country, followed by Zimbabwe, Rwanda and Botswana. Those at the bottom of the list were mainly underdeveloped countries where issues such as political and armed conflicts are prevalent.")
                        )
                        
                        ),
                        column(3,numericInput("num", "Number of countries to show:",
                                     value = 5, min = 1, max = 20, step = 1)),
                        column(4,sliderInput("Year", "Year:", min = 2005, max = 2021, 
                                    value = 2021, step = 1, 
                                    animate = animationOptions(interval = 3000, loop = TRUE))),br(),
                        column(5,box(title = "Virtual tour to happiest Country: Finland",status = "success",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/qolKmOdQwdg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                        column(6,plotlyOutput("plot1", width  = 800)),
                        column(6,plotlyOutput("plot2", width = 800))),br(),
                    fluidRow(
                        box(
                            title = "Data",solidHeader = TRUE,
                            status = "primary",width = 12,collapsible = TRUE,collapsed = FALSE,
                            radioButtons("type1","Choose one: ",c("Happiest"=1,"Saddest"=2),inline = TRUE),
                            dataTableOutput("d1")
                        )
                        
                    )
 
                        
                    
                    ),
            tabItem(
                tabName = "page2",
                h2("World Map"),
                fluidRow(
                    box(
                        title = "Guide", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                        h4(" This panel is the world map that shows the distribution of one factor. Light Blue means higher score and dark blue means lower score. You can choose to show Happiness or one of the six factors and you can select the year. You can also use the button below 2021 to play the change of distribution automatically."),
                        h5("* The countries in grey color mean data unavailable. The map may take several seconds to load. Please be patient : )")
                    )),
                fluidRow(
                    column(3,
                           selectizeInput("Factor","Choose one:",c("Happiness_Score","Economy","Society","Health","Freedom","Generosity","Justice"))),
                    column(3,sliderInput("year","Year:",min = 2005,max = 2021,value = 2021,step = 1,animate = animationOptions(interval = 2000, loop = TRUE)))
                    
                    ),
            
                fluidRow(
                  column(6,plotOutput("WorldMap",width = 1080,height = 600)),
                  column(4,offset = 2,box(
                      title = "Analysis", collapsible = TRUE,collapsed = TRUE,status = "info",solidHeader = TRUE,width = 12,
                      tags$span("From the world map, we can see the trend of the happiness and all six factors over time and space. From 2005 to 2021, North America and North Europe are the happiest region all over the world.
                         The Oceania continent also performs well. East Asia region keeps improving the happiness score as the color turns from dark blue to lighter blue. Unfortunately, the situation of West Asia and Africa
                         is not very good. Many areas have happiness score under 25.The trends of economy factor and health factor are very similar to the trend of the overall happiness. The trend of society factor and freedom factor are less similar, but
                          there is still some positive relationship between them and happiness. As for the justice factor and generosity factor, the trends of some years are almost opposite. For example, the generosity for east Asia are dark blue most of time, but
                                 the happiness score is still light blue. We can guess that there might be positive correlation between economy, health and happiness. ")
                    ))
                   
                
                )
                    
                    
                
                
            ),
            tabItem(
                tabName = "page3",
                h2("Country Comparison 2021"),
                fluidRow(
                    box(
                        title = "Guide", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                        h4(" This panel is for comparison. You can choose two countries all over the world, or use continent filter to choose two countries from the same continent. The left hexagon shows their stats of six factors and right figure shows their data value. You can also see the historical trend using the button on the top right."),
                        tags$span("* The data are for Year 2021. Comparison of historical data is unavailable due to the incompleteness of historical dataset. Please note:",tags$strong("Country 1 is in red and Country 2 is in blue."))
                    )),

                fluidRow(
                    column(3,
                           selectizeInput('continent12','Continent:',c("All","Asia","Americas","Africa","Europe","Oceania"))
                    ),
                    column(3,
                           selectizeInput('country1','Country 1:',countries$Country)
                    ),
                    column(3,
                           selectizeInput('country2','Country 2:',countries$Country)
                    ),
                    br(),
                    column(3,checkboxInput("PastData","Show Historical Happiness Trend",value = FALSE)),
                    column(4,plotlyOutput("Comparison",width = 600,height = 600)),
                    column(7,offset = 1,plotlyOutput("CompareData",width = 960,height = 600))
                ),br(),
                fluidRow(
                    box(
                    title = textOutput("con1"),solidHeader = TRUE,
                    status = "primary", width = 6,collapsible = TRUE,collapsed = FALSE,
                    dataTableOutput("d2")
                ),
                    box(
                    title = textOutput("con2"),solidHeader = TRUE,
                    status = "primary", width = 6,collapsible = TRUE,collapsed = FALSE,
                    dataTableOutput("d3")
                ) 
                )

            ),
            tabItem(tabName = "page4",
                    h2("Happiness Analysis"),
                    fluidRow(
                        box(title="Guide",solidHeader = TRUE,
                            status="primary", width=12, collapsible=TRUE, collapsed = FALSE,
                            h4("This panel shows how different factors affect happiness score in various continents.
                               You can choose up to two factors in the bars below. The right matrix shows the correlation of all factors of the continent selected. 
                               You can select the significance level to delete the insignificant correlation. Darker blue means stronger positive correlation and darker red means stronger negative correlation.")
                        )
                    ),
                    fluidRow(
                      box(
                        title = "Results", solidHeader = TRUE,
                        status = "info",width = 12,collapsible = TRUE,collapsed = TRUE,
                        tags$span("For all over the world, at 1% significance level, there are three factors that have strong positive correlation with happiness: Economy, Health, Society. Generosity has relatively weak correlation and Justice has negative correlation with happiness.
                                  For Americas and Asia, at 1% significance level, the pattern is almost the same as the global pattern except that the correlation between generosity and happiness become insignificant.
                                   For Africa, the situation of economy and health is relatively not good so happiness score is not very high and the effect of other factors are limited. 
                                  For Oceania, since our dataset only includes Australia and New Zealand, the relationship between happiness and six factors are hard to deceided. 
                                   For Europe, at 1% significance level, the pattern is even clearer than the global pattern. One interesting fact is that the correlation between Justice and Happiness is -0.81. This indicates
                                   that people are more likely to be happy when there is more corruption in Europe. ")
                        
                      )
                    ),
                    fluidRow(
                        column(3,
                               selectizeInput('Continents', 'Continents:',
                                              c("All",'Africa','Americas','Asia','Europe','Oceania'), multiple=F)),
                        column(3,
                               selectizeInput('Factor1', 'Factor1:',
                                              c("Economy","Society","Health","Freedom","Generosity","Justice"), multiple=F)),
                        column(3,
                               selectizeInput('Factor2', 'Factor2:',
                                              c("Economy","Society","Health","Freedom","Generosity","Justice"), multiple=F)),
                        column(3,
                               radioButtons("sig","Choose significance level",c("1%"=0.01,"5%"=0.05,"10%"=0.1),inline = TRUE)),
                        column(7,plotOutput("plot3",width = 960, height = 600)),
                        column(5,plotOutput("plot4",width = 600, height = 600))
                    ),br(),
                    fluidRow(
                        box(
                            title = "Data",solidHeader = TRUE,
                            status = "primary",width = 12,collapsible = TRUE,collapsed = FALSE,
                            dataTableOutput("d4")
                        )
                    )
                    
            )
        )
    ))
server <- function(input,output){
    load("data.Rdata")
    countries = data%>%filter(Year == 2021)
    countries = countries[c("Country","continent")]
    observe({
        x=input$continent12
        if (x!="All"){
            small_set=countries%>%filter(continent == x)
            small_set = small_set$Country
            updateSelectizeInput(session = getDefaultReactiveDomain(),
                                 inputId = "country1",
                                 label = "Country 1:",
                                 choices = small_set)
            updateSelectizeInput(session = getDefaultReactiveDomain(),
                                 inputId = "country2",
                                 label = "Country 2:",
                                 choices = small_set)
            }else{updateSelectizeInput(session = getDefaultReactiveDomain(),inputId = "country1",label = "Country 1:",choices = countries$Country)
                updateSelectizeInput(session = getDefaultReactiveDomain(),inputId = "country2",label = "Country 2:",choices = countries$Country)}
        
    })
    output$plot1 = renderPlotly({
        
        thisYear=input$Year
        cnum=input$num
        filteredData=data %>% filter(Year==thisYear) %>% arrange(desc(Happiness_Score)) %>% head(cnum)
        filteredData=filteredData%>%mutate(country_rank = rank(-Happiness_Score),Country = paste(Country, ' (', country_rank, ')'))
        p1=ggplot(filteredData,aes(x=Happiness_Score,y=reorder(Country,Happiness_Score),fill=continent))+
            geom_bar(stat="identity",width=0.8)+
            labs(x="Happiness Score (Max 100)", 
                 y="Countries",
                 title = "Top Happiest Countries in the World",
                 fill="Continent",
                 caption="Source: The World Happiness Report")+
            scale_y_discrete()+
            scale_x_continuous(breaks = seq(0,100,20),limits = c(0,100))+
            scale_fill_brewer(palette="Pastel1")+
            theme_ipsum(grid='')+
            theme(plot.title=element_text(size=18,face="bold"),
                  axis.text=element_text(size=13),
                  axis.title.y=element_blank(),
                  axis.title.x=element_text(size=18),
                  legend.title=element_text(size=15),
                  legend.background=element_rect(color="black"))
        
        ggplotly(p1,tooltip=c("Happiness_Score","continent"))
      
        
    })
    
    output$plot2= renderPlotly({
        
        thisYear=input$Year
        cnum=input$num
        filteredData2=data%>%filter(Year==thisYear) %>% arrange(Happiness_Score)%>%head(cnum)
        filteredData2=filteredData2%>%mutate(country_rank = rank(Happiness_Score),Country = paste(Country, ' (', country_rank, ')'))
        p2=ggplot(filteredData2, aes(x=Happiness_Score,y=reorder(Country,-Happiness_Score),fill=continent))+
            geom_bar(stat="identity",width=0.8)+
            labs(x="Happiness Score (Max 100)", 
                 y="Countries",
                 title = "Top Saddest Countries in the World",
                 fill="Continent",
                 caption="Source: The World Happiness Report")+
            scale_y_discrete()+
            scale_x_continuous(limits = c(0,100))+
            scale_fill_brewer(palette="Pastel2")+
            theme_ipsum(grid='')+
            theme(plot.title=element_text(size=18,face="bold"),
                  axis.text=element_text(size=13),
                  axis.title.y=element_blank(),
                  axis.title.x=element_text(size=18),
                  legend.title=element_text(size=15),
                  legend.background=element_rect(color="black"))
        
        ggplotly(p2,tooltip=c("Happiness_Score","continent"))})
        
    output$Comparison = renderPlotly({
        angle = c(pi/2,pi/6,-pi/6,-pi/2,-5*pi/6,5*pi/6,pi/2)
        base_r = c(0.25,0.5,0.75,1)
        base = data.frame("Economy"=base_r,"Society"=base_r,"Health"=base_r,"Freedom"=base_r,"Generosity"=base_r,"Justice"=base_r)
        base = base %>% mutate("dummy"= Economy)
        
        p = ggplot()+
            scale_x_continuous(limits = c(-1.5,1.5))+
            scale_y_continuous(limits = c(-1.5,1.5))+
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(),axis.title = element_blank(),axis.text.x = element_blank())
        
        position = data.frame(t(base[1,]))
        position = position %>%mutate(base_x = position[,1]*cos(angle),base_y = position[,1]*sin(angle))
        p=p+geom_path(data = position,aes(x=base_x,y=base_y,text = "Gridline: 25%"),color = "grey",size = 1)
        
        position = data.frame(t(base[2,]))
        position = position %>%mutate(base_x = position[,1]*cos(angle),base_y = position[,1]*sin(angle))
        p=p+geom_path(data = position,aes(x=base_x,y=base_y,text = "Gridline: 50%"),color = "grey",size = 1)
        
        position = data.frame(t(base[3,]))
        position = position %>%mutate(base_x = position[,1]*cos(angle),base_y = position[,1]*sin(angle))
        p=p+geom_path(data = position,aes(x=base_x,y=base_y,text = "Gridline: 75%"),color = "grey",size = 1)
        
        position = data.frame(t(base[4,]))
        position = position %>%mutate(base_x = position[,1]*cos(angle),base_y = position[,1]*sin(angle))
        p=p+geom_path(data = position,aes(x=base_x,y=base_y,text = "Gridline: 100%"),color = "black",size = 2)
        
        p = p+annotate("text", x = 0,y = 1.15, size = 6,color = "black",label = "Economy")+
            annotate("text", x = 1.15,y = 0.5, size = 6,color = "black",label = "Society")+
            annotate("text", x = 1.13,y = -0.5, size = 6,color = "black",label = "Health")+
            annotate("text", x = 0,y = -1.15, size = 6,color = "black",label = "Freedom")+
            annotate("text", x = -1.25,y = -0.5, size = 6,color = "black",label = "Generosity")+
            annotate("text", x = -1.15,y = 0.5, size = 6,color = "black",label = "Justice")
        
        data_2021 = data%>%filter(Year == 2021)
        E_max = max(data_2021$Economy)
        E_min = min(data_2021$Economy)
        S_max = max(data_2021$Society)
        S_min = min(data_2021$Society)
        H_max = max(data_2021$Health)
        H_min = min(data_2021$Health)
        F_max = max(data_2021$Freedom)
        F_min = min(data_2021$Freedom)
        G_max = max(data_2021$Generosity)
        G_min = min(data_2021$Generosity)
        J_max = max(data_2021$Justice)
        J_min = min(data_2021$Justice)
        a=data%>%filter(Country == input$country1)%>%filter(Year == 2021)
        a = a[,4:9]
        a$Economy=(a$Economy-E_min)/(E_max-E_min)*0.9+0.1
        a$Society=(a$Society-S_min)/(S_max-S_min)*0.9+0.1
        a$Health=(a$Health-H_min)/(H_max-H_min)*0.9+0.1
        a$Freedom=(a$Freedom-F_min)/(F_max-F_min)*0.9+0.1
        a$Generosity=(a$Generosity-G_min)/(G_max-G_min)*0.9+0.1
        a$Justice=(a$Justice-J_min)/(J_max-J_min)*0.9+0.1
        a[7]=a[1]
        a=data.frame(t(a))
        a=a%>%mutate(x = a[,1]*cos(angle),y = a[,1]*sin(angle))
        p=p+geom_path(data = a,aes(x=x,y=y,text=input$country1),color = "#E88487",size = 2)
        b=data%>%filter(Country == input$country2)%>%filter(Year == 2021)
        b = b[,4:9]
        b$Economy=(b$Economy-E_min)/(E_max-E_min)*0.9+0.1
        b$Society=(b$Society-S_min)/(S_max-S_min)*0.9+0.1
        b$Health=(b$Health-H_min)/(H_max-H_min)*0.9+0.1
        b$Freedom=(b$Freedom-F_min)/(F_max-F_min)*0.9+0.1
        b$Generosity=(b$Generosity-G_min)/(G_max-G_min)*0.9+0.1
        b$Justice=(b$Justice-J_min)/(J_max-J_min)*0.9+0.1
        b[7]=b[1]
        b=data.frame(t(b))
        b=b%>%mutate(x = b[,1]*cos(angle),y = b[,1]*sin(angle))
        p=p+geom_path(data = b,aes(x=x,y=y,text = input$country2),color = "#4481B4",size = 2)
        ggplotly(p,tooltip = "text")
        
    })
    output$WorldMap = renderPlot({
        c1 = data%>%filter(Year == input$year)
        c2 = c1[,c("Country","Year",input$Factor)]
        colnames(c2) =c("region","Year",input$Factor)
        world = map_data("world")
        d = left_join(world,c2,by="region")
        m=ggplot()+
            geom_map(data = d,map=world,aes_string("long","lat",map_id = "region",fill = input$Factor))+
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),legend.text = element_text(size = 12),legend.title = element_text(size = 12),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(),axis.title = element_blank(),axis.text.x = element_blank())+
            scale_fill_continuous(breaks=seq(0,100,25),limits=c(0,100))+
            scale_fill_gradient(low="#000033",high = "#99CCFF")
        show(m)
    })
    output$CompareData=renderPlotly({
        if(input$PastData==FALSE){
            if(input$country1 == input$country2){check_same = 1}else{check_same = 0 }
            agg1 = data%>%filter(Year == 2021)%>%filter(Country==input$country1)
            agg2 = data%>%filter(Year == 2021)%>%filter(Country==input$country2)
            
            agg1 = agg1[c(-1,-2,-10)]
            test1 = data.frame(t(agg1))
            colnames(test1)="Value"
            test1$score = rownames(test1)
            test1$Country = input$country1
            
            agg2 = agg2[c(-1,-2,-10)]
            test2 = data.frame(t(agg2))
            colnames(test2)="Value"
            test2$score = rownames(test2)
            test2$Country = input$country2
            test = add_row(test2,test1)
            test = test[c(1,8,2,9,3,10,4,11,5,12,6,13,7,14),]
            test = data.frame(test,row.names = NULL)
            if(check_same == 0){con_order = c(input$country2,input$country1)}else{con_order = input$country1}
            test$Country = factor(test$Country,levels = con_order)
            p1 <- ggplot() +
                geom_col(data = test, aes(y =  score,x = Value,fill=Country,text = sprintf("%s",Value)),alpha = 0.7,position = "dodge")+
                scale_fill_manual(values=if(check_same == 0){c('#4481B4','#E88487')}else{"#4481B4"})+
                scale_y_discrete(limits=c("Justice","Generosity","Freedom","Health","Society","Economy","Happiness_Score"))+
                theme(axis.ticks.y = element_blank(),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),axis.title.y = element_blank(),axis.text = element_text(size=14),
                      axis.title.x = element_text(size=16),legend.title = element_text(size=12),legend.text = element_text(size=10))+
                labs(x="Score",fill="Country",size = 14)
            ggplotly(p1,tooltip = "text")
            
        }else{
            his1 = data%>%filter(Country == input$country1)
            his2 = data%>%filter(Country == input$country2)
            his=add_row(his1,his2)
            tt=ggplot(his)+
                geom_point(aes(x=Year,y=Happiness_Score,color = Country),size = 2)+
                geom_smooth(aes(x=Year,y=Happiness_Score,color = Country),span = 1,level = 0.8)+
                theme(axis.text = element_text(size=15),axis.title.y = element_blank(),
                      axis.title.x = element_text(size=15),legend.title = element_blank(),legend.text = element_blank())+
                scale_x_continuous(breaks = seq(2004,2021,4),limits=c(2004,2021))+
                scale_y_continuous(breaks = seq(0,100,10),limits=c(0,100))+
                scale_color_manual(values=c('#E88487','#4481B4'))
                
            
            tt=ggplotly(tt)%>%layout(legend=list(x=100,y=0.5,title = list(text = "<b>  Country")))
            tt$x$data[[1]]$name = input$country1
            tt$x$data[[2]]$name = input$country2
            tt

            
        }
        
    })
    output$plot3 = renderPlot({
        if(input$Continents == "All"){df = data}else{df = data%>%filter(continent==input$Continents)}
        p = ggplot(df)+
            geom_point(mapping=aes_string(x=input$Factor1, y="Happiness_Score",color=input$Factor2))+
            labs(y='Happniess Score',
                 x=input$Factor1,
                 color=input$Factor2)+
            geom_smooth(aes_string(x=input$Factor1, y="Happiness_Score"))+
            scale_fill_gradient(low="000033",high="99CCFF")+
            scale_color_continuous(breaks=seq(0,100,25),limits=c(0,100))+
            theme(axis.title = element_text(size = 15),legend.text = element_text(size = 14),axis.text = element_text(size = 15))+
            scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
            scale_x_continuous(breaks = seq(0,100,20),limits = c(0,100))
        show(p)
    })
    output$plot4 = renderPlot({
        if(input$Continents == "All"){corp = data}else{corp = data%>%filter(continent==input$Continents)}
        corp = corp[c(3:9)]
        corp=na.omit(corp)
        M=cor(corp)
        PM=cor.mtest(corp)
        sign = as.numeric(input$sig)
        corrplot(M,type = "upper",addCoef.col = "black",order = "original",p.mat = PM$p,sig.level = sign,tl.srt=45)
    })
    output$d1 = renderDataTable({
        if(input$type1 == 2){
            data1 = data%>%filter(Year == input$Year)%>%arrange(Happiness_Score)%>%head(input$num)
        }else{
            data1 = data%>%filter(Year == input$Year)%>%arrange(desc(Happiness_Score))%>%head(input$num)
        }
        return(datatable(data1,rownames = FALSE))
    })
    output$con1 = renderText({
        return(input$country1)
    })
    output$con2 = renderText({
        return(input$country2)
    })
    output$d2 = renderDataTable({
        data2 = data%>%filter(Country == input$country1)
        data2 = data2[c(-1,-10)]
        return(datatable(data2,rownames = FALSE))
    })
    output$d3 = renderDataTable({
        data3 = data%>%filter(Country == input$country2)
        data3 = data3[c(-1,-10)]
        return(datatable(data3,rownames = FALSE))
    })
    output$d4 = renderDataTable({
        if(input$Continents == "All"){
            data4 = data
        }else{
            data4 = data%>%filter(continent == input$Continents)
        }
        
        return(datatable(data4,rownames = FALSE))
    })
    output$datashow = renderDataTable({
      return(datatable(data,rownames = FALSE))
    })
        
    
}
shinyApp(ui = ui, server = server)