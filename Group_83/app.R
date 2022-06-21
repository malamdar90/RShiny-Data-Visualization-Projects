library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library('tidyverse')
library('scales')
library('ggthemes')
library(dplyr)
library(leaflet)
library(googleVis)
library(ggvis)
library(DT)
library(plotly)

zori <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv'))
zhvi <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv'))
citybr <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/cityallbr.csv'))
datec <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/dateforgvis.csv'))
dateb <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/dateformap.csv'))


ui <- dashboardPage(
    skin = "yellow",
    title = "RadaR",
    dashboardHeader(
        title = span(img(src = 'carey_pic.png', height = 35), 'Final Project'),
        titleWidth = 300,
        dropdownMenu(
            type = 'notifications',
            headerText = strong('Feedback & Suggestions'),
            icon = icon('envelope'),
            notificationItem(
                text = 'Email: xhuang720315@gmail.com'
            )
        )
    ),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem(
                'HomePage',
                tabName = 'HP',
                icon = icon('address-card')
            ),
            br(),
            menuItem(
                'Home Value Trend of Top Cities',
                tabName = '3rd',
                startExpanded = FALSE,
                icon = icon('map-marked-alt')
            ),
            br(),
            menuItem(
                'Overall Home Value Trend by States',
                tabName = '2h',
                icon = icon('map-marked-alt')
            ),
            br(),
            menuItem(
                'Different Room Types and Home Value',
                tabName = '4th',
                icon = icon('chart-line')
            ),
            br(),
            menuItem(
                    'Covid-19 and Home Value',
                    tabName = '5th',
                    icon = icon('chart-line')
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'HP',
                    h2("Changes of US Home Value"),
                fluidRow(
                    box(
                        tags$div(h4("The constant changes of home value is a topic that people care about day-to-day, and
                        people are interested in an overall trend. The benefits of knowing the home value are
                        essential, no matter when people try to sell, purchase, or invest in a house. It also 
                        provides a picture of a person's overall financial health. Based on this background, the 
                        web application that we build is intended to assist the audience to visualize and understand 
                        the trend of home values over the years. And we will further discuss different room types separately
                        and also explore the relationship between Covid-19 and the change of home value by showing 
                        variations, curves, comparisons based on geographic factors and year changes. ")),
                        solidHeader = TRUE,
                        title = 'Project Description',
                        width = 12,
                        status = 'danger'
                    )
                ),
                fluidRow(
                    box(
                        title = "Research Questions: ", solidHeader = TRUE, 
                        status = "info", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                   fluidRow(
                                       column(12,
                                              tags$li(tags$strong("What is the home value trend of top cities over the years?")),
                                              
                                              tags$li(tags$strong("How does the overall home value change in different states over years?")), 
                                              
                                              tags$li(tags$strong("How does the home value of different room types change over years?")), 
                                              
                                              tags$li(tags$strong("What is the relationship between Covid-19 confirmed cases and Home Value in the US?"))
                                       )
                                   )
                               )
                        )
                    )),
                fluidRow(
                    box(column(12, 
                               tags$div(
                                   tags$span(
                                       "Below is our data references:"),
                                   br(), br(),
                                   fluidRow(column(6, tags$li(tags$a(href = "https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv","ZORI index from Zillow (Metro)")), 
                                                              tags$li(tags$a(href = "https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/cityallbr.csv","ZHVI index from Zillow (1 to 5+ bedrooms)")), 
                                                              tags$li(tags$a(href = "https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv","ZHVI index from Zillow (Metro)")), 
                                                              tags$li(tags$a(href = "https://github.com/MarkHzzz/JHU_Data_Visualization_Project/blob/main/Input_Data/uscities.csv","City location with Lng and Lat")))
                                            )
                                   )
                               
                    ),
                        solidHeader = TRUE,
                        title = 'References:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        title = "About Us", solidHeader = TRUE, 
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                   fluidRow(
                                       column(4, tags$img(src="carey1.png", height=140, width=300))
                                   )
                               ),
                               br(),
                               tags$li("If you have any suggestion, question, or review for this app, comments are open! 
                       Please send an email to ", tags$a(href = "mailto: xhuang72@jhu.edu", "xhuang72@jhu.edu"), "and refer to this Shiny app.")
                        )
                    ))
            ),
            tabItem(tabName = '4th',
                    h2('Different Room Types and Home Value'),
                    fluidRow(
                        box(h4('How does the home value of different room types change over years?'),
                            solidHeader = TRUE,
                            title = 'Research Question:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "    We collected five groups of data from Zillow.com, 
                            which recorded ZHVI for all homes with 1, 2, 3, 4 and 5+ 
                            bedrooms ($). We chose to analyze the data from 2015 to 2021 
                            and created visualizations to better understand home value trends 
                            (ZHVI) for all homes with 1, 2, 3, 4 and 5+ bedrooms ($) in different 
                            cities in 6 years.",
                            "",
                            "    Zillow Home Value Index (ZHVI): A smoothed, seasonally adjusted measure 
                            of the typical home value and market changes across a given region and housing type. 
                            It reflects the typical value for homes in the 35th to 65th percentile range. "
                            
                        ),
                        solidHeader = TRUE,
                        title = 'Description of Dataset:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "In this section, you can observe how ZHVI 
                            changes over years by selecting cities and room types. 
                            We found that the overall ZHVI trend for homes with 1 bedroom 
                            changed relatively more drastically compared to other room types. 
                            Meanwhile, the overall home value for all types of rooms and cities 
                            is gradually increasing."
                        ),
                        solidHeader = TRUE,
                        title = 'Analysis:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                               selectizeInput('CityForbr',
                                              'City:',
                                              choices = citybr$RegionName,
                                              multiple = F,
                                              options = list(placeholder = 'Select a City', maxItems = 1, create = TRUE)
                               )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                               radioButtons('radioForbr',
                                            'How Many Bedrooms:',
                                            choices = list('One Bedroom' = 'br1',
                                                           'Two Bedroom' = 'br2',
                                                           'Three Bedroom' = 'br3',
                                                           'Four Bedroom' ='br4',
                                                           'Five or more' = 'br5')
                               )
                        )
                    ),
                    br(),
                    fluidRow(column(12,plotlyOutput('plotforbr')))
            ),
            tabItem(tabName = '2h',
                    h2('Overall Home Value Trend by States'),
                    fluidRow(
                        box(h4('How does the overall home value change in different states over years?'),
                            solidHeader = TRUE,
                            title = 'Research Question:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "    We collected one group of data from Zillow.com, which 
                            recorded ZHVI for all homes by states, and another group data of 
                            US States Map. ",
                            "",
                            "    Zillow Home Value Index (ZHVI): A smoothed, seasonally 
                            adjusted measure of the typical home value and market changes 
                            across a given region and housing type. It reflects the typical 
                            value for homes in the 35th to 65th percentile range. "
                            
                        ),
                            solidHeader = TRUE,
                            title = 'Description of Dataset:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "In this section, you can observe how ZHVI changes in 
                            different states over years by selecting dates. If the color 
                            of a specific area is darker, this state gets a higher ZHVI. 
                            If the color is lighter, then this state gets a relatively low 
                            ZHVI compared to other states. In addition, we provide a table 
                            of ZHVI in different states for your reference, and you can also 
                            get the data you want by adding filters. We compare different states' 
                            ZHVI over years, and assume that the overall change of ZHVI is gentle. "
                        ),
                            solidHeader = TRUE,
                            title = 'Analysis:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                               selectizeInput('Dateforgvis',
                                              'Date:',
                                              choices = datec$Date,
                                              multiple = F,
                                              options = list(placeholder = 'Select a Date', maxItems = 1, create = TRUE)
                               )
                        )
                    ),
                    fluidRow(column(12,htmlOutput('myGvisMap1'))),
                    br(),
                    fluidRow(
                        column(6,
                               selectizeInput('dateforgvistable',
                                              'Date:',
                                              choices = datec$Date,
                                              multiple = F,
                                              options = list(placeholder = 'Select a Date', maxItems = 1, create = TRUE)
                               )
                        )
                    ),
                    br(),
                    fluidRow(column(12, DT::dataTableOutput('gvistable')))
                    ),
            tabItem(tabName = '5th',
                    h2('Covid-19 and Home Value'),
                    fluidRow(
                        box(h4('What is the relationship between Covid-19 confirmed cases and Home Value in the US?'),
                            solidHeader = TRUE,
                            title = 'Research Question:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "    We collected data of Covid-19 positive cases from CDC 
                            and data of home value from Zillow",
                            "",
                            "Since the pandemic lasts from 2020 to 2021, we selected 
                            data from 2020/01 to 2021/06 to research on the variation.",
                            "",
                            "    Zillow Home Value Index (ZHVI): A smoothed, seasonally 
                            adjusted measure of the typical home value and market changes 
                            across a given region and housing type. It reflects the typical 
                            value for homes in the 35th to 65th percentile range. "
                            
                        ),
                        solidHeader = TRUE,
                        title = 'Description of Dataset:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "We evaluate Covid-19 positive cases and home value by states. ",
                            "",
                            "According to the line chart for Covid-19 positive cases, the positive 
                            cases are gradually increasing with occasional fluctuation, varied by states, 
                            indicating the pandemic becomes increasingly severe and the growth speeds of 
                            different states under different periods of the pandemic are irregular. ",
                            "",
                            "According to the chart for home value index, the increasing 
                            trends of different states are relatively flat and gentle, 
                            as in previous years. Trends hardly fluctuate.",
                            "",
                            "We can conclude that the overall trends of Covid-19 positive 
                            cases and home value are increasing, but cannot infer the causal 
                            relationship between the two variables. From the previous analysis, we 
                            find that US ZHVI has a steady upward trend every year, and the increasing 
                            trend during the pandemic is similar to trends in previous years. As a result, we 
                            cannot say Covid-19 leads to appreciation or depreciation on US home values."
                        ),
                        solidHeader = TRUE,
                        title = 'Analysis:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                               selectInput('stateforcoivd',
                                           'State:',
                                           choices = state.abb,
                                           multiple = F
                                )
                        )
                    ),
                    br(),
                    fluidRow(column(12,plotlyOutput('covid'))),
                    br(),
                    fluidRow(column(12,plotlyOutput('covidzhvi')))
            ),
            tabItem(tabName = '3rd',
                    h2('Home Value Trend of Top Cities'),
                    fluidRow(
                        box(h4('What is the all-home value trend of top cities over years?'),
                            solidHeader = TRUE,
                            title = 'Research Question:',
                            width = 12,
                            status = 'warning',
                            collapsible = TRUE,
                            collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "    We collect one group data of home value from Zillow, which 
                            include ZHVI of all homes (SFR, Condo/Co-op) from Jan,1996 to May, 
                            2021. We also collect the US cities location data from Simplemaps.com to 
                            identify specific locations of each city.",
                            "",
                            "    Zillow Home Value Index (ZHVI): A smoothed, seasonally adjusted measure 
                            of the typical home value and market changes across a given region and housing type. 
                            It reflects the typical value for homes in the 35th to 65th percentile range. "
                            
                        ),
                        solidHeader = TRUE,
                        title = 'Description of Dataset:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    fluidRow(
                        box(tags$div(
                            "In this part, we can find the home value trend of top cities in the last 25 years.",
                            "",
                            "When we type 5 in the number of cities to show, We will get five blue circles in the 
                            US map, which shows the five cities with the highest home value. The size of the circle 
                            represents the number of home values. We find that the five cities will always come from 
                            six to seven limited cities. As time draws closer to the present, top cities' home value keeps 
                            increasing and growing faster and faster. The growth rates of home values are also 
                            different between cities. From the beginning of relative equality to now, we can see the difference
                            clearly. When we try to increase the number of cities to show, we found that most cities with high 
                            house values are concentrated on the west coast, and the distributions of other cities are similar 
                            in the middle and East Coast of the US at the beginning. Over time, the situation on the west coast 
                            is almost the same, but the other cities with high home values tend to concentrate in the middle of the US."
                        ),
                        solidHeader = TRUE,
                        title = 'Analysis:',
                        width = 12,
                        status = 'warning',
                        collapsible = TRUE,
                        collapsed = TRUE
                        )
                    ),
                    br(),
                fluidRow(
                        column(6,
                               selectizeInput('Dateformap',
                                              'Date:',
                                              choices = dateb$Date,
                                              multiple = F,
                                              options = list(placeholder = 'Select a Date', maxItems = 1, create = TRUE)
                               )
                        )
                ),
                br(),
                fluidRow(
                        column(6,
                               numericInput('num',
                                            'Number of cities to show:',
                                            value = 5,
                                            min=0, max=15, step =1
                                )
                        )
                ),
                fluidRow(column(12,leafletOutput('plot1'))),
                br(),
                fluidRow(
                    column(6,
                           selectizeInput('dateformaptable',
                                          'Date:',
                                          choices = dateb$Date,
                                          multiple = F,
                                          options = list(placeholder = 'Select a Date', maxItems = 1, create = TRUE)
                           )
                    )
                ),
                br(),
                fluidRow(column(12, DT::dataTableOutput('maptable')))
                )
            )
        )
    )



server = function(input, output){
    zori <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv'))
    zhvi <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv'))
    
    zhvis <- read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/State_zhvi_uc_sfrcondo.csv'))
    zhvis <- select(zhvis, 5, 294:311)
    filteredzhvis <- pivot_longer(data = zhvis, cols = 2:19,
                                  names_to = 'Date', values_to = 'ZHVIS')

    covid = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/all-states-history.csv'))
    covid = select(covid, 1,2,20)
    
    statea = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/State_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month%20(2).csv'))
    
    #################jtf##############
    
    price=read_csv(url("https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/price.csv"))
    
    price2=pivot_longer(price, colnames(price)[5:309], names_to="Day", values_to="values")
    us=read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/uscities.csv'))
    us1=us%>%select(1,3,7,8)
    colnames(us1) <- c("RegionName","StateName","lat","lng")
    price1=price2%>%inner_join(us1,by=c("RegionName","StateName"))
    price1 = na.omit(price1)
    price1=price1%>%mutate(popusText=paste(RegionName))
        
    
    
    #############################
    output$gvistable <- DT::renderDataTable(DT::datatable(({
        dateforgvistable = input$dateforgvistable
        dataforgvis <- statea %>% select(RegionName, StateName, dateforgvistable)
        colnames(dataforgvis) <- c('State', 'Abbreviation', 'Home Value Index')
        dataforgvis
        
    })))
    
    output$maptable <- DT::renderDataTable(DT::datatable(({
        dateformaptable = input$dateformaptable
        dataformap <- price %>% select(RegionName, StateName, dateformaptable)
        colnames(dataformap) <- c('State', 'Abbreviation', 'Home Value Index')
        dataformap
        
    })))
    
    output$myGvisMap1 <- renderGvis({
        
        stateaa <- select(statea, 3, 114:311)
        Dateaa = input$Dateforgvis
        statebb = select(stateaa, 1, Dateaa)
        
        colnames(statebb) <- c('RegionName', 'Home Value Index')
        g = gvisGeoChart(statebb, locationvar= 'RegionName', colorvar='Home Value Index',
                         options=list(region="US", displayMode="regions", resolution="provinces", 
                                      width="100%",
                                      colorAxis="{colors:['#43C6AC', '#191654']}",
                                      backgroundColor="gray")
        )
        
        
        g
        
    })
    
        
    output$plotforbr <- renderPlotly({
        if ( input$radioForbr == 'br1'){
            br = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/br1.csv'))
        } else if (input$radioForbr == 'br2'){
            br = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/br2.csv'))
        } else if (input$radioForbr == 'br3'){
            br = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/br3.csv'))
        } else if (input$radioForbr == 'br4'){
            br = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/br4.csv'))
        } else{
            br = read_csv(url('https://raw.githubusercontent.com/MarkHzzz/JHU_Data_Visualization_Project/main/Input_Data/br5.csv'))
        }
        
        city11 = input$CityForbr
        
        br <- select(br, -1)
        
        citytobr= br %>% filter( RegionName == city11)
        
        citytobr<- pivot_longer(data = citytobr, cols = 2:79,
                                names_to = 'Date', values_to = 'BR')
        citytobr$Date = as.Date(citytobr$Date)
        
        f <- ggplot(data = citytobr, mapping = aes(x = Date, y = BR)) +
            geom_point(colour = 'green')+
            theme_grey()
        
        
        ggplotly(f)
        
        f
        
    })
    
    output$covid<- renderPlotly({
        statecovid <- filter(covid, state == input$stateforcoivd)
        c <-ggplot()+
            geom_line(data = statecovid, aes(x = date, y = positive), colour = 'red')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            theme_grey()
        ggplotly(c)
        
        c
        
    })
    
    output$covidzhvi<- renderPlotly({
        statezhvi <- filter(filteredzhvis, StateName == input$stateforcoivd)
        cz<-ggplot()+
            geom_point(data = statezhvi, aes(x = as.Date(Date), y = ZHVIS), colour = 'blue')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            theme_grey() 
        ggplotly(cz)
        
        cz
        
    }) 

    output$plot1 <- renderLeaflet({
        theday=input$Dateformap
        cnum= input$num
        citiesToShow= price1%>%filter(Day==theday)%>%arrange(-values) %>%pull(RegionName) %>% head(cnum)
        l <- leaflet(data=price1%>%
                         filter(Day==theday)%>%
                         filter(RegionName %in% citiesToShow))%>%addTiles()%>%
            addCircles(lat=~lat,
                       lng=~lng,
                       weight=0,
                       radius=~values/2,
                       fillColor="blue",
                       fillOpacity=0.3,
                       popup=~popusText)
        l
        
        
        
    })
    
}

shinyApp(ui = ui, server = server)

