library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
library(wesanderson)
library(DT)

#CREATING REGION AND COORDINATE COLUMNS
west <- list("AK","AZ","CA","CO","HI","ID","MT","NM","NV","OR","WA","WY","UT")
south <- list("AR","LA","MS","OK","TX")
north <- list("IA","IL","IN","KS","MI","MN","MO","ND","NE","OH","SD","WI")
east <- list("CT","DC","DE","MA","MD","ME","NJ","NY","PA","VA","VT","WV")
southeast <- list("AL","FL","GA","GU","KY","NC","SC","TN")

ui = dashboardPage(skin = "black",
    dashboardHeader(title = "Covid-19 in the US",titleWidth = 200),
    dashboardSidebar(width = 125,
        sidebarMenu(
            menuItem("Context", tabName = "context", icon = icon("book")),
            menuItem("National", tabName = "national", icon = icon("dashboard")),
            menuItem("Regional", tabName = "regional", icon = icon("map-marker")),
            menuItem("State", tabName = "state", icon = icon("road")),
            menuItem("Data", tabName = "data", icon = icon("folder-open"))
            
        )
    ),
    dashboardBody(skin = "black",
        tabItems(
            tabItem(tabName= "context",
                    fluidRow(box(width = 12,h2("About"),
                                 h4("In 2019, the Coronavirus sweapt through the planet, causing a global pandemic and  
                                    infected millions in the Untied States. This dashboard seeks to use data 
                                    visualization to gain insights in just how deadly the virus was for the US population."))),
                    fluidRow(box(width = 12,h2("The Data"),
                                 h4("The data was sourced from The Covid Tracking Project. The data was collect on a national level
                                    and state level from January of 2020 until March of 2021. The data as well as the URL for the data 
                                    is present located on the 'Data' tab.")))
                    
                    
                    
                    ),
            
            tabItem(tabName = "national",
                    fluidRow(box(width = 12,h2('Deaths'))),
                    fluidRow(box(title = "Line Chart", solidHeader = TRUE, 
                                      plotlyOutput("deathC"),width=12)),
                    fluidRow(box(title = "Line Chart", solidHeader = TRUE,
                                      plotlyOutput("deathI"),width=12)),
                    fluidRow(box(width = 12,h2('Hospitlalizations'))),
                    plotOutput("hospCvI"),
                    fluidRow(box(width = 12,h2('Testing'))),
                    plotOutput("testingCvI")
            ),
            
            tabItem(tabName = "state", 
                    selectInput("stateInput", "Select a State:",
                                                   list(`East Coast` = east,
                                                        `North` = north,
                                                        `South` = south,
                                                        `West Coast` = west)),
                    plotlyOutput("stateFilterH"),
                    plotlyOutput("stateFilterD")
            ),
            
            
            tabItem(tabName = "regional",
                    plotlyOutput("scatter"),
                    plotlyOutput("deathr"),
                    plotlyOutput("hospitalr")
            ),
            
            tabItem(tabName = "data",
                    fluidRow(box(title = "SOURCE: https://covidtracking.com/")),
                    fluidRow(box(title = "State Data", width = 12)),
                    dataTableOutput("covid_data"),
                    fluidRow(box(title = "National Data", width = 12)),
                    dataTableOutput("national_data")
                    )
                                                    
        )
    )
    
)

server = function(input, output, session) {
    #READING AND DATA MANIPULATION
    covid_data <- read_csv("all-states-history.csv")
    # covid_data[is.na(covid_data)] = 0
    covid_data<-subset(covid_data, state!="AS")
    
    national_data <- read_csv("national-history.csv") 
    national_data[is.na(national_data)] = 0
    
    Fdata<-reactive({
        return(covid_data %>% filter(state==input$stateInput))
    })    
    
    
    covid_data <- covid_data %>%
        mutate(region = ifelse(state %in% west,"west",
                               ifelse(state %in% south,"south",
                                      ifelse(state %in% north, "north",
                                             ifelse(state %in% east, "east","southeast"))))) %>% 
        
        mutate(lat = ifelse(region == "west",36.973945,
                            ifelse(region == "south",31.9686,
                                   ifelse(region == "north", 41.8780,
                                          ifelse(region == "east", 40.7128 ,32.1656))))) %>%
        
        mutate(long = ifelse(region == "west",-120.2819,
                             ifelse(region == "south",-99.9018,
                                    ifelse(region == "north", -93.0977,
                                           ifelse(region == "east", -74.0060 ,-82.9001))))) 
    
    
    #REDERING PLOTS
    #NATIONAL
    #Hospitalization
    output$hospCvI <- renderPlot(height = 350,
        {pivot <- pivot_longer(national_data,c("hospitalizedIncrease","hospitalizedCurrently","onVentilatorCurrently"),
                              names_to = "Type", values_to = 'Count')
            ggplot(pivot, aes(x=date))+
            geom_area(aes(y =Count,fill = Type)) + 
            scale_y_continuous(name="Count", labels = comma, minor_breaks = c(1, 50000, 1000))+
            ggtitle(label = "Comparison of New, Currnet Hospitalizationa and Ventillator Usage")+
            theme_dark()+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            scale_fill_discrete(label = c("New Hospitalizations","Current Hospitalizations","Current Ventillator"))+
            scale_x_date(limits = as.Date(c('2020-03-10',NA)), name = "Date")       
    })
    
    output$deathC <- renderPlotly(
        {pivot2 <- pivot_longer(national_data,c("death"),
                                names_to = "Type", values_to = 'Count')
        death<-ggplot(pivot2, aes(x=date)) + 
            geom_line(aes(y =Count,color = Type)) + 
            scale_y_continuous(name="Count", labels = comma) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Daily Deaths")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")+
            scale_color_discrete(labels = c("Deaths","New Deaths"))        
        
        ggplotly(death)
    })
    
    output$deathI <- renderPlotly(
        {pivot7 <- pivot_longer(national_data,c("deathIncrease"),
                                names_to = "Type", values_to = 'Count')
        death<-ggplot(pivot7, aes(x=date)) + 
            geom_line(aes(y =Count,color = Type)) + 
            scale_y_continuous(name="Count", labels = comma) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("New Daily Deaths")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")+
            scale_color_discrete(labels = c("Deaths","New Deaths"))        
        
        ggplotly(death)
        })
    
    
    output$testingCvI <- renderPlot({
        pivot3 <- pivot_longer(national_data,c("positiveIncrease","totalTestResultsIncrease"),
                               names_to = "Type", values_to = 'Count')
        ggplot(pivot3, aes(x=date, y =Count,color = Type)) + 
            geom_line() + 
            scale_y_continuous(name="Count", labels = comma, limits=c(0,NA)) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Comparing New Infections with New Tests")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")+
            scale_color_discrete(labels = c("New Viral Tests","New Tests"))        
    })
    
    output$ventCvI <- renderPlot({
        ggplot(national_data, aes(x=date)) + 
            geom_line(aes(y =inlcuCurrently), color = "black",linetype="twodash") + 
            geom_line(aes(y = positiveIncrease), color="red") +
            scale_y_continuous(name="Count", labels = comma)+
            ggtitle("National Testing")+
            ylim(0,NA)
        
    })
    
    output$stateFilterH <- renderPlotly({
        stateh <- ggplot(Fdata(), aes(x=date)) + 
            geom_line(aes(y = death), color="red") +
            geom_line(aes(y = recovered), color="green") +
            geom_line(aes(y = hospitalized), color="black")+ 
            scale_y_continuous(name="Count", limits=c(0, NA), labels = comma)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Comparing Deaths, Recoveries, and Hospitalizations of Cases")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")
        ggplotly(stateh, height = 300)
        
    })
    
    
    output$stateFilterD <- renderPlotly({
        stated <- ggplot(Fdata(), aes(x=date)) + 
            geom_line(aes(y = positiveCasesViral), color="red") +
            geom_line(aes(y = totalTestResults), color="black") +
            scale_y_continuous(name="Count", limits=c(0, NA), labels = comma)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Comparing Positive Tests to Total Tests")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")
        ggplotly(stated)
        
    })
    
    output$scatter <- renderPlotly({
        scatter <- ggplot(covid_data) + 
            geom_point(aes(x = deathIncrease, y = positiveIncrease, color = region,
                           size = totalTestEncountersViralIncrease, alpha = 0.5)) +
            scale_y_continuous(name="New Deaths", labels = comma,limits=c(0, NA))+
            scale_x_continuous(name="New Cases", labels = comma,limits=c(0, 1000))+
            theme(legend.title = element_blank())+
            ggtitle(label = "Distibution of New Deaths and New Cases", subtitle = "Distribution of New Cases and New Deaths Across Regions")+
            theme(plot.title = element_text(lineheight = 0.9))+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank())+
            scale_size_continuous(name="New Cases", labels = comma)
        ggplotly(scatter,height = 350)
        
    })
    
    output$hospitalr <- renderPlotly({
        hospitalr <- ggplot(covid_data, aes(x=date))+ 
            geom_line(aes(y =hospitalized,color = region))+ 
            scale_y_continuous(name="Hospitalizations", labels = comma, limits=c(0,NA)) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Comaparing Hospitalization Across Regions")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")
        ggplotly(hospitalr, height = 350) 
    })
    
    output$deathr <- renderPlotly({
        deathr <- ggplot(covid_data, aes(x=date))+ 
            geom_line(aes(y =death,color = region))+ 
            scale_y_continuous(name="Deaths", labels = comma, limits=c(0,NA)) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), legend.title = element_blank())+
            ggtitle("Comaparing Deaths Across Regions")+
            scale_x_date(limits = as.Date(c(NA,NA)), name = "Date")
        ggplotly(deathr, height = 350) 
    })
    
    
    output$covid_data = renderDataTable({
        datatable(covid_data)
    })
    output$national_data = renderDataTable({
        datatable(national_data)
    })
    
    
    
    
    
}

shinyApp(ui = ui, server = server)
