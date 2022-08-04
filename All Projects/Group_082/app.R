###############################################################################
# [ShinyApp: Global Vaccination Progress]
###############################################################################

library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(ggthemes)

ui <- dashboardPage(
    dashboardHeader(title = "Vaccination Progress"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "myTabForIntroduction", icon = icon("fas fa-book")),
            menuItem("Data", tabName = "myTabForData", icon = icon("table")),
            menuItem("Worldwide Progress", tabName = "myTabForWorld", icon = icon("fas fa-map-marked-alt")),
            menuItem("Country Comparison", tabName = "myTabForComparison", icon = icon("far fa-eye")),
            menuItem("Manufacturer Distribution", tabName = "myTabForManufacturer", icon = icon("bar-chart-o"))
        )
    ),

    dashboardBody(
    tabItems(
        # 1st: Intro
        tabItem("myTabForIntroduction",
                fluidRow(
                    box(
                        title = "About the Application", solidHeader = TRUE,
                        status = "success", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                   tags$span(
                                       "This app,",tags$strong("Global Vaccination Progress,"), 
                                       "is the shiny application designed to examine and compare the current vaccination progress in each country. Specifically, our application can answer the following research questions:"),
                                       br(),
                                       tags$li("1) What is the overall vaccination progress globally?"), 
                                      
                                       tags$li("2) Which country has a more advanced vaccination progress?"),
                                       
                                       tags$li("3) What is the vaccine portfolio of each country?"), style = "font-size:16px"
                                   )
                               
                    )
                )),
                fluidRow(
                    box(
                        title = "About the Dataset", solidHeader = TRUE,
                        status = "primary", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                   tags$span("Vaccination data is collected daily from Our World in Data GitHub repository for covid-19, merged and uploaded. Country level vaccination data is gathered and assembled in one single file. Then, this data file is merged with locations data file to include vaccination sources information.
                                    A second file, with manufacturers information, is also included. COVID-19 daily and summary data files are collected from woldometers.info on 2021-06-30.", style = "font-size:16px"),
                                   br(), br(),
                                   tags$li(tags$strong("Source: "),
                                  br(),
                                  tags$a(href = "https://www.kaggle.com/gpreda/covid-world-vaccination-progress", "COVID-19 World Vaccination Progress Dataset")),
                                   tags$a(href = "https://www.kaggle.com/josephassaker/covid19-global-dataset", "COVID-19 Global Dataset")
                               )
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = "About Us", solidHeader = TRUE, 
                        status = "info", width = 12, collapsible = TRUE,
                        column(12, 
                               tags$div(
                                   fluidRow(
                                       
                                       column(12, tags$div("Team members are all students from the MSM program at Carey Business School, Johns Hopkins University:", style = "font-size:16px")
                                       )
                                   )
                               ),
                               br(),
                               tags$li("If you have any suggestion, question, or review for this app, comments are open! 
                       Please send an email to ", tags$a(href = "mailto: tluu2@jh.edu", "tluu2@jh.edu"), "and refer to this Shiny app.")
                        )
                    )),
                  fluidRow(
                      box(
                        title = "Presentation Video", solidHeader = TRUE, 
                        status = "info", width = 12, collapsible = TRUE,
                        column(12,
                               tags$div(
                                 tags$a(href = "https://youtu.be/zJtzN53X5LE", "Youtube Link")
                               )
                        )
                      )
                    ),
        ),
        # 2nd: Data
        tabItem(tabName = "myTabForData",
                h2("Datasets"),
                fluidRow(
                    box(title = "Daily Vaccinations by Country", solidHeader = TRUE,
                        status = "success", width = 12,
                        
                    )
                ),
                fluidRow(
                    dataTableOutput("myTable1")
                ),
                br(),
                fluidRow(
                    box(
                        title = "Vaccinations Progress by Manufacturers", solidHeader = TRUE,
                        status = "success", width = 12,
                       
                    )
                ),
                fluidRow(
                    dataTableOutput("myTable2")
                ),
                br(),
                fluidRow(
                    box(
                        title = "Worldometer Coronavirus Daily Data", solidHeader = TRUE,
                        status = "success", width = 12,
                      
                    )
                ),
                fluidRow(
                    dataTableOutput("myTable3") 
                ),
                br(),
                fluidRow(
                    box(
                        title = "Worldometer Coronavirus Summary Data", solidHeader = TRUE,
                        status = "success", width = 12,
                      
                    )
                ),
                fluidRow(
                    dataTableOutput("myTable4")
                )
        ),
        # 3rd: Worldwide vaccination progress
        tabItem("myTabForWorld",
                h2("Worldwide Vaccination Progress"),
                fluidRow(
                    box(
                        title = "How to Use", solidHeader = TRUE,
                        status = "warning", width = 12, collapsible = TRUE, collapsed = FALSE,
                        h4("* Please be patient because it can take up to 10 seconds to load the map"),
                        h5("This world map shows the fully vaccinated rate by country across the world. The color becomes darker as the fully vaccinated rate increases. You can select a specific time with the slider and below and click on the play button see see how the visualization changes over time.")
                    )
                ),
                fluidRow(
                    box( 
                        sliderInput("slider", "Date:", min = as.Date("2021-02-22"), max = as.Date("2021-07-15"), 
                                    value = as.Date("2021-07-15"), timeFormat = "%b %Y",
                                    animate = animationOptions(interval = 1000, loop = FALSE)
                        
                    )
                    
                )),
                br(),
                fluidRow(
                    column(8, plotOutput("worldmap")
                        )
                    ),
                br(),
                fluidRow(
                    box(title = "Countries Ranked by Total Vaccinations", solidHeader = TRUE,
                        status="warning", width=6, collapsible = FALSE, collapsed = FALSE,
                        h5("China, India and the US are among the top 3 countries in terms of total vaccinations. However, this statistic is expected considering they are among the countries with the highest population."),
                        plotOutput("CountriesRankedbyTotalVaccinations")),
                    box(title = "Countries Ranked by Fully Vaccinated Rate", solidHeader = TRUE,
                        status="warning", width=6, collapsible = FALSE, collapsed = FALSE,
                        h5("Gibraltar, Malta and Iceland are among the top 3 countries for vaccination rate. China, India, and the US are no longer included in the top 20."),
                        plotOutput("CountriesRankedbyVacRate"))
                ) 
        ),
        # 4th: Country comparison
        tabItem("myTabForComparison",
                h2("Vaccination Progress Comparison"),
                fluidRow(
                    box(
                        title = "How to Use", solidHeader = TRUE,
                        status="warning", width=12, collapsible = TRUE, collapsed = FALSE,
                        h5("The Vaccination Progress Comparison tool provides a way to compare vaccination progress of two countries in the world. You can choose two countries, then the graphs will show their results. You can also sort the results in the table by state.",
                           tags$strong("Country with no data will return empty graph."))
                    )
                ),
                fluidRow(
                    column(6, 
                           selectizeInput('singleSelectForCountry1', 'Country 1:', 
                                          c("Choose one"= '', 'Afghanistan', 'Albania', 'Algeria', 'Andorra', 'Angola', 'Anguilla', 'Antigua And Barbuda', 'Argentina', 'Armenia', 'Aruba', 'Australia', 'Austria', 'Azerbaijan', 'Bahamas', 'Bahrain', 'Bangladesh', 'Barbados', 'Belarus', 'Belgium', 'Belize', 'Benin', 'Bermuda', 'Bhutan', 'Bolivia', 'Bosnia And Herzegovina', 'Botswana', 'Brazil', 'British Virgin Islands', 'Brunei Darussalam', 'Bulgaria', 'Burkina Faso', 'Burundi', 'Cabo Verde', 'Cambodia', 'Cameroon', 'Canada', 'Caribbean Netherlands', 'Cayman Islands', 'Central African Republic', 'Chad', 'Channel Islands', 'Chile', 'China Hong Kong Sar', 'China Macao Sar', 'China', 'Colombia', 'Comoros', 'Congo', 'Costa Rica', 'Cote D Ivoire', 'Croatia', 'Cuba', 'Curacao', 'Cyprus', 'Czech Republic', 'Democratic Republic Of The Congo', 'Denmark', 'Djibouti', 'Dominica', 'Dominican Republic', 'Ecuador', 'Egypt', 'El Salvador', 'Equatorial Guinea', 'Eritrea', 'Estonia', 'Ethiopia', 'Faeroe Islands', 'Falkland Islands Malvinas', 'Fiji', 'Finland', 'France', 'French Guiana', 'French Polynesia', 'Gabon', 'Gambia', 'Georgia', 'Germany', 'Ghana', 'Gibraltar', 'Greece', 'Greenland', 'Grenada', 'Guadeloupe', 'Guatemala', 'Guinea Bissau', 'Guinea', 'Guyana', 'Haiti', 'Holy See', 'Honduras', 'Hungary', 'Iceland', 'India', 'Indonesia', 'Iran', 'Iraq', 'Ireland', 'Isle Of Man', 'Israel', 'Italy', 'Jamaica', 'Japan', 'Jordan', 'Kazakhstan', 'Kenya', 'Kuwait', 'Kyrgyzstan', 'Laos', 'Latvia', 'Lebanon', 'Lesotho', 'Liberia', 'Libya', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia', 'Madagascar', 'Malawi', 'Malaysia', 'Maldives', 'Mali', 'Malta', 'Marshall Islands', 'Martinique', 'Mauritania', 'Mauritius', 'Mayotte', 'Mexico', 'Micronesia', 'Moldova', 'Monaco', 'Mongolia', 'Montenegro', 'Montserrat', 'Morocco', 'Mozambique', 'Myanmar', 'Namibia', 'Nepal', 'Netherlands', 'New Caledonia', 'New Zealand', 'Nicaragua', 'Niger', 'Nigeria', 'Norway', 'Oman', 'Pakistan', 'Panama', 'Papua New Guinea', 'Paraguay', 'Peru', 'Philippines', 'Poland', 'Portugal', 'Qatar', 'Reunion', 'Romania', 'Russia', 'Rwanda', 'Saint Barthelemy', 'Saint Kitts And Nevis', 'Saint Lucia', 'Saint Martin', 'Saint Pierre And Miquelon', 'Saint Vincent And The Grenadines', 'Samoa', 'San Marino', 'Sao Tome And Principe', 'Saudi Arabia', 'Senegal', 'Serbia', 'Seychelles', 'Sierra Leone', 'Singapore', 'Sint Maarten', 'Slovakia', 'Slovenia', 'Solomon Islands', 'Somalia', 'South Africa', 'South Korea', 'South Sudan', 'Spain', 'Sri Lanka', 'State Of Palestine', 'Sudan', 'Suriname', 'Swaziland', 'Sweden', 'Switzerland', 'Syria', 'Taiwan', 'Tajikistan', 'Tanzania', 'Thailand', 'Timor Leste', 'Togo', 'Trinidad And Tobago', 'Tunisia', 'Turkey', 'Turks And Caicos Islands', 'Uganda', 'UK', 'Ukraine', 'United Arab Emirates', 'Uruguay', 'USA', 'Uzbekistan', 'Vanuatu', 'Venezuela', 'Vietnam', 'Wallis And Futuna Islands', 'Western Sahara', 'Yemen', 'Zambia', 'Zimbabwe'
                                          )
                           )),
                    column(6,
                           selectizeInput('singleSelectForCountry2', 'Country 2:', 
                                          c("Choose one"= '', 'Afghanistan', 'Albania', 'Algeria', 'Andorra', 'Angola', 'Anguilla', 'Antigua And Barbuda', 'Argentina', 'Armenia', 'Aruba', 'Australia', 'Austria', 'Azerbaijan', 'Bahamas', 'Bahrain', 'Bangladesh', 'Barbados', 'Belarus', 'Belgium', 'Belize', 'Benin', 'Bermuda', 'Bhutan', 'Bolivia', 'Bosnia And Herzegovina', 'Botswana', 'Brazil', 'British Virgin Islands', 'Brunei Darussalam', 'Bulgaria', 'Burkina Faso', 'Burundi', 'Cabo Verde', 'Cambodia', 'Cameroon', 'Canada', 'Caribbean Netherlands', 'Cayman Islands', 'Central African Republic', 'Chad', 'Channel Islands', 'Chile', 'China Hong Kong Sar', 'China Macao Sar', 'China', 'Colombia', 'Comoros', 'Congo', 'Costa Rica', 'Cote D Ivoire', 'Croatia', 'Cuba', 'Curacao', 'Cyprus', 'Czech Republic', 'Democratic Republic Of The Congo', 'Denmark', 'Djibouti', 'Dominica', 'Dominican Republic', 'Ecuador', 'Egypt', 'El Salvador', 'Equatorial Guinea', 'Eritrea', 'Estonia', 'Ethiopia', 'Faeroe Islands', 'Falkland Islands Malvinas', 'Fiji', 'Finland', 'France', 'French Guiana', 'French Polynesia', 'Gabon', 'Gambia', 'Georgia', 'Germany', 'Ghana', 'Gibraltar', 'Greece', 'Greenland', 'Grenada', 'Guadeloupe', 'Guatemala', 'Guinea Bissau', 'Guinea', 'Guyana', 'Haiti', 'Holy See', 'Honduras', 'Hungary', 'Iceland', 'India', 'Indonesia', 'Iran', 'Iraq', 'Ireland', 'Isle Of Man', 'Israel', 'Italy', 'Jamaica', 'Japan', 'Jordan', 'Kazakhstan', 'Kenya', 'Kuwait', 'Kyrgyzstan', 'Laos', 'Latvia', 'Lebanon', 'Lesotho', 'Liberia', 'Libya', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia', 'Madagascar', 'Malawi', 'Malaysia', 'Maldives', 'Mali', 'Malta', 'Marshall Islands', 'Martinique', 'Mauritania', 'Mauritius', 'Mayotte', 'Mexico', 'Micronesia', 'Moldova', 'Monaco', 'Mongolia', 'Montenegro', 'Montserrat', 'Morocco', 'Mozambique', 'Myanmar', 'Namibia', 'Nepal', 'Netherlands', 'New Caledonia', 'New Zealand', 'Nicaragua', 'Niger', 'Nigeria', 'Norway', 'Oman', 'Pakistan', 'Panama', 'Papua New Guinea', 'Paraguay', 'Peru', 'Philippines', 'Poland', 'Portugal', 'Qatar', 'Reunion', 'Romania', 'Russia', 'Rwanda', 'Saint Barthelemy', 'Saint Kitts And Nevis', 'Saint Lucia', 'Saint Martin', 'Saint Pierre And Miquelon', 'Saint Vincent And The Grenadines', 'Samoa', 'San Marino', 'Sao Tome And Principe', 'Saudi Arabia', 'Senegal', 'Serbia', 'Seychelles', 'Sierra Leone', 'Singapore', 'Sint Maarten', 'Slovakia', 'Slovenia', 'Solomon Islands', 'Somalia', 'South Africa', 'South Korea', 'South Sudan', 'Spain', 'Sri Lanka', 'State Of Palestine', 'Sudan', 'Suriname', 'Swaziland', 'Sweden', 'Switzerland', 'Syria', 'Taiwan', 'Tajikistan', 'Tanzania', 'Thailand', 'Timor Leste', 'Togo', 'Trinidad And Tobago', 'Tunisia', 'Turkey', 'Turks And Caicos Islands', 'Uganda', 'UK', 'Ukraine', 'United Arab Emirates', 'Uruguay', 'USA', 'Uzbekistan', 'Vanuatu', 'Venezuela', 'Vietnam', 'Wallis And Futuna Islands', 'Western Sahara', 'Yemen', 'Zambia', 'Zimbabwe'
                                          )
                    )
                )),
                fluidRow(
                    box(
                        title = "Daily Vaccinations vs. Daily New Cases (country 1)", solidHeader = TRUE,
                        status="warning", width=6,
                        plotOutput("VacvsNewCase1")
                    ),
                    box(
                        title = "Daily Vaccinations vs. Daily New Cases (country 2)", solidHeader = TRUE,
                        status="warning", width=6,
                        plotOutput("VacvsNewCase2") 
                    )
                ),
                fluidRow(
                    box(
                        title = "Vaccination Progress by Manufacturer (country 1)", solidHeader = TRUE,
                        status="warning", width=6,
                        plotOutput("VacProgressbyManubyCountry1")
                    ),
                    box(
                        title = "Vaccination Progress by Manufacturer (country 2)", solidHeader = TRUE,
                        status="warning", width=6,
                        plotOutput("VacProgressbyManubyCountry2") 
                    )
                )
        ),
        # 5th: Manufacturer Distribution
        tabItem("myTabForManufacturer",
                h2("Vaccine Distribution by Manufacturers"),
                fluidRow(
                    box(title = "Vaccine Portfolio by Country", solidHeader = TRUE,
                        status="warning", width=6,
                        h5("This graph shows the distribution of vaccine types by total vaccinations in each country.
                           We can see that each country often has a portfolio of 3-4 vaccine types. Pfizer/BioNTech is the most frequently used in most countries.
                           Uruguay and Chile are the 2 only countries where Sinovac is the most common."),
                        plotOutput("VaccinePortfoliobyCountry")),
                    box(title = "Vaccine Popularity", solidHeader = TRUE,
                        status="warning", width=6,
                        h5("This graph shows the number of countries using each type of vaccine out of 33 countries with the highest total vaccinations. 
                           We can see that Pfizer/BioNTEch, Moderna, Johnson&Johnson, and Oxford/AstraZeneca are far more popular than the rest."),
                        plotOutput("NumberOfCountriesUsingEachVaccine"))
                        )
                    )
                )
            )
        )
    


server <- function(input, output) { 
    
    world_summary = read.csv("worldometer_coronavirus_summary_data.csv")
    world_daily = read.csv("worldometer_coronavirus_daily_data.csv")
    vac_daily = read.csv("country_vaccinations.csv")
    vac_manu = read.csv("country_vaccinations_by_manufacturer.csv")
    colors <- c("Daily vaccinations" = "navyblue", "Daily new cases" = "red4")

    ##########################################################
    # Data manipulation (for rankings table)
    ###########################################################

    vac_total <- vac_daily %>% group_by(country) %>% summarise(total_vaccinations = last(total_vaccinations))
    vac_total_top_20 <- vac_total %>% arrange(desc(total_vaccinations)) %>% slice(1:20) 
    
    fully_vac_total <- vac_daily %>% group_by(country) %>% summarise(people_fully_vaccinated = last(people_fully_vaccinated))
    
    fully_vac_rate <- vac_daily %>% group_by(country) %>%
        summarise(fully_vaccination_rate = last(people_fully_vaccinated_per_hundred))
    fully_vac_rate_top_20 <- fully_vac_rate %>% arrange(desc(fully_vaccination_rate)) %>% slice(1:20)

    
    ##########################################################
    # Data manipulation (for vaccine manufacturer)
    ###########################################################
    vac_manu$date = as.Date(vac_manu$date, format = "%Y-%m-%d")
    
    country_groups <- vac_manu %>%
        group_by(location, vaccine) %>%
        summarize(total = sum(total_vaccinations)) %>%
        mutate(avg = total / sum(total))
    
    vac_count <- country_groups %>%
      group_by(vaccine) %>%
      summarise(ObsCount = n())
    
    ##########################################################
    # Data manipulation (for country comparison)
    ###########################################################
    
    updateDataManuByCountry1 <- reactive({  
      dataManuFilteredByCountry1 <- vac_manu 
      if(input$singleSelectForCountry1 != ""){
        dataManuFilteredByCountry1 <- dataManuFilteredByCountry1[(input$singleSelectForCountry1 == dataManuFilteredByCountry1$location),]
      }
      
      dataManuFilteredByCountry1
    })
    
    updateDataManuByCountry2 <- reactive({  
      dataManuFilteredByCountry2 <- vac_manu 
      if(input$singleSelectForCountry2 != ""){
        dataManuFilteredByCountry2 <- dataManuFilteredByCountry2[(input$singleSelectForCountry2 == dataManuFilteredByCountry2$location),]
      } 
      dataManuFilteredByCountry2
    })

    
    vac_daily$date = as.Date(vac_daily$date, format = "%Y-%m-%d")
    world_daily$date = as.Date(world_daily$date, format = "%Y-%m-%d")
    
    joint_data = full_join(vac_daily, world_daily, by = c("country" = "country", "date" = "date"))
    joint_data <- joint_data[c(1,3,8,17)]
    joint_data <- na.omit(joint_data)
    
    updateDataJointCountry1 <- reactive({  
      dataJointFilteredByCountry1 <- joint_data 
      if(input$singleSelectForCountry1 != ""){
        dataJointFilteredByCountry1 <- dataJointFilteredByCountry1[(input$singleSelectForCountry1 == dataJointFilteredByCountry1$country),]
      }
      
      dataJointFilteredByCountry1
    })
    
    updateDataJointCountry2 <- reactive({  
      dataJointFilteredByCountry2 <- joint_data 
      if(input$singleSelectForCountry2 != ""){
        dataJointFilteredByCountry2 <- dataJointFilteredByCountry2[(input$singleSelectForCountry2 == dataJointFilteredByCountry2$country),]
      } 
      dataJointFilteredByCountry2
    })
    
    ####################################################################################################################
    # Rendering Section
    #####################################################################################################################
    
    #////////////////////////////////////////////////////////////////////////////////
    # DataTable
    #////////////////////////////////////////////////////////////////////////////////
        output$myTable1 = renderDataTable({
            return(datatable(vac_daily, rownames = FALSE))
        })
        output$myTable2 = renderDataTable({
            return(datatable(vac_manu, rownames = FALSE))
        })
        output$myTable3 = renderDataTable({
            return(datatable(world_daily, rownames = FALSE))
        })
        output$myTable4 = renderDataTable({
            return(datatable(world_summary, rownames = FALSE))
        })
    
    #////////////////////////////////////////////////////////////////////////////////
    # World map
    #////////////////////////////////////////////////////////////////////////////////
    
        output$worldmap <- renderPlot({
          worldmap <- map_data("world")
          worldmap
          vacdate = input$slider
          
          x=vac_daily %>% 
            filter(!is.na(total_vaccinations)) %>%
            inner_join(world_summary,"country") 
          
          x=x %>% mutate(vacRate = total_vaccinations/population)
          
          filteredData <- x[x$date ==vacdate,]
          colnames(filteredData)[1] = "region"
          newfiltereddata <- full_join(filteredData,worldmap, by = "region")
          
          ggplot(newfiltereddata,aes(x=long, y= lat))+
            geom_map(data = newfiltereddata,map =newfiltereddata,aes(long,lat,map_id= region))+
            aes(fill = vacRate) +
            scale_fill_gradient(low = "seashell1", high = "darkorange3")
          
        })
    
    #////////////////////////////////////////////////////////////////////////////////
    # Countries ranking graphs
    #////////////////////////////////////////////////////////////////////////////////
    
    output$CountriesRankedbyTotalVaccinations <- renderPlot({
        p1 <- ggplot(vac_total_top_20, aes(x = reorder(country, total_vaccinations), y = total_vaccinations)) +
            geom_col(fill = "orange") +
            coord_flip() +
            labs(
                title = "Top 20 Countries",
                x = NULL,
                y = "Total Vaccinations"
            )
        p1
    })
    
    output$CountriesRankedbyVacRate <- renderPlot({
        p2 <- ggplot(fully_vac_rate_top_20, aes(x = reorder(country,fully_vaccination_rate), y = fully_vaccination_rate)) +
            geom_col(fill = "orange") +
            coord_flip() + 
            labs(
                title = "Top 20 Countries",
                x = NULL,
                y = "Fully Vaccinated per Hundred"
            )
        p2
    })
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Country comparison graphs
    #////////////////////////////////////////////////////////////////////////////////
    
    output$VacProgressbyManubyCountry1 <- renderPlot({
        Data1 <- updateDataManuByCountry1() 
        ggplot(Data1, aes(x = date, y = total_vaccinations, color = vaccine)) + geom_line(size = 1) +
          labs(
            y = "Vaccinations",
            x = NULL,
            color = "Vaccine")
    })  
    
    output$VacProgressbyManubyCountry2 <- renderPlot({
      Data2 <- updateDataManuByCountry2() 
      ggplot(Data2, aes(x = date, y = total_vaccinations, color = vaccine)) + geom_line(size = 1) +
        labs(
          y = "Vaccinations",
          x = NULL,
          color = "Vaccine")
    })  
    
    output$VacvsNewCase1 <- renderPlot({
      Data3 <- updateDataJointCountry1() 
      ggplot(Data3, aes(x = date)) + geom_line(aes(y = daily_new_cases, color = "Daily new cases"), size = 1) +
        geom_line(aes(y = daily_vaccinations, color = "Daily vaccinations"), size = 1) +
        labs(subtitle =  "Comparing the trend daily new cases and daily vaccinations",x = "Date", y = "Count", color = "Legend")+
        scale_y_continuous("Count", labels = scales::label_number_si(),trans = "sqrt") +
        scale_colour_manual(values = colors)
    })
    
    output$VacvsNewCase2 <- renderPlot({
      Data4 <- updateDataJointCountry2() 
      ggplot(Data4, aes(x = date)) + geom_line(aes(y = daily_new_cases, color = "Daily new cases"), size = 1) +
        geom_line(aes(y = daily_vaccinations, color = "Daily vaccinations"), size = 1) +
        labs(subtitle =  "Comparing the trend daily new cases and daily vaccinations", x = "Date", y = "Count", color = "Legend")+
        scale_y_continuous("Count", labels = scales::label_number_si(),trans = "sqrt") +
        scale_colour_manual(values = colors)
    })
    
    #////////////////////////////////////////////////////////////////////////////////
    # Vaccine progress by manufacturers
    #////////////////////////////////////////////////////////////////////////////////
    output$VaccinePortfoliobyCountry <- renderPlot({
      p3 <- ggplot(data = country_groups, mapping = aes(x = avg, y = location, fill = vaccine)) +
        geom_bar(stat = "identity") +
        labs(
          y = "Country",
          x = "Total Vaccinations",
          color = "Vaccine")
      p3
    })  
    
    output$NumberOfCountriesUsingEachVaccine <- renderPlot({
      p4 <- ggplot(data = vac_count, mapping = aes(x = reorder(vaccine, ObsCount), y = ObsCount)) +
        geom_col(fill = "orange") +
        coord_flip() + 
        labs(
          y = "Number of Countries",
          x = NULL)
      p4
    })  
    
}
##########################################################
# ShinyApp main function
###########################################################
shinyApp(ui = ui, server = server)