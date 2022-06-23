library(shiny)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(ggrepel)
library(scales)
library(ggthemes)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Formula One"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("Mercedes", tabName = "page2", icon = icon("car")),
      menuItem("Driver", tabName = "page3", icon = icon("user")),
      menuItem("Constructor", tabName = "page4", icon = icon("users")),
      menuItem("Grand Prix", tabName = "page5", icon = icon("road")),
      menuItem("Data Souce", tabName = "page6", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      "body { font-size: 16px; }"
    ),
    tabItems(
      tabItem(
        tabName = "page1",
        h2(strong("Home")),
        fluidRow(
          box(
            title = "About F1",
            solidHeader = TRUE,
            status = "danger",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(fluidRow(
                     column(
                       9,
                       h4(
                         strong("Formula One"),
                         "also called",
                         strong("Formula 1"),
                         "or",
                         strong("F1"),
                         "is the highest
                                           class of international auto racing for single-seater formula racing cars sanctioned by the International
                                           Automobile Federation (FIA). The World Drivers' Championship, which became the FIA Formula One
                                           World Championship in 1981, has been one of the premier forms of racing around the world since its
                                           inaugural season in 1950.",
                         br(),
                         br(),
                         "The word formula in the name refers to the set of rules to which all
                                           participants' cars must conform. A Formula One season consists of a series of races, known as Grands Prix,
                                           which take place worldwide on both purpose-built circuits and closed public roads."
                       ),
                       br(),
                       a(h5("Source: Wikipedia"), href =
                           'https://en.wikipedia.org/wiki/Formula_One')
                     ),
                     column(3, tags$img(src = "F1.jpg", width = "100%"))
                   )),)
          )
        ),
        fluidRow(
          box(
            title = "About the Application",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              9,
              h4(
                "This website titled",
                strong("Formula One"),
                "aims to provide data visualization and statistics for Formula One fans and
                                   researchers alike.",
                br(),
                br(),
                "The",
                strong("Mercedes"),
                "tab tells a story about Mercedes' F1 team dominance of the V6 turbo-hybrid era of Formula 1 (2014-2020).",
                br(),
                br(),
                "The ",
                strong("Driver, Constructor and Grand Prix"),
                "tabs provide you with more detailed views that let you dig deeper into anything you may be interested to know about F1 in recent years."
              )
            ),
            column(3, tags$img(src = "cars.jpg", width = "100%"))
          )
        ),
        
        fluidRow(
          box(
            title = "About Us",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(fluidRow(
                     column(
                       9,
                       h4(
                         "We developed this website for the completion of Data Visualization course final project during our MS Information Systems degree at Johns Hopkins Carey Business School."
                       )
                     ),
                     column(3, tags$img(src = "carey.jpg", width = "100%"))
                   )),),
          )
        ),
      ),
      
      ###Page 2
      tabItem(
        tabName = "page2",
        h2(
          strong("Mercedes F1 Team during the Hybrid engine era (2014-2020)")
        ),
        br(),
        fluidRow(
          column(
            5,
            box(
              div(
                span(
                  "Mercedes, in its various forms, is one of the most successful teams in the 70-year-old history of Formula 1.
             The team, better known as Silver Arrows first competed in the sport in the 1954 French Grand Prix.
             Since then, the German outfit has won 7 Constructors' Championships and 9 Drivers Championships.
             Mercedes have won all the seven F1 Championships since the introduction
    of the turbo-hybrid regulations in 2014."
                )
              ),
              br(),
              div(
                span(
                  "The seven consecutive titles are unprecedented in the history of Formula 1.
This is considerably ahead of the dominance considered to be greatest in the last
             century - Senna-Prost at McLaren. The iconic driver duo won four
             consecutive Constructors' title for McLaren, but their race victories
             and one-twos percentage is considerably lower than the Mercedes of 2014 to 2020."
                )
              ),
              br(),
              div(
                span(
                  "After a remarkable 4th World Championship for Sebastian Vettel with Red Bull,
             the F1 rules (and engines) changed once again, but ushered in a period of
             sustained Mercedes AMG and Lewis Hamilton dominance no one really anticipated.
             And so the Formula One saga continues, with the ebbs and flows the sport has witnessed for
             nearly seven decades."
                )
              ),
              br(),
              a(
                h5("Source", href = 'https://thesportsrush.com/f1-news-mercedes-writing-an-era-of-domination-in-formula-1-is-there-any-end-to-seamless-triumphs/')
              ),
              title = "A little bit of history...",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = "100%"
            )
          ),
          column(
            7,
            br(),
            br(),
            br(),
            a(
              "Mercedes-AMG Petronas F1 Team Wiki page",
              style = "font-size:24px;",
              href = "https://en.wikipedia.org/wiki/Mercedes-Benz_in_Formula_One"
            ),
            br(),
            br(),
            imageOutput('team')
          )
        ),
        fluidRow(column(
          5,
          box(
            h4(
              "Below you can find charts and data that tell the story of the Mercedes F1 team Hybrid era performance.",
              br(),
              "Throughout the page the dominant ",
              strong("turquoise"),
              " color correspons to the result of the Mercedes team, while grey and other colour - to other teams.",
              br(),
              br(),
              "The ",
              strong("Grand Prix finishing time analysis"),
              "tab presents visualization that
                          enables to dig deeper into Mercedes drivers' performance in each Grand Prix held during the period of 2014-2020.",
              br(),
              br(),
              "Select the GP you are interested in from the drop-down list to see team's performance over the years on selected circuit.",
              br(),
              br(),
              "The ",
              strong("Racing and Qualifying performance analysis"),
              "tab present detailed information about the Mercedes performance over the period of 2014-2020.",
              br(),
              br(),
              "To view the information for the year that you are interested in just pull the slider into the corresponding year.",
              br(),
              br(),
              br(),
              "The ",
              strong("Mercedes drivers 2014-2020"),
              "tab shows information about and aggregated performance summary of Mercedes drivers' during 2014-2020"
            ),
            title = "How to Use",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%"
          )
        ),
        column(7,  br(), br(), br(), imageOutput('car'))),
        fluidRow(
          box(
            title = "7 year victorious journey",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              4,
              (
                "Seven constructors' titles, more than a century of victories (and counting),
                 all manner of records and the unofficial naming rights to an entire era of
                 Formula 1 - Mercedes' dominance of the V6 turbo-hybrid formula has been total."
              ),
              br(),
              br(),
              tableOutput(outputId = 'merc_sum'),
            ),
            column(8, imageOutput('mercedes'))
          )
        ),
        hr(),
        fluidRow(
          box(
            title = "Grand Prix finishing time analysis",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            fluidRow(column(
              12,
              column(
                4,
                div(
                  "The following viz provides you with the detailed view of Mercedes drivers' finishing times in comparison to other drivers' in various Grand Prix."
                ),
                br(),
                div(
                  "Please select the GP to see the performance of Mercedes driver's for each year that GP was held during the hybrid era (2014-2020)"
                ),
                br(),
                selectInput("circuit", "Grand Prix", choices = "")
              ),
              column(8, plotOutput(outputId = 'tracks_plot'))
            ))
          )
        ),
        hr(),
        fluidRow(
          box(
            title = "Racing and Qualifying performance analysis",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            fluidRow(
              column(
                4,
                div(
                  "The following charts present a more detailed view into Mercedes team performance throughout the each year of the Hybrid era"
                ),
                br(),
                div("Please select the year to show the data for that particular year "),
                br(),
                sliderInput(
                  "year_top",
                  "Year:",
                  min = 2014,
                  max = 2020,
                  value = 2014,
                  step = 1,
                  animate = animationOptions(interval = 2000, loop = FALSE)
                )
              ),
              column(8, plotOutput(outputId = 'races_plot'))
            ),
            br(),
            
            
            fluidRow(column(4, plotOutput(outputId = 'points_plot')),
                     column(8, plotOutput(outputId = 'quali_plot'))),
            fluidRow(column(9, ""), column(
              3,
              a(h5("Inspired by Formula 1 70th Anniversary"), href = "https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/")
            ))
          )
        ),
        hr(),
        fluidRow(
          box(
            title = "Mercedes drivers 2014-2020",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            fluidRow(
              column(3, ""),
              column(3,
                     imageOutput('hamilton')),
              column(3,
                     imageOutput('rosberg')),
              column(3,
                     imageOutput('bottas')),
            ),
            fluidRow(
              column(2, offset = 1, h3(
                style = "text-align: left;", strong('Name '), h5("(Click for Wiki)")
              )),
              column(
                3,
                a(h3(style = "text-align: center; ", ('Lewis Hamilton')), href =
                    'https://en.wikipedia.org/wiki/Lewis_Hamilton')
              ),
              column(3,
                     a(h3(
                       style = "text-align: center;", ('Nico Rosberg')
                     ), href = 'https://en.wikipedia.org/wiki/Nico_Rosberg')),
              column(
                3,
                a(h3(style = "text-align: center;", ('Valterri Bottas')), href =
                    'https://en.wikipedia.org/wiki/Valtteri_Bottas')
              ),
            ),
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong('Nationality'))),
              column(
                3,
                img(src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg", width =
                      30,
                    style = "display: block;margin-left: auto;margin-right: auto;")
              ),
              column(
                3,
                img(src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg", width =
                      30,
                    style = "display: block;margin-left: auto;margin-right: auto;")
              ),
              column(
                3,
                img(src = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fi.svg", width =
                      30,
                    style = "display: block;margin-left: auto;margin-right: auto;")
              )
            ),
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong('Number'))),
              column(3,
                     h4(style = "text-align: center;", ('44'))),
              column(3,
                     h4(style = "text-align: center;", ('6'))),
              column(3,
                     h4(style = "text-align: center;", ('77'))),
            ),
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong('Alias'))),
              column(3,
                     h4(style = "text-align: center;", ('HAM'))),
              column(3,
                     h4(style = "text-align: center;", ('ROS'))),
              column(3,
                     h4(style = "text-align: center;", ('BOT'))),
            ),
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong('Date of birth'))),
              column(3,
                     h4(style = "text-align: center;", ('Jan 7th 1985'))),
              column(3,
                     h4(style = "text-align: center;", ('Jun 27th 1985'))),
              column(3,
                     h4(style = "text-align: center;", ('Aug 28th 1989'))),
            ),
            
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong(
                'Years with Mercedes'
              ))),
              column(3,
                     h4(style = "text-align: center;", ('2013 - present'))),
              column(3,
                     h4(style = "text-align: center;", ('2010 - 2016'))),
              column(3,
                     h4(style = "text-align: center;", ('2017 - present'))),
            ),
            fluidRow(
              column(2, offset = 1, h4(style = "text-align: left;", strong('Season Entries'))),
              column(3,
                     h4(style = "text-align: center;", ('15'))),
              column(3,
                     h4(style = "text-align: center;", ('11'))),
              column(3,
                     h4(style = "text-align: center;", ('9'))),
            ),
            br(),
            h2(style = "text-align: center;", ('Mercedes drivers\' performance summary')),
            br(),
            fluidRow(column(11, offset = 1, dataTableOutput('dr_table')))
          )
        )
      ),
      
      
      
      #Page3
      #####
      tabItem(
        tabName = "page3",
        h2(strong("The Driver")),
        fluidRow(column(
          8,
          h4(""),
          
          box(
            h5(
              "The first chart visualizes the circuits of selected driver attended in the selected year.
                           in the",
              em(strong("last 7 years (2014-2020)")),
              ". You can use",
              strong("Select Year"),
              "and",
              strong("Select Driver"),
              "function below to filter. The second
                            chart visualizes the position changes of each driver in each grand prix. These charts only shows data for the
                           latest",
              em(strong("2020 season")),
              ". "
            ),
            title = "How to Use",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%"
          ),
          box(
            h5(
              strong(
                "Not all of the 20 current active drivers have participated in the last seven seasons."
              ),
              "As some of the drivers only joined in the recent years. Therefore, no charts/data will be shown if the selected driver does not participate in the selected year or circuit."
            ),
            title = "Disclaimer",
            status = "warning",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%"
          )
        )),
        
        hr(),
        
        fluidRow(
          box(
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              4,
              sliderInput(
                "year",
                "Select Year:",
                min = 2014,
                max = 2020,
                value = 2014,
                step = 1,
                animate = animationOptions(interval = 1500, loop = FALSE)
              )
            ),
            column(4, selectInput("driver",
                                  "Select Driver:",
                                  "",
                                  multiple = FALSE))
          )
          ,
          box(
            title = "Circuits attended map",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(9, leafletOutput("driverMap"))
          ),
          
        ),
        
        hr(),
        fluidRow(
          box(
            soildHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              4,
              sliderInput(
                "yearD",
                "Select Year:",
                min = 2014,
                max = 2020,
                value = 2014,
                step = 1,
                animate = animationOptions(interval = 1500, loop = FALSE)
              )
            ),
            column(4, selectInput("driverD",
                                  "Select Driver:",
                                  "",
                                  multiple = FALSE))
          )
        ),
        
        fluidRow(
          box(
            title = "Driver standing",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(6, plotOutput("driverPlot2"),
                   fluidRow(column(width = 10, div(
                     span(
                       a(
                         "Source:https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/",
                         href =
                           "https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/"
                       )
                     )
                   )))),
            column(6, plotOutput("driverPlot1")),
            column(12, dataTableOutput("driverTable"))
          )
        )
      ),
      #####
      #Page4
      #####
      tabItem(
        tabName = "page4",
        h2(strong("The Constructor")),
        fluidRow(column(
          8,
          box(
            h5(
              "This page shows the performance of different constructors in different GPs.
                    The upper part of the page is the ranking of constructors and top 3 constructors
                       in 2020. In the lower part of the page, users can",
              strong("select the constructor"),
              "and the
                       year to view its specific ranking and points in a",
              strong("certain year"),
              ". The specific data
                       table is at the bottom right."
            ),
            title = "How to Use",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%"
          )
        )),
        hr(),
        
        fluidRow(
          box(
            title = "Constructor standings in 2020 and top 3 Constructors",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(width = 6, plotOutput(
              "plot2", height = 500, width = 600
            )),
            column(width = 6, img(
              src = "img1.jpg",
              height = 500,
              width = 600
            )),
            column(width = 10, div(span(
              a(
                "Source:https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/",
                href = "https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/"
              )
            )))
          )
        ),
        fluidRow(
          box(
            title = "Find your interested Constructor",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              4,
              selectInput(
                inputId = "Constructor",
                label = "Select Constructor",
                choices = c(
                  "Mercedes",
                  "Alfa Romeo",
                  "Ferrari",
                  "McLaren",
                  "Renault",
                  "Red Bull",
                  "Haas F1 Team",
                  "Williams",
                  "AlphaTauri",
                  "Racing Point"
                ),
                selected = "Mercedes",
                width = 600,
                multiple = FALSE
              )
            ),
            column(
              4,
              sliderInput(
                inputId = "year1",
                label = "Select Year:",
                width = 600,
                min = 2014,
                max = 2020,
                value = 2014,
                step = 1,
                animate = animationOptions(interval = 1000, loop = FALSE)
              )
            ),
            fluidRow(column(6, mainPanel(
              plotOutput("plot1", height = 500, width = 600)
            )),
            column(
              6, dataTableOutput("myTable")
            ))
          )
        )
        
      ),
      #####
      #Page5
      #####
      tabItem(
        tabName = "page5",
        h2(strong("The Grand Prix")),
        fluidRow(column(
          8,
          h4(
            "The F1 World Championship season consists of a series of races,
                        known as Grands Prix, usually held on purpose-built circuits,
                        and in a few cases on closed city streets. Grands Prix are
                        frequently named after the country they occur in, and in some
                        seasons, nations have hosted more than one race. Source:",
            a("Wikipedia", href = "https://en.wikipedia.org/wiki/Formula_One")
          ),
          
          box(
            h5(
              "The chart visualizes the",
              strong("the lap records"),
              "for all",
              strong("current active drivers in 2020 (20 in total)"),
              "in each Grand Prix
                           in the",
              strong("last seven years (2014-2020)."),
              "You can use the",
              strong("Select Year"),
              "and",
              strong("Select Driver"),
              "functions below to filter.",
              br(),
              br(),
              "These visualizations is drawn using Plotly library thus you can",
              strong("Hover on the graph"),
              "to see detailed information for each data points
                              such as",
              strong("quantiles, median, upper and lower limit."),
              "Beneath the chart is the",
              strong("corresponding data table"),
              "that also",
              strong("adapts"),
              "to your filter selection."
            ),
            title = "How to Use",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%"
          ),
          
          box(
            h5(
              strong(
                "Not all of the 20 current active drivers have participated in the last seven seasons."
              ),
              "As some of the drivers only joined in the recent years. Therefore, no charts/data will be shown if the selected driver does not participate in the selected year."
            ),
            title = "Disclaimer",
            status = "warning",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%"
          )
        ),
        column(
          4,
          img(src = "circuit.jpg", alt = "Circuit", width = "100%")
        )),
        
        hr(),
        
        box(
          fluidRow(
            column(
              3,
              h5(
                "This chart shows every Grand Prix's lap records of a driver in a specific year as filtered by the function below.
                       The year input is shown for the last 7 years (2014-2020), while the driver input is available for the 20 currently active drivers in Formula 1.
                       The table display corresponding data as shown in the chart."
              ),
              br(),
              sliderInput(
                "yearR",
                "Select Year:",
                min = 2014,
                max = 2020,
                value = 2020,
                step = 1,
                animate = animationOptions(interval = 1500, loop = FALSE)
              ),
              selectInput("driverR",
                          "Select Driver:",
                          "",
                          multiple = FALSE)
            ),
            column(9, plotlyOutput("lapTimesPlot"))
          ),
          title = "Lap Records",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          collapsed = FALSE,
          width = "100%"
        ),
        
        hr(),
        
        fluidRow(column(12, dataTableOutput("lapTimesTable2"))),
        
        hr(),
        
        fluidRow(column(
          4,
          HTML(
            '<iframe width = "100%" height="300"
                    src = "https://www.youtube.com/embed/yywWWGad6s0" frameborder="0"
                    allowfullscreen></iframe>'
          )
        ),
        
        column(
          8,
          img(
            src = "banner.jpg",
            alt = "Banner",
            width = "100%",
            height = "300"
          )
        ))
      ),
      #####
      #Page6
      #####
      tabItem(
        tabName = "page6",
        h2(strong("Data Source")),
        fluidRow(column(
          12,
          h4(
            "Below are the data tables visualization of some data sources our team used in building this website.
                              The data source is based of a website called",
            strong("Ergast Developer API"),
            "which provides a historical record of motor racing data for non-commercial purposes.
                              You can access the source dataset by visiting this",
            a("link.", href = "http://ergast.com/mrd/")
          ),
          h4(
            "The design language of this website is taking a reference from a Kaggle publication named",
            strong("Formula 1 70th Anniversary"),
            "including color code for chart background.
                              You can access the Kaggle entry by visiting this",
            a("link.", href = "https://www.kaggle.com/ekrembayar/formula-1-70th-anniversary/")
          )
        )),
        
        fluidRow(column(
          12, h4(strong("Driver Standings")), dataTableOutput("driverStandingsTable")
        )),
        
        fluidRow(column(
          12,
          h4(strong("Constructor Standings")),
          dataTableOutput("constructorStandingsTable")
        )),
        
        fluidRow(column(
          12, h4(strong("Qualifying")), dataTableOutput("qualifyingTable")
        )),
        
        fluidRow(column(
          12, h4(strong("Results")), dataTableOutput("resultsTable")
        )),
        
        fluidRow(column(
          12, h4(strong("Lap Times")), dataTableOutput("lapTimesTable")
        ))
      )
    )
  )
)
#####select recent 7 years for data visualization
server <- function(input, output, session) {
  target_year <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
  
  #read data/add specifical data individually
  races <- read.csv("races.csv") %>%
    filter(year %in% target_year) %>%
    rename(circuit = name)
  
  #2020 for last year visualization
  races2020 <- read.csv("races.csv") %>%
    filter(year == 2020) %>%
    rename(circuit = name)
  
  status = read.csv("status.csv")
  results <- read.csv("results.csv") %>%
    filter(raceId %in% races$raceId) %>%
    left_join(status, by = "statusId") %>%
    select(-statusId, -resultId)
  
  driver_standings <- read.csv("driver_standings.csv") %>%
    filter(raceId %in% races$raceId) %>%
    select(-driverStandingsId)
  
  constructor_standings <-
    read.csv("constructor_standings.csv") %>%
    filter(raceId %in% races$raceId) %>%
    select(-constructorStandingsId)
  constructor_standings2020 <-
    read.csv("constructor_standings.csv") %>%
    filter(raceId %in% races2020$raceId) %>%
    select(-constructorStandingsId)
  
  lap_times <- read.csv("lap_times.csv") %>%
    filter(raceId %in% races$raceId)
  
  qualifying <- read.csv("qualifying.csv") %>%
    filter(raceId %in% races$raceId)
  
  drivers <- read.csv("drivers.csv", encoding = "UTF-8") %>%
    filter(driverId %in% driver_standings$driverId) %>%
    unite(driver.name, c("forename", "surname"), sep = " ") %>%
    select(driverId, code, number, driver.name, dob, nationality, url) %>%
    rename(driver.number = number)
  
  constructors <- read.csv("constructors.csv") %>%
    filter(constructorId %in% constructor_standings$constructorId) %>%
    select(-constructorRef) %>% rename(cons.name = name)
  constructors2020 <- read.csv("constructors.csv") %>%
    filter(constructorId %in% constructor_standings2020$constructorId) %>%
    select(-constructorRef) %>% rename(cons.name = name)
  
  #data manipulation
  gp <- races %>% select(raceId, round, circuit, year)
  dr <-
    drivers %>% select(driverId, driver.name, code, driver.number)
  cons <- constructors %>% select(constructorId, cons.name)
  gp2020 <- races2020 %>% select(raceId, round, circuit, year)
  cons2020 <-
    constructors2020 %>% select(constructorId, cons.name)
  
  results %<>% left_join(gp, by = "raceId") %>%
    left_join(dr, by = "driverId") %>%
    left_join(cons, by = "constructorId")
  
  constructor_standings %<>% left_join(gp, by = "raceId") %>%
    left_join(cons, by = "constructorId") %>%
    select(-raceId, -constructorId)
  constructor_standings2020 %<>%  left_join(gp2020, by = "raceId") %>%
    left_join(cons2020, by = "constructorId") %>%
    select(-raceId,-constructorId)
  
  driver_standings %<>% left_join(gp, by = "raceId") %>%
    left_join(dr, by = "driverId") %>%
    select(-raceId, -driverId)
  
  lap_times %<>%  left_join(gp, by = "raceId") %>%
    left_join(dr, by = "driverId") %>%
    select(-raceId, -driverId)
  
  
  qualifying %<>%  left_join(gp, by = "raceId") %>%
    left_join(dr, by = "driverId") %>%
    left_join(cons, by = "constructorId")
  
  
  #color add
  color <-
    read.csv(
      "color.csv",
      encoding = "UTF-8",
      sep = ";",
      stringsAsFactors = FALSE
    )
  
  #constructor_standings
  constructor_standings2020 %<>%  left_join(color %>% select(-driver.name) %>% distinct())
  
  clr <- constructor_standings %>%
    filter(cons.name == "Mercedes") %>%
    #pull(colors) %>%
    unique() %>%
    as.character()
  my_vals = unique(constructor_standings$cons.name)
  my_colors = ifelse(my_vals == "Mercedes", clr, "")
  my_colors2 = ifelse(my_vals == "Mercedes", 'white', "black")
  
  
  races_driver = read_csv("races.csv") %>%
    select(-c("time", "url")) %>%
    rename(circuit = name)
  
  circuits = read_csv("circuits.csv") %>%
    select(-c("circuitRef", "url")) %>%
    rename(circuitName = name)
  
  driver_standings_driver = read_csv("driver_standings.csv") %>%
    select(-c("positionText")) %>%
    filter(raceId %in% races_driver$raceId,!driverId %in% c(807, 850, 851)) %>%
    select(-driverStandingsId)
  
  drivers_driver = read_csv("drivers.csv") %>%
    filter(driverId %in% driver_standings_driver$driverId) %>%
    unite(driver.name, c("forename", "surname"), sep = " ") %>%
    select(driverId, code, number, driver.name, dob, nationality) %>%
    rename(driver.number = number)
  
  driver_standings_driver <- driver_standings_driver %>%
    left_join(races_driver, by = "raceId") %>%
    left_join(drivers_driver, by = "driverId") %>%
    left_join(circuits, by = "circuitId")
  driver_standings_driver <- driver_standings_driver %>%
    left_join(
      driver_standings_driver %>%
        group_by(driver.name, year) %>%
        summarise(Points = max(points), raceAttended = n()) %>%
        ungroup()
    ) %>%
    select(-c("raceId", "circuitId", "driverId"))
  driver_standings_driver %<>%  left_join(color)
  
  #Zhiqi's datatable of Constructor part
  output$myTable = renderDataTable({
    thisYear = input$year1
    constructors = input$Constructor
    filteredData <- subset(constructor_standings, year == thisYear)
    constructor_standings %<>%  left_join(color %>% select(-driver.name) %>% distinct())
    datatable(
      filteredData %>%
        mutate(circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>%
        filter(thisYear == input$year1, cons.name == input$Constructor) %>%
        arrange(-points) %>%
        select(round, cons.name, circuit, wins, points, year),
      options = list(pageLength = 10,
                     scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'cons.name',
        target = 'row',
        backgroundColor = styleEqual(my_vals, my_colors),
        color = styleEqual(my_vals, my_colors)
      )
    
  })
  
  #Zhiqi filter and select part of constructor part
  output$plot1 <- renderPlot({
    thisYear = input$year1
    constructors = input$Constructor
    filteredData <- subset(constructor_standings, year == thisYear)
    constructor_standings %<>%  left_join(color %>% select(-driver.name) %>% distinct())
    #filteredData<-filteredData %>%
    #filter(cons.name==constructors)
    filteredData %>%
      mutate(
        colors = case_when(cons.name %in% constructors ~ colors,
                           TRUE ~ "gray"),
        circuit = str_replace_all(circuit, " Grand Prix", " GP")
      ) %>%
      ggplot(
        aes(
          reorder(circuit, round),
          points,
          color = colors,
          label = thisYear,
          group = cons.name,
          size = colors,
          alpha = colors
        )
      ) +
      geom_line(show.legend = FALSE) +
      scale_color_identity() +
      scale_size_manual(values = c(1.5, 1.5, 1.5, 1, 1, 1, 0.8, 0.8, 0.8, 0.8)) +
      scale_alpha_manual(values = c(1, 1, 1, 0.7, 0.7, 0.7, 0.5, 0.5, 0.5, 0.5)) +
      #Theme
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 70, hjust = 1)
      ) +
      #  cnum=3)
      # countriesToShow= filteredData %>%
      #   arrange(-points) %>%
      #   pull(cons.name) %>%
      #   head(cnum)
      #geom_text(data= filteredData %>% filter(cons.name %in% countriesToShow),
      #mapping = aes(label = cons.name))+
      #Labs
      labs(x = "Circuit", y = "Points", title = "Constructor Standings")
    
    
  })
  
  #Zhiqi top3 cons in 2020(added reference)
  output$plot2 <- renderPlot({
    constructor_standings2020 %>%
      mutate(
        colors = case_when(
          cons.name %in% c("Mercedes", "Red Bull", "McLaren") ~ colors,
          TRUE ~ "gray"
        ) ,
        circuit = str_replace_all(circuit, " Grand Prix", " GP")
      ) %>%
      ggplot(aes(
        reorder(circuit, round),
        points,
        color = colors,
        group = cons.name,
        size = colors,
        alpha = colors
      )) +
      geom_line(show.legend = FALSE) +
      scale_color_identity() +
      scale_size_manual(values = c(1.5, 1.5, 1.5, 1)) +
      scale_alpha_manual(values = c(1, 1, 1, 0.7)) +
      # Theme
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 70, hjust = 1)
      ) +
      # Labs
      labs(
        x = "Circuit",
        y = "Points",
        title = "Constructor Standings in 2020",
        subtitle = "1.Mercedes \t 2.Red Bull \t 3.McLaren"
      )
    
    
  })
  
  #Pengxiang driver part
  output$driverTable = renderDataTable({
    selectDriver = input$driverD
    selectYear = input$yearD
    filteredData_driver = driver_standings_driver %>%
      filter(driver.name == selectDriver) %>%
      filter(year == selectYear) %>%
      mutate(
        popusText = paste(
          circuitName,
          " ",
          "Round: ",
          round,
          "\rCurrent Positions: ",
          position,
          " Current Points: ",
          points
        )
      )
    
    filteredData_driver = arrange(filteredData_driver, round)
    filteredData_driver$points
    lag1 = lag(filteredData_driver$points)
    lag1[1] = 0
    filteredData_driver$values = filteredData_driver$points - lag1
    
    datatable(
      filteredData_driver %>%
        arrange(-points) %>%
        select(round, position, driver.name, circuit, date, wins, points) %>%
        rename(driver = driver.name) %>% distinct(),
      options = list(pageLength = 8,
                     scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'driver',
        target = 'row',
        backgroundColor = styleEqual(my_vals, my_colors),
        color = styleEqual(my_vals, my_colors2)
      )
  })
  
  #Pengxiang driver standing part
  output$driverPlot1 <- renderPlot({
    selectDriver = input$driverD
    selectYear = input$yearD
    
    filteredData_driver = driver_standings_driver %>%
      filter(driver.name == selectDriver) %>%
      filter(year == selectYear) %>%
      mutate(
        popusText = paste(
          circuitName,
          " ",
          date,
          "\nCulmulative Positions: ",
          position,
          " Culmulative Points: ",
          points
        )
      )
    
    
    filteredData_driver = arrange(filteredData_driver, round)
    lag1 = lag(filteredData_driver$points)
    lag1[1] = 0
    filteredData_driver$values = filteredData_driver$points - lag1
    
    filteredData_driver %>%
      filter(year == selectYear) %>%
      mutate(
        colors = if_else(driver.name == selectDriver, colors, "gray"),
        circuit = str_replace_all(circuit, " Grand Prix", " GP")
      ) %>%
      ggplot(
        aes(
          reorder(circuit, round),
          values,
          color = colors,
          group = driver.name,
          alpha = colors,
          size = colors
        )
      ) +
      geom_line(show.legend = FALSE) +
      scale_color_identity() +
      scale_alpha_manual(values = c(1, .6)) +
      scale_size_manual(values = c(1, .3)) +
      # Theme
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, hjust = 1)
      ) +
      # Labs
      labs(x = "Circuit", y = "Points", title = "Points Earned Per Circuit")
  })
  
  #Pengxiang leaflet of driver part
  output$driverMap <- renderLeaflet({
    selectDriver = input$driver
    selectYear = input$year
    filteredData_driver = driver_standings_driver %>%
      filter(driver.name == selectDriver) %>%
      filter(year == selectYear) %>%
      mutate(
        popusText = paste(
          circuitName,
          " ",
          date,
          "\nCurrent Positions: ",
          position,
          " Current Points: ",
          points
        )
      )
    
    filteredData_driver = arrange(filteredData_driver, round)
    filteredData_driver$points
    lag1 = lag(filteredData_driver$points)
    lag1[1] = 0
    filteredData_driver$values = filteredData_driver$points - lag1
    
    getColor <- function(filteredData_driver) {
      sapply(filteredData_driver$values, function(values) {
        if (values >= 10) {
          "orange"
        } else if (values >= 8) {
          "white"
        } else if (values >= 6) {
          "green"
        } else {
          "black"
        }
      })
    }
    
    
    icons <- awesomeIcons(
      icon = 'car',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(filteredData_driver)
    )
    
    leaflet(filteredData_driver) %>%
      addTiles() %>%
      addAwesomeMarkers( ~ lng,
                         ~ lat,
                         icon = icons,
                         label =  ~ as.character(popusText)) %>%
      addLegend(
        "bottomright",
        colors = c("orange", "white", "green", "black"),
        labels = c("1st", "2nd", "3rd", "4th+")
      ) %>%
      addMiniMap("bottomleft", toggleDisplay = TRUE)
  })
  
  #Pengxiang driver standing part(added reference)
  output$driverPlot2 <- renderPlot({
    selectDriver = input$driver
    selectYear = input$year
    
    driver_standings_driver %>%
      filter(year == selectYear) %>%
      mutate(
        colors = if_else(driver.name == selectDriver, colors, "gray"),
        circuit = str_replace_all(circuit, " Grand Prix", " GP")
      ) %>%
      ggplot(
        aes(
          reorder(circuit, round),
          points,
          color = colors,
          group = driver.name,
          alpha = colors,
          size = colors
        )
      ) +
      geom_line(show.legend = FALSE) +
      scale_color_identity() +
      scale_alpha_manual(values = c(1, .6)) +
      scale_size_manual(values = c(1, .3)) +
      # Theme
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, hjust = 1)
      ) +
      # Labs
      labs(x = "Circuit", y = "Points", title = "Driver Standings")
  })
  
  
  #Yevhenii Lukianchuk's qualifying performance
  qualifying_n <-
    qualifying %>%  left_join(color, by = "driver.name") %>%
    group_by(round) %>%
    mutate_at(vars(q1:q3), funs(as.character)) %>%
    arrange(q1) %>%
    mutate(pos1 = row_number(),
           q2 = if_else(q2 %in% "\\N", "10:00.000", q2)) %>%
    arrange(q2) %>%
    mutate(
      pos2 = row_number(),
      pos2 = if_else(pos2 == "10:00.000", position, pos2),
      q3 = if_else(q3 %in% "\\N", "10:00.000", q3)
    ) %>%
    select(circuit, position, q1, q2, q3, driver.name, pos1, pos2, colors) %>%
    mutate(rmean = (position + pos1 + pos2) / 3) %>%
    ungroup() %>%
    mutate(
      q2 = if_else(q2 %in% "10:00.000", "\\N", q2),
      q3 = if_else(q3 %in% "10:00.000", "\\N", q3)
    )
  lap_times %<>% left_join(color)
  constructor_standings %<>% left_join(color %>% select(-driver.name) %>% distinct())
  driver_standings %<>% left_join(color)
  results %<>% left_join(color, by = c('cons.name', 'driver.name'))
  qualifying_n %<>% left_join(color %>% select(driver.name, cons.name), by = "driver.name")
  
  distinct_driver <- lap_times %>%
    filter(year == 2020) %>%
    select(8) %>%
    distinct()
  
  distinct_circuit <- lap_times %>%
    filter(year == 2020) %>%
    select(6) %>%
    distinct()
  
  observe({
    updateSelectInput(session,
                      "driver",
                      choices = distinct_driver$driver.name)
  })
  
  observe({
    updateSelectInput(session,
                      "circuit",
                      choices = distinct_circuit$circuit)
  })
  
  observe({
    updateSelectInput(session,
                      "driverR",
                      choices = distinct_driver$driver.name)
  })
  
  observe({
    updateSelectInput(session,
                      "circuitR",
                      choices = distinct_circuit$circuit)
  })
  
  observe({
    updateSelectInput(session,
                      "driverD",
                      choices = distinct_driver$driver.name)
  })
  
  #Raihan's lap records
  output$lapTimesPlot <- renderPlotly({
    m <- lap_times %>%
      filter(
        driver.name == input$driverR,
        year == input$yearR,
        !milliseconds %in% boxplot.stats(milliseconds)$out,
      ) %>%
      mutate(circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>%
      ggplot(aes(
        reorder(circuit, round),
        milliseconds / 1000,
        fill = colors,
        group = round
      )) +
      geom_boxplot(outlier.color = "white", color = "white") +
      scale_fill_identity() +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 12),
        title = element_text(size = 12),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1),
        axis.title.y = element_blank()
      ) +
      labs(
        y = "Seconds",
        title = paste("Lap Records for", input$driverR, "in", input$yearR)
      ) +
      coord_flip()
    
    hide_legend(m)
  })
  
  output$driverStandingsTable = renderDataTable({
    return(datatable(driver_standings, rownames = FALSE))
  })
  
  output$constructorStandingsTable = renderDataTable({
    return(datatable(constructor_standings, rownames = FALSE))
  })
  
  output$qualifyingTable = renderDataTable({
    return(datatable(qualifying_n, rownames = FALSE))
  })
  
  output$resultsTable = renderDataTable({
    return(datatable(results, rownames = FALSE))
  })
  
  output$lapTimesTable = renderDataTable({
    return(datatable(lap_times, rownames = FALSE))
  })
  
  
  #Raihan's datatable for Lap records
  output$lapTimesTable2 = renderDataTable({
    return(datatable(
      lap_times %>%
        filter(driver.name == input$driverR,
               year == input$yearR) %>%
        mutate(
          circuit = str_replace_all(circuit, " Grand Prix", " GP"),
          seconds = milliseconds / 1000
        ) %>%
        select(driver.name, code, driver.number, circuit, lap, seconds),
      rownames = FALSE
    ))
  })
  
  #Yevhenii Lukianchuk's Mercedes analysis part
  selected_year = reactive({
    input$year_top
  })
  
  
  mercedes_races =  reactive({
    results %>%
      filter(cons.name == "Mercedes") %>% filter(year == input$year_top) %>%
      #inner_join(select(color, -driver.name), by = 'cons.name') %>%
      mutate(colors = case_when(cons.name %in% c('Mercedes') ~ colors,
                                TRUE ~ "dark grey")) %>% arrange(round)
  })
  
  quali_winners = reactive({
    qualifying %>% filter(position == 1) %>%
      left_join(races, by = c('round', 'circuit', 'year')) %>%
      filter(year == input$year_top) %>% left_join(color, by = "cons.name")
    
  })
  
  
  
  tracks_best_times <- results %>% filter(milliseconds != '\\N')
  tracks_best_times <-
    tracks_best_times %>% mutate(milliseconds = as.numeric(gsub(
      "[^0-9.-]", "", tracks_best_times$milliseconds
    )))
  tracks_best_times <- tracks_best_times %>% mutate(colors = case_when(cons.name %in% c('Mercedes') ~ colors,
                                                                       TRUE ~ 'grey'))
  tracks_best_times <- na.omit(tracks_best_times)
  
  observe({
    updateSelectInput(session,
                      "circuit",
                      choices = unique(tracks_best_times$circuit))
  })
  statistic <-
    c(
      "Constructors' titles",
      "Drivers' titles",
      "Victories",
      "Poles",
      " Fastest laps",
      "Podiums",
      "1-2 Finishes"
    )
  number <- c(7, 7, 99, 105, 69, 204, 53)
  percentage <-
    c('100%', '100%', '71.7%', '76%', '51.49%', '73.91%', '38.4%')
  mercedes_summary <- data.frame(statistic, number, percentage)
  
  
  output$races_plot <- renderPlot({
    p <- mercedes_races() %>% ggplot() +
      # Q-Line
      geom_hline(
        aes(yintercept = 3),
        color = "seagreen",
        size = 1,
        linetype = 2
      ) +
      geom_hline(
        aes(yintercept = 10),
        color = "orange",
        size = 1,
        linetype = 2
      ) +
      geom_hline(
        aes(yintercept = 1),
        color = "white",
        size = 1,
        alpha = 0.5
      ) +
      geom_hline(
        aes(yintercept = 20),
        color = "white",
        size = 1,
        alpha = 0.5
      ) +
      geom_point(aes(round, as.numeric(position), color = colors), size = 4.3) +
      
      # Q-Text
      geom_text(aes(x = 1, y = 10),
                label = "Points",
                alpha = 0.07,
                color = "white") +
      geom_text(aes(x = 1, y = 3),
                label = "Podium",
                alpha = 0.07,
                color = "white") +
      scale_color_identity() +
      theme(
        panel.grid = element_blank(),
        text = element_text(color = "white", size = 15),
        axis.text = element_text(color = "white", size = 12),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 11)
      ) +
      labs(
        color = NULL,
        x = "Round",
        y = "Position",
        title = paste("Race performance ", input$year_top),
        subtitle = paste("Mercedes drivers' finishing positions in ", input$year_top)
      ) +
      scale_y_reverse(breaks = 1:22, labels = paste0("P", 1:22)) +
      scale_x_continuous(breaks = 1:22)
    p
  })
  
  output$quali_plot <- renderPlot({
    quali_winners() %>% ggplot() + geom_point(
      mapping = aes(
        x = round,
        y = year,
        color = colors,
        group = cons.name
      ),
      size = 7,
      alpha = 0.9
    ) +
      scale_color_identity() + scale_size_manual(values = c(1.5, 1.5, 1.5, 1)) +
      scale_alpha_manual(values = c(1, 1, 1, 0.7)) +
      # Theme
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text.y = element_blank()
      ) +
      labs(
        color = NULL,
        x = "Round",
        y = paste("Season ", input$year_top),
        title = paste("Qualifying performance"),
        subtitle = paste(
          "Teams that secured pole positions throughout the season ",
          input$year_top
        )
      )
  })
  output$points_plot <- renderPlot({
    col_plot_data <-
      results %>% group_by(cons.name, year) %>% summarise(total = sum(points)) %>% filter(year == input$year_top)
    col_plot_data <- col_plot_data %>% mutate(colors = case_when(cons.name %in% c('Mercedes') ~ '#00D2BE',
                                                                 TRUE ~ "dark grey"))
    ggplot(col_plot_data) + geom_col(mapping = aes(
      x = reorder(cons.name,-total),
      y = total,
      fill = colors
    )) +
      scale_fill_identity() + scale_size_manual(values = c(1.5, 1.5, 1.5, 1)) +
      scale_alpha_manual(values = c(1, 1, 1, 0.7)) +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 15),
        title = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 70, hjust = 1)
      ) +
      # Labs
      labs(
        x = "Team",
        y = "Points",
        title = "Constructor Standings",
        subtitle = paste("Season ", input$year_top)
      )
  })
  
  
  
  track_best_time = reactive({
    track_best_time <-
      tracks_best_times %>%  filter(circuit == input$circuit)
    track_best_time$milliseconds <-
      as.numeric(as.character(track_best_time$milliseconds))
    b_year_time <-
      track_best_time %>% group_by(year) %>% summarise(best = min (milliseconds))
    track_best_time <-
      track_best_time %>% left_join(b_year_time, by = 'year')
    track_best_time <- track_best_time %>%
      mutate(circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>%
      mutate(difference_from_lead = (milliseconds - best) / 1000)
  })
  
  
  
  output$tracks_plot <- renderPlot({
    ggplot(track_best_time(),
           mapping = aes(x = year, y = difference_from_lead)) +
      geom_point(aes(color = colors), size = 6, alpha = 0.9) +
      scale_color_identity() +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 13),
        title = element_text(size = 15),
        axis.ticks = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = 'white'),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, hjust = 1),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "white",
          fill = NA,
          size = 2
        )
      ) +
      labs(
        x = input$circuit,
        y = "Seconds",
        title = "Finishing Times",
        subtitle = "Number of seconds from the leader of the race"
      ) +
      scale_y_continuous() +
      scale_x_continuous(breaks = seq(2014, 2020, 1))
  })
  
  output$hamilton <- renderImage({
    filename <- normalizePath("www/hamilton_new.png")
    list(src = filename,
         height = 400,
         style = "display: block;margin-left: auto;margin-right: auto;")
  })
  output$rosberg <- renderImage({
    filename <- normalizePath("www/rosberg_new.jpg")
    list(src = filename,
         height = 400,
         style = "display: block;margin-left: auto;margin-right: auto;")
  })
  output$bottas <- renderImage({
    filename <- normalizePath("www/bottas_new.png")
    list(src = filename,
         height = 400,
         style = "display: block;margin-left: auto;margin-right: auto;")
  })
  output$mercedes <- renderImage({
    filename <- normalizePath("www/mercedes.jpeg")
    list(src = filename, height = 325)
  })
  
  output$car <- renderImage({
    filename <- normalizePath("www/merc_Car.jpeg")
    list(src = filename, height = 300)
  })
  
  output$team <- renderImage({
    filename <- normalizePath("www/merc_team_1.jpg")
    list(src = filename, height = 225)
  })
  
  output$dr_table <- renderDataTable({
    mecr_drivers <-
      results %>% filter(cons.name == 'Mercedes' &
                           driver.name != 'George Russell')
    mecr_drivers_sum <-
      mecr_drivers %>% group_by(driver.name) %>% summarise(
        wins = sum(positionOrder == 1),
        races = n(),
        podiums = sum(positionOrder %in% c(1, 2, 3)),
        poles = sum(grid == '1'),
        points = sum(points),
        point_per_race = round(sum(points) / races, 2),
        dnfs = sum(!(status %in% c(
          'Finished', '+1 Lap'
        )))
      )
  })
  output$merc_sum <-
    renderTable(
      mercedes_summary,
      align = 'l',
      digits = 0,
      bordered = TRUE
    )
}
#shiny
shinyApp(ui = ui, server = server)