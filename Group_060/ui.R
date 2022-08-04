
ui <- dashboardPage(
  header = dashboardHeader(title = "Air Travel in Covid-19"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "Overview"),
      menuItem(
        "About Covid-19",
        tabName = "About-Covid-19",
        menuSubItem("Confirmed cases by countries", tabName = "subitem1"),
        menuSubItem("Travel bans by countries", tabName = "subitem2")
      ),
      menuItem("Airline Trending", tabName = "Airline-Trending"),
      menuItem("Airline Map", tabName = "Airline-Map")
    )
  ),
  body = dashboardBody(tabItems(
    tabItem("Overview",
            fluidRow(
              box(
                title = "Abstract",
                solidHeader = TRUE,
                status = "success",
                width = 12,
                collapsible = TRUE,
                column(
                  12,
                  tags$div(
                    "Over the past two years, the covid-19 pandemic has harmed the whole world. In order to control the situation and avoid virus transmission due to crowding, countries have issue travel restrictions. In this case, the number of flights around the world decreased dramatically. 

Thus, our group want to analyze and visualize the flight data, and thus help businessmen to make flight decisions and help policymakers to mitigate the resulting threats to public health caused by the air flights.
"
                  ),
                  style = "font-size:14px"
                )
              ),
              box(
                title = "About the dataset",
                solidHeader = TRUE,
                status = "danger",
                width = 12,
                collapsible = TRUE,
                column(12, tags$div(
                  paste0(
                    "The dataset contains information derived and cleaned ",
                    "from the full OpenSky dataset to illustrate the development ",
                    "of air traffic during the COVID-19 pandemic. ",
                    "Dataset includes callsign, number, typecode, origin, destination, day and etc. ",
                    "The data spans all flights seen by the network's more ",
                    "than 2500 members since 1 January 2020.Source: ",
                    "Crowdsourced air traffic data from The OpenSky Network 2020"
                  )
                ), style = "font-size:14px")
              )
            )),
    tabItem("subitem1",
            fluidRow(
              box(
                width = 4,
                height = "630px",
                radioButtons(
                  "covid_status",
                  "Pleace select a type ",
                  choices = covid_types,
                  selected = "confirmed",
                  width = "100%",
                  inline = TRUE
                ),
                valueBoxOutput("covid_confirmed", width = "100%"),
                valueBoxOutput("covid_recovered", width = "100%"),
                valueBoxOutput("covid_death", width = "100%")
              ),
              box(
                width = 8,
                height = "630px",
                leafletOutput("covid_map", width = "100%", height = "600px")
              )
            )),
    tabItem("subitem2",
            fluidRow(
              box(
                width = 12,
                column(
                  width = 6,
                  selectInput(
                    "travel_country",
                    "Pleace select a County ",
                    choices = travel_countrys,
                    width = "100%",
                    selected = "All Country"
                  )
                ),
                column(width = 6, uiOutput("travel_airline_ui", width = "100%"))
              ),
              box(width = 12, DT::dataTableOutput("travel_dt"))
            )),
    tabItem("Airline-Trending", fluidRow(box(
      width = 12,
      box(width = 6, img(
        src = "plot_01.png",
        width = "100%",
        height = "100%"
      )),
      box(width = 6, img(
        src = "plot_02.png",
        width = "100%",
        height = "100%"
      )),
      box(width = 6, img(
        src = "plot_03.png",
        width = "100%",
        height = "100%"
      )),
      box(width = 6, img(
        src = "plot_04.png",
        width = "100%",
        height = "100%"
      )),
      box(
        title = "Reference",
        status = "danger",
        width = 12,
        collapsible = TRUE,
        column(
          12,
          tags$div(
            "Kuan, C. (2020, September 3). Visualization of Air Traffic during Covid-19 Pandemic. Medium. https://towardsdatascience.com/visualization-of-air-traffic-during-covid-19-pandemic-c5941b049401"
          )
        ),
        style = "font-size:12px"
      )
    ))),
    tabItem("Airline-Map", fluidRow(box(
      width = 12,
      height = "100%",
      box(
        width = 4,
        selectInput(
          "airline_map_region",
          "Pleace select a Region ",
          choices = regions,
          selected = "All Regions",
          width = "100%"
        ),
        uiOutput("airline_map_country", width = "100%"),
        uiOutput("airline_map_date", width = "100%")
      ),
      box(
        width = 8,
        leafletOutput("airline_map", width = "100%", height = "600px")
      )
    )))
  )),
  skin = "purple" # Set header colour scheme
)