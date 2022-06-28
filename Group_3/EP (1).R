rm(list = ls())
library(DT)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(socviz)
library(maps)
library(mosaic)
library(ggmap)
library(Hmisc)
#library(zipcode)
library(treemap)
library(treemapify)
library(viridis)
#install.packages("shinyWidgets")



ui <- dashboardPage(
  header = dashboardHeader(
    title = HTML(
      "<p style='font-family:Lucida Console; font-style:italic; font-weight:900; align:left'> ProjectName</p>"
    ),
    titleWidth = 230,
    
    tags$li(
      class = "dropdown",
      checked = NA,
      width = 280,
      tags$a(
        tags$i(class = "glyphicon glyphicon-envelope"),
        href = "",
        "Contact Us"
      )
    )
    
  ),
  
  footer = dashboardFooter(
    HTML(
      '<p align=center>
            While using this site, you agree to have read and accepted our terms of use, cookie and privacy policy.
            </br> Copyright 2019 by XXX Team. All Rights Reserved.
            </br> &copy ProjectName
        </p>'
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarSearchForm(label = "Enter a keyword", "searchText", "searchButton"),
    
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("Overview", tabName = "page2", icon = icon("dashboard")),
      menuItem(
        "Eligible Professionals(EP)",
        tabName = "page3",
        icon = icon("user-md")
      ),
      menuItem(
        "Eligible Hospitals(EH)",
        tabName = "page4",
        icon = icon("hospital-alt")
      ),
      menuItem("Summary", tabName = "page5", icon = icon("list-alt")),
      menuItem("About", tabName = "page6", icon = icon("info-circle"))
    )
  ),
  
  body = dashboardBody(
    #CUSTOMIZED CSS
    tags$head(tags$style(
      HTML(
        '
                  /* logo */
                  .skin-blue .main-header .logo {
                      background-image: linear-gradient(#0F2027,#203A43);
                      position: fixed;
                  }

                  /* logo when hovered */
                  .skin-blue .main-header .logo:hover {
                      opacity: 0.7;
                  }

                   /* navbar (rest of the header) */
                  .skin-blue .main-header .navbar {
                      background-image: linear-gradient(#0F2027,#203A43);
                      position: fixed;
                      width: 81%;
                  }

                  /* main sidebar */
                  .skin-blue .main-sidebar {
                      background-image: linear-gradient(#203A43,#2C5364);
                      position: fixed;
                  }

                  /* main siderbar search */
                  .skin-blue .sidebar-form .btn, .skin-blue .sidebar-form input[type=text] {
                      background-color: #2C5364;
                      color: white;
                  }

                  /* active selected tab in the sidebarmenu */
                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                      background-color: dark green;
                      opacity: 0.7;
                  }

                  /* other links in the sidebarmenu */
                  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                      background-color: NULL;
                      color: white;
                  }

                  /* other links in the sidebarmenu when hovered */
                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                      background-color: dark green;
                      opacity: 0.7;
                  }

                  /* toggle button when hovered  */
                  .skin-blue .main-header .navbar .sidebar-toggle:hover{
                      background-color: dark green;
                      opacity: 0.7;
                  }

                  /* body */
                  .content-wrapper, .right-side {
                      background-image: linear-gradient(#203A43,#2C5364);
                      color: #C7C7C7;
                      margin-top: 35px;

                  }

                  /* footer */
                  .main-footer {
                      background-color: #2C5364;
                      color: #C7C7C7;
                  }

   '
      )
    )),
    
    
    tabItems(
      #PAGE1
      tabItem(
        tabName = "page1",
        h1("About the Project"),
        fluidRow(
          #1.1 Initial Idea
          userBox(
            title = userDescription(
              "Initial Idea",
              subtitle = "Where the project started from",
              ##width = 12,
              type = 2,
              image  = "https://img.icons8.com/dusk/2x/idea.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/gradient-3C8DBC.svg",
            HTML("
                       <p style='color:black'>
                          Some text here!
                       </p>"),
            footer = ""
          ),
          
          
          #1.2 Data and Sources
          userBox(
            userDescription(
              title = "Data and Sources",
              subtitle =  "What the project worked with",
              ##width = 12,
              type = 2,
              image = "https://img.icons8.com/dusk/2x/export-csv.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/gradient-3C8DBC.svg",
            HTML(
              "
                       <p style='color:black'>
                          The raw data were retrieved from
                          <a href='https://www.cms.gov/Regulations-and-Guidance/Legislation/EHRIncentivePrograms/DataAndReports.html'> CMS.gov </a>
                          (Centers for Medicare & Medicaid Services), a federal agency administering the Medicare program and multiple health related standards.
                       </p>

                       <p style='color:black'>
                          In 2011, CMS established the Medicare and Medicaid EHR Incentive Programs (now known as the Promoting Interoperability programs) to encourage eligible professionals (EPs) and eligible hospitals (EHs) to adopt, implement, upgrade (AIU), and demonstrate meaningful use of certified electronic health record technology (CEHRT).
                          Historically, the Programs consisted of three stages:
                          <ul style='color:black'>
                          <li> <b> Stage 1 </b> set the foundation for the Promoting Interoperability Programs by establishing requirements for the electronic capture of clinical data, including providing patients with electronic copies of health information. </li>
                          <li> <b> Stage 2 </b> expanded upon the Stage 1 criteria with a focus on advancing clinical processes and ensuring that the meaningful use of EHRs supported the aims and priorities of the National Quality Strategy. </li>
                          <li> <b> Stage 3 </b> focused on using CEHRT to improve health outcomes and modified Stage 2 to ease reporting requirements and align with other CMS programs. </li>
                          </ul>
                       </p>

                       <p style='color:black'>
                          The specific datasets used in this project provided records about eligible professionals and eligible hospitals that have received Medicare Promoting Interoperability Program payments since 2011.
                          With more than 【】 lines of records,
                       </p>"
            ),
            footer = ""
          ),
          
          #1.3 Credit
          userBox(
            userDescription(
              title = "Credit",
              subtitle = "Who contributed to the project",
              ##width = 12,
              type = 2,
              image = "https://img.icons8.com/dusk/2x/user-group-man-woman.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/gradient-3C8DBC.svg",
            HTML(
              "
                       <li style='color:black'> JH </li>
                       <li style='color:black'> SQS </li>
                       <li style='color:black'> SYZ </li>
                       <li style='color:black'> YMZ </li>
                       "
            ),
            footer = ""
          )
          
        )
      ),
      
      #PAGE2
      tabItem(tabName = "page2",
              h1("Overview"),
              
              #2.0 Timeline
              fluidRow(column(
                width = 6,
                timelineBlock(
                  reversed = FALSE,
                  timelineEnd(color = "red"),
                  
                  #Timeline-2018
                  timelineLabel(2018, color = "teal"),
                  timelineItem(
                    title = "Item 1",
                    icon = "gears",
                    time = "now",
                    footer = "Here is the footer",
                    "This is the body"
                  ),
                  
                  #Timeline-2017
                  timelineLabel(2017, color = "teal"),
                  timelineItem(
                    title = "Item 1",
                    icon = "gears",
                    time = "now",
                    footer = "Here is the footer",
                    "This is the body"
                  ),
                  
                  #Timeline-2016
                  timelineLabel(2016, color = "teal"),
                  timelineItem(
                    title = "Item 1",
                    icon = "gears",
                    time = "now",
                    footer = "Here is the footer",
                    "This is the body"
                  ),
                  
                  #Timeline-2015
                  timelineLabel(2015, color = "teal"),
                  timelineItem(
                    title = "Item 1",
                    icon = "gears",
                    time = "now",
                    footer = "Here is the footer",
                    "This is the body"
                  )
                )
              ))),
      
      #PAGE3
      tabItem(
        tabName = "page3",
        h1("Eligible Professionals (EPs)"),
        
        h3("Total Payments"),
        column(
          width = 4,
          tabBox(
            width = 12,
            id = "tabset1",
            height = "250px",
            tabPanel(
              title = "Total Payments",
              radioButtons(
                inputId = "ep1",
                label = "State VS City",
                choices = list(
                  "State" = "state",
                  "City" = "city",
                  "Zipcode" = "zipcode",
                  "Treemap" = "treemap"
                ),
                selected = "state"
              )
            )
          )
        ),
        column(width = 8,
               box(
                 width = 12,
                 plotOutput(outputId = "epplot1",
                            height = 500)
               )),
        br(),
        br(),
        
        h3("2011-2016 Annual Payments by State"),
        column(
          width = 4,
          tabBox(
            width = 12,
            id = "tabset1",
            height = "250px",
            tabPanel(
              title = "Annual Payments by State",
              sliderInput(
                inputId = "ep2",
                label = "Year",
                min = 2011,
                max = 2016,
                value = 2011
              )
            )
          )
        ),
        column(width = 8,
               box(
                 width = 12,
                 plotOutput(outputId = "epplot2",
                            height = 500)
               )),
        br(),
        br(),
        
        h3("2011-2016 Annual Payments by City"),
        column(
          width = 4,
          tabBox(
            width = 12,
            id = "tabset1",
            height = "250px",
            tabPanel(
              title = "Annual Payments by City",
              sliderInput(
                inputId = "ep3",
                label = "Year",
                min = 2011,
                max = 2016,
                value = 2011
              )
            )
          )
        ),
        column(width = 8,
               box(
                 width = 12,
                 plotOutput(outputId = "epplot3",
                            height = 500)
               )),
        br(),
        br()
        
      ),
      
      #PAGE4
      tabItem(
        tabName = "page4",
        h1("Eligible Hospitals (EHs)"),
        
        h3(
          "Total Payments by City  VS  Total Payments by Zipcode (select using radio button)"
        ),
        img(src = "./EH1city.png", width = "450px"),
        img(src = "./EH1zip.png", width = "450px"),
        br(),
        br(),
        
        h3(
          "Start Year & Total Payments Received by City  VS  by Zipcode (select using radio button)"
        ),
        img(src = "./EH2city.png", width = "450px"),
        img(src = "./EH2zip.png", width = "450px"),
        br(),
        br(),
        
        h3(
          "2011-2016 Annual Payments by City  VS by Zipcode (combine into one graph and control year by slider)"
        ),
        img(src = "./EH2011.png", width = "300px"),
        img(src = "./EH2012.png", width = "300px"),
        img(src = "./EH2013.png", width = "300px"),
        br(),
        img(src = "./EH2014.png", width = "300px"),
        img(src = "./EH2015.png", width = "300px"),
        img(src = "./EH2016.png", width = "300px"),
        br(),
        br()
        
      ),
      
      #PAGE5
      tabItem(tabName = "page5", h1("Summary")),
      
      #PAGE6
      tabItem(tabName = "page6", h1("About"))
    )
  )
)










server <- function(input, output) {
  eps <- read.csv("eps.csv")
  map_eps <- read.csv("mapeps.csv")
  map_epc <- read.csv("mapepc.csv")
  map_epz <- read.csv("mapepz.csv")
  us_states <- map_data("state")
  TotalPayment <- function(region) {
    if (region == "state") {
      ggplot(data = map_eps,
             aes(
               x = long,
               y = lat,
               group = group,
               fill = map_eps$Total_Payments
             )) +
        geom_polygon(color = "gray90", size = 0.1) + # polygon outline
        labs(title = "Total Payments by State", fill = NULL) + # labels
        coord_equal() + #does let the relative scale of the map change
        coord_map(projection = "albers",
                  lat0 = 39,
                  lat1 = 45) + # gives the curve to the map
        theme_map() + # gets rid of x,y axis, labels, and the background
        theme(legend.position = "none")
    }
    else if (region == "city") {
      ggplot(map_epc, aes(lng, lat)) +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          color = 'gray',
          fill = NA,
          alpha = .35
        ) +
        geom_point(
          data = map_epc,
          aes(
            x = lng,
            y = lat,
            size = map_epc$Total_Payments,
            fill = NULL,
            group = NULL
          ),
          color = "blue"
        ) +
        labs(title = "Total Payments by City", fill = NULL) + # labels
        coord_equal() + #does let the relative scale of the map change
        coord_map(projection = "albers",
                  lat0 = 39,
                  lat1 = 45) + # gives the curve to the map
        theme_map() +
        theme(legend.position = "none")
    }
    else if (region == "zipcode") {
      ggplot(map_epz, aes(longitude, latitude)) +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          color = 'gray',
          fill = NA,
          alpha = .35
        ) +
        geom_point(aes(color = map_epz$Total_Payments),
                   size = .25,
                   alpha = .25) +
        labs(title = "Total Payments by Zipcode", fill = NULL) + # labels
        coord_equal() + #does let the relative scale of the map change
        coord_map(projection = "albers",
                  lat0 = 39,
                  lat1 = 45) + # gives the curve to the map
        theme_map() +
        theme(legend.position = "none")
    }
    else if (region == "treemap") {
      #ep1 <- data.frame(aggregate(ep$CALC.PAYMENT.AMT...., list(ep$region), sum))
      #names(ep1) <- c("region", "Total_Payments")
      #ggplot(ep1, aes(area = ep1$Total_Payments, fill = ep1$region, label = ep1$region)) +
      #  geom_treemap() +
      #  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
      #                    grow = TRUE)
      eps$region <- capitalize(as.character(eps$region))
      max <- max(eps$Total_Payments)
      min <- min(eps$Total_Payments)
      treemap(
        eps,
        index = c("region"),
        title = "Total Payments Treemap",
        title.legend = "Total Payments",
        vSize = "Total_Payments",
        vColor = "Total_Payments",
        type = "manual",
        fontcolor.labels = "black",
        border.col = '#63B8FF',
        palette = c("#FFFFFF00", "#1C86EE00"),
        range = c(min, max),
        position.legend = "none"
      )
    }
  }
  
  AnnualPaymentState <- function(year) {
    filename <- paste0(year, "mapeps.csv")
    map_eps <- read.csv(filename)
    ti <- paste0("Annual Payments by State in ", year)
    ggplot(data = map_eps,
           aes(
             x = long,
             y = lat,
             group = group,
             fill = map_eps$Total_Payments
           )) +
      geom_polygon(color = "gray90", size = 0.1) + # polygon outline
      labs(title = ti, fill = NULL) + # labels
      coord_equal() + #does let the relative scale of the map change
      coord_map(projection = "albers",
                lat0 = 39,
                lat1 = 45) + # gives the curve to the map
      theme_map() + # gets rid of x,y axis, labels, and the background
      theme(legend.position = "none")
  }
  
  AnnualPaymentCity <- function(year) {
    filename <- paste0(year, "mapepc.csv")
    map_epc <- read.csv(filename)
    ti <- paste0("Annual Payments by City in ", year)
    ggplot(map_epc, aes(lng, lat)) +
      geom_polygon(
        data = us_states,
        aes(x = long, y = lat, group = group),
        color = 'gray',
        fill = NA,
        alpha = .35
      ) +
      geom_point(
        data = map_epc,
        aes(
          x = lng,
          y = lat,
          size = map_epc$Total_Payments,
          fill = NULL,
          group = NULL
        ),
        color = "blue"
      ) +
      labs(title = ti, fill = NULL) + # labels
      coord_equal() + #does let the relative scale of the map change
      coord_map(projection = "albers",
                lat0 = 39,
                lat1 = 45) + # gives the curve to the map
      theme_map() +
      theme(legend.position = "none")
  }
  
  output$epplot1 <- renderPlot({
    TotalPayment(input$ep1)
  })
  output$epplot2 <- renderPlot({
    AnnualPaymentState(input$ep2)
  })
  output$epplot3 <- renderPlot({
    AnnualPaymentCity(input$ep3)
  })
}
shinyApp(ui = ui, server = server)
