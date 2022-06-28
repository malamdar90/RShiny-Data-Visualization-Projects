rm(list = ls())
library(sf)
library(DT)
library(maps)
library(Hmisc)
library(dplyr)
library(shiny)
library(ggmap)
library(scales)
library(plotly)
library(mosaic)
library(ggplot2)
library(ggrepel)
#library(zipcode)
library(treemap)
library(viridis)
library(tidyverse)
library(treemapify)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

#install.packages("treemap")



ui <- dashboardPage(
  header = dashboardHeader(
    title = HTML(
      "<p style='font-family:Lucida Console; font-style:italic; font-weight:900; align:left'>Project_M&M</p>"
    ),
    titleWidth = 230,
    
    tags$li(
      class = "dropdown",
      checked = NA,
      width = 280,
      tags$a(
        tags$i(class = "glyphicon glyphicon-envelope"),
        href = "mailto:lycohh@gmail.com",
        "Contact Us"
      )
    )
    
  ),
  
  footer = dashboardFooter(
    HTML(
      '<p align=center>
            While using this site, you agree to have read and accepted our terms of use, cookie and privacy policy.
            </br> Copyright 2019 by Project_M&M Team. All Rights Reserved.
            </br> &copy Project_M&M
        </p>'
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarSearchForm(
      label = "Enter a keyword",
      textId = "dbSearchTrm",
      buttonId = "dbSearchBtn"
    ),
    
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
                  background-color: rgb(17,64,90);
                   position: fixed;
               }

               /* logo when hovered */
               .skin-blue .main-header .logo:hover {
                   opacity: 0.7;
               }

               /* navbar (rest of the header) */
               .skin-blue .main-header .navbar {
                   background-image: url(https://loeye.com/sites/all/themes/LansingOp/images/bluebg_bar.png);
                   position: fixed;
                   width: 81%;
               }

               /* main sidebar */
               .skin-blue .main-sidebar {
                   background-color: rgb(17,64,90);
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
                   opacity: 0.5;
               }

               /* toggle button when hovered  */
               .skin-blue .main-header .navbar .sidebar-toggle:hover{
                   background-color: dark green;
                   opacity: 0.7;
               }

               /* body */
               .content-wrapper, .right-side {
                   color: rgb(17,64,90);
                   margin-top: 35px;
               }

               /* box background color */
               .box-body, .box-footer {
                   background-color: #E9E7E2;
               }

               /* box footer */
               .box-footer {
                   padding: 0px 0 0px 0;
               }

               /* footer */
               .main-footer {
                   background-image: url(https://loeye.com/sites/all/themes/LansingOp/images/greenfooter_tile2.png);
                   color: rgb(17,64,90);
               }

               /* dataTables */
               #DataTables_Table_0_wrapper, #DataTables_Table_1_wrapper {
                   zoom: 0.73;
                   word-wrap: break-word;
               }

               /* Timelinne */
               .timeline {
                   margin-left: 15px;
               }

               /* StageDevelopment Box*/
               #StageDev {
                   margin-left: 3px;
                   width: 99.7%;
               }

               /* Search Box*/
               #searchbox, .box.box-solid.box-success {
                 visibility: hidden;
               }

               /* EH&EP-h3*/
               #EHh3, #EPh3 {
                 margin-left: 15px;
               }


   '
      )
    )),
    
    #CUSTOMIZED JS
    tags$head(tags$script(src = "js/enter_as_click.js")),
    
    
    tabItems(
      #PAGE1
      tabItem(
        tabName = "page1",
        h1("About the Project"),
        fluidRow(
          #1.1 Initial Idea
          userBox(
            uwidth = 12,
            userDescription(
              title = "Initiative",
              subtitle = "Where the project started from",
              ##
              type = 2,
              image = "https://img.icons8.com/dusk/2x/idea.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-48708C.svg",
            HTML(
              "
                    <p style='color:black;float:left;'>
                      <iframe height='260' src='https://www.youtube.com/embed/f4AI7hD3vGo' frameborder='0' allowfullscreen></iframe>
                      <div style='padding-left:320px;padding-top:25px;font-size:15px;letter-spacing:0.6px'>
                        <p>
                          Medicare and Medicaid have long been controversial topics.
                          While many believe the Medicaid expansion results in gains in coverage and improvements in interoperability for states,
                          some argue that the Medicaid expansion offers poor quality and limited access to providers.
                        </p>

                        <p style='font-size:16px;'> <b> <i>
                          So, what's the effect of Medicare and Medicaid?
                          </br>
                          Do they really promote medical coverage and interoperability?
                        </b> </i> </p>

                        To answer these questions, our team decides to turn to data and examine evidence from there...
                      </div>
                    </p>
                  "
            )
            
            
          ),
          
          
          #1.2 Data & Sources
          userBox(
            width = 12,
            userDescription(
              title = "Datasets",
              subtitle = "What the project worked with",
              ##
              type = 2,
              image = "https://img.icons8.com/dusk/2x/export-csv.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-377483.svg",
            HTML(
              "
                       <p style='color:black'>
                       The raw data were retrieved from
                       <a href='https://www.cms.gov/Regulations-and-Guidance/Legislation/EHRIncentivePrograms/DataAndReports.html'> CMS.gov</a>
                       (U.S. Centers for Medicare & Medicaid Services), a federal agency administering the Medicare program and multiple health related standards.
                       </p>

                       <p style='color:black'>
                       In 2011, CMS established the Medicare and Medicaid EHR Incentive Programs (now known as the Promoting Interoperability programs) to encourage eligible professionals (EPs) and eligible hospitals (EHs) to adopt, implement, upgrade (AIU), and demonstrate meaningful use of certified electronic health record technology (CEHRT).
                       Historically, the Programs consisted of three stages:
                       </p> "
            ),
            
            sliderTextInput(
              "stage",
              "Select stage",
              c("Stage1", "Stage2", "Stage3"),
              selected = "Stage1",
              grid = "show"
            ),
            
            fluidRow(column(width = 4, uiOutput("imgoutput")),
                     column(width = 7, uiOutput("stageoutput"))),
            
            
            HTML(
              "<p style='color:black'>
                       The specific datasets used in this project provided records about eligible professionals and eligible hospitals that have received Medicare Promoting Interoperability Program payments since 2011.
                       With more than 1,000,000 lines of records, we believe these datasets could draw a good picture about how the Promoting Interoperability programs performed so far.
                       </p>"
            ),
            footer = ""
          ),
          
          #1.3 Team
          userBox(
            width = 12,
            userDescription(
              title = "Team",
              subtitle = "Who contributed to the project",
              
              type = 2,
              image = "https://img.icons8.com/dusk/2x/user-group-man-woman.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-18A39D.svg",
            HTML(
              "
                       <li style='color:black'> <b>Ji Huang:</b> Just a learner with incurable curiosity, hoping to explore the beauty of R and even more coding. </li>
                       <li style='color:black'> <b>SiQi Song:</b> Energetic, Sports Lover, and data-driven. Finding data visualization can help people descirbe information elegantly. </li>
                       <li style='color:black'> <b>SongYan Zhao:</b> Curious，dig deeper and find more. Believes data can speak, and visualization is the language. </li>
                       <li style='color:black'> <b>YiMing Zheng:</b> Eat, sleep, and code. Help people in discovering the hidden insight by data visualization. </li>
                       "
            ),
            footer = ""
          )
          
        )
      ),
      
      #PAGE2
      tabItem(
        tabName = "page2",
        h1("Overview"),
        
        #2.1 Data Description
        fluidRow(
          userBox(
            width = 12,
            userDescription(
              title = "Data Description",
              
              subtitle = "About the Datasets",
              ##
              type = 2,
              image = "https://img.icons8.com/doodle/48/000000/search.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-48708C.svg",
            HTML(
              "
                     <p style='color:black'>
                     The project utilizes two datasets, one is eligible professionals(EPs) and the other is eligible hospitals(EHs). Both datasets contain 13 variables, including
                     provider name, provider state, provider city, stage number, program year, payment amount, etc.
                     </p>
                     "
            ),
            
            h4("Eligible Professionals (EP)"),
            DT::dataTableOutput('table1'),
            
            br(),
            
            h4("Eligible Hospitals (EH)"),
            DT::dataTableOutput('table2'),
            
            footer = ""
          ),
          
          #2.2 Sentiment Analysis
          userBox(
            width = 12,
            userDescription(
              title = "Sentiment Analysis",
              subtitle = "Search on Google - Promoting Interoperability Program",
              #
              type = 2,
              image = "https://img.icons8.com/dusk/2x/communication.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-377483.svg",
            
            fluidRow(
              column(width = 5,
                     img(src = "./sentiment.png", width = "450px")),
              column(
                width = 6,
                offset = 1,
                br(),
                br(),
                p(
                  "This word cloud is generated from all the websites that appear on first three pages when you Google 'Promoting Interoperability programs'. We add some visual effects based on sentiment analysis to make it more intuitive."
                ),
                p(
                  "The size of word indicates frequency. The larger one word is, the more times it appears on those websites. Green marks positive opinions and red represents negative ones. We can tell that people are probably in favor of some newly available patient care, while feeling annoyed about some hardship, such as payment issue. Overall, the positive and negative sides seem fairly equal."
                )
              )
            ),
            
            
            
            footer = ""
          ),
          
          
          
          #2.3 Timeline
          userBox(
            width = 12,
            userDescription(
              title = "Timeline",
              subtitle = "Milestones in chronological order",
              ##
              type = 2,
              image = "https://img.icons8.com/dusk/64/000000/bar-chart.png"
            ),
            ##background = TRUE,
            ##backgroundUrl = "https://convertingcolors.com/plain-528E9D.svg",
            
            fluidRow(column(
              width = 12,
              timelineBlock(
                reversed = FALSE,
                timelineEnd(color = "red"),
                
                #Timeline-2011
                timelineLabel(2011, color = "teal"),
                timelineItem(
                  title = "Meaningful Use Stage 1 begins",
                  icon = "thermometer-empty",
                  time = "2011",
                  "27% of hospitals and 34% of providers adopted EHRs"
                ),
                
                #Timeline-2012
                timelineLabel(2012, color = "teal"),
                timelineItem(
                  title = " Healtheway launched",
                  icon = "thermometer-quarter",
                  time = "2012",
                  "The Consolidated Clinical Document Architecture (CDA), a unified
                        standard for summary care records is created"
                  
                ),
                
                #Timeline-2013
                timelineLabel(2013, color = "teal"),
                timelineItem(
                  title = "The Department of Health and Human Services (HHS) HIE Acceleration Strategy Released",
                  icon = "thermometer-half",
                  time = "2013",
                  "51% of hospitals can electronically query other organizations for health information"
                  
                ),
                
                #Timeline-2014
                timelineLabel(2014, color = "teal"),
                timelineItem(
                  title = "Meaningful Use Stage 2 attestations began",
                  icon = "thermometer-three-quarters",
                  time = "2014",
                  "80% of hospitals can electronically query other organizations for health information"
                  
                ),
                
                #Timeline-2015
                timelineLabel(2015, color = "teal"),
                timelineItem(
                  title = "Interoperability Standards Advisory released",
                  icon = "thermometer-full",
                  time = "2015",
                  "Additional State HIE Cooperative Agreement funds awarded for breakthrough innovations"
                  
                ),
                
                #Timeline-2016
                timelineLabel(2016, color = "teal"),
                timelineItem(
                  title = "Still growing",
                  icon = "infinity",
                  time = "2016",
                  "Approximately 78 percent of hospitals electronically sent a summary of care document and 56 percent
                        received a summary of care document"
                  
                )
                
                
              )
            )),
            
            
            footer = ""
          ),
          
          
          
          
          #2.4 Stage Development
          fluidRow(
            id = "StageDev",
            userBox(
              width = 12,
              userDescription(
                title = "Stage Development",
                subtitle = "over 2011-2016",
                ##
                type = 2,
                image = "https://img.icons8.com/dusk/64/000000/line-chart.png"
              ),
              ##background = TRUE,
              ##backgroundUrl = "https://convertingcolors.com/plain-18A39D.svg",
              
              h3("EP Stage Change vs EH Stage Change"),
              img(src = "./epstage.png", width = "450px"),
              img(src = "./ehstage.png", width = "450px"),
              
              
              br(),
              br(),
              br(),
              
              sliderInput(
                "year1",
                "Select year",
                2011,
                2016,
                2011,
                step = 1,
                animate = animationOptions(interval = 2600, loop = TRUE)
              ),
              
              fluidRow(column(
                width = 6, plotOutput(outputId = "OVplot2", height = 460)
              ),
              column(
                width = 6, plotOutput(outputId = "OVplot1", height = 460)
              )),
              
              
              
              footer = ""
            )
          )
        )
      ),
      
      #PAGE3
      tabItem(
        tabName = "page3",
        h1("Eligible Professionals (EPs)"),
        
        fluidRow(
          h3(id = "EPh3",
             "Total Payments"),
          column(
            width = 12,
            tabBox(
              width = 12,
              id = "tabset1",
              height = "160px",
              tabPanel(
                title = "Total Payments",
                radioButtons(
                  inputId = "ep1",
                  label = "",
                  choices = list(
                    "State" = "state",
                    "City" = "city",
                    "Zipcode" = "zipcode"
                  ),
                  selected = "state"
                )
              )
            )
          ),
          br(),
          br(),
          column(width = 12,
                 plotlyOutput(outputId = "epplot1",
                              height = 500)),
          
          
          h3(id = "EPh3",
             "Treemap"),
          column(width = 12,
                 plotOutput(outputId = "epplot2",
                            height = 500)),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          
          h3(id = "EPh3",
             "2011-2016 Annual Payments by State"),
          column(
            width = 12,
            tabBox(
              width = 12,
              id = "tabset1",
              height = "160px",
              tabPanel(
                title = "Annual Payments by State",
                sliderInput(
                  inputId = "ep3",
                  label = "Year",
                  min = 2011,
                  max = 2016,
                  value = 2011,
                  animate = animationOptions(interval = 2600, loop = TRUE)
                )
              )
            )
          ),
          column(width = 12,
                 plotlyOutput(outputId = "epplot3",
                              height = 500)),
          br(),
          br(),
          
          h3(id = "EPh3",
             "2011-2016 Annual Payments by City"),
          column(
            width = 12,
            tabBox(
              width = 12,
              id = "tabset1",
              height = "160px",
              tabPanel(
                title = "Annual Payments by City",
                sliderInput(
                  inputId = "ep4",
                  label = "Year",
                  min = 2011,
                  max = 2016,
                  value = 2011,
                  animate = animationOptions(interval = 2600, loop = TRUE)
                )
              )
            )
          ),
          column(width = 12,
                 plotlyOutput(outputId = "epplot4",
                              height = 500)),
          br(),
          br()
        )
        
      ),
      
      
      #PAGE4
      tabItem(
        tabName = "page4",
        h1("Eligible Hospitals (EHs)"),
        
        fluidRow(
          h3(id = "EHh3",
             "Total Payments"),
          tabBox(width = 12, column(
            width = 12,
            radioButtons(
              "radio1",
              "Year Filter",
              choices = list("With year filter" , "Without year filter"),
              selected = "With year filter"
            )
          )),
          br(),
          br(),
          column(width = 12, plotlyOutput(
            "plot1", height = "370px", width = "100%"
          )),
          
          h3(
            id = "EHh3",
            "Calculated payment for each city's providers in different year"
          ),
          tabBox(width = 12,
                 column(
                   width = 8,
                   sliderInput(
                     "yearSlider",
                     "Start Year:",
                     min = 2011,
                     max = 2016,
                     value = 2011,
                     step = 1,
                     animate = animationOptions(interval = 2600, loop = TRUE)
                   )
                 )),
          
          column(width = 12, plotlyOutput("plot2", height = 370))
          
        )
      ),
      
      #PAGE5
      tabItem(tabName = "page5",
              h1("Summary"),
              fluidRow(
                userBox(
                  width = 12,
                  userDescription(
                    title = "Conclusion",
                    subtitle = "What we learn from the analyses",
                    ##
                    type = 2,
                    image = "https://img.icons8.com/dusk/2x/brief.png"
                  ),
                  ##background = TRUE,
                  ##backgroundUrl = "https://convertingcolors.com/plain-48708C.svg",
                  HTML(
                    "
                    <p> Back to the questions: what's the effect of Medicare and Medicaid? Do they really promote medical coverage and interoperability?</p>
                    <p> Based on previous analyses, we believe the answer is yes: </p>

                    <p>In terms of <b>coverage</b>, the programs covered every state and major cities, as well as the majority of eligible healthcare providers, including hospitals and medical professionals;</p>
                    <p>In terms of <b>interoperability</b>, most eligible hospitals and professionals achieved stage 2, which exceed the minimal technical requirements for inter-operating. With this basis, interoperability across the healthcare industry may only be a matter of time.</p>

                         "
                  )
                )
              )),
      
      #PAGE6
      tabItem(tabName = "page6",
              h1("About"),
              
              fluidRow(
                #6.1 Acknowledgement
                userBox(
                  width = 12,
                  userDescription(
                    title = "Acknowledgement",
                    subtitle = "With our transcending gratitude",
                    ##
                    type = 2,
                    image = "https://img.icons8.com/dusk/2x/idea.png"
                  ),
                  ##background = TRUE,
                  ##backgroundUrl = "https://convertingcolors.com/plain-48708C.svg",
                  HTML(
                    "
                         <p>
                            Throughout the development of this project we have received  a great deal of support and assistance.
                            We would first like to thank our instructor, Dr. Mohammad Ali Alamdar Yazdi, whose expertise was invaluable in the formulating of the project topic and techniques in particular.
                            We also appreciate all professors who equipped us with precious data analytics and network architecture knowledge that widely applied in this project.
                         </p>

                         <p>
                            The project is far from perfection, and we will continuously work to improve it. If you have any comments, thoughts, or suggestions, please feel free to contact us through the link at upper right corner.
                         </p>

                         "
                  )
                ),
                
                #6.2 References
                userBox(
                  width = 12,
                  userDescription(
                    title = "References",
                    subtitle = "Resources and techniques inspired this project",
                    ##
                    type = 2,
                    image = "https://img.icons8.com/dusk/2x/export-csv.png"
                  ),
                  ##background = TRUE,
                  ##backgroundUrl = "https://convertingcolors.com/plain-377483.svg",
                  HTML(
                    "
                         <a href='https://icons8.com/icons/set'>Icons8</a>
                         </br>

                         <a href='https://github.com/gyang274/ygdashboard_app'>ygdashboard</a>
                         </br>

                         <a href='https://convertingcolors.com/'>Converting Colors</a>
                         </br>

                         <a href='https://rstudio.github.io/DT/options.html'>DataTables Options</a>
                         </br>

                         <a href='https://rinterface.com/shiny/shinydashboardPlus/'>shinydashboardPlus</a>
                         </br>

                         <a href='https://fontawesome.com/icons?d=gallery'>Font Awesome Icons</a>
                         </br>

                         <a href='https://shiny.rstudio.com/gallery/widget-gallery.html'>Shiny Widgets Gallery</a>
                         </br>

                         <a href='https://getbootstrap.com/docs/3.3/components/'>Bootstrap - Components</a>
                         </br>

                         <a href='https://adminlte.io/themes/AdminLTE/index2.html'>AdminLTE 2 | Dashboard</a>
                         </br>

                         <a href='https://rdrr.io/'>R Package Documentation</a>
                         </br>

                         <a href='https://divadnojnarg.github.io/blog/awesomedashboards/'>Build awesome dashboards with shiny</a>
                         </br>

                         "
                  )
                ),
                
                box(
                  id = "searchbox",
                  title = "Database Search Box - Title",
                  footer = "Database Search Box - Footer",
                  status = "success",
                  solidHeader = TRUE,
                  background = NULL,
                  width = 6,
                  # height = 400,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  uiOutput("dbSearchRtn")
                )
              ))
    )
  )
)
#  rightsidebar = rightSidebar(),
#  title = "DashboardPage"









server <- function(input, output) {
  #- create text response to user search (hit enter or click with JS)
  create_dbSearchRtn <- eventReactive(input$dbSearchBtn, {
    return(p(
      "The user is searching for ",
      strong(input$dbSearchTrm),
      " ...\n"
    ))
    
  })
  
  output$dbSearchRtn <- renderUI(create_dbSearchRtn())
  
  #StageInfo
  
  stageinfo <- function(info) {
    if (input$stage == "Stage1") {
      d <-
        "set the foundation for the Promoting Interoperability Programs by establishing requirements for the electronic capture of clinical data, including providing patients with electronic copies of health information."
    }
    else if (input$stage == "Stage2") {
      d <-
        " expanded upon the Stage 1 criteria with a focus on advancing clinical processes and ensuring that the meaningful use of EHRs supported the aims and priorities of the National Quality Strategy."
    }
    else if (input$stage == "Stage3") {
      d <-
        "focused on using CEHRT to improve health outcomes and modified Stage 2 to ease reporting requirements and align with other CMS programs."
    }
    return(d)
  }
  
  output$stageoutput <- renderUI({
    wellPanel(style = "background: #E9E7E2; line-spacing: 1.2",
              h3(strong(input$stage)),
              h4(stageinfo(input$stage)))
  })
  
  
  output$imgoutput <- renderUI({
    img(
      src = paste0(input$stage, ".png"),
      align = "center",
      height = 180
    )
    
  })
  
  
  
  #Overview
  eh <- function(year) {
    read.csv("HOSP_ProvidersPaidByEHR_06_2018.csv") -> EH
    input$year1 -> year
    EH <- EH[EH$PROGRAM.YEAR == year,]
    na.omit(EH) -> EH
    EH_groupby <- EH %>%
      group_by(STAGE.NUMBER, PROVIDER.STATE) %>% summarize(N = n())
    EH_groupby
    p <-
      ggplot(EH_groupby, aes(x = PROVIDER.STATE, y = N, fill = STAGE.NUMBER))
    p = p + geom_col(position = "stack") + labs(title = paste0(year, " EH Stage"),
                                                x = "Provider State",
                                                y = "Count")
    p
    p  + coord_flip() -> ploteh
    return(ploteh)
  }
  
  ep <- function(date) {
    input$year1 -> date
    read.csv("EPdata.csv") -> EP
    na.omit(EP) -> EP
    EP <- EP[EP$PROGRAM.YEAR == date,]
    EP_groupby <- EP %>%
      group_by(STAGE.NUMBER, PROVIDER.STATE) %>% summarize(N = n())
    p <-
      ggplot(EP_groupby, aes(x = PROVIDER.STATE, y = N, fill = STAGE.NUMBER))
    p = p + geom_col(position = "stack") + labs(title = paste0(date, "EP Stage"),
                                                x = "Provider State",
                                                y = "Count")
    p
    p  + coord_flip() -> plotep
    return(plotep)
  }
  
  
  output$OVplot1 <- renderPlot({
    eh(input$year1)
  })
  output$OVplot2 <- renderPlot({
    ep(input$year1)
  })
  
  output$table1 <-  DT::renderDataTable({
    read.csv("EPdata.csv") -> EP
    EP[1:5, 1:12] -> EP1
    datatable(
      EP1,
      rownames = FALSE,
      colnames = c(
        'PROVIDER.NPI',
        'NAME',
        'STATE',
        'CITY',
        'ADDRESS',
        'ZIP.5CD',
        'ZIP.4CD',
        'PHONE.NUM',
        'EXT',
        'STAGE',
        'YEAR',
        'CALC.PAYMENT'
      ),
      options = list(
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(list(
          targets = c(1, 4, 7),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 6 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
            "}"
          )
        ))
      )
    )
  })
  
  output$table2 <-  DT::renderDataTable({
    read.csv("EH.csv") -> EH
    EH[1:5, 1:13] -> EH1
    datatable(
      EH1,
      rownames = FALSE,
      colnames = c(
        'PROVIDER.NPI',
        'CCN',
        'NAME',
        'STATE',
        'CITY',
        'ADDRESS',
        'ZIP.5CD',
        'ZIP.4CD',
        'PHONE.NUM',
        'EXT',
        'STAGE',
        'YEAR',
        'CALC.PAYMENT'
      ),
      options = list(
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(list(
          targets = c(2, 5, 8),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 6 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
            "}"
          )
        ))
      )
    )
  })
  
  
  
  #EP
  eps <- read.csv("eps.csv")
  map_eps <- read.csv("mapeps.csv")
  map_epc <- read.csv("mapepc.csv")
  map_epz <- read.csv("mapepz.csv")
  states <- map_data("state") %>% select(1, 2, 3, 5)
  colnames(states) <- c("long1", "lat1", "group", "State")
  state_name <- aggregate(. ~ State, data = states, mean)
  color_for_payment_cities <-
    scale_color_gradient2(low = "white", mid = "slategray1", high = "steelblue")
  color_for_startyear <- scale_colour_brewer(palette = "Set3")
  
  TotalPayment <- function(region) {
    if (region == "state") {
      us_states <- map_data("state")
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
        theme(
          plot.title = element_text(colour = "#2C5364"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent")
        ) +
        scale_fill_gradient(name = "Total Payments", labels = comma)
      #theme(legend.position = "none")
    }
    else if (region == "city") {
      us_states <- map_data("state")
      p <- ggplot(map_epc, aes(lng, lat)) +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          fill = "gray90",
          color = "gray",
          size = 0.1
        ) +
        geom_point(
          position = "jitter",
          data = map_epc,
          aes(
            x = lng,
            y = lat,
            size = Total_Payments,
            color = Total_Payments,
            City = city
          )
        ) +
        scale_size(name = "", range = c(-0.8, 6.5)) +
        color_for_payment_cities +
        guides(size = guide_legend("Payment")) +
        labs(title = "Total Payments by City") +
        theme_map() +
        theme(
          plot.title = element_text(colour = "#2C5364"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent")
        ) +
        coord_equal() +
        coord_map(projection = "albers",
                  lat0 = 39,
                  lat1 = 45)
      #geom_text_repel(data = state_name,aes(x=long1,y=lat1,label = State))
      #theme(legend.position = "none")
    }
    else if (region == "zipcode") {
      us_states <- map_data("state")
      p <- ggplot(map_epz, aes(longitude, latitude)) +
        geom_polygon(
          data = us_states,
          aes(x = long, y = lat, group = group),
          fill = "gray90",
          color = "gray",
          size = 0.1
        ) +
        geom_point(
          position = "jitter",
          data = map_epz,
          aes(
            x = longitude,
            y = latitude,
            size = Total_Payments,
            color = Total_Payments,
            City = city
          )
        ) +
        scale_size(name = "", range = c(-0.8, 6.5)) +
        color_for_payment_cities +
        #guides(size=guide_legend("Payment")) +
        labs(title = "Total Payments by Zipcode") +
        theme_map() +
        theme(
          plot.title = element_text(colour = "#2C5364"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent")
        ) +
        coord_equal() +
        coord_map(projection = "albers",
                  lat0 = 39,
                  lat1 = 45)
      #geom_text_repel(data = state_name,aes(x=long1,y=lat1,label = State))
      #theme(legend.position = "none")
    }
  }
  
  TotalTreemap <- function() {
    eps$region <- capitalize(as.character(eps$region))
    ggplot(eps,
           aes(
             area = eps$Total_Payments,
             fill = eps$Total_Payments,
             label = eps$region
           )) +
      geom_treemap() +
      geom_treemap_text(
        colour = "white",
        place = "centre",
        grow = TRUE,
        min.size = 4,
        max.size = 7
      ) +
      theme_map() +
      theme(
        plot.title = element_text(colour = "#2C5364"),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
      ) +
      scale_fill_gradient(name = "Total Payments", labels = comma)
    #theme(legend.position = "none")
    #max <- max(eps$Total_Payments)
    #min <- min(eps$Total_Payments)
    #treemap(
    #  eps,
    #  index=c("region"),
    #  title = "Total Payments Treemap",
    #  title.legend = "Total Payments",
    #  vSize="Total_Payments",
    #  vColor="Total_Payments",
    #  type="manual",
    #  fontcolor.labels = "black",
    #  border.col='#63B8FF',
    #  palette=c("#FFFFFF00", "#1C86EE00"),
    #  range=c(min, max),
    #  position.legend = "none"
    #)
  }
  
  AnnualPaymentState <- function(year) {
    filename <- paste0(year, "eps.csv")
    epstate <- read.csv(filename)
    epstate$hover <-
      with(epstate,
           paste(region, '<br>', "Total Payments", Total_Payments))
    ti <- paste0("Annual Payments by State in ", year)
    l <- list(color = toRGB("white"), width = 2)
    g3 <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      bgcolor = "#ecf0f5"
    )
    p3 <- plot_geo(epstate, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ Total_Payments,
        text = ~ region,
        locations = ~ code,
        color = ~ Total_Payments,
        colors = 'Blues'
      ) %>%
      colorbar(title = "Millions USD") %>%
      layout(
        title = ti,
        geo = g3,
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)'
        #fig_bgcolor='rgb(255, 255, 255)'
      )
  }
  
  AnnualPaymentCity <- function(year) {
    filename <- paste0(year, "mapepc.csv")
    map_epc <- read.csv(filename)
    us_states <- map_data("state")
    ti <- paste0("Annual Payments by City in ", year)
    p <- ggplot(map_epc, aes(lng, lat)) +
      geom_polygon(
        data = us_states,
        aes(x = long, y = lat, group = group),
        fill = "gray90",
        color = "gray",
        size = 0.1
      ) +
      geom_point(
        position = "jitter",
        data = map_epc,
        aes(
          x = lng,
          y = lat,
          size = Total_Payments,
          color = Total_Payments,
          City = city
        )
      ) +
      scale_size(name = "", range = c(-0.8, 6.5)) +
      color_for_payment_cities +
      guides(size = guide_legend("Payment")) +
      labs(title = ti) +
      theme_map() +
      theme(
        plot.title = element_text(colour = "#2C5364"),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
      ) +
      coord_equal() +
      coord_map(projection = "albers",
                lat0 = 39,
                lat1 = 45) +
      geom_text_repel(data = state_name, aes(x = long1, y = lat1, label = State))
    #theme(legend.position = "none")
  }
  
  output$epplot1 <- renderPlotly({
    TotalPayment(input$ep1)
  })
  output$epplot2 <- renderPlot({
    TotalTreemap()
  })
  output$epplot3 <- renderPlotly({
    AnnualPaymentState(input$ep3)
  })
  output$epplot4 <- renderPlotly({
    AnnualPaymentCity(input$ep4)
  })
  
  
  #EH
  #process states data
  us_states <- map_data("state") %>% select(1, 2, 3, 5)
  colnames(us_states) <- c("long1", "lat1", "group", "State")
  state_name <- aggregate(. ~ State, data = us_states, mean)
  
  color_for_payment_cities <-
    scale_color_gradient2(low = "white", mid = "slategray1", high = "steelblue")
  color_for_startyear <- scale_colour_brewer(palette = "Set3")
  
  providers <- read.csv("processed_data.csv")
  providers$start_year <- as.character(providers$start_year)
  
  p <- ggplot(data = us_states,
              aes(x = long1, y = lat1, group = group)) +
    geom_polygon(fill = "gray90",
                 color = "gray",
                 size = 0.1)
  
  mapplot <- function(color_mode, dataset, class) {
    p +
      geom_point(
        position = "jitter",
        data = dataset,
        aes(
          x = lng,
          y = lat,
          size = Payment,
          color = class,
          City = city,
          State = State
        )
      ) +
      scale_size(name = "", range = c(-0.8, 6.5)) +
      color_mode +
      guides(size = guide_legend("Payment")) +
      labs(title = "Calculated Payment For Each City") +
      theme_map() +
      theme(
        plot.title = element_text(colour = "#2C5364"),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
      ) +
      coord_equal() +
      coord_map(projection = "albers",
                lat0 = 39,
                lat1 = 45) +
      geom_text_repel(data = state_name, aes(x = long1, y = lat1, label = State))
  }
  subData <-
    reactive({
      providers[providers$start_year == input$yearSlider,]
    })
  
  output$plot1 <- renderPlotly({
    if (input$radio1 == "With year filter") {
      the_color_mode <- color_for_startyear
      mapplot(the_color_mode, providers, providers$start_year)
    } else{
      the_color_mode <- color_for_payment_cities
      mapplot(the_color_mode, providers, providers$Payment)
    }
    
  })
  output$plot2 <- renderPlotly({
    the_color_mode1 <- color_for_payment_cities
    mapplot(the_color_mode1, subData(), subData()$Payment)
  })
  
  
  
}
shinyApp(ui = ui, server = server)
