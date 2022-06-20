# load packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)
library(data.table)
library(wordcloud2)
library(tm)

data_final <- read.csv("data_final.csv")
setnames(data_final, "Q8", "Job")
setnames(data_final, "Q9", "Industry")

data_final[data_final=="2+, non-Hispanic"] <- "Multiracial"
data_final[data_final=="Asian, non-Hispanic"] <- "Asian"
data_final[data_final=="Black, non-Hispanic"] <- "Black"
data_final[data_final=="Other, non-Hispanic"] <- "Other"
data_final[data_final=="White, non-Hispanic"] <- "White"

data_final_v6 <- pivot_longer(data_final, c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q3_10"), names_to = "Priority", values_to = "Evaluation")
data_final_v7 <- pivot_longer(data_final_v6, c("Q2AL","Q2L","Q2Z","Q2AF","Q2AP","Q2J","Q2X","Q2K","Q2P","Q2Q","Q2G","Q2W","Q2AJ","Q2AN","Q2I","Q2M","Q2A","Q2C","Q2S"), names_to = "Quality", values_to = "Rating")

data_final_v7 <- data_final_v7 %>%
  filter(Evaluation=="Yes" & Rating>3)

mydashboardheader <- function(..., title = "Introspection", disable = FALSE, title.navbar=NULL, .list = NULL) {
  items <- c(list(...), .list)
  tags$header(class = "main-header",
              style = if (disable) "display: none;",
              span(class = "logo", title),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       # Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       title.navbar,
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                           )
                       )
              )
  )
}

ui <- dashboardPage(
  mydashboardheader(title="Introspection"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Creativity", tabName = "creativity", icon = icon("paint-brush")),
      menuItem("Motivations", tabName = "motivations", icon = icon("bullseye")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("hourglass-end")),
      menuItem("Biography", tabName = "contact", icon = icon("smile"))
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #764545;
                              color: white;
                              font-family: helvetica;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #764545;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #ac8f8f;
                              }
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #764545;
                              font-family: helvetica;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f8f3f3;
                              color:black;
                              }
                              
                              /* sidebar when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                              color:white;
                              background-color:  #764545;
                              }
                              
                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #f8f3f3;
                              backround-size: cover
                              }'))),
    
    
    
    tabItems(
      tabItem("home",
                img(src="collage1.png", width="100%", height="100%"), align="center"),
      
      tabItem("about", 
              fluidRow(
                img(src="collage2.png", width="95%"), align="center"),
              
              fluidRow(
                column(12,
                panel(icon("question-circle"), "Background",
                      style="font-size:18px; background-color:#ac8f8f", align="center"), 
                
                    "I designed this Shiny web application to encourage users to reflect on their creativity, personalities, strengths, and weaknesses, among other attributes. 
                    To begin this initiative, I sourced survey data (n = 3,447) on self-perceptions of creativity from the Inter-university Consortium for Political and Social Research (ICPSR) at University of Michigan. 
                    Respondents were asked a variety of questions about their demographics, demeanors, dispositions, and priorities, to name a few. 
                    The majority of questions required users to select one or more options from a list of predefined responses (e.g. demographic information, level of agreement, creativity rating, etc.). ",
                    style="font-size:14px; background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px", align="left")),
                
              fluidRow(
                column(12,
                       panel(icon("bullseye"), "Goals",
                             style="font-size:18px; background-color:#ac8f8f", align="center"), 
                       
                       "Using ICPSR's survey data, I aim to accomplish three goals:",
                       br(),
                       br(),
                       "1. Show users that creativity means something different to everyone. 
                       To showcase the multifaceted nature of this word, users are encouraged to interact with the word cloud under the 'Creativity' tab.
                       Users can move the slider to control the number of words that are displayed at a time. 
                       Hovering the mouse over the words in the word cloud will present the number of times that word appeared in the corresponding survey response.",
                       br(),
                       br(),
                       "2. Prompt users to evaluate their own creativity. 
                       Under the 'Creativity' tab, users are presented with 8 activities, each of which is accompanied by 5 radio buttons that represent the user's self-perception of creativity compared to others.
                       In other words, for each activity, users select an integer between 1 and 5 to represent their self-rating of creativity in relation to demographically similar individuals.
                       With each selected radio button, the figure to the right will update in real time so users can see their overall distribution of responses.
                       When users are finished with their ratings, they are invited to click on the checkbox under the plot to view average responses of survey respondents.
                       Users are then able to visualize how their self-perceptions of creativity fare with those of the survey respondents.",
                       br(),
                       br(),
                       "3. Assist users with career planning. Users are guided through a questionnaire on demographics, personality traits, and priorities in the job selection process.
                       When users have answered all questions, they will then click the submit button and view recommended industries and potentially specific jobs to pursue. 
                       Recommendations are based on respondents who share the same demographics, personality traits, and priorities.",
                       style="font-size:14px; background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px", align="left")),
              
              fluidRow(
                column(12,
                       panel(icon("edit"), "Methodology",
                             style="font-size:18px; background-color:#ac8f8f", align="center"),
                       "This Shiny application involved rigorous data cleaning and manipulation processes. Survey data initially contained almost 3,500 rows and 258 columns.
                       Because variables were labeled with question ID number, I closely relied on the data dictionary to group variables according to question type (e.g. demographics, personality traits, etc.).
                       After doing so, I conducted an extensive exploratory data analysis process using various chart types to understand the characteristics of respondents and overall patterns and trends.
                       Subsequently, I subsetted the data, selecting a handful of variables from each group using an intuitive and inclusive mindset.
                       All data cleaning reduced the original dataset to nearly 1,679 rows and 120 columns. 
                       Data was further manipulated and subsetted to answer specific research questions. 
                       Specifically, questions that asked respondents for a level of agreement recorded responses in character format (e.g. agree, disagree). 
                       For ease and clarity of analysis, such responses were converted to numeric format, where each level of agreement or verbal rating was translated to a number between 1 and 5.
                       The key for numeric ratings is displayed as appropriate throughout the application. Other responses were also condensed to eliminate unnecessary numbering.",
                       "The application itself was developed using R Shiny. Shiny contains imbedded elements of HTML and CSS to enable the user to solely utilize R functions.
                       However, I included some CSS code to customize certain aspects of the webpage.", 
                       style="font-size:14px; background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px", align="left")),
              fluidRow(
                column(12,
                       panel(icon("pencil-alt"), "Initial Sketches",
                             style="font-size:18px; background-color:#ac8f8f", align="center"),
                       "The sketches below reflect the brainstorming process for designing this web application.", 
                       style="font-size:14px; background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px", align="left",
                       img(src="collage4.png", width="100%"), align="center")),
              fluidRow(
                column(12,
                       panel(icon("book"), "References",
                             style="font-size:18px; background-color:#ac8f8f", align="center"),
                       "Novak-Leonard, J. L., Rugg, G., Robinson, M., & Bradburn, N., (2018).", em("Self-perceptions of creativity & arts participation, United States, 2018"), "(ICPSR 37853; Version V1) [Data set]. ICPSR. https://doi.org/10.3886/ICPSR37853.v1",
                       br(),
                       br(),
                       "Novak-Leonard, J. L,, Rugg, G., Robinson, M., & Bradburn, N. (2019, March 6).", em("National survey: Self-perceptions of creativity & arts participation."), "National Archive of Data on Arts & Culture. https://www.icpsr.umich.edu/web/NADAC/studies/37853/datadocumentation",
                       style="font-size:14px; background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px", align="left"))
              
                
              ),
      tabItem("dataset",
              fluidRow(
              panel(icon("line-chart"), "Data Description",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
              "As mentioned in the About section of this web application, the dataset (n=3,447) used for this project was sourced from the Inter-university Consortium for Political and Social Research.
              Surveyers distributed surveys to adults of various demographics, who were instructed to answer questions in 'six major creative domains: 
              artistic creativity, creativity in math/science, creativity in business/entrepreneurship, creativity in social settings, creativity in civic settings, and creativity in everyday activities.'
              Many questions presented users with a personality trait or activity and asked users to indicate the extent to which they embody that trait or how creative they are at performing the activity compared to demographically similar individuals.",
              br(),
              br(),
              "The vast majority of questions required users to select an answer choice from a predefined list; however, some questions required users to type in a response.
              For ease of analysis, levels of agreement with various statements were converted to a numeric scale, with 1 representing strong disagreement and 5 representing strong agreement. 
              These values are reflected in the data table below. The data table below is a subset of the larger dataset with 12 columns visible. 
              To prevent clutter, columns are labeled according to question ID. 
              Clicking on the links below will allow you to download the entire cleaned data file and data dictionary, respectively.",
              br(),
              br(),
              "To download the cleaned dataset, click", downloadLink("downloadData", label="here.", style="color: blue"),
              br(),
              "To download the data dictionary, click", tags$a("here.", href="37853-0001-Codebook-ICPSR.pdf", style="color: blue"),
                    style="background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px"),
              fluidRow(
                panel(icon("table"), "Dataset",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
              dataTableOutput('table1'),
              style="background-color:#f8f3f3; padding-left:50px; padding-right:50px; padding-top:10px")),

      tabItem("demographics", 
              fluidRow(
              panel(icon("list-ul"), "Fast Facts",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
              infoBox("Female", "58%", icon=icon("venus"), color="purple", width=4),
              infoBox("Avg. Children per Household", "0.66", icon=icon("child"), color="aqua", width=4),
              infoBox("Married", "50%", icon=icon("heart"), color="red", width=4),
              infoBox("< $100k Annual Income", "72%", icon=icon("dollar-sign"), color="green", width=4),
              infoBox("White, non-Hispanic", "62%", icon=icon("user-alt"), color="yellow", width=4),
              infoBox("25-64 Years Old", "88%", icon=icon("sort-numeric-up-alt"), color="light-blue", width=4),
              em(h5("Note: All percentages above reflect the most common characteristics of survey respondents.
                    For a more complete picture of respondent characteristics, please view the figures below.", align="left"))),
              br(),
              fluidRow(
                panel(icon("line-chart"), "Further Exploration",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
                box(title = "Household Size",
                    status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotOutput("plot1")),
                box(title = "Marital Status",
                    status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotOutput("plot2")),
                box(title = "Race & Ethnicity",
                    status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotOutput("plot3")),
                box(title = "Age",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotOutput("plot4"))),
              style="padding-left:50px; padding-right:50px; padding-top:10px"),
      tabItem("creativity",
               fluidRow(
                 panel(icon("rainbow"), "The Many Meanings of a Single Word",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
                column(5,
                       panel("When you hear the word 'creativity', what do you think of?",
                             h5("Question is taken directly from original survey.", style="font-size:11px"),
                       br(),
                       br(),
                       sliderInput("wc", "Number of words to display:", min=25, max=150, value=25, step=10)),
                       "What does creativity mean to you?"),
                column(7,
              wordcloud2Output("wordcloud", width="100%"))),
              br(),
              fluidRow(
                panel(icon("check-square"), "Evaluate your Own Creativity",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
                column(5,
                       panel(
                         h5(strong("Creative Self-Assessment")),
                  h5("Compared to people of approximately your age and life experience, how would you rate yourself for each of the following activities?"),
                  h5("Question is taken directly from original survey.", style="font-size:11px"),
                  h5("(1 = Much less creative, 5 = Much more creative)"),
                  ### radio buttons
                  radioButtons("radio1", h5("1. Writing a poem"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                   radioButtons("radio2", h5("2. Thinking of many different solutions to a problem"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio3", h5("3. Thinking of a new invention"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio4", h5("4. Solving math puzzles"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio5", h5("5. Mediating a dispute or argument between two friends"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio6", h5("6. Making up lyrics to a song"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio7", h5("7. Communicating with people from different cultures"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),
                  
                  radioButtons("radio8", h5("8. Building something mechanical, like a robot"),
                               choices = list("1" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5), 
                               inline=TRUE,
                               selected=1),

                  style="padding-left:15px; padding-right:15px")),
                column(7,
                       panel(
                  h5(strong("Your Results")),
                  plotOutput("comparison", width="100%"), 
                  checkboxInput("view","View Average Results")),
                  h5("Are you surprised with the results?"),
                  style="padding-right:15px")),
              style="padding-left:50px; padding-right:50px; padding-top:10px"),
      tabItem("motivations",
              fluidRow(
                panel(icon("briefcase"),"Career Aspirations",
                    style="font-size:18px; background-color:#ac8f8f", align="center"),
                h5("Answer the following questions to the best of your ability. 
                   Then scroll down to the bottom of the page to view industries and/or jobs that may be suitable for you.", style="padding-bottom:15px"),
                column(4,
                       panel(h5(strong("Demographic Information")),
                             h5("Select the option that best describes your..."),
                             selectInput("select_gender", h5("Gender"),
                                         choices = unique(data_final$GENDER)),
                             
                       selectInput("select_edu", h5("Education"),
                                   choices = list("No high school diploma","High school graduate or equivalent","Some college","College graduate or above")),
                       
                       selectInput("select_age", h5("Age"),
                                   choices = list("18-24","25-34","35-44","45-54","55-64","75+")),
                       
                       selectInput("select_inc", h5("Desired Income"),
                                   choices = unique(data_final$INCOME)),
                       style="height:680px")),
                
                column(4,
                       panel(h5(strong("Qualities")),
                h5("Select the top 3 personal qualities you would use to describe yourself."),
                checkboxGroupInput("qualities", "",
                            choices = list("Accountable" = "Q2AL","Argumentative" = "Q2L","Confident" = "Q2Z",
                                           "Considerate" = "Q2AF","Cooperative" = "Q2AP","Curious" = "Q2J",
                                           "Emotionally Stable" = "Q2X","Energetic" = "Q2K","Enthusiastic" = "Q2P",
                                           "Forgiving" = "Q2Q","Helpful" = "Q2G","Lazy" = "Q2W","Outgoing" = "Q2AJ","Reflective" = "Q2AN",
                                           "Relaxed" = "Q2I","Reliable" = "Q2M","Talkative" = "Q2A","Thorough" = "Q2C","Worried Often" = "Q2S"),
                            selected=c("Q2AL","Q2L","Q2Z","Q2AF","Q2AP","Q2J","Q2X","Q2K","Q2P","Q2Q","Q2G","Q2W","Q2AJ","Q2AN","Q2I","Q2M","Q2A","Q2C","Q2S")),
                       style="height:680px")),
                
                
                column(4,
                       panel(h5(strong("Priorities")),
                             h5("Select your top two priorities when picking a job or career."), 
                             checkboxGroupInput("select_priorities", "",
                                                choices = list("Making a lot of money" = "Q3_1", "Opportunities to be original and creative" = "Q3_2",
                                                               "Opportunities to be helpful to others or useful to society" = "Q3_3", "Avoiding a high-pressure job" = "Q3_4",
                                                               "Freedom from supervision in your work" = "Q3_5", "Opportunities for moderate but steady progress rather than chance of extreme success or failure" = "Q3_6",
                                                               "A chance to exercise leadership" = "Q3_7", "Opportunities to work with people rather than things" = "Q3_10"),
                                                selected = c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q3_10")),
                             style="height:680px")
                ),
                
                dataTableOutput("recs"),
                em(h5("Note: A value of 77 or 98 in the 'Job' column indicates that the question was left blank.", align="left"))),
              style="padding-left:50px; padding-right:50px; padding-top:10px"),
      
      tabItem("conclusion", 
              img(src="collage5.png", style="width:95%"), align="center",
               fluidRow(
                 panel(icon("mountain"),"Reflection",
                       style="font-size:18px; background-color:#ac8f8f", align="center"),
                 p("After interacting with all components of this Shiny web application, I encourage you to evaluate your answers to the following questions:",
                br(),
                br(),
                 "1. Did your self-perception of creativity change after interacting with this application? If so, how?",
                br(),
                br(),
                "2. Did you identify a strength you did not know you had?",
                br(),
                br(),
                "3. Do you plan to enhance any of your skills to become a more balanced individual and/or a more competitive job applicant?",
                br(),
                br(),
                "4. Does your current role highlight your strengths? Or, did you identify another industry or role that better aligns with your strengths?"), align="left",
        style="padding-left:50px; padding-right:50px; padding-top:10px")),
      
      tabItem("contact", fluidRow(
         align="center",
                column(12,
              p("Greetings!"),
             
               p("All too often, we get so caught up in our busy lives that we neglect to reflect on our accomplishments, relationships, and experiences.
                It is my hope that while you are navigating this website, you will dedicate some time to evaluate who you are and who you hope to become.
                By assessing your strengths and weaknesses and developing short and long-term goals, you will be well on your way to leading more enriching, meaningful, and balanced lives.
                For a fun exercise, you can compare your abilities with those of demographically-similar individuals to determine your commonalities and differences.
                This project is a culmination of the analytical, communication, and visualization skills I have developing over the last five years."), 
             
               p("As an individual who is intrigued by human behavior, values, and interactions, I am fortunate to have taken numerous courses at the Carey Business School that encourage interdisciplinary thinking.
                My data visualization course in particular was an invaluable opportunity for me to enhance my data cleaning, transformation, manipulation, and visualization skills using the R programming language.
                As I embark on the next chapter in my professional career and beyond, I will continue to build on the skills I have developed in my graduate studies."),
                strong(p("Please feel free to reach out with any questions at ssaltzm5@jhu.edu. I hope you have as much fun interacting with this website as I had creating it!")),
                style="font-size: 14px"), align="left"),
              style="padding-left:50px; padding-right:50px; padding-top:10px")

              )
    
      )
    ) 


server <- function(input, output) {
  
  
  
  output$table1 = renderDataTable(datatable(data_final[,c(3:14)],
                                            options = list(scrollX = TRUE, lengthMenu=c(5,10,15,20), pageLength=5, pageWidth=10))
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_final", ".csv", sep="")
      },
    
    content = function(file) {
      write.csv(data_final,file)
    }
    )
    
    # household size
    data_final_v1 <- data_final %>%
      group_by(HHSIZE) %>%
      summarise(per=(n()/nrow(data_final))*100)
     
    output$plot1 <- renderPlot({
      p <- data_final_v1 %>%
      ggplot(aes(x=HHSIZE,y=per))+ labs(x="", y="Percentage", title="Most common household size is 2 people")+
      geom_col(fill="#454545")+
      scale_x_discrete(limits=c("1","2","3","4","5","6"))+
      theme_minimal()+
      theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank())
      p
    })
    
    # marital status
    data_final_v2 <- data_final %>%
      group_by(MARITAL) %>%
      summarise(per=(n()/nrow(data_final))*100) %>%
      arrange(-per) %>%
      mutate(MARITAL = factor(MARITAL, MARITAL))
    
    output$plot2 <- renderPlot({
      p1 <- data_final_v2 %>%
        ggplot(aes(x=MARITAL, y=per))+ labs(x="", y="Percentage", title="Half of respondents are married")+
        geom_bar(stat="identity", fill="#454545")+
        theme_minimal()+
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank())
      p1
    })
    
    # race/ethnicity
    data_final_v3 <- data_final %>%
      group_by(RACETHNICITY) %>%
      summarise(per=(n()/nrow(data_final))*100) %>%
      arrange(-per) %>%
      mutate(RACETHNICITY = factor(RACETHNICITY, RACETHNICITY))
    
    output$plot3 <- renderPlot({
      p2 <- data_final_v3 %>%
        ggplot(aes(x=RACETHNICITY, y=per))+ labs(x="", y="Percentage", title="Nearly two-thirds of respondents are White")+
        geom_bar(stat="identity", fill="#454545")+
        theme_minimal()+
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank())
      p2
    })
    
    # age
    data_final_v4 <- data_final %>%
      group_by(AGE7) %>%
      summarise(per=(n()/nrow(data_final))*100)
    
    output$plot4 <- renderPlot({
      p3 <- data_final_v4 %>%
        ggplot(aes(x=AGE7,y=per))+ labs(x="", y="Percentage", title="Most respondents are between 25 and 64 years old")+
        geom_col(fill="#454545")+
        theme_minimal()+
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank())
      p3
    })
    
    # evaluate your own creativity
    data_final_v5 <- data_final %>% 
      select("Q4A","Q4E","Q4G","Q4J","Q4K","Q4N","Q4S","Q4AD")
    
    data_final_v5 <- data_final_v5 %>%
      pivot_longer(c("Q4A","Q4E","Q4G","Q4J","Q4K","Q4N","Q4S","Q4AD"), names_to="Activity", values_to="Rating") %>%
      group_by(Activity) %>%
      summarise(avg_rating = mean(Rating))
    
    data_final_v5[data_final_v5=="Q4A"] <- "Making up lyrics to a song"
    data_final_v5[data_final_v5=="Q4E"] <- "Writing a poem"
    data_final_v5[data_final_v5=="Q4G"] <- "Solving math puzzles"
    data_final_v5[data_final_v5=="Q4J"] <- "Thinking of a new invention"
    data_final_v5[data_final_v5=="Q4K"] <- "Building something mechanical, like a robot"
    data_final_v5[data_final_v5=="Q4N"] <- "Communicating with people from different cultures"
    data_final_v5[data_final_v5=="Q4S"] <- "Mediating a dispute or argument between two friends"
    data_final_v5[data_final_v5=="Q4AD"] <- "Thinking of many different solutions to a problem"
    
    output$comparison <- renderPlot ({
      p4 <- data_final_v5 %>%
        ggplot(aes(x=Activity, y=avg_rating))+ labs(x="",y="")+
        ylim(1,5)+ 
        coord_flip()+
        theme_minimal()+
        theme(panel.grid.major.y=element_line(colour = "#d1d1d1"), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.line=element_blank())
      
      if(input$radio1=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Writing a poem"), size=2, color="red")
      
      }
      
      if(input$radio1=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Writing a poem"), size=2, color="red")
        
      }
      
      if(input$radio1=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Writing a poem"), size=2, color="red")
        
      }
      
      if(input$radio1=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Writing a poem"), size=2, color="red")
        
      }
      
      if(input$radio1=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Writing a poem"), size=2, color="red")
        
      }
      
      if(input$radio2=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Thinking of many different solutions to a problem"), size=2, color="red")
        
      }
      
      if(input$radio2=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Thinking of many different solutions to a problem"), size=2, color="red")
        
      }
      
      if(input$radio2=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Thinking of many different solutions to a problem"), size=2, color="red")
        
      }
      
      if(input$radio2=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Thinking of many different solutions to a problem"), size=2, color="red")
        
      }
      
      if(input$radio2=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Thinking of many different solutions to a problem"), size=2, color="red")
        
      }
      
      if(input$radio3=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Thinking of a new invention"), size=2, color="red")
        
      }
      
      if(input$radio3=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Thinking of a new invention"), size=2, color="red")
        
      }
      
      if(input$radio3=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Thinking of a new invention"), size=2, color="red")
        
      }
      
      if(input$radio3=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Thinking of a new invention"), size=2, color="red")
        
      }
      
      if(input$radio3=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Thinking of a new invention"), size=2, color="red")
        
      }
      
      if(input$radio4=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Solving math puzzles"), size=2, color="red")
        
      }
      
      if(input$radio4=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Solving math puzzles"), size=2, color="red")
        
      }
      
      if(input$radio4=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Solving math puzzles"), size=2, color="red")
        
      }
      
      if(input$radio4=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Solving math puzzles"), size=2, color="red")
        
      }
      
      if(input$radio4=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Solving math puzzles"), size=2, color="red")
        
      }
      
      if(input$radio5=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Mediating a dispute or argument between two friends"), size=2, color="red")
        
      }
      
      if(input$radio5=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Mediating a dispute or argument between two friends"), size=2, color="red")
        
      }
      
      if(input$radio5=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Mediating a dispute or argument between two friends"), size=2, color="red")
        
      }
      
      if(input$radio5=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Mediating a dispute or argument between two friends"), size=2, color="red")
        
      }
      
      if(input$radio5=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Mediating a dispute or argument between two friends"), size=2, color="red")
        
      }
      
      if(input$radio6=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Making up lyrics to a song"), size=2, color="red")
        
      }
      
      if(input$radio6=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Making up lyrics to a song"), size=2, color="red")
        
      }
      
      if(input$radio6=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Making up lyrics to a song"), size=2, color="red")
        
      }
      
      if(input$radio6=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Making up lyrics to a song"), size=2, color="red")
        
      }
      
      if(input$radio6=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Making up lyrics to a song"), size=2, color="red")
        
      }
      
      if(input$radio7=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Communicating with people from different cultures"), size=2, color="red")
        
      }
      
      if(input$radio7=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Communicating with people from different cultures"), size=2, color="red")
        
      }
      
      if(input$radio7=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Communicating with people from different cultures"), size=2, color="red")
        
      }
      
      if(input$radio7=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Communicating with people from different cultures"), size=2, color="red")
        
      }
      
      if(input$radio7=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Communicating with people from different cultures"), size=2, color="red")
        
      }
      
      if(input$radio8=="1"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=1, x="Building something mechanical, like a robot"), size=2, color="red")
        
      }
      
      if(input$radio8=="2"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=2, x="Building something mechanical, like a robot"), size=2, color="red")
        
      }
      
      if(input$radio8=="3"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=3, x="Building something mechanical, like a robot"), size=2, color="red")
        
      }
      
      if(input$radio8=="4"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=4, x="Building something mechanical, like a robot"), size=2, color="red")
        
      }
      
      if(input$radio8=="5"){
        p4 = p4+ geom_point(data=data_final_v5, aes(y=5, x="Building something mechanical, like a robot"), size=2, color="red")
        
      }
     
      if(input$view==TRUE){
        p4 = p4+ geom_point(data=data_final_v5, size=2, color="black") 
      }
      
      p4
      
      
      
    })
    
    # word cloud
    data_final_wc <- data_final$Q1
    docs <- Corpus(VectorSource(data_final_wc))
    
    docs <- docs %>%
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    
    docs <- tm_map(docs, content_transformer(tolower))
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words), freq=words)
    
    df <- df[-c(1,4,8:10,12,15,25,26,28,30,36,39,40,45,51,56,57,63,92,94,95,114,118:120,124,133,138,151,176,182:184),]
    
    output$wordcloud <- renderWordcloud2 ({
      df=df[which(!grepl("[^\x01-\x7F]+", df$word)),]
      df=df %>% slice(1:input$wc)
      wordcloud2(data=df,size=.6,backgroundColor = "#f8f3f3", color=rep_len(c("#010057","#a200ff","#f47835","#fa3c4c","#8ec127"), nrow(df)))
    })
    
    # career aspirations
    output$recs <- renderDataTable({
 
        data_final_v7 <- data_final_v7 %>%
          filter(GENDER==input$select_gender, AGE7==input$select_age, EDUC4==input$select_edu, INCOME==input$select_inc, Priority==input$select_priorities, Quality==input$qualities) %>%
          select(Industry,Job) %>%
          unique()
        
      datatable(data_final_v7,
                options = list(scrollX = TRUE, lengthMenu=c(5,10,15,20), pageLength=5, pageWidth=10))
    })
}

shinyApp(ui, server)
