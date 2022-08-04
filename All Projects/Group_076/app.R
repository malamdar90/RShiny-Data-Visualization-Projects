library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(gridExtra)
library(shinythemes)
library(gtable)


RecentGrads = na.omit(read.csv("recent-grads.csv"))
WomenStem = read.csv("women-stem.csv")
AllAges = read_csv("all-ages.csv")
GradStudents = read_csv("grad-students.csv")
MajorsList = na.omit(read_csv("majors-list.csv"))
df1<-aggregate(data = RecentGrads, ShareWomen~Major_category,mean)
large <- read_csv("all-ages.csv") %>% na.omit()

groups <- large %>% group_by(Major_category) %>% summarise()

low1 <- large %>% group_by(Major_category) %>% 
  summarize(med = mean(Unemployment_rate), mean_med = mean(Median))%>% 
  arrange(med)

high1 <- large %>% group_by(Major_category) %>% 
  summarize(med = mean(Unemployment_rate), mean_med = mean(Median))%>% 
  arrange(desc(med))

  
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Majors Employment"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Us", tabName = "page7", icon=icon("id-badge")),
      menuItem("Majors List", tabName="page1", icon = icon("database")),
      menuItem("Major Category Analysis", tabName="page2", icon = icon("group")),
      menuItem("Major Status Comparison", tabName="page3", icon = icon("mortar-board")),
      menuItem("STEM Majors Salary", tabName = "page4",icon = icon("briefcase")),
      menuItem("Women in STEM", tabName = "page5",icon = icon("female")),
      menuItem("Recent Grads", tabName = "page6",icon = icon("table"))
     
   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page7",
              fluidRow(
               
                box(
                  title = "Our Application", solidHeader = TRUE,
                  status = "info",width = 13, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             "This application,",tags$strong("College Majors Employment Analysis,"), 
                             "is the shiny dashboard application designed to create 
                             visualizations on the employment rates of different college 
                             majors to provide a guide for",tags$strong("people who are choosing their 
                             majors based on employment status."),style = "font-size:16px"),
                           br(), 
                           br(),
                        
                           fluidRow((tags$span("The datasets are from Kaggle, 
                          containing data originally from American Community Survey 2010-2012 
                          Public Use Microdata Series. The datasets provided to us include labor 
                         force information of all ages in file all-ages.csv, recent grads with ages 
                        younger than 28 in file recent recent-grads.csv, and grad students with 
                        ages older than 25 in file grad-students.csv. There is also a list of 
                        college majors in file majors-file.csv, and a subset of recent-grads.csv 
                        with information of women in stem in file women-stem.csv. 
                        Some of the attributes in these files include major, major 
                        categories, total number of people with the major, 
                        number of people employed, number of people unemployed, major median salary,
                        Percentage of women with the major, and etc.", 
                          tags$strong("Links to the datasets are listed below")))
                           )
                         )
                  )
                )
              ),
              selectInput('website',"Links to original Kaggle datasets:",
                          list(WomenStem = "https://www.kaggle.com/tunguz/college-majors?select=women-stem.csv",
                               RecentGrads = "https://www.kaggle.com/tunguz/college-majors?select=recent-grads.csv",
                               MajorsList = "https://www.kaggle.com/tunguz/college-majors?select=majors-list.csv",
                               GradStudents = "https://www.kaggle.com/tunguz/college-majors?select=grad-students.csv",
                               AllAges = "https://www.kaggle.com/tunguz/college-majors?select=all-ages.csv")),
              htmlOutput("contactinfo"),
              uiOutput("TeamMembers")),
      tabItem(tabName = "page1", 
              dataTableOutput("myMajorTable"),
              textOutput("fod1p")),
      tabItem(tabName = "page2",
              h2("Major Category Analysis"),
              fluidRow(
                box(
                  title = "Brief Usage Guide", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h4("This page helps you explore and understand the relationship between earnings and unemployment rates of large major categories."),
                  h5("Use the slider on the left to highlight the number of categories that you would like to see."),
                  h5("Use the right dropdown menu to see categories with either the highest or lowest means."),
                  h5("**Note that the highest and lowest highlights are given to mean unemployment rates and not medians.")
                )
              ),
              fluidRow(
                column(6,
                       sliderInput("majors_n", "Number to Highlight:", min = 1, max = 10, value = 1, step = 1)
                ),
                column(6,
                       selectInput("majors_hl","Highest or Lowest: ",c("high","low"))
                )),
              
              plotOutput("plot21"),
              fluidRow(
                column(8,      
                       plotOutput("plot22")
                ),
                column(4,
                       plotOutput("plot23")
                ),
              ),
              checkboxInput("earnings", label = "Descending Earnings", value = FALSE),
              tableOutput("data21"),
              fluidRow(
                box(
                  title = "Graph Guide", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h4("Unemployment vs Earnings of Major Categories"),
                  h5("Move your mouse on a dot, it would show you the information of the dot."),
                  h5("TotalNum is the total number of people in the major category group, y axis shows the mean earnings, x axis shows the mean unemployment rate."),
                  
                )
              ),
              plotlyOutput("plot24")
      ),
      tabItem(tabName = "page3",
              #h2("Major Status"),
              #selectInput(inputId = "majors",label = "All majors:",c(MajorsList$Major)),
              #plotOutput("MajorStat"),
              h2("Major Status Comparison"),
              fluidRow(
                box(
                  title = "Comparison Guide", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h4("Pick two majors from major categories."),
                  h5("The first graph shows the major as a blue dot in its major category. The x axis is its unemployment rate and y axis is median earnings."),
                  h5("The next two black bar graphs show the 25, 50, 75 percentile of earnings for all ages of this major. The last two red bar graphs show the 25, 50, 75 percentile of earnings for graduate students. "),
                  
                )
              ),
              fluidRow(
                column(6,      
                       selectInput(inputId = "majors1",label = "Category",c(large$Major_category))
                ),
                column(6,
                       selectInput(inputId = "majors3",label = "Category:",c(large$Major_category))
                )
              ),
              fluidRow(
                column(6,      
                       selectInput(inputId = "majors2",label = "Pick Major:",c(""))
                ),
                column(6,
                       selectInput(inputId = "majors4",label = "Pick Major:",c(""))
                )
              ),
              fluidRow(
                column(6,      
                       plotOutput("plot41")
                ),
                column(6,
                       plotOutput("plot42")
                )
              ),
              fluidRow(
                column(6,      
                       plotOutput("plot43")
                ),
                column(6,
                       plotOutput("plot45")
                )
              ),
              fluidRow(
                column(6,      
                       plotOutput("plot44")
                ),
                column(6,
                       plotOutput("plot46")
                )
              ),
              plotOutput("plot2",click = "plot_click"),
              verbatimTextOutput("info"), placeholder = TRUE,
          ),
      
      
      tabItem(tabName = "page4",
              selectInput(inputId = "majors",label = "All STEM majors:",c(WomenStem$Major)),
              tableOutput("Stat"),
              uiOutput("AllAgePay"),
              plotOutput("WomenPercentPay")),
             
      tabItem(tabName = "page5",
              selectInput("categories","All major categories:",c(distinct(WomenStem,Major_category))),
              plotOutput("plot3")),
      tabItem(tabName = "page6",
              selectInput("categ","All major categories:",c(distinct(RecentGrads,Major_category))),
              plotOutput("plot4"))
    )
  )
)



server <- function(input,output,session){

  AllAges = read_csv("all-ages.csv")
  GradStudents = read_csv("grad-students.csv")
  MajorsList = read_csv("majors-list.csv")
  RcentGrads = read_csv("recent-grads.csv")
  WomenStem = read_csv("women-stem.csv")
  df1<-aggregate(data = RecentGrads, ShareWomen~Major_category,mean)
  
  output$myMajorTable = DT::renderDataTable({MajorsList})
  output$fod1p = renderText({
    HTML("*FOD1P is the major code")
  })
  ## ---- cleaning and other ----
  Names = reactive({large %>% filter(Major_category == input$majors1) %>% distinct(Major)})
  observe({updateSelectInput(session = session, inputId = "majors2", choices = Names())})
  
  Names2 = reactive({large %>% filter(Major_category == input$majors3) %>% distinct(Major)})
  observe({updateSelectInput(session = session, inputId = "majors4", choices = Names2()) })
  
  ## ------- output -------
  

  output$myMajorTable = DT::renderDataTable({MajorsList})
  
  output$plot21 = renderPlot({
    x <- c(1:input$majors_n)
    
    #    hightable <- high1[1:input$majors_n]
    
    high <- high1 %>% pull(Major_category) %>% head(input$majors_n)
    
    # find min four  median
    
    #    lowtable <- low1[1:input$majors_n]
    low <- low1 %>% pull(Major_category) %>% head(input$majors_n)
    
    phl <- low
    if(input$majors_hl=="high"){phl<-high}
    
    majorgroup <- large%>%mutate(color = "other")
    for(i in x){
      for(j in 1:nrow(majorgroup)){
        if (majorgroup$Major_category[j] == phl[i]){
          majorgroup$color[j] = "yes"
        }
      }
    }
    # box plot of major big groups
    p21 <- majorgroup %>% group_by(Major_category) %>% 
      ggplot(aes(y=reorder(Major_category,-Unemployment_rate, na.rm = TRUE), x = Unemployment_rate,color=color))+ 
      geom_boxplot() +
      labs(title = "Unemployment Rate for Different Major Categories", 
           y = "Major Categories", 
           x = "Unemployment Rate")+ 
      scale_color_manual(values=c("grey","blue")) +
      scale_alpha_manual(values=c(1,0.1)) +
      theme_bw() + 
      theme(legend.position = "none")
    p21
  })
  
  output$plot22 = renderPlot({
    x <- c(1:input$majors_n)
    #    hightable <- high1[1:input$majors_n]
    high <- high1 %>% pull(Major_category) %>% head(input$majors_n)
    
    # find min four median
    
    #    lowtable <- low1[1:input$majors_n]
    low <- low1 %>% pull(Major_category) %>% head(input$majors_n)
    
    phl <- low
    if(input$majors_hl=="high"){phl<-high}
    majorgroup <- large%>%mutate(color = "other")
    for(i in x){
      for(j in 1:nrow(majorgroup)){
        if (majorgroup$Major_category[j] == phl[i]){
          majorgroup$color[j] = "yes"
        }
      }
    }
    # average median earnings 
    p22<- majorgroup %>% group_by(Major_category,color) %>% 
      summarize(mean_med = mean(Median)) %>%
      ggplot(aes(y=reorder(Major_category,mean_med),x=mean_med, fill = color))+geom_bar(stat = "identity")+
      labs(title = "Earnings for Different Major Categories", 
           y = "Major Categories", 
           x = "Mean of Earnings")+ 
      scale_fill_manual(values=c("grey","blue")) +
      scale_alpha_manual(values=c(1,0.1)) +
      theme_bw() + 
      theme(legend.position = "none")
    p22
  })
  
  output$plot23 = renderPlot({
    x <- c(1:input$majors_n)
    #    hightable <- high1[1:input$majors_n]
    high <- high1 %>% pull(Major_category) %>% head(input$majors_n)
    
    # find min four median
    
    #    lowtable <- low1[1:input$majors_n]
    low <- low1 %>% pull(Major_category) %>% head(input$majors_n)
    
    phl <- low
    
    if(input$majors_hl=="high"){phl<-high}
    
    majorgroup <- large%>%mutate(color = "other")
    for(i in x){
      for(j in 1:nrow(majorgroup)){
        if (majorgroup$Major_category[j] == phl[i]){
          majorgroup$color[j] = "yes"
        }
      }
    }
    p23 <- majorgroup %>% group_by(Major_category) %>% 
      summarize(mean_med = mean(Median),color,Median) %>%
      ggplot(aes(y=reorder(Major_category,mean_med),x=Median, color=color)) + geom_boxplot()+
      scale_color_manual(values=c("grey","blue")) +
      scale_alpha_manual(values=c(1,0.1)) +
      theme_bw() + 
      theme(legend.position = "none")+
      theme(axis.title.y = element_blank())+ 
      theme(axis.text.y=element_blank()) +
      labs(title = "Earnings for Different Major Categories",x = "Boxplot of Earnings")
    p23
  })
  
  output$plot24 = renderPlotly({
    large %>%
      arrange(desc(Total)) %>%
      group_by(Major_category) %>% summarize(TotalNum = sum(Total), mean_earning = mean(Median), unemploy_rate=mean(Unemployment_rate)) %>%
      ggplot(aes(x=unemploy_rate, y=mean_earning, size=TotalNum, fill=Major_category)) +
      geom_point(alpha=0.5, shape=21, color="black") +
      scale_size(name="Major Categories") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      ylab("Earnings") +
      xlab("Unemployment Rate")
  })
  
  output$data21 = renderTable({
    hightable <- high1[1:input$majors_n,]
    lowtable <- low1[1:input$majors_n,]
    p11 <- lowtable
    if(input$majors_hl=="high"){p11 <- hightable}
    
    if(input$earnings==TRUE){
      p11 <- p11 %>% arrange(desc(mean_med))
    }
    colnames(p11) <- c("Major Categories", "Mean Unemployment Rate", "Mean of Median Earnings")
    return(p11)
  
  })
  
  
  output$plot2 = renderPlot({
    plot(AllAges$Unemployment_rate,AllAges$Total,
         main = "Click on the data point to see Unemployment Rate and Total number of people with the major",
         col = "purple",xlab = "Unemployment Rate",
         ylab = "Total number of people with the major")})
    
  output$info <- renderPrint({
    point <- nearPoints(AllAges,input$plot_click,threshold = 5, maxpoints = 1, addDist = TRUE, 
               xvar = "Unemployment_rate", yvar = "Total")
    wellPanel(
      paste0("\nMajor Category: ",point$Major_category," - ",point$Major, 
             "\nTotal population: ",point$Total, "\nUnemployment rate:",point$Unemployment_rate,"\n")
    )
  })
  
  output$plot41 = renderPlot({
    p1 <- large %>% filter(Major_category == input$majors1)
    p1 <- p1%>%mutate(color = "other")
    for(j in 1:nrow(p1)){
      if (p1$Major[j] == input$majors2){
        p1$color[j] = "yes"
      }
    }
    p1 %>% ggplot(aes(y=Median, x = Unemployment_rate, color=color))+
      geom_point()+
      scale_color_manual(values=c("grey","blue")) +
      scale_alpha_manual(values=c(1,0.1)) +
      theme_bw() + 
      theme(legend.position = "none")+
      theme(axis.title.y = element_blank())+ 
      labs(x = "Earnings vs Unemployment")+
      scale_y_continuous(limits =c(0,150000))+
      scale_x_continuous(limits =c(0,0.1))
  })
  
  
  output$plot42 = renderPlot({
    p2 <- large %>% filter(Major_category == input$majors3)
    p2 <- p2%>%mutate(color = "other")
    for(j in 1:nrow(p2)){
      if (p2$Major[j] == input$majors4){
        p2$color[j] = "yes"
      }
    }
    p2 %>% ggplot(aes(y=Median, x = Unemployment_rate, color=color))+
      geom_point()+
      scale_color_manual(values=c("grey","blue")) +
      scale_alpha_manual(values=c(1,0.1)) +
      theme_bw() + 
      theme(legend.position = "none")+
      theme(axis.title.y = element_blank())+ 

      labs(x = "Earnings vs Unemployment ")+
      scale_y_continuous(limits =c(0,150000))+
      scale_x_continuous(limits =c(0,0.1))
  })
  
  output$plot43 = renderPlot({
    # low med high plot
    data1 <- pivot_longer(large,
                          c("Median","P25th","P75th"),
                          names_to= "Values", values_to = "PerEarnings")
    data1 %>% filter(Major == input$majors2) %>% ggplot(aes(x=reorder(Values,PerEarnings),y=PerEarnings)) +
      geom_bar(stat = "identity")+ geom_text(aes(label=PerEarnings), vjust=-0.25) +
      scale_y_continuous(limits =c(0,160000))+
      labs(title = "Earnings for Different Percentiles", 
           y = "Earnings", 
           x = "Percentiles")+ theme(legend.position = "none")
  })
  
  output$plot44 = renderPlot({
    data2 <- pivot_longer(GradStudents,
                          c("Grad_median","Grad_P25","Grad_P75"),
                          names_to= "Values", values_to = "PerEarnings")
    data2 %>% filter(Major == input$majors2) %>% ggplot(aes(x=reorder(Values,PerEarnings),y=PerEarnings,fill=Major)) +
      geom_bar(stat = "identity")+ geom_text(aes(label=PerEarnings), vjust=-0.25) + 
      scale_y_continuous(limits =c(0,160000))+
      labs(title = "Earnings for Different Percentiles", 
           y = "Earnings", 
           x = "Percentiles")+ theme(legend.position = "none")
  })
  
  output$plot45 = renderPlot({
    # low med high plot
    data1 <- pivot_longer(large,
                          c("Median","P25th","P75th"),
                          names_to= "Values", values_to = "PerEarnings")
    data1 %>% filter(Major == input$majors4) %>% ggplot(aes(x=reorder(Values,PerEarnings),y=PerEarnings)) +
      geom_bar(stat = "identity")+ geom_text(aes(label=PerEarnings), vjust=-0.25) +
      scale_y_continuous(limits =c(0,160000))+
      labs(title = "Earnings for Different Percentiles", 
           y = "Earnings", 
           x = "Percentiles")+ theme(legend.position = "none")
  })
  
  output$plot46 = renderPlot({
    data2 <- pivot_longer(GradStudents,
                          c("Grad_median","Grad_P25","Grad_P75"),
                          names_to= "Values", values_to = "PerEarnings")
    data2 %>% filter(Major == input$majors4)%>% ggplot(aes(x=reorder(Values,PerEarnings),y=PerEarnings,fill=Major)) +
      geom_bar(stat = "identity")+ geom_text(aes(label=PerEarnings), vjust=-0.25) +
      scale_y_continuous(limits =c(0,160000))+
      labs(title = "Earnings for Different Percentiles", 
           y = "Earnings", 
           x = "Percentiles")+ theme(legend.position = "none")
  })
  
  
  output$Stat<- renderTable({
    WomenStem %>%
      filter(Major == input$majors)
    
  })
  
  output$WomenPercentPay <- renderPlot({
    pay_corr <- WomenStem[,c(8:9)]
    plot(pay_corr,main = "Correlation of Percentage of Women and Median Salary",
         xlab = "Percentage of Women with the Major",
         ylab = "Median Salary",
         col = "purple")
    
  })
  
  output$AllAgePay <- renderUI({
    age_pay <-
      AllAges %>%
      filter(Major == input$majors)
    
    recent_pay <-
      RecentGrads %>%
      filter(Major == input$majors)
    
    women_pay <-
      WomenStem %>%
      filter(Major == input$majors)
    
    
    HTML(paste("<b>For a graduate in",input$majors,
               "<b>All Age Median Pay: ",age_pay$Median,
               "<b>Recent Grad Median Pay: ",recent_pay$Median,
               "<b>Women Median Pay: ",women_pay$Median,
               sep = "<br/>"))
    
  })
  
  

  
  
  
  output$contactinfo <-renderUI({
    tags$a(href = input$website,input$website)
  })
  
  output$TeamMembers <-renderUI({
    HTML("<br><b>Please contact us for further information,
         question, or collaboration. <br>")
  })  #women stem plot
  pt3 <- reactive({
    thisCategory=input$categories
    filteredData <- filter(WomenStem,Major_category==thisCategory)
    
    ggplot(data=filteredData,mapping = aes(x = ShareWomen, y = Median)) + 
      geom_point(aes(color=Major_category))+labs(title = paste("Women's median earning majoring in",thisCategory),
                                                 y= "Median Earning",
                                                 x="Women's share of majors",
                                                 color="Major Category")+
      scale_y_continuous(labels=scales::dollar_format(),limits = c(20000,110000))+
      scale_x_continuous(limits = c(0,1),labels = scales::percent)+theme(legend.position="bottom")
    
  })
  pt4 <- reactive({
    thisCategory=input$categories
    filteredData <- filter(WomenStem,Major_category==thisCategory)
    
    ggplot(data=WomenStem,mapping = aes(x = ShareWomen, y = Median)) + 
      geom_point(aes(color=Major_category))+labs(title = "Women’s median earning for all major categories",
                                                 y= "Median Earning",
                                                 x="Women's share of majors",
                                                 color="Major Category")+
      scale_y_continuous(labels=scales::dollar_format(),limits = c(20000,110000))+
      scale_x_continuous(limits = c(0,1),labels = scales::percent)+theme(legend.position="bottom")
    
  })
  output$plot3 <- renderPlot({
    
    
    ptlist1 <- list(pt4(),pt3())
    
    
    grid.arrange(grobs=ptlist1,height=2,ncol=2)
  })
  #sharewomen and major category plot
  pt1 <- reactive({
    #if (!input$donum1) return(NULL)
    df2<-rbind(df1,df1)
    colnames(df2)[2]<-"Shares"
    df2$measure<-"ShareWomen"
    df2[17:32,]$measure<-"ShareMen"
    df2[17:32,]$Shares<-1-df2[1:16,]$Shares
    
    ggplot(data=df2, aes(x = Major_category, y = Shares,fill=measure))+geom_bar(position="stack", stat="identity")+
      aes(stringr::str_wrap(Major_category, 15), Shares)+
      labs(title = "Percentage of women for different major categories",
           y= "Share of Gender",
           x="Major categories")+
      scale_y_continuous(limits = c(0,1),labels = scales::percent)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  pt2 <- reactive({
    #if (!input$donum1) return(NULL)
    thisCateg=input$categ
    filteredData1 <- filter(RecentGrads,Major_category==thisCateg)
    
    
    
    ggplot(data=filteredData1,aes(x = Major, y = ShareWomen))+geom_col()+
      
      theme( 
            axis.text.x=element_text(size = 6.2, angle=90,hjust=1,vjust=0.5))+
      aes(stringr::str_wrap(Major, 20), ShareWomen)+
      labs(title = paste("Percentage of women majoring in",thisCateg),
           y= "Percentage of women",
           x="Major")+scale_y_continuous(limits = c(0,1),labels = scales::percent) 
    # + coord_flip()
  })
  output$plot4 <- renderPlot({
    ptlist <- list(pt1(),pt2())
    # remove the null plots from ptlist and wtlist
    #to_delete <- !sapply(ptlist,is.null)
    #ptlist <- ptlist[to_delete] 
    #par(mfrow=c(2,1))
    #show(pt1)
    #pt2
    grid.arrange(grobs=ptlist, height=2, ncol = 2)
    

  })
  
}




shinyApp(ui = ui, server = server)




