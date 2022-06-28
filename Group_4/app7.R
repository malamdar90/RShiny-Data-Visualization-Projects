library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
#install.packages("plotly")
library(wordcloud)

library(DT)
#install.packages("DT")
library(readxl)

library(readr)
library(tidyverse)
brands <-
  data.frame(
    brand = c(
      'adidas',
      'adidas',
      'adidas',
      'adidas',
      'adidas',
      'Jordan',
      'Jordan',
      'Jordan',
      'Jordan',
      'Jordan',
      'Jordan',
      'Jordan',
      'Nike',
      'Nike',
      'Nike',
      'Nike',
      'Nike',
      'Nike',
      'Nike',
      'Nike'
    ),
    cate2 = c(
      'adidas EQT',
      'adidas Iniki',
      'adidas NMD',
      'adidas Ultra Boost',
      'adidas Yeezy',
      'Air Jordan Five',
      'Air Jordan Four',
      'Air Jordan One',
      'Air Jordan Seven',
      'Air Jordan Six',
      'Air Jordan Three',
      'Air Jordan Two',
      'Air Force',
      'Air Max',
      'Basketball',
      'Foamposite',
      'KD',
      'Kobe',
      'LeBron',
      'Other'
    )
  )


ui <- dashboardPage(
  skin = "black",
  # change the apperance of app, it makes the top bar gray and other parts black
  
  header = dashboardHeader(title = "Group Project"),

  controlbar = dashboardControlbar(),
  sidebar = dashboardSidebar(sidebarMenu(
    hr(),
    menuItem(
      text = "Home",
      tabName = "page1",
      icon = icon("home")
    ),
    menuItem(
      text = "Data Table",
      tabName = "Tables",
      icon = icon("cog", lib = "glyphicon")
    ),
    menuItem(
      text = "Data Virtualization",
      tabName = "page3",
      icon = icon("cog", lib = "glyphicon")
    ),
    menuItem(
      text = "Color Matching",
      tabName = "shoes",
      icon = icon("cog", lib = "glyphicon")
    )
  )),
  body = dashboardBody(
    tags$script(type = "text/javascript", 'function show2(x){
              Shiny.setInputValue("in5",x);
                }'),
    tabItems(
    
    ##########################################
    tabItem(
      tabName = "page1",
      h3("Description"),
      p(
        "The inspiration for this project is from the ted talk “why sneakers are a great investment” by Josh Luber, the creator of StockX the online sneaker marketplace. This is the kind of investment that you can control and that can pay out in dividends much greater than the historic 9% per year. In fact, it’s not uncommon to make 100-200% on one single pair of shoes. For example, Off-White Air Jordan 1 returns a price premium of 942.1% over the original retail price. However, a lot of sneakers don’t have any investment values. Our project will help our target customer understand which types/colors/sizes/styles of the upcoming sneakers are better investments through the historical data stored in our database and the value evaluation module."
      ),
      br(),
      h3("Research Questions"),
      p(
        span(
          "What shoes have the greater sales potential and better investment value based on the information on"
        ),
        a(href = "https://stockx.com", target = "_blank", "stockx"),
        span(
          "a ecommerce platform with an emphasis on the sneaker resale market."
        )
      ),
      br(),
      # adds an empty line
      h3("Methods"),
      p(
        "The main methods in this small project are Web Scrapping, Color Matching, Wordcloud Visualization, Image matching"
      ),
      br(),
      h4("Web Scrapping"),
      p(
        "We use webscrapping methods to collect all the data and images from the stockx websites."
      ),
      br(),
      
      h4("Wordcloud Visualization"),
      p(
        code("Wordcould"),
        span("package in R has been used to visualize the most frequent words.")
      ),
      br(),
      h4("Color matching"),
      p(
        "We compare the color customer painted on the shoes to match the sneakers in the system"
      ),
      br(),
      h4("Image matching"),
      p(
        "We use the sneaker brand and category of customer choose to calculated the best sale and best earn sneakers and display the related products to the customer match the sneakers in the system"
      ),
      br(),
      
      h3("Tabs"),
      h4("Data Table"),
      p("User are able to see the data set we use in this project"),
      h4("Data Visualization"),
      p(
        "Users are able to choose brand and category of the shose and see the associated results. The realted plot designed in this page that helps users know more about the information of their interesed sneakers. The features avaible are as follows:"
      ),
      # we create an unorder list using ul and li.
      tags$ul(
        tags$li(
          "Side column displaying brand and category name that helps users to make decision"
        ),
        tags$li("Price movement of different shoes by its size"),
        tags$li("Wordcloud of the popular shoes by users' choice")
        
      ),
      h4("Color Matching"),
      p(
        "User are able choose the color and paint the shoes as their preferrence and see the related output based upon the color matching"
      ),
      br(),
      h3("Team"),
      box(
        width = 12,
        # The total width a window is 12.
        fluidRow(column(
          width = 3,
          img(
            src = 'img0.JPG',
            align = "left",
            height = 100,
            width = 100
          ) # img1.JPG should be placed in the www folder
        ),
        column(
          width = 9,
          p(
            "Sean sheng, a programmer, really enjoys the proress of teamwork in the project."
          ),
          p(
            "However, if we have more time, I believe our product can be better than it is now. "
          )
        )),
        fluidRow(column(
          width = 3,
          img(
            src = 'img2.JPG',
            align = "left",
            height = 100,
            width = 100
          ) # img1.JPG should be placed in the www folder
        ),
        column(
          width = 9,
          p(
            "Wenting Xu is currently a informstion system major student in Johns Hopkins University"
          ),
          p("She graduated from Bradley University with Accounting major. ")
        )),
        fluidRow(column(
          width = 3,
          img(
            src = 'img3.JPG',
            align = "left",
            height = 100,
            width = 100
          ) # img1.JPG should be placed in the www folder
        ),
        column(
          width = 9,
          p(
            "Zixiao Guo is currently a informstion system major student in Johns Hopkins University"
          ),
          p("She graduated from Zhejiang University with Finance major. ")
        )),
        fluidRow(column(
          width = 3,
          img(
            src = 'img4.JPG',
            align = "left",
            height = 100,
            width = 100
          ) # img1.JPG should be placed in the www folder
        ),
        column(
          width = 9,
          p(
            "I'm Yixiong Shang, innovative, self-motivated and data-driven storyteller with excellent analytical and communication skills. I’m experienced in IT and business operations utilizing various data analysis and visualization tools."
          )
        ))
      ),
      br(),
      h3("How to use this app"),
      
      HTML(
        '<iframe width="100%" height="300" src="https://www.youtube.com/embed/IznecePLSkE" frameborder="0" allowfullscreen></iframe>'
      )
    ),
    tabItem(tabName = "shoes"),
    tabItem(tabName = "Tables",
            
            column(
              width = 12,
              tabBox(
                width = 12,
                height = 500,
                tabPanel(
                  title = "Shoes Chart",
                  height = 500,
                  dataTableOutput("myShoes")
                  
                ),
                tabPanel(title = "sales Chart",
                         dataTableOutput("mySales"))
              )
            )),
    
    tabItem(
      tabName = "page3",
      column(
        width = 2,
        p("Choose by brand and category to view visualization"),
        tabPanel(
          "brand",
          checkboxGroupInput("brand1", "adidas", brands[brands$brand ==
                                                          "adidas", ]$cate2),
          checkboxGroupInput("brand2", "Nike", brands[brands$brand ==
                                                        "Nike", ]$cate2),
          checkboxGroupInput("brand3", "Jordan", brands[brands$brand ==
                                                          "Jordan", ]$cate2)
        )
      ),
      
      column(
        width = 6,
        h1("Information Overview"),
        mainPanel(
          width = "600px",
          tabsetPanel(
            tabPanel("View table", dataTableOutput('out2')),
            tabPanel(
              "Price visualizaion",
              style = "overflow-x:hidden;",
              radioButtons(
                inputId = "s7",
                "Variable",
                c(
                  "Highest bid price" = "highestBid",
                  "Lowest ask rice" = "lowestAsk",
                  "Average sales price" = "averageDeadstockPrice"
                ),
                selected = "highestBid"
              ),
              radioButtons(
                inputId = "s8",
                "Method",
                c("mean", "max", "min", "median", "mode"),
                selected = "mean"
              ),
              plotlyOutput("out7", height = "600px")
            ),
            tabPanel(
              "Popularity visualization",
              box(
                #width = "600px",
                style = "overflow-x:hidden;",
                radioButtons(
                  inputId = "s9",
                  "Method",
                  c(
                    "Best Sell (Calculated by selling amount)" = "Best Sell",
                    "Best Earn (calculated by total revenue)" = "Best Earn"
                  ),
                  selected = "Best Sell"
                ),
                plotOutput(
                  outputId = "out3",
                  width = "600px",
                  height = 500
                )
              )
            ),
            tabPanel("Top shoes", style = "overflow-y:scroll; max-height: 750px;overflow-x:hidden;", uiOutput("out1", width = "600px"))
          )
        )
        
      ),
      column(
        width = 4,
        height = NULL,
        h1("Detail of Shoes"),
        p("choose in Top shoes TAB to view detail"),
        uiOutput("shoename"),
        wellPanel(
          style = "overflow-y:scroll; max-height: 400px;overflow-x:hidden;",
          plotlyOutput("out6"),
          plotlyOutput("out5"),
          plotlyOutput("out4")
        )
        
        
        
        #textInput("in3",label="2",value=3),
        #p("clickclickclick",onclick="show1()")，
        #verbatimTextOutput("text1")
        
      )
    )
  ))
)



server <- function(input, output) {
  Shoess = read.csv('www/csv/Shoes_Norepeart.csv')
  saless = read.csv('www/csv/Sales_Norepeat.csv')
  
  
  
  output$myShoes = renderDataTable(Shoess, options = list(pageLength = 6))
  output$mySales = renderDataTable(saless, options = list(pageLength = 6))
  
  ######## for visualization page  ########################
  shoes3 <- Shoess
  shoes3$release <-
    as.POSIXct(shoes3$releaseDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  shoes3$cate2 <- as.character(shoes3$category)
  shoes3$cate2[shoes3$brand == "Nike"] <- "Other"
  shoes3$cate2[grep("Air Force",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "Air Force"
  shoes3$cate2[grep("Air Max",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "Air Max"
  shoes3$cate2[grep("Basketball",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "Basketball"
  shoes3$cate2[grep("Foamposite",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "Foamposite"
  shoes3$cate2[grep("KD",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "KD"
  shoes3$cate2[grep("Kobe",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "Kobe"
  shoes3$cate2[grep("LeBron",
                    shoes3$category,
                    ignore.case = TRUE,
                    fixed = FALSE)] <- "LeBron"
  shoes3$highestBid <- as.numeric(as.character(shoes3$highestBid))
  #sum(is.na(shoes3$highestBid)) ##63
  shoes3$lowestAsk <- as.numeric(as.character(shoes3$lowestAsk))
  #sum(is.na(shoes3$lowestAsk)) ##72
  shoes3$averageDeadstockPrice <-
    as.numeric(as.character(shoes3$averageDeadstockPrice))
  #sum(is.na(shoes3$averageDeadstockPrice)) ##67
  #shoes3%>%group_by(cate2)%>%summarise(n=n())
  
  sales3 <- saless
  sales3$time <-
    as.POSIXct(sales3$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  sales3$size <- as.numeric(gsub("W|Y|K|C", "", sales3$size))
  #sum(is.na(sales3))
  print("chos2")
  print(head(sales3))
  ids <- sales3 %>% 
    group_by(id) %>% 
    dplyr::summarise(n = n(),
      max = max(time),
      min = min(time),
      df = as.numeric(max - min) + 1,
      total = sum(amount),
      f1 = n / df,
      f2 = total / df
    )
 
  print(head(ids))
  ids <- ids[order(ids$f1, decreasing = T), ]
  
  brands <- shoes3 %>% group_by(brand, cate2) %>% dplyr::summarise(n=n())
  ######## end visualization page  ########################
  
   print("guz")
  
  ############ visualization page ############################
  shoes2 <- reactive({
    if ((length(input$brand1) + length(input$brand2) + length(input$brand3)) ==
        0) {
      return(shoes3)
    } else{
      return(shoes3[(shoes3$cate2 %in% c(input$brand1, input$brand2, input$brand3)), ])
    }
  })
  
  ### column2 information1: table
  output$out2 <-
    renderDataTable(shoes2()[, c('brand', 'category', 'name')], options = list(pageLength = 6))
  
  ### column2 information2: mean
  getmean <- function(shoes, a, b) {
    shoes2 <- shoes[, c("category", a)]
    names(shoes2)[2] <- "mean"
    if (b == "mean") {
      mean <-
        shoes2 %>% group_by(category) %>% summarise(mean = mean(mean, na.rm = TRUE))
    } else if (b == "min") {
      mean <-
        shoes2 %>% group_by(category) %>% summarise(mean = min(mean, na.rm = TRUE))
    } else if (b == "max") {
      mean <-
        shoes2 %>% group_by(category) %>% summarise(mean = max(mean, na.rm = TRUE))
    } else if (b == "median") {
      mean <-
        shoes2 %>% group_by(category) %>% summarise(mean = median(mean, na.rm =
                                                                    TRUE))
    } else{
      getmode1 <- function(mode) {
        uniqv <- unique(mode)
        uniqv[which.max(tabulate(match(mode, uniqv)))]
      }
      mean <-
        shoes2 %>% filter(mean != "NA") %>% group_by(category) %>% summarise(mean =
                                                                               getmode1(mean))
    }
    mean
  }
  
  show3 <- function(t) {
    t %>% arrange(desc(mean)) %>% ggplot(mapping = aes(x = reorder(category, mean), y =
                                                         mean)) + geom_bar(stat = "identity", aes(fill = category), width = 0.6) +
      theme_light() + coord_flip() + labs(title = "Sneaker Brands Comparison", x =
                                            "Brand", y = "Price (USD)") + geom_text(
                                              aes(label = as.integer(mean)),
                                              vjust = 0.4,
                                              hjust = 1.5,
                                              color = "black",
                                              position = position_dodge(0.9),
                                              size = 4
                                            ) + guides(fill = FALSE)
  }
  output$out7 <- renderPlotly({
    show3(getmean(shoes2(), input$s7, input$s8))
  })
  
  
  ### column2 information3: cloud
  c2 <- function(f0, sales, shoes) {
    print("chos")
    ids <-
      sales3 %>% group_by(id) %>% summarise(
        n = n(),
        max = max(time),
        min = min(time),
        df = as.numeric(max - min) + 1,
        total = sum(amount),
        f1 = n / df,
        f2 = total / df
      )
    print("guz")
    ids <- ids[order(ids$f1, decreasing = T), ]
    if (f0 == "Best Sell") {
      freq = (ids$f1 * (10 / max(ids$f2))) ^ 2
    }
    #freq=replace(ids$f1/10,ids$f1/10>50,50)
    else
      freq = replace(ids$f2 / 1000, ids$f2 / 1000 > 50, 50)
    wordcloud(
      words = merge(ids, shoes, by = "id", all = F)[, 'name'],
      freq = freq,
      scale = c(4, .5),
      max.words = 30,
      random.order = F,
      rot.per = 0.35,
      min.freq = 1,
      colors = brewer.pal(8, "Dark2")
    )
  }
  output$out3 <- renderPlot({
    c2(input$s9, sales3, shoes2())
  })
  
  ### column2 information4: boxes
  output$out1 <- renderUI({
    t2 <- merge(ids, shoes2(), by = "id", all = F)[1:5, ]
    fluidRow(column(12, id = "columns",
                    apply(t2, 1, function(item) {
                      wellPanel(style = 'display:inline-block;height:250px;width:250px;background-color:rgb(255,255,255);margin-right:10px;',
                                HTML(
                                  paste0(
                                    "<div onclick=show2('",
                                    item['id'],
                                    "') >",
                                    "<h2>",
                                    item['brand'],
                                    "</h2>",
                                    "<p>",
                                    item['name'],
                                    "</p>",
                                    "<img src='",
                                    item['img'],
                                    "' style='height:100px;width:150px;' /></div>"
                                  )
                                ))
                    })))
  })
  
  
  sales2 <- reactive({
    if (length(input$in5) == 0) {
      return(sales3)
    } else{
      return(sales3[sales3$id == input$in5, ])
    }
  })
  ### column3 detail0: showname
  output$shoename <- renderUI({
    if (length(input$in5) == 0) {
      return()
    }
    else{
      item <- shoes3[shoes3$id == input$in5, ]
      tmptitle <- item$category
      if (item$brand == "Nike")
        tmptitle <- paste0(item$brand, " ", item$category)
      return(box(
        #width="100%",
        h4(tmptitle),
        h2(item$name),
        p(
          paste(
            item$brand,
            item$name,
            "was released on",
            item$releaseDate,
            "After",
            as.integer(sales2()$time[which.max(sales2()$amount)] - item$release),
            "days, it reaches the highest bid price: $",
            max(sales2()$amount),
            sep = " "
          )
        ),
        hr(),
        sliderInput(
          "sizeSlider",
          "Shoes Size:",
          min = 5,
          max = 18,
          value = 11,
          step = 0.5,
          width = "100%",
          animate = animationOptions(interval = 2000, loop = TRUE)
        )
      ))
    }
  })
  
  
  ### column3 detail1: by size animation
  output$out6 <- renderPlotly({
    if (length(input$in5) == 0) {
      return()
    }
    else
      ggplot(data = sales2()[(sales2()$size == input$sizeSlider), ],
             mapping = aes(
               x = time,
               y = amount,
               color = as.character(size)
             )) + geom_point(size = 0.1, alpha = 0.7) + geom_smooth(method = "loess", se = FALSE) +
      labs(title = "Sneaker Price movement in 2019", x = "month", y = "Price (USD)") +
      xlim(min(sales2()$time), max(sales2()$time)) + ylim(0, max(sales2()$amount)) +
      guides(fill = FALSE)
    
  })
  ### column3 detail2: all sales
  output$out5 <- renderPlotly({
    if (length(input$in5) == 0) {
      return()
    }
    else
      ggplot(data = sales2(),
             mapping = aes(
               x = time,
               y = amount,
               color = as.character(size)
             )) + geom_point(size = 2, alpha = 0.7)
  })
  ### column3 detail3: box plot
  output$out4 <- renderPlotly({
    if (length(input$in5) == 0) {
      return()
    }
    else
      ggplot(data = sales2(),
             mapping = aes(
               x = size,
               y = amount,
               fill = as.character(size)
             )) + geom_boxplot()
  })
  
  #output$text1<-renderText(input$in3)
  # output$text1<-renderText({
  # is.na(input$in5)
  #length(input$brand1)
  # })
  
  
}

shinyApp(ui = ui, server = server)
