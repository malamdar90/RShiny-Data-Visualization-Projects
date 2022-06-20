library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(corrplot)

data <- read.csv("World Happiness.csv")
colnames(data)[1] <- "Country.name"
colnames(data)[3] <- "Happiness.score"

ui <- dashboardPage(
  dashboardHeader(title = "World Happiness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "page1", icon = icon("globe")),
      menuItem("Overview", tabName = "page2", icon = icon("map-o")),
      menuItem(
        "Happiness",
        tabName = "page3",
        icon = icon("line-chart")
      ),
      menuItem(
        "Life Expectancy",
        tabName = "page4",
        icon = icon("area-chart")
      ),
      menuItem("Data", tabName = "page5", icon = icon("table"))
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "page1",
      fluidRow(
        box(
          title = "About the Report",
          solidHeader = TRUE,
          status = "warning",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$div(
              tags$span(
                "The World Happiness Report is published each year using survey data on the happiness level of people in countries around the world. The report was first released in 2012. Typically, experts analyze this data to evaluate how nations are progressing and to inform global citizen of their policy-making decisions. Most importantly, the report allows readers to have a better understanding of the way new science of happiness impacts personal and national happiness degrees. The dataset uses the following six predictors of happiness: ",
                style = "font-size:20px"
              ),
              br(),
              tags$div(
                tags$li(tags$strong("Economic Production")),
                tags$li(tags$strong("Social Support")),
                tags$li(tags$strong("Life Expectancy")),
                tags$li(tags$strong("Freedom")),
                tags$li(tags$strong("Perception of Corruption")),
                tags$li(tags$strong("Generosity")),
                br(),
                fluidRow(tags$mark(
                  tags$i("This data was gathered by Gallup World Poll in 2021.")
                )),
                style = "font-size:20px"
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "The Dataset",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$div(
              tags$span(
                "The 2021 dataset has 20 columns compared to 15 columns in previous years. The dataset includes information collected from 149 distinct countries in main regions (Sub-Saharan Africa, Western Europe, North America and ANZ, Middle East and North Africa, Latin America and Caribbean, Central and Eastern Europe, East Asia, Southeast Asia, and the Commonwealth of Independent States).",
                style = "font-size:20px"
              ),
              br(),
              tags$span(
                "The last tab titled Data will show a brief display of the dataset.",
                style = "font-size:20px"
              ),
              br(),
              br(),
              tags$li(
                tags$strong("Source: "),
                tags$a(
                  href = "https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021",
                  "World Happiness Report Data",
                  style = "font-size:18px"
                ),
                br(),
                br(),
                img(
                  src = "https://static.wikia.nocookie.net/a85a6018-8546-4af6-9336-be50b951af75/scale-to-width/755",
                  height = 450,
                  width = 800
                )
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "The Project",
          solidHeader = TRUE,
          status = "info",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$div(fluidRow(
              column(
                4,
                tags$div(
                  "This web application was created as a project (Group #1) for a Data Visualization class at Johns Hopkins Carey Business School in Summer 2021.",
                  style = "font-size:20px"
                )
              )
            )),
            br(),
            tags$li(
              tags$strong("For more information: "),
              tags$a(href = "https://carey.jhu.edu/", "Johns Hopkins Carey Business School")
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "page2",
      fluidPage(
        box(
          title = "Motivations",
          solidHeader = TRUE,
          status = "info",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$div(
              tags$span(
                "Many factors contribute to an individual's happiness. The report provides six factors that are meant to be the indicators of happiness for the larger population of any specific country. According to the data, Finland ranked as the number 1 country that is the happiest country in the world. But is that true? The country is cold and relatively dark throughout the year. Are people truly happy in such conditions? The YouTube Video below explores life in Finland and several people are asked if they are truly happy.",
                style = "font-size:20px"
              ),
              br(),
              br(),
              br(),
              HTML(
                '<center><iframe width="800" height="450" src="https://www.youtube.com/embed/9FPU4F-Ajh8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>'
              ),
              br(),
              br(),
              br(),
              tags$span(
                "As indicated in the video, even the Finish people themselves do not quite understand how their country is rated as the happiest. However, one of the researchers in the video explains that there are factors such as the wealth of a country and quality of life that contribute to happiness. Therefore, our motivation for this project was to determine how different factors such as GDP, life expectancy, generosity and so forth correlate to the happiness levels of the general population.",
                style = "font-size:20px"
              ),
              br(),
              br(),
              br(),
              plotOutput("plot5", width = 1000, height = 600),
              br(),
              br(),
              br(),
              tags$span(
                "The correlation plot above shows that GDP per capita, healthy life expectancy, social support, and freedom to make life choices have the strongest positive correlation with Happiness Scores. On the other hand, Generosity does not have any correlation with Happiness Score whereas Perceptions of Corruption have a negative correlation with Happiness Scores. With regards to generosity, this could mean that people intuitively choose to be generous without the intention of making themselves happy.",
                style = "font-size:20px"
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Research Questions",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$div(fluidRow(
              column(
                10,
                tags$span(
                  "The following are the research questions for this project, the first one already being answered above with the correlation plot between all the socio-economic indicators and Happiness Score:",
                  style = "font-size:20px"
                ),
                br(),
                tags$li(
                  tags$strong("What is the most significant predictor of happiness?"),
                  style = "font-size:20px"
                ),
                tags$li(
                  tags$strong("Which country/region has the highest happiest score?", style = "font-size:20px")
                ),
                tags$li(
                  tags$strong(
                    "How is the relationship between Health Life Expectancy and Happiness Scores in different regions?",
                    style = "font-size:20px"
                  )
                )
              )
            ))
          )
        )
      )
    ),
    tabItem(
      tabName = "page3",
      plotlyOutput("plot3", height = 800),
      br(),
      br(),
      column(
        12,
        tags$div(tags$span(
          tags$i(
            "As seen the plot above, Western Europe is leading in their Happiness Score followed by North America. South Asia has the lowest Happiness Score.",
            style = "font-size:20px"
          )
        ))
      )
    ),
    tabItem(
      tabName = "page4",
      plotlyOutput("plot2", height = 800)
    ),
    tabItem(
      tabName = "page5",
      dataTableOutput("myTable"),
      a(
        href = "https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report.csv", target =
          "_blank"
      )
    )
  ))
)


server <- function(input, output, session) {
  data1 <- data[, c(
    "Happiness.score",
    "Social.support",
    "Logged.GDP.per.capita",
    "Healthy.life.expectancy",
    "Freedom.to.make.life.choices",
    "Generosity",
    "Perceptions.of.corruption"
  )]
  data1_corr <- cor(data1)

  output$plot5 <- renderPlot({
    corrplot(data1_corr,
      method = "circle",
      type = "upper",
      tl.cex = 1.2
    )
  })

  # Which region has the highest happiness score
  output$plot3 <- renderPlotly({
    g <-
      ggplot(
        data,
        aes(
          x = reorder(Regional.indicator, Happiness.score, na.rm = TRUE),
          y = Happiness.score,
          fill = Regional.indicator
        )
      ) +
      geom_boxplot() +
      theme(legend.position = "none") +
      labs(x = NULL) +
      coord_flip()
    ggplotly(g)
  })

  # Country indicators on healthy life expectancy
  output$plot2 <- renderPlotly({
    q <-
      ggplot(
        data,
        aes(y = Happiness.score, x = Healthy.life.expectancy, colour = Regional.indicator)
      ) +
      geom_point() +
      facet_wrap(~Regional.indicator) +
      labs(
        title = "Relationship Between Healthy Life Expectancy and Happiness Score in Different Regions", x =
          "Healthy life expectancy", y = "Happiness Score"
      )
    ggplotly(q)
  })

  output$myTable <- renderDataTable({
    return(datatable(data, rownames = FALSE))
  })
}

shinyApp(ui = ui, server = server)
