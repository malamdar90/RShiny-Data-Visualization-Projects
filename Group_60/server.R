library(shiny)


shinyServer(function(input, output) {
    
    output$covid_map <- renderLeaflet({
        print(input$covid_status)
        plot_data <- covid_data %>% filter(type == input$covid_status)
        colorpal <- colorBin("PuOr", plot_data$cases, bins = 8, pretty = TRUE)
        
        plot_data$text <- sprintf(
            "<strong>Country Name: <br/> %s</strong><br/> %s Case: %s", 
            plot_data$country, str_to_title(plot_data$type), plot_data$cases
        ) %>% lapply(htmltools::HTML)
        
        leaflet(plot_data) %>%
            addTiles() %>%
            addCircleMarkers(
                lng = ~ long, lat = ~ lat, fillOpacity = 0.8,
                radius = ~ sqrt(cases / max(plot_data$cases) * 1000),
                opacity = 0,
                color = ~ colorpal(cases),
                fillColor = ~ colorpal(cases),
                label = ~ text
            ) %>%
            addLegend(
                pal = colorpal, values = ~cases, opacity = 0.6
            )
    })
    
    # sub1: dashboards
    output$covid_confirmed <- renderValueBox({
        valueBox(
            formatC(covid_total_confirmed, format = "d", big.mark = ','), 
            'Total Confirmed',
            icon = icon("stats", lib = 'glyphicon'),
            color = "purple"
        )
    })
    output$covid_recovered <- renderValueBox({ 
        valueBox(
            formatC(covid_total_recovered, format = "d", big.mark = ','),
            'Total Recoverd',
            icon = icon("stats", lib = 'glyphicon'),
            color = "green")
    })
    output$covid_death <- renderValueBox({
        valueBox(
            formatC(covid_total_death, format = "d", big.mark = ','),
            "Top Death",
            icon = icon("stats", lib = 'glyphicon'),
            color = "yellow")
    })
    
    ## sub2: Table
    
    output$travel_airline_ui <- renderUI({
        airlines <- travel_airlines
        if (input$travel_country != "All Country") {
            airlines <- airlines %>% filter(Country == input$travel_country)
        }
        airlines <- c("All Airline", sort(unique(airlines$Airline)))
        selectInput(
            'travel_airline', 'Pleace select a Airline ', choices = airlines,
            width = "100%", selected = "All Airline")
    })

    output$travel_dt <-  DT::renderDataTable(DT::datatable({
        travel <- travel_restrictions
        if (input$travel_country != "All Country") {
            travel <- travel %>% filter(Country == input$travel_country)
        }
        if (input$travel_airline != "All Airline") {
            travel <- travel %>% filter(Airline == input$travel_airline)
        }
        travel
    }))
    
    ## Map
    output$airline_map_country <- renderUI({
        map_countrys <- region_country_dates %>% select(region, country)
        if (input$airline_map_region != "All Regions") {
            map_countrys <- map_countrys %>% 
                filter(region == input$airline_map_region)
        }
        
        map_countrys <- c("All Country", sort(unique(map_countrys$country)))
        selectInput(
            'map_country', 'Pleace select a Country ', choices = map_countrys,
            width = "100%", selected = "All Country")
    })
    
    output$airline_map_date <- renderUI({
        map_dates <- region_country_dates
        if (input$airline_map_region != "All Regions") {
            map_dates <- map_dates %>% filter(region == input$airline_map_region)
        }
        if (input$map_country != "All Country") {
            map_dates <- map_dates %>% filter(country == input$map_country)
        }
        min_date <- min(map_dates$air_date, na.rm = TRUE)
        max_date <- max(map_dates$air_date, na.rm = TRUE)
        print(paste0(input$airline_map_region, " ~ ", input$map_country,
                     " ~ ", min_date, " ~ ", max_date))
        
        dateRangeInput(
            "DateRange", "Please select date range",
            start = min(map_dates$air_date, na.rm = TRUE), 
            end = max(map_dates$air_date, na.rm = TRUE),
            min = min_date, max = max_date
        )
    })
    
    output$airline_map <- renderLeaflet({
        plot_data <- flightlist
        
        if (input$airline_map_region != "All Regions") {
            plot_data <- plot_data %>% filter(region == input$airline_map_region)
        }
        if (input$map_country != "All Country") {
            plot_data <- plot_data %>% filter(name == input$map_country)
        }
        
        plot_data <- plot_data %>% 
            filter(air_date >= input$DateRange[1] & 
                       air_date <= input$DateRange[2]) %>%
            group_by(origin, airports_name, region, name, lat, long) %>% 
            summarise(airlines = n()) %>% 
            as_tibble()
        
        colorpal <- colorBin("BuPu", plot_data$airlines, 9, pretty = TRUE)
        
        plot_data$text <- sprintf(
            "<strong>Airport Name: %s</strong><br/>Region: %s<br/>County: %s<br/>Airlines Number: %s<br/>", 
            paste0(plot_data$origin, " - ", plot_data$airports_name),
            plot_data$region, plot_data$name, plot_data$airlines
        ) %>% lapply(htmltools::HTML)
        
        leaflet(plot_data) %>%
            addTiles() %>%
            addCircleMarkers(
                lng = ~ long, lat = ~ lat, fillOpacity = 0.8,
                radius = ~ airlines / max(plot_data$airlines) * 8,
                opacity = 0,
                color = ~ colorpal(airlines),
                fillColor = ~ colorpal(airlines),
                label = ~ text
            ) %>%
            addLegend(
                pal = colorpal, values = ~airlines, opacity = 1.0
            )
    })
})
