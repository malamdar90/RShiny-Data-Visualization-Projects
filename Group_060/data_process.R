library(dplyr)
library(data.table)
library(jsonlite)
library(coronavirus) 

countries <- fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
countries <- countries %>% 
    filter(region != "") %>%
    select(name, `alpha-2`, `alpha-3`, `country-code`, region, `sub-region`, `region-code`)


airports_json <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/mwgg/Airports/master/airports.json")

airports <- lapply(airports_json, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
})

airports <- as.data.frame(do.call("rbind", airports))
colnames(airports)[3] <- "airports_name"

airports <- airports %>% 
    mutate(lat = as.numeric(lat), long = as.numeric(lon)) %>% 
    select(-lon)

airports <- airports %>%
    left_join(countries, by = c("country" = "alpha-2"))


flightlist <- fread(
    "~/Downloads/flightlist/flightlist_20200701_20200731.csv.gz", na.strings = "")


flightlist <- flightlist %>%
    filter(!is.na(origin) & !is.na(destination)) %>%
    mutate(air_date = as.Date(substring(lastseen, 1, 10))) %>%
    group_by(origin, air_date) %>%
    summarise(air_line_count = n()) %>%
    as_tibble()


flightlist <- flightlist %>% 
    inner_join(airports %>% filter(icao != "EDDB"), by = c("origin" = "icao"))


data("coronavirus")

covid_data <- coronavirus %>% 
    group_by(country, type) %>%
    summarise(cases = sum(cases)) %>%
    as_tibble() %>%
    left_join(
        coronavirus %>% 
            select(country, lat, long) %>% 
            unique() %>%
            group_by(country) %>%
            summarise(
                lat = mean(lat, na.rm = TRUE),
                long = mean(long, na.rm = TRUE)
            ),
        by = "country"
    )

save(airports, countries, flightlist, covid_data, file = "data/shiny_data.RData")

