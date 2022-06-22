library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(stringr)
library(rsconnect)
library(DT)


load("data/shiny_data.RData")


# sub1:covid_data
covid_types <- unique(covid_data$type)

covid_total_confirmed <- sum(covid_data[covid_data$type == "confirmed", ]$cases)
covid_total_recovered <- sum(covid_data[covid_data$type == "recovered", ]$cases)
covid_total_death <- sum(covid_data[covid_data$type == "death", ]$cases)

# sub2: COVID-19 travel restrictions by country - July.xlsx
travel_restrictions <- read.csv(
    "data/COVID-19 travel restrictions by country - July.csv")
travel_restrictions <- travel_restrictions %>% 
    mutate(Airline = ifelse(is.na(Airline), "No Name", Airline))
colnames(travel_restrictions)[1]="Country"
travel_countrys <- c("All Country", sort(unique(travel_restrictions$Country)))
travel_airlines <- travel_restrictions %>% select(Country, Airline) %>% unique()

regions <- c("All Regions", sort(unique(flightlist$region)))
region_country_dates <- flightlist %>% 
    select(region, country = name, air_date) %>% unique()
