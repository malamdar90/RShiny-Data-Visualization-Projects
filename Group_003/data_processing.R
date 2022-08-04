library(sf)
library(dplyr)
library(tidyverse)

#read EH file and US cities file
eh <- read.csv("EH.csv",na.strings="") 
c <- read.csv("cities.csv",na.strings="") 
#select useful columns
city_location <- select(c,1,4,6,7,8)
#remove cities not in the US
city_location$State[city_location$State=="Hawaii"|city_location$State=="Alaska"|city_location$lat<20] <- NA
city_location <- na.omit(city_location)
#rename the EH file column name
colnames(eh) <- c("NPI", "CCN", "Name", 
                  "State", "city", 
                  "Address", "Zip5",
                  "Zip4", "Phone", "P_EXT", 
                  "Stage", "Program_year", 
                  "Payment")
eh$Stage <- as.integer(eh$Stage )
#calculate the payment according to each provider
eh_payment <- aggregate(.~city+State,data=eh,sum)
eh_dataset <- group_by(eh,city,State)
new_eh <- summarise(eh_dataset,start_year=min(Program_year),reach_stage=max(Stage))
city_location$State<- as.character(city_location$State)
eh$State<- as.character(eh$State)
#process states data
us_states <- map_data("state") %>% select(1,2,3,5)
colnames(us_states) <- c("long1", "lat1", "group","State")
#join state, city, year stage and EH data together
us_cities_p <- left_join(eh_payment, city_location)
us_providers <- left_join(us_cities_p,us_states)
providers <- as.data.frame(left_join(us_providers,new_eh) %>% select(1,2,13,15,16,19,20,21))

write.csv(providers,"processed_data.csv")