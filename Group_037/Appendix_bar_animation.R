library(ggplot2)
library(lubridate)
library(tidyverse)
library(usmap)
library(gganimate)
library(scales)
library(gifski)
library(viridis)
library(reshape2)
library(ggthemes)
library(sf)
library(tmap)
library(spData)
library(viridis)
library(lubridate)
library(base)
library(openintro)

#setwd("~/Desktop/OneDrive - Johns Hopkins/Study/JHU/A_JHU_Terms/Summer/Data visualization/Project/Prepare")
daily.state <- read.csv("state.csv")


#########################
#loading dataset
daily.state$date <- ymd(daily.state$date)
#selecting columns to work with
totalCase <- daily.state %>% select("date",
                                    "state",
                                    "positive")
names(totalCase)[3] <- c("total")

#retrieve all states 
n <- unique(totalCase$state) 

#function to sum the total cases per state
state <- function(x){
  total2 <- totalCase %>% filter(state == x)
  sum(total2$total)
}

#return a list with all total cases per state
state_total <- sapply(n, function(x) state(x))

#creating a dataframe with top 10 total cases per state, desc
df <- do.call(rbind, Map(data.frame, 
                         state=n,
                         Total_confirmed=state_total))
df2 <- df %>% arrange(desc(Total_confirmed))
df3 <- head(df2, n=10)
            #write.csv(df3,"~/Desktop/total_confirmed.csv")

#plotting the top 10 countries leading in the total suicide rates
ggplot(df3,
       aes(reorder(state, Total_confirmed),
           Total_confirmed,
           fill=as.factor(state))) +
  geom_col() +
  coord_flip(clip = "off", expand = FALSE) +
  guides(fill = FALSE) +
  labs(title="Total confirmed cases per state from xxxx-xxxx", 
       y="Total cases per state", x="State")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste(Total_confirmed,"")), 
            hjust = 1)

#subset initial data with top 10 countries
top_case <- totalCase %>%  
  # group in date  
  dplyr::group_by(date) %>%
  # rank descending; label number
  mutate(rank = base::rank(-total),
         Value_lbl = paste0(" ", round(total))) 
#filtering years with consistent data
top_case2 <- top_case # %>% filter(date %in%c(1990:2014)) 
#top_case2$sex<-as.factor(top_suicide2$sex)

#summing the total male & female suicides per country for each year
sm3 <- aggregate(total~state+date,top_case2,sum)

#* 1 ensures we have non-integer ranks while sliding
sm4 <- sm3 %>% group_by(date) %>% 
  mutate(rank = min_rank(-total) * 1) %>%
  ungroup()

#plotting static plot
static_plot <- ggplot(sm4,
                    aes(rank,
                        group = state,
                        fill=as.factor(state),
                        color=as.factor(state))) +
  geom_tile(aes(y = total/2,
                height = total,
                width = 0.9), 
            alpha = 0.8, 
            color = NA) +
  geom_text(aes(y = 0, 
                label = paste(state, "")), 
            vjust = 0.2, 
            hjust = 1) +
  geom_text(aes(y=total, 
                label = paste("",total)), 
            hjust=0)+
  coord_flip(clip = "off", 
             expand = TRUE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  scale_x_reverse() +
  guides(color = FALSE, 
         fill = FALSE) +
  theme_minimal() +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="Black", vjust=-1),
    plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
    plot.caption =element_text(size=10, hjust=0.5, face="italic", color="grey"),
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    plot.margin = margin(1,0.2,1,4, "cm"),
    panel.grid =element_blank()
  ) +
  xlab("") +
  ylab("")
static_plot

#creating final animation
plt <- static_plot + 
  transition_states(states = date, 
                    transition_length = 1, 
                    state_length = 3) + 
  ease_aes("cubic-in-out") +
  view_follow(fixed_y = TRUE) +
  labs(title = "Confirmed cases in each state (Top 10): {closest_state}",
       subtitle = " ",       
       caption = "The COVID Tracking Project | Data Source: CovidTracking.com")


#rendering the animation for gif
final_animation <- animate(plt, 
                           nframes = 1400, 
                           fps = 20, 
                           width = 1200, 
                           height = 600, 
                           renderer = gifski_renderer("bar.gif"))

















