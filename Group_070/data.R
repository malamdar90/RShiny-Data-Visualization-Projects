

library(shiny)
library(lessR)
library(reader)
library(plyr)
library(ggplot2)
library(dplyr)


d <- read.csv("data.csv", stringsAsFactors = TRUE)
colnames(d)[colnames(d) == "Watch.time.Minutes."] <- "WatchTime"
colnames(d)[colnames(d) == "Stream.time.minutes."] <- "StreamTime"

tt <- Read("top.xlsx", stringsAsFactors = TRUE)


rcount <- nrow(d)
sd <- subset(d, select=-(Channel))
R <- cr(sd)$R

choices <- names(d)[2:(length(d) - 3)]