# Initializing UFC data

library(tidyverse)
library(lubridate)

UFCProcess <- function(df){
  df$fight.id <- as.character(df$fight.id)
  df$round <- as.factor(df$round)
  df$time <- as.character(df$time)
  df$weight.class.id <- as.factor(df$weight.class.id)
  df$date <- as.Date(df$date, "%m/%d/%y")
  df$year <- year(df$date)
  df$month <- month(df$date)
  df$day <- day(df$date)
  df$minute <- as.integer(substr(df$time, 1, 1))
  df$second <- as.integer(substr(df$time, 3, 4))
  df$round.seconds <- df$minute * 60 + df$second
  df$fight.seconds <- (as.integer(df$round) - 1) * 300 + df$round.seconds
  df$title.fight <- as.factor(df$title.fight)
  df$five.rounds <- as.factor(df$five.rounds)
  df$gender <- as.factor(ifelse(df$weight.class.id == 24 | 
                                  df$weight.class.id == 25 |
                                  df$weight.class.id == 32 | 
                                  df$weight.class.id == 88, "Female", "Male"))
  return(df)
}

ufc <- read.csv("UFCMaster.csv")

ufc <- UFCProcess(ufc)
