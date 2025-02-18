## DAY OF WEEK FREQUENCY TABLE ##

library(tidyverse)
rm(list = ls())

# Read useful functions
source("scripts/usefulFunctions.R", encoding = "UTF-8")

# Read model data
trips <- readRDS("data/model_data/trips.Rds") %>% 
  filter(t.purpose != "NA") %>% 
  mutate(t.purpose = factor(t.purpose),
         t.timeOfweek = factor(case_when(t.day.name == "monday" | t.day.name == "tuesday" | t.day.name == "wednesday"  | 
                                           t.day.name == "thursday"| t.day.name == "friday" ~ "weekday",
                                         TRUE ~ t.day.name), levels = c("weekday","saturday","sunday")),
         t.day = recode_factor(t.day.name, `monday` = "M", `tuesday` = "T", `wednesday` = "W",
                               `thursday` = "R", `friday` = "F", `saturday` = "S", `sunday` = "U", .ordered = TRUE))

# Create time of week table
timeOfWeekTable <- trips %>% 
  group_by(hh.id,p.id,t.purpose) %>%
  summarise(trips = n(),
            days = paste(sort(t.day),collapse = "")) %>%
  group_by(t.purpose, trips, days) %>%
  summarise(frequency = n()) %>%
  filter(frequency > 1, 
         trips <= 7 | ((t.purpose == "NHBW" | t.purpose == "HBO") & trips <= 9)) %>%
  rename(purpose = t.purpose)

# Write to CSV
write_csv(timeOfWeekTable,"daysOfWeek.csv")
