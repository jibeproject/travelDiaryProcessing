options(digits = 15)

######################## SETUP ######################## 
library(tidyverse)
rm(list = ls())

# Read useful functions
source("scripts/usefulFunctions.R", encoding = "UTF-8")

# Read model data
trips <- read_csv("data/manchester/processed/tripsForApollo.csv") %>% 
  filter(t.purpose != "NA") %>% 
  mutate(t.purpose = factor(t.purpose))

trips = trips %>% mutate(t.arrivalTime = t.arrivalTime/60,
                         t.departureTime = t.departureTime/60)

######################## TIME OF DAY  ######################## 
timeOfDayTable <- data.frame(minute = 1:1440)
densityData <- list()
homeBasedPurposes <- c("HBW","HBE","HBS","HBR","HBO","HBA")
arrivalTimePurposes <- c("HBW","HBE","HBS","HBR","HBO","HBA","NHBW","NHBO")

# Prepare activity duration data
homeBasedTrips <- read_csv("data/manchester/processed/tripsForApollo.csv") %>% 
  mutate(t.arrivalTime = t.arrivalTime/60,
         t.departureTime = t.departureTime/60,
         t.travelTime = t.travelTime/60)%>%
  filter(t.full_purpose %in% homeBasedPurposes) %>% 
  mutate(lastTrip = hh.id != lead(hh.id, default = "null") | (hh.id == lead(hh.id, default = "null") & (p.id != lead(p.id, default = 0))),
         hasReturn = !lastTrip & !is.na(t.purpose) & is.na(lead(t.purpose)) & t.full_purpose == lead(t.full_purpose),
         t.activityDuration = case_when(hasReturn ~ (lead(t.departureTime) - (t.departureTime + t.travelTime))))%>%
  filter(hasReturn,
         t.activityDuration <= 1440,
         t.activityDuration >0)

# Initial plots
ggplot(filter(trips, t.purpose %in% arrivalTimePurposes), aes(x = t.arrivalTime, colour = t.purpose)) + 
  geom_density(adjust = 1) + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity arrival times") + labs(colour = "purpose", x = "arrival time (minutes from midnight)") 
ggplot(homeBasedTrips, aes(x = t.activityDuration, colour = t.purpose)) + 
  geom_density() + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity durations") + labs(colour = "purpose", x = "minutes") 

# Create arrival time table
for(purpose in arrivalTimePurposes) {
  data <- trips[trips$t.purpose == purpose,]
  col_name = paste0("arrival_",purpose)
  # you can set the "adjust" parameter to define the smoothing bandwidth, the higher the smoother curve. 
  # we want the curve not too sharp
  densityData[[col_name]] <- density(data$t.arrivalTime, kernel = "gaussian",adjust = 3,from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}


# Create duration table
for(purpose in homeBasedPurposes) {
  data <- homeBasedTrips[homeBasedTrips$t.purpose == purpose,]
  col_name = paste0("duration_",purpose)
  densityData[[col_name]] <- density(data$t.activityDuration, kernel = "gaussian",from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}

# Plot table results to check
timeOfDayTable %>%
  pivot_longer(cols = starts_with("arrival")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line()

timeOfDayTable %>%
  pivot_longer(cols = starts_with("duration")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line()

######################## WRITE OUTPUTS ######################## 
write.csv(timeOfDayTable, file = "result/manchester/dayAndTime/timeOfDay.csv", quote = FALSE, row.names = FALSE)
