options(digits = 15)

############## DAY & TIME MODELLING FOR NTS ############## 

######################## SETUP ######################## 
library(tidyverse)
rm(list = ls())

# Read model data
trips <- NTS$trips %>%
  filter(t.purpose != "NA", !is.na(t.arrivalTime)) %>% 
  mutate(t.purpose = factor(t.purpose))

######################## TIME OF DAY  ######################## 
timeOfDayTable <- data.frame(minute = 1:1440)
densityData <- list()
homeBasedPurposes <- c("HBW","HBE","HBA","HBS","HBR","HBO")
arrivalTimePurposes <- c("HBW","HBE","HBA","HBS","HBR","HBO","NHBW","NHBO")

# Prepare activity duration data
homeBasedTrips <- NTS$trips %>% 
  filter(t.full_purpose %in% homeBasedPurposes) %>% 
  mutate(lastTrip = hh.id != lead(hh.id, default = 0) | (hh.id == lead(hh.id, default = 0) & (p.id != lead(p.id, default = 0))),
         hasReturn = !lastTrip & t.purpose != "NA" & lead(t.purpose) == "NA" & t.full_purpose == lead(t.full_purpose),
         t.activityDuration = case_when(hasReturn ~ (1440*lead(t.day.id) + lead(t.departureTime)) - (1440*t.day.id + t.departureTime + t.travelTime))) %>%
  filter(hasReturn,
         t.activityDuration <= 1440)

# Initial plots
ggplot(filter(trips, t.purpose %in% arrivalTimePurposes), aes(x = t.arrivalTime)) + 
  geom_density(adjust = 3) + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity arrival times") + labs(colour = "purpose", x = "arrival time (minutes from midnight)") + 
  facet_wrap(~t.purpose, scales = "free_y")
ggplot(homeBasedTrips, aes(x = t.activityDuration)) + 
  geom_density(adjust = 3) + scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  ggtitle("Activity durations") + labs(colour = "purpose", x = "minutes")  + 
  facet_wrap(~t.purpose, scales = "free_y")

# Create arrival time table
for(purpose in arrivalTimePurposes) {
  data <- trips[trips$t.purpose == purpose,]
  col_name = paste0("arrival_",purpose)
  densityData[[col_name]] <- density(data$t.arrivalTime, kernel = "gaussian",adjust = 3,from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}

# Create duration table
for(purpose in homeBasedPurposes) {
  data <- homeBasedTrips[homeBasedTrips$t.purpose == purpose,]
  col_name = paste0("duration_",purpose)
  densityData[[col_name]] <- density(data$t.activityDuration, kernel = "gaussian",adjust = 3,from = 1,to = 1440, n = 1440)
  timeOfDayTable[[col_name]] <- densityData[[col_name]][["y"]]
}

# Plot table results to check
timeOfDayTable %>%
  pivot_longer(cols = starts_with("arrival")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free_y")

timeOfDayTable %>%
  pivot_longer(cols = starts_with("duration")) %>%
  ggplot(aes(x = minute, y = value, colour = name)) + geom_line() + facet_wrap(~name, scales = "free_y")

######################## WRITE OUTPUTS ######################## 
write.csv(timeOfDayTable, file = "result/England/timeOfDay.csv", quote = FALSE, row.names = FALSE)
