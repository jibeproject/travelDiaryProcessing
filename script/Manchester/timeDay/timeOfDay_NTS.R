### Time of day choice modelling using NTS data ###  
library(tidyverse)

# Read datasets
hh <- read_tsv("data/UKDA-5340-tab/tab/household_eul_2002-2020.tab",col_names = TRUE,
               cols_only(HouseholdID = "c", HHoldGOR_B02ID = "i"))

trips <- read_tsv("data/UKDA-5340-tab/tab/trip_eul_2002-2020.tab",col_names = TRUE,
                  cols_only(HouseholdID = "c", MainMode_B03ID = "i", TripStart = "i", TripTotalTime = "i")) %>% left_join(hh)

selected_trips <- filter(trips, HHoldGOR_B02ID == 2, MainMode_B03ID >= 5 & MainMode_B03ID <= 16, !is.na(TripStart))

# Car trip start time distribution
ggplot(selected_trips,aes(x = TripStart)) + geom_density(adjust = 2) + 
  scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60))

# En-route distributions
TripStarts <- selected_trips$TripStart
TripTimes <- selected_trips$TripTotalTime
maxTime <- max(TripTimes)

result <- rep(0,1440)

for(i in 1:maxTime) {
  if(i %% 100 == 0) {
    print(i)
  }
  TripStarts = TripStarts + 1
  TripStarts[TripStarts == 1441] = 1
  now <- tabulate(TripStarts, nbins = 1440)
  result = result + now
  TripTimes = TripTimes - 1
  TripStarts[TripTimes < 1] = NA
}

mins <- data.frame(mins = 0:1439,
                   count = result,
                   wt = result / sum(result))


ggplot(mins,aes(x = mins,weight = wt)) + geom_density(adjust = 0.2) + 
  scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + xlab("En-route") + 
  ggtitle("North west england only")

d <- density(0:1439,weights = mins$wt, kernel = "gaussian",adjust = 0.2,from = 1,to = 1440, n = 1440)[["y"]]


# Plot density
mins$d <- d
ggplot(mins,aes(x = mins, y = d)) + geom_line() + 
  scale_x_continuous(breaks = seq(0,1440,180), minor_breaks = seq(0,1440,60)) + 
  xlab("En-route") + ggtitle("North west england only")

densities <- data.frame(time = 0:1439,
                        density = d)

# Vehicles en-route
write_csv(densities$density, file = "result/vehicleEnRouteTimes.csv")
