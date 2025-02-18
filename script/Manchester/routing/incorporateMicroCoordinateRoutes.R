library(tidyverse)

### READ RAW PERSONS DATA ###
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")
trips <- TRADS$raw$trips %>% 
  select(IDNumber,PersonNumber,TripNumber,StartPurpose,EndPurpose,StartOutputArea,EndOutputArea,TripLength) %>%
  mutate(sameOD = StartOutputArea == EndOutputArea,
         commute = 
           (StartPurpose == "Home" & EndPurpose == "Usual place of work") | 
           (StartPurpose == "Home" & EndPurpose == "Education as pupil, student") | 
           (StartPurpose == "Usual place of work" & EndPurpose == "Home") |
           (StartPurpose == "Education as pupil, student" & EndPurpose == "Home"))

setwd("~/manchester")

# Read (new) OA-level results 
routesOA <- readr::read_csv("TfGM/routesForModeChoice.csv", col_types = "ciilllllcccinnnnn", na = c("","null")) %>% 
  transmute(IDNumber,PersonNumber,TripNumber,HomeWithinBoundary,DestinationWithinBoundary,
            home_gm = HomeWithinBoundary,
            dest_gm = DestinationWithinBoundary,
            gm = home_gm & dest_gm,
            Route,time,dist) %>%
  pivot_wider(names_from = "Route", values_from = c(time,dist)) %>%
  select(-time_car_freespeed,-dist_car_freespeed,-dist_car_congested)


routes <- indiv %>% 
  left_join(routesOA) %>% 
  filter(gm) %>%
  mutate(car_time = ifelse(sameOD,car_time_micro,time_car_congested),
         bike_time = ifelse(sameOD,bike_time_micro,time_bike_fast),
         walk_time = ifelse(sameOD,walk_time_micro,time_walk_fast),
         bike_dist = ifelse(sameOD,dist_micro,dist_bike_short),
         walk_dist = ifelse(sameOD,dist_micro,dist_walk_short))

# Rotues
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_dist,walk_dist), "TfGM/routesShort.csv")
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_time,walk_time), "TfGM/routesFast.csv")