library(tidyverse)
setwd("~/Documents/manchester")

### READ RAW PERSONS DATA ###
TRADS <- readRDS("~/Documents/JIBE/travelDiaryProcessing/data/Manchester/processed/TRADS.rds")
indiv <- TRADS$raw$indiv %>% 
  select(IDNumber,PersonNumber,OutputArea,WorkOutputArea) %>%
  mutate(WorkOutputArea = gsub(" ","",WorkOutputArea)) %>%
  filter(WorkOutputArea != "") %>%
  mutate(sameOD = OutputArea == WorkOutputArea)

# Read (new) OA-level results 
routesOA <- readr::read_csv("TfGM/modeSetRoutes.csv", col_types = "ciilllllcccinnnnn", na = c("","null")) %>% 
  transmute(IDNumber,PersonNumber,TripNumber,HomeWithinBoundary,DestinationWithinBoundary,
            home_gm = HomeWithinBoundary,
            dest_gm = DestinationWithinBoundary,
            gm = home_gm & dest_gm,
            Route,time,dist) %>%
  pivot_wider(names_from = "Route", values_from = c(time,dist))

meanCarSpeed <- mean(routesOA$dist_car_congested / routesOA$time_car_congested, na.rm = TRUE)
meanBikeSpeed <- mean(routesOA$dist_bike_short / routesOA$time_bike_short, na.rm = TRUE)
meanWalkSpeed <- mean(routesOA$dist_walk_short / routesOA$time_walk_short, na.rm = TRUE)

# Read micro-level output (trip length available only) for trips with matching start/end OA to home and workplace
routesMicro <- TRADS$raw$trips %>% 
  semi_join(indivSameOD,by = c("IDNumber","PersonNumber","StartOutputArea" = "OutputArea","EndOutputArea" = "WorkOutputArea")) %>%
  select(IDNumber,PersonNumber,TripNumber,StartOutputArea,EndOutputArea,TripLength) %>%
  group_by(IDNumber,PersonNumber) %>%
  summarise(dist_micro = mean(TripLength),
            car_time_micro = dist_micro / meanCarSpeed,
            bike_time_micro = dist_micro / meanBikeSpeed,
            walk_time_micro = dist_micro / meanWalkSpeed) %>%
  ungroup()

routes <- indiv %>% 
  left_join(routesOA) %>% 
  left_join(routesMicro) %>%
  filter(gm) %>%
  mutate(car_time = ifelse(sameOD,car_time_micro,time_car_congested),
         bike_time = ifelse(sameOD,bike_time_micro,time_bike_fast),
         walk_time = ifelse(sameOD,walk_time_micro,time_walk_fast),
         bike_dist = ifelse(sameOD,dist_micro,dist_bike_short),
         walk_dist = ifelse(sameOD,dist_micro,dist_walk_short))

# Rotues
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_dist,walk_dist), "TfGM/routesShort.csv")
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_time,walk_time), "TfGM/routesFast.csv")