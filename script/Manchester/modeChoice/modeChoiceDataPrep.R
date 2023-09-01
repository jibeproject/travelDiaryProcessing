library(tidyverse)
setwd("~/Documents/manchester")


### READ RAW TRIP DATA ###
TRADS <- readRDS("~/RProjects/travelDiaryProcessing/data/Manchester/processed/TRADS.rds")
trips <- TRADS$raw$trips %>% 
  select(IDNumber,PersonNumber,TripNumber,StartPurpose,EndPurpose,StartOutputArea,EndOutputArea,TripLength) %>%
  mutate(sameOD = StartOutputArea == EndOutputArea,
         commute = 
           (StartPurpose == "Home" & EndPurpose == "Usual place of work") | 
           (StartPurpose == "Home" & EndPurpose == "Education as pupil, student") | 
           (StartPurpose == "Usual place of work" & EndPurpose == "Home") |
           (StartPurpose == "Education as pupil, student" & EndPurpose == "Home"))

# Read (new) OA-level results 
routesOA <- readr::read_csv("TfGM/routes.csv", col_types = "ciiccccccccinnnnn") %>% 
  transmute(IDNumber,PersonNumber,TripNumber,
            orig_gm = na_if(OriginWithinBoundary,"null") == "true",
            dest_gm = na_if(DestinationWithinBoundary,"null") == "true",
            gm = orig_gm & dest_gm,
            Route,time,dist) %>%
  pivot_wider(names_from = "Route", values_from = c(time,dist))

meanBikeSpeed <- mean(routesOA$dist_bike_short / routesOA$time_bike_short, na.rm = TRUE)
meanWalkSpeed <- mean(routesOA$dist_walk_short / routesOA$time_walk_short, na.rm = TRUE)

# Read (old) micro-level output. Re-do speeds as averages as previous calculations were based on old averages.
routesMicro <- read_csv("TfGM/microOutput.csv") %>%
  semi_join(routesOA) %>%
  transmute(IDNumber,PersonNumber,TripNumber,
            orig_gm_micro = na_if(OriginWithinBoundary,"null") == "true",
            dest_gm_micro = na_if(DestinationWithinBoundary,"null") == "true",
            gm_micro = orig_gm_micro & dest_gm_micro,
            time_car_micro = CarTime,
            dist_bike_micro = BikeCost * 1000,
            time_bike_micro = dist_bike_micro / meanBikeSpeed,
            dist_walk_micro = WalkCost * 1000,
            time_walk_micro = time_bike_micro / meanWalkSpeed)

routes <- trips %>% 
  left_join(routesOA) %>% 
  left_join(routesMicro) %>%
  filter(gm_micro,gm) %>%
  mutate(car_time = time_car_micro,
         bike_time = ifelse(sameOD,time_bike_micro,time_bike_fast),
         walk_time = ifelse(sameOD,time_walk_micro,time_walk_fast),
         bike_dist = dist_bike_micro,
         walk_dist = dist_walk_micro)

# Rotues
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_dist,walk_dist), "TfGM/routesShort.csv")
write_csv(select(routes,IDNumber,PersonNumber,TripNumber,car_time,bike_time,walk_time), "TfGM/routesFast.csv")


### PREP OUTPUT AREA LEVEL DATA ###
intrazonalRoutes <- routes %>% filter(sameOD) %>% transmute(IDNumber,PersonNumber,TripNumber,geo_code = StartOutputArea)

zoneLinksWalk <- read_csv("TfGM/corridors/outputAreaWalk.csv")
zoneLinksBike <- read_csv("TfGM/corridors/outputAreaBike.csv")

intrazonalWalk <- left_join(intrazonalRoutes,zoneLinksWalk, multiple = "all") %>% select(-geo_code) %>% mutate(detour = 0)
intrazonalBike <- left_join(intrazonalRoutes,zoneLinksBike, multiple = "all") %>% select(-geo_code) %>% mutate(detour = 0)

write_csv(intrazonalWalk,"TfGM/corridors/intrazonalWalk.csv")
write_csv(intrazonalBike,"TfGM/corridors/intrazonalBike.csv")

### PREP NETWORK DATA / ATTRIBUTES ###
networkBike <- sf::read_sf("network/network2wayBike.gpkg") %>% 
  sf::st_drop_geometry() %>% 
  transmute(linkID,length,time = cycleTime,
            vgvi,shannon,POIs,negPOIs,crime,streetLights,stressLink = f_bikeStress,stressJct = f_bikeStressJct)

networkWalk <- sf::read_sf("network/network2wayWalk.gpkg") %>%
  sf::st_drop_geometry() %>% 
  transmute(linkID,length,time = walkTime,
            vgvi,shannon,POIs,negPOIs,crime,streetLights,stressLink = f_walkStress,stressJct = f_walkStressJct)

# Write network data with attributes
write_csv(networkBike,"TfGM/routeLinks/networkBike.csv")
write_csv(networkWalk,"TfGM/routeLinks/networkWalk.csv")