## Add Main OA to original dataset (for rubber-banding analysis)
library(tidyverse)
rm(list = ls())

TRADS <- readRDS("data/Manchester/processed/TRADS_routed.rds")

trips <- TRADS$trips_abm %>% 
  left_join(select(TRADS$trips,hh.id,p.id,t.id,t.startOA,t.endOA)) %>% 
  filter(tour.purpose != "disregard") %>%
  group_by(hh.id,p.id,tour.id,tour.purpose) %>% 
  mutate(t.n_mainOA = n_distinct(t.endOA[act.type == "main"]),
         t.mainOA = first(t.endOA[act.type == "main"])) %>%
  filter(t.n_mainOA == 1) %>%
  ungroup() %>%
  transmute(IDNumber = hh.id,
            PersonNumber = p.id,
            TripNumber = t.id,
            MainOutputArea = t.mainOA)

# Read centroids
centroids <- sf::read_sf("~/Documents/manchester/gis/OAcentroids/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroids <- centroids %>% sf::st_drop_geometry() %>% cbind(sf::st_coordinates(centroids$geometry)) %>% select(OA11CD,X,Y)

trips_raw <- trips %>% 
  left_join(TRADS$raw$trips) %>%
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,OutputArea,MainOutputArea,StartOutputArea,EndOutputArea)
  

# Create trips file with only the data required for routing
tripsWithXY <- trips_raw %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,OutputArea,MainOutputArea,StartOutputArea,EndOutputArea) %>%
  left_join(centroids, by = c("OutputArea" = "OA11CD")) %>%
  rename(HomeEasting = X, HomeNorthing = Y) %>%
  left_join(centroids, by = c("MainOutputArea" = "OA11CD")) %>%
  rename(MainEasting = X, MainNorthing = Y) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.csv(tripsWithXY,file = "data/Manchester/processed/tripsWithXY_main.csv", quote = FALSE)