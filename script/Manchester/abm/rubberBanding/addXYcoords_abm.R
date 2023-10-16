## Add Main OA to original dataset (for rubber-banding analysis)
library(tidyverse)
rm(list = ls())

TRADS <- readRDS("data/Manchester/processed/TRADS.rds")

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
            MainOutputArea = t.mainOA,
            act.type)


# Read centroids
centroids <- sf::read_sf("data/manchester/gis/OA_centroids/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroids <- centroids %>% sf::st_drop_geometry() %>% cbind(sf::st_coordinates(centroids$geometry)) %>% select(OA11CD,X,Y)

# Create trips file with only the data required for routing
tripsWithXY <- trips %>% 
  left_join(TRADS$raw$trips) %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,MainMode,StartPurpose,EndPurpose,OutputArea,MainOutputArea,StartOutputArea,EndOutputArea) %>%
  left_join(centroids, by = c("OutputArea" = "OA11CD")) %>%
  rename(HomeEasting = X, HomeNorthing = Y) %>%
  left_join(centroids, by = c("MainOutputArea" = "OA11CD")) %>%
  rename(MainEasting = X, MainNorthing = Y) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.table(filtered,file = "data/Manchester/processed/tripsWithXY_abm.csv", sep = ";", row.names = FALSE, quote = FALSE)

# Create dataset of stops only
stops <- trips %>% filter(act.type == "stop_out" | act.type == "stop_return")

stopsWithXY <- stops %>%
  left_join(TRADS$raw$trips) %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,MainMode,StartPurpose,EndPurpose,OutputArea,MainOutputArea,StartOutputArea,EndOutputArea) %>%
  left_join(centroids, by = c("OutputArea" = "OA11CD")) %>%
  rename(HomeEasting = X, HomeNorthing = Y) %>%
  left_join(centroids, by = c("MainOutputArea" = "OA11CD")) %>%
  rename(MainEasting = X, MainNorthing = Y) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.table(stopsWithXY,file = "data/Manchester/processed/stopsWithXY_abm.csv", sep = ";", row.names = FALSE, quote = FALSE)

