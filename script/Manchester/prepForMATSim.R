###### CONVERT OUTPUT AREAS TO X/Y COORDINATES BASED ON POPULATION-WEIGHTED CENTROIDS ######
# To be run after read.R

#################   REQUIRED TO PREPARE DATASET FOR ROUTING IN MATSIM     ################# 
library(tidyverse)
trips <- readRDS("data/Manchester/processed/TRADS.rds")$raw$trips

###### Read population-weighted centroids ######
centroids <- sf::read_sf("~/Documents/manchester/gis/OAcentroids/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroids <- centroids %>% sf::st_drop_geometry() %>% cbind(sf::st_coordinates(centroids$geometry)) %>% select(OA11CD,X,Y)

# Create trips file with only the data required for routing
tripsWithXY <- trips %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,OutputArea,StartOutputArea,EndOutputArea) %>%
  left_join(centroids, by = c("OutputArea" = "OA11CD")) %>%
  rename(HomeEasting = X, HomeNorthing = Y) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.csv(tripsWithXY,file = "data/Manchester/processed/tripsWithXY.csv", quote = FALSE)