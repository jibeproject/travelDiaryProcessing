###### CONVERT OUTPUT AREAS TO X/Y COORDINATES BASED ON POPULATION-WEIGHTED CENTROIDS ######
#################   REQUIRED TO PREPARE DATASET FOR ROUTING IN MATSIM     ################# 
library(tidyverse)
trips <- readRDS("data/Manchester/processed/TRADS.rds")$raw$trips

###### Read population-weighted centroids ######
centroids <- sf::read_sf("~/Documents/TfGM/Output_Areas__December_2011__Population_Weighted_Centroids-shp/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroids <- centroids %>% sf::st_drop_geometry() %>% cbind(sf::st_coordinates(centroids$geometry)) %>% select(OA11CD,X,Y)

# Create trips file with only the data required for routing
tripsWithXY <- trips %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,StartOutputArea,EndOutputArea) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.csv(tripsWithXY,file = "~/Documents/TfGM/tripsWithXY.csv", quote = FALSE)