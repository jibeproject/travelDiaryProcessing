###### CONVERT OUTPUT AREAS TO X/Y COORDINATES BASED ON POPULATION-WEIGHTED CENTROIDS ######
# Creates anaonymised car trips file from diary data with minimal identifying information

#################   REQUIRED TO PREPARE DATASET FOR ROUTING IN MATSIM     ################# 
library(tidyverse)
raw <- readRDS("data/Manchester/processed/TRADS.rds")$raw
trips <- raw$trips

###### Read population-weighted centroids ######
net_boundary <- sf::st_read("data/Manchester/gis/NetworkBoundary.gpkg")
gm_boundary <- sf::st_read("data/Manchester/gis/GM_Boundary.gpkg")
centroids <- sf::st_read("data/Manchester/gis/OA_centroids/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroid_coords <- centroids %>% sf::st_transform(4326) %>% sf::st_coordinates(geometry)
reference <- centroids %>% mutate(IN_NET = lengths(sf::st_intersects(.,net_boundary)) > 0,
                                  IN_GM = lengths(sf::st_intersects(.,gm_boundary)) > 0) %>% sf::st_drop_geometry() %>% cbind(centroid_coords) %>% select(OA11CD,IN_NET,IN_GM,X,Y)


### Create trips file with data required for MATSim processing
tripsWithXY <- trips %>% 
  filter(MainMode == "Car or van driver") %>%
  select(StartTime,StartOutputArea,EndOutputArea) %>%
  mutate(StartTime = round(StartTime)) %>%
  left_join(reference, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartX = X, StartY = Y, StartNet = IN_NET, StartGM = IN_GM) %>%
  left_join(reference, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndX = X, EndY = Y, EndNet = IN_NET, EndGM = IN_GM) %>%
  filter(StartNet & EndNet & (StartGM | EndGM)) %>%
  mutate(Time = (StartTime + round(rnorm(nrow(.),mean = 0, sd = 600))) %% 86400) %>%
  select(-StartTime,-ends_with("OutputArea"),-ends_with("Net"),-ends_with("GM")) %>%
  na.omit() %>%
  arrange(Time)

write.table(tripsWithXY,file = "data/Manchester/processed/testTrips.csv", sep = ";", 
            row.names = FALSE, quote = FALSE)
