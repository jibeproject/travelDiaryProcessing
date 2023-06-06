###### CONVERT OUTPUT AREAS TO X/Y COORDINATES BASED ON POPULATION-WEIGHTED CENTROIDS ######
# To be run after read.R

#################   REQUIRED TO PREPARE DATASET FOR ROUTING IN MATSIM     ################# 
library(tidyverse)
trips <- readRDS("data/Manchester/processed/TRADS.rds")$raw$trips

###### Read population-weighted centroids ######
centroids <- sf::read_sf("~/Documents/TfGM/Output_Areas__December_2011__Population_Weighted_Centroids-shp/Output_Areas__December_2011__Population_Weighted_Centroids.shp")
centroids <- centroids %>% sf::st_drop_geometry() %>% cbind(sf::st_coordinates(centroids$geometry)) %>% select(OA11CD,X,Y)

# Create trips file with only the data required for routing
tripsWithXY <- trips %>% 
  select(IDNumber,PersonNumber,TripNumber,StartTime,EndTime,MainMode,StartPurpose,EndPurpose,OutputArea,StartOutputArea,EndOutputArea) %>%
  mutate(MainMode = gsub(",","",MainMode)) %>%
  left_join(centroids, by = c("OutputArea" = "OA11CD")) %>%
  rename(HomeEasting = X, HomeNorthing = Y) %>%
  left_join(centroids, by = c("StartOutputArea" = "OA11CD")) %>%
  rename(StartEasting = X, StartNorthing = Y) %>%
  left_join(centroids, by = c("EndOutputArea" = "OA11CD")) %>%
  rename(EndEasting = X, EndNorthing = Y)

write.table(tripsWithXY,file = "data/Manchester/processed/tripsWithXY.csv", sep = ";", 
            row.names = FALSE, quote = FALSE)

###### CREATE SAMPLE (USEFUL FOR TESTING/DEBUGGING) ######
filtered <- filter(tripsWithXY,
                  (IDNumber == "MA36068N" & PersonNumber == 4 & TripNumber == 1) |
                     (IDNumber == "MA44388D" & PersonNumber == 1 & TripNumber == 1) |
                     (IDNumber == "MA36169K" & PersonNumber == 3 & TripNumber == 1) |
                     (IDNumber == "MA35690Q" & PersonNumber == 1 & TripNumber == 2) |
                     (IDNumber == "BL39631G" & PersonNumber == 1 & TripNumber <= 2) |
                     (IDNumber == "BL43452M" & PersonNumber == 2 & TripNumber <= 2) |
                     (IDNumber == "WG43154L" & PersonNumber == 3 & TripNumber <= 2) |
                     (IDNumber == "WG47464J" & PersonNumber == 2 & TripNumber <= 2) |
                     (IDNumber == "SK46895N" & PersonNumber == 1 & TripNumber <= 2) |
                     (IDNumber == "TD41242H" & PersonNumber == 1 & TripNumber == 3) |
                     (IDNumber == "TD41242H" & PersonNumber == 1 & TripNumber == 4) |
                     (IDNumber == "RD37516L" & PersonNumber == 5 & TripNumber <= 2) |
                     (IDNumber == "BU39803Q" & PersonNumber == 2 & TripNumber <= 2) | 
                     (IDNumber == "TA38220H" & PersonNumber == 1 & TripNumber == 11) |
                     (IDNumber == "TA38220H" & PersonNumber == 1 & TripNumber == 12) |
                     (IDNumber == "BL35304P" & PersonNumber == 1 & TripNumber <= 2))

write.table(filtered,file = "data/Manchester/processed/tripsWithXY_sample.csv", sep = ";", 
            row.names = FALSE, quote = FALSE)
