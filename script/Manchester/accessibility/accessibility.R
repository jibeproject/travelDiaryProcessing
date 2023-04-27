#### Destination attraction ####
library(tidyverse)

## FOOD
foodstores <- read_csv("~/Documents/manchester/JIBE/attraction/AccesstoFood.csv", col_names = FALSE, col_types = "i")$X1
POIs <- readr::read_csv("~/Documents/manchester/JIBE/attraction/poi_XY.csv", col_names = TRUE,
                        col_types = cols_only(prop_id = "i", prop_area = "n", name = "c", use = "c", pointx_class = "i", classname = "c",
                                              feature_easting = "n", feature_northing = "n"))

POIs$food <- POIs$pointx_class %in% foodstores

POIs <- POIs %>% group_by(prop_id) %>% mutate(area = prop_area / n()) %>% ungroup()

food <- filter(POIs, food,
               use != "STORAGE/WAREHOUSING",
               use != "STORAGE/WAREHOUSING WITH LINKED OFFICE BLOCK",
               use != "INDUSTRY - MANUFACTURING/PROCESSING",
               use != "GENERAL COMMERCIAL - MIXED USE") %>%
  transmute(ID = paste(c(1:n()),gsub("'","" , gsub("[^0-9A-Za-z///' ]","'" , name ,ignore.case = TRUE) ,ignore.case = TRUE)),
            X = feature_easting,
            Y = feature_northing,
            WEIGHT = area)

write_csv(food,"~/Documents/manchester/JIBE/accessibility/food.csv")

## GREENSPACE
greenspace <- read_csv("~/Documents/manchester/JIBE/accessibility/green_sites_access_XY.csv")

green <- greenspace %>% 
  transmute(ID = id_green,
            X,Y,
            WEIGHT = green_area)

write_csv(green,"~/Documents/manchester/JIBE/accessibility/green.csv")

## RESULTS
bikeJibe <- read_delim("~/Documents/manchester/JIBE/accessibility/results/foodBikeJibe.csv", delim = ";")
bikeDist <- read_delim("~/Documents/manchester/JIBE/accessibility/results/foodBikeDist.csv", delim = ";")
walkJibe <- read_delim("~/Documents/manchester/JIBE/accessibility/results/foodWalkJibe.csv", delim = ";")
walkDist <- read_delim("~/Documents/manchester/JIBE/accessibility/results/foodWalkDist.csv", delim = ";")

bike <- left_join(bikeJibe,bikeDist, by = c("ZONE")) %>%
  rename(bikeJibe = ACCESSIBILITY.x,
         bikeDist = ACCESSIBILITY.y) %>%
  mutate(ZONE = gsub(" ","",ZONE),
         bikeRatio = bikeDist / bikeJibe)

walk <- left_join(walkJibe,walkDist, by = c("ZONE")) %>%
  rename(walkJibe = ACCESSIBILITY.x,
         walkDist = ACCESSIBILITY.y) %>%
  mutate(ZONE = gsub(" ","",ZONE),
         walkRatio = walkDist / walkJibe)

foodResults <- left_join(bike, walk)

write_delim(foodResults,"~/Documents/manchester/JIBE/accessibility/results/foodResults.csv", delim = ";")
