#### Destination attraction ####
library(tidyverse)

## Read POIs
POIs <- readr::read_csv("~/Documents/manchester/JIBE/attraction/poi_XY.csv", col_names = TRUE,
                        col_types = cols_only(prop_id = "i", prop_area = "n", name = "c", use = "c", pointx_class = "i", classname = "c",
                                              feature_easting = "n", feature_northing = "n"))

POIs <- POIs %>% group_by(prop_id) %>% mutate(area = prop_area / n()) %>% ungroup()

## Disregarded uses
POIs$disregard <- POIs$use %in% c("")

## FOOD
foodstores <- read_csv("~/Documents/manchester/JIBE/attraction/AccesstoFood.csv", col_names = FALSE, col_types = "i")$X1

POIs$food <- POIs$pointx_class %in% foodstores

food <- POIs %>% filter(food,
                        use != "STORAGE/WAREHOUSING",
                        use != "STORAGE/WAREHOUSING WITH LINKED OFFICE BLOCK",
                        use != "INDUSTRY - MANUFACTURING/PROCESSING",
                        use != "GENERAL COMMERCIAL - MIXED USE") %>%
  transmute(ID = paste(c(1:n()),gsub("'","" , gsub("[^0-9A-Za-z///' ]","'" , name ,ignore.case = TRUE) ,ignore.case = TRUE)),
            X = feature_easting,
            Y = feature_northing,
            WEIGHT = area)

write_csv(food,"~/Documents/manchester/JIBE/accessibility/food.csv")

## SHOPS AND SERVICES
shopsServices <- read_csv("~/Documents/manchester/JIBE/attraction/AccesstoShopsandServ.csv", col_names = FALSE, col_types = "i")$X1

POIs$shopsServ <- POIs$pointx_class %in% shopsServ

shopsServ <- POIs %>% 
  filter(shopsServ) %>%
  transmute(ID = paste(c(1:n()),gsub("'","" , gsub("[^0-9A-Za-z///' ]","'" , name ,ignore.case = TRUE) ,ignore.case = TRUE)),
            X = feature_easting,
            Y = feature_northing,
            WEIGHT = area)

shopsServ <- filter(POIs,)

## GREENSPACE
greenspace <- read_csv("~/Documents/manchester/JIBE/accessibility/green_sites_access_XY.csv")

green <- greenspace %>% 
  transmute(ID = id_green,
            X,Y,
            WEIGHT = green_area)

write_csv(green,"~/Documents/manchester/JIBE/accessibility/green.csv")