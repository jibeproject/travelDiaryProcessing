rm(list=ls())

library(readr)
library(sf)
library(dplyr)
library(tidyverse)

source("functions.R")

# TODO: check the switch from LSOA to OA and vice versa/ clean up the code
# TODO: may be better to group all the data outputs in a single folder
# TODO: check that the spatial join makes sense using QGIS


########################################
#==# READ IN GEO DATASET #=============#
########################################


#geospatial_data <- sf::st_read("sa/sa1.gpkg") %>% 
#  mutate(IDspatial=sa1_maincode_2016) %>% 
#  select(IDspatial, geom)

# /Users/ismailsaadi/Cambridge/travel_survey/travelDiaryProcessing/data/Manchester/gis/zones/LSOA_studyArea.shp

#OA_lookup <- readr::read_csv(paste0("/Users/ismailsaadi/Cambridge/travel_survey/travelDiaryProcessing/data/Manchester/gis/OA_lookup.csv"), col_select = c("OA11CD","LSOA11CD","MSOA11CD"))
boundary_type <- "LSOA"  # Change this to "LSOA" when needed

zoning_system <- read_csv("geodata/OAList.csv")

OA_lookup <- zoning_system %>% 
  dplyr::select(OA21CD, lsoa21cd, msoa21cd) %>% 
  rename(LSOA21CD = lsoa21cd, MSOA21CD= msoa21cd)

geospatial_data_all <- sf::st_read(paste0("geodata/", boundary_type, "_2021.gpkg"))
check <- geospatial_data_all[[paste0(boundary_type, "21CD")]] %in% unique(OA_lookup[[paste0(boundary_type, "21CD")]])
geospatial_data <- geospatial_data_all[check,]

geospatial_data <- geospatial_data %>%
  mutate(IDspatial = LSOA21CD) %>%
  #mutate(IDspatial = case_when(
  #  boundary_type == "OA" ~ OA21CD,
  #  boundary_type == "LSOA" ~ LSOA21CD)) %>% # change here to have OA
  dplyr::select(IDspatial, SHAPE) %>%
  rename(geometry = SHAPE)

sf::st_write(geospatial_data, "manchesterData/geospatialData.gpkg", driver = "GPKG", append=FALSE)

########################################
#==# READ IN POI DATASETS #============#
########################################

#### COMBINE poi ####
poi <- list()

poi$community_health <- read_delim("poi/Community_health_resources.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$early_year <- read_delim("poi/Early_year_access.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$eating_establishments <- read_delim("poi/Eating_establishments.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$education <- read_delim("poi/Education.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$financial <- read_delim("poi/Financial.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$food_retail <- read_delim("poi/Food_retail.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$primary_health <- read_delim("poi/Primary_health_care.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$public_space <- read_delim("poi/Public_open_space_v3.0.csv", delim = ";", col_types = "cnnccnnnnn")
poi$recreational <- read_delim("poi/Recreational_sports_pitches_and_facilities.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$services <- read_delim("poi/Services.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
poi$social_cultural <- read_delim("poi/Social_and_culture_locations.csv", delim = ";", col_types = "cnnncc") %>% dplyr::select(-name)
#poi$hospitals <-read_delim("poi/hospitals.csv", delim = ";", col_types = "innncc")
#poi$schools <-read_delim("poi/schools.csv", delim = ";", col_types = "innncc") 
# TODO: check hospitals and schools


# Add indicators
poi$community_health$Indicator <- "Community health resources"
poi$early_year$Indicator <- "Early year access"
poi$eating_establishments$Indicator <- "Eating establishments"
poi$education$Indicator <- "Education"
poi$financial$Indicator <- "Financial"
poi$food_retail$Indicator <- "Food retail"
poi$primary_health$Indicator <- "Primary health care"
poi$public_space$Indicator <- "Public open space"
poi$recreational$Indicator <- "Recreational sports pitches and facilities"
poi$services$Indicator <- "Services"
poi$social_cultural$Indicator <- "Social and culture locations"

# Re public open space, we adjust weights based on the number of POIs with the same ID

poi$public_space <- poi$public_space %>%
  group_by(ID) %>%
  mutate(n = n(),           # Count number of rows per ID
         adjusted_WEIGHT = WEIGHT / n) %>%
  ungroup()

poi$public_space <- poi$public_space %>% 
  mutate(WEIGHT=adjusted_WEIGHT) %>% 
  dplyr::select(ID, X, Y, WEIGHT, ref_no, Indicator)

# poi$public_space$WEIGHT <- poi$public_space$WEIGHT^(1/4)


##
FLG_TR<-TRUE
if(FLG_TR){
  poi$social_cultural$WEIGHT2 <- poi$social_cultural$WEIGHT^(1/4)
  poi$education$WEIGHT2 <- poi$education$WEIGHT^(1/1)
  poi$primary_health$WEIGHT2 <- poi$primary_health$WEIGHT^(1/2)
  poi$community_health$WEIGHT2 <- poi$community_health$WEIGHT^(1/2)

  poi$recreational$WEIGHT2 <- poi$recreational$WEIGHT^(1/4) # 1/4
  poi$early_year$WEIGHT2 <- poi$early_year$WEIGHT^(1/2)
  poi$food_retail$WEIGHT2 <- poi$food_retail$WEIGHT^(1/2)
  poi$eating_establishments$WEIGHT2 <- poi$eating_establishments$WEIGHT^(1/2)

  poi$financial$WEIGHT2 <- poi$financial$WEIGHT^(1/2)
  poi$services$WEIGHT2 <- poi$services$WEIGHT^(1/2)
  # PT
  poi$public_space$WEIGHT2 <- poi$public_space$WEIGHT^(1/4)
  
  # poi$social_cultural$WEIGHT2 <- log(1+poi$social_cultural$WEIGHT)
  # poi$education$WEIGHT2 <- log(1+poi$education$WEIGHT)
  # poi$primary_health$WEIGHT2 <- log(1+poi$primary_health$WEIGHT)
  # poi$community_health$WEIGHT2 <- log(1+poi$community_health$WEIGHT)
  # 
  # poi$recreational$WEIGHT2 <- log(1+poi$recreational$WEIGHT)
  # poi$early_year$WEIGHT2 <- log(1+poi$early_year$WEIGHT)
  # poi$food_retail$WEIGHT2 <- log(1+poi$food_retail$WEIGHT)
  # poi$eating_establishments$WEIGHT2 <- log(1+poi$eating_establishments$WEIGHT)
  # 
  # poi$financial$WEIGHT2 <- log(1+poi$financial$WEIGHT)
  # poi$services$WEIGHT2 <- log(1+poi$services$WEIGHT)
  # # PT
  # poi$public_space$WEIGHT2 <- log(1+poi$public_space$WEIGHT)
  
}

allPois<-do.call(rbind, poi)
allPois <- st_as_sf(allPois, coords = c("X", "Y"), crs = 27700)
allPois<- st_join(allPois, geospatial_data, join = st_intersects)


if(FLG_TR){
  allPois <- allPois %>% dplyr::mutate(weight = WEIGHT2) # here to select which wights I want
}else{
  allPois <- allPois %>% dplyr::mutate(weight = WEIGHT)
}

allPois <- allPois %>% dplyr::select(c("IDspatial", "Indicator", "weight", "geometry"))

# Remove NA values in IDspatial
allPois <- allPois[!is.na(allPois$IDspatial), ]

allPois <- allPois %>%
  mutate(
    Indicator = case_when(
      Indicator == "Early year access" ~ "EYA",
      Indicator == "Education" ~ "EDU",
      Indicator == "Food retail" ~ "FR",
      Indicator == "Recreational sports pitches and facilities" ~ "RSPF",
      Indicator == "Social and culture locations" ~ "SCL",
      Indicator == "Community health resources" ~ "CHR",
      Indicator == "Eating establishments" ~ "EE",
      Indicator == "Financial" ~ "FIN",
      Indicator == "Primary health care" ~ "PHC",
      Indicator == "Public open space" ~ "POS",
      Indicator == "Services" ~ "SER",
      TRUE ~ Indicator  # Keep other values unchanged
    )
  )

# TODO: column Attribute does not exist for Manchester
sf::st_write(allPois, "manchesterData/allPois.gpkg", driver = "GPKG", append=FALSE)

# TODO: I already have it in map
#aggregatedPOIs <- allPois %>% 
#  st_drop_geometry() %>% 
#  group_by(IDspatial, Indicator) %>% 
#  summarise(total= sum(weight)) %>% 
#  ungroup() %>%
#  pivot_wider(names_from = Indicator, values_from= total, values_fill=0)

# head(aggregatedPOIs)

########################################
#==# READ IN TFGM DATASET #============#
########################################

# Read trips file
trips <- foreign::read.spss("diary/Yrs 6,7,8 HouseholdPersonTrip Academic.sav", to.data.frame = T)

# Aggregate purposes
trips$purpose <- recode(trips$EndPurpose, 
                        "Home" = "home",
                        "Usual place of work" = "work",
                        "Work - Business, other" = "business",
                        "Moving people or goods in connection with employment" = "business",
                        "Education as pupil, student" = "education",
                        "Shopping Food" = "shop",
                        "Shopping Non food" = "shop",
                        "Social - Entertainment, recreation, Participate in sport, pub, restaurant" = "recreation",
                        "Tourism, sightseeing" = "recreation",
                        "Escorting to place of work, pick-up, drop-off" = "escort",
                        "Escorting to place of education, pick-up, drop-off" = "escort",
                        "Childcare  taking or collecting child to or from babysitter, nursery etc" = "escort",
                        "Accompanying or giving lift to other person, not school, or work" = "escort",
                        "Visit friends or relatives" = "visit",
                        "Use Services, Personal Business, bank, hairdresser, library etc" = "other",
                        "Health or medical visit" = "other",
                        "Worship or religious observance" = "other",
                        "Unpaid, voluntary work" = "other",
                        "Staying at hotel or other temporary accommodation" = "other",
                        "Round trip walk, cycle, drive for enjoyment" = "rrt")

trips$purposeAtOrigin <- recode(trips$StartPurpose, 
                        "Home" = "home",
                        "Usual place of work" = "work",
                        "Work - Business, other" = "business",
                        "Moving people or goods in connection with employment" = "business",
                        "Education as pupil, student" = "education",
                        "Shopping Food" = "shop",
                        "Shopping Non food" = "shop",
                        "Social - Entertainment, recreation, Participate in sport, pub, restaurant" = "recreation",
                        "Tourism, sightseeing" = "recreation",
                        "Escorting to place of work, pick-up, drop-off" = "escort",
                        "Escorting to place of education, pick-up, drop-off" = "escort",
                        "Childcare  taking or collecting child to or from babysitter, nursery etc" = "escort",
                        "Accompanying or giving lift to other person, not school, or work" = "escort",
                        "Visit friends or relatives" = "visit",
                        "Use Services, Personal Business, bank, hairdresser, library etc" = "other",
                        "Health or medical visit" = "other",
                        "Worship or religious observance" = "other",
                        "Unpaid, voluntary work" = "other",
                        "Staying at hotel or other temporary accommodation" = "other",
                        "Round trip walk, cycle, drive for enjoyment" = "rrt")

trips$mitoPurpose <- with(trips, case_when(((purposeAtOrigin == "home" & purpose == "work") | (purposeAtOrigin == "work" & purpose == "home")) ~ "HBW",
                                           ((purposeAtOrigin == "home" & purpose == "education") | (purposeAtOrigin == "education" & purpose == "home")) ~ "HBE",
                                           ((purposeAtOrigin == "home" & purpose == "shop") | (purposeAtOrigin == "shop" & purpose == "home")) ~ "HBS",
                                           ((purposeAtOrigin == "home" & purpose == "escort") | (purposeAtOrigin == "escort" & purpose == "home")) ~ "HBA",
                                           ((purposeAtOrigin == "home" & purpose == "recreation") | (purposeAtOrigin == "recreation" & purpose == "home")) ~ "HBR",
                                           ((purposeAtOrigin == "home" & purpose == "other") | (purposeAtOrigin == "other" & purpose == "home")) ~ "HBO",
                                           ((purposeAtOrigin == "home" & purpose == "rrt") | (purposeAtOrigin == "rrt" & purpose == "home")) ~ "RRT",
                                           (purpose == "home") ~ "NA",
                                           (purposeAtOrigin == "work" | purpose == "work") ~ "NHBW",
                                           TRUE ~ "NHBO"))

# remove trips to Airport
# trips <- trips %>% filter(EndOutputArea != "E01005316")

# Define coefficients to find associations between purposes and mitoPurposes
tripsSubset<- trips %>%
  filter(purpose %in% c("work", "education", "escort", "other", "recreation", "shop", "rrt")) # no double counting
convert<-table(tripsSubset$purpose, tripsSubset$mitoPurpose)
f<-row.names(convert) %in% c("work","education","shop","escort","recreation","other", "rrt")
convert <- convert[f,]
convert<-t(round(t(convert)/colSums(convert), 2))
convert<- as.data.frame(convert) %>% pivot_wider(names_from=Var1, values_from=Freq)

trips <- left_join(trips, OA_lookup, by=c("EndOutputArea"="OA21CD"))
trips <- trips %>% 
  mutate(OA21CD = EndOutputArea)

test <- trips %>% count(EndPurpose, purpose) %>% arrange(desc(n)) # ??
trips <- filter(trips, purpose != "home" & purpose != "business") # purpose != "work" & purpose != "education"

trips <- left_join(trips, geospatial_data, by = c("LSOA21CD"= "IDspatial")) %>% # TODO:change here for OA or LSOA
  mutate(IDspatial = case_when(
    boundary_type == "OA" ~ OA21CD,  # If boundary_type is OA, use OA21CD
    boundary_type == "LSOA" ~ LSOA21CD  # If boundary_type is LSOA, use LSOA21CD
  ))  %>% 
  mutate(trippurp =EndPurpose)  %>%
  dplyr::select(c("IDspatial","purpose","trippurp"))

# Print results
write_csv(trips, "manchesterData/trips.csv")
write_csv(convert, "manchesterData/table.csv")