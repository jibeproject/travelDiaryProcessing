####### SETUP #######
library(tidyverse)
rm(list = ls())

####### FUNCTIONS #######
echo <- function(msg) {
cat(paste0(as.character(Sys.time()), ' | ', msg, "\n"))
}

transform <- function(df,x,y,newX,newY,old_crs,new_crs) {
  coordinates <- df %>%
    select(all_of(c(x,y))) %>%
    sf::st_as_sf(coords = c(x,y),crs = old_crs) %>%
    sf::st_transform(crs = new_crs) %>%
    sf::st_coordinates()
  
  df[,newX] = coordinates[,1]
  df[,newY] = coordinates[,2]
  
  return(df)
}

####### LOAD SPATIAL DATA #######

# Load study area boundary
studyRegion <- sf::st_read("data/Melbourne/gis/absRegionsReprojected.sqlite",layer="GCCSA_2016_AUST") %>% 
  sf::st_buffer(1)

# Load SA1 regions
sa1 <- sf::st_read("data/Melbourne/gis/absRegionsReprojected.sqlite",layer="sa1_2016_aust") %>% 
  dplyr::select(sa1_maincode_2016,sa2_maincode_2016,sa3_code_2016)

####### LOAD VISTA DATA #######

test <- readr::read_csv("data/Melbourne/raw/T_VISTA_1220_Coord.csv")

raw <- list()
raw$trips <- read_csv("data/Melbourne/raw/T_VISTA_1220_Coord.csv", col_names = TRUE, na = c("N/A",""),
                        col_types = cols_only(tripid = "c", persid = "c", hhid = "c", tripno = "i", stops = "i",
                                              origplace1 = "c",origplace2 = "c", origpurp1 = "c", origpurp2 = "c", startime = "i", origsa4 = "i", origlong = "n", origlat = "n",
                                              destplace1 = "c",destplace2 = "c", destpurp1 = "c", destpurp2 = "c", arrtime = "i", destsa4 = "i", destlong = "n", destlat = "n",
                                              linkmode = "c", cumdist = "n", duration = "i",
                                              mode1 = "c", mode2 = "c", mode3 = "c", mode4 = "c", mode5 = "c", mode6 = "c", mode7 = "c", mode8 = "c", mode9 = "c",
                                              time1 = "i", time2 = "i", time3 = "i", time4 = "i", time5 = "i", time6 = "i", time7 = "i", time8 = "i", time9 = "i",
                                              dist1 = "n", dist2 = "n", dist3 = "n", dist4 = "n", dist5 = "n", dist6 = "n", dist7 = "n", dist8 = "n", dist9 = "n",
                                              wdtripwgt_sa3 = "n", wetripwgt_sa3 = "n"), n_max = 221819)

raw$persons <- read_csv("data/Melbourne/raw/P_VISTA_1220_Coord.csv", col_names = TRUE, 
                        col_types = cols_only(persid = "c", hhid = "c", age = "i", sex = "c", carlicence = "c", 
                                              studying = "c", mainact = "c", wdperswgt_sa3 = "n", weperswgt_sa3 = "n"))

raw$households <- read_csv("data/Melbourne/raw/H_VISTA_1220_Coord.csv", col_names = TRUE, na = c("N/A",""),
                           col_types = cols_only(hhid = "c", surveyperiod = "c", homesa4 = "i", hhsize = "i", cars = "i", hhinc = "i",
                                                 wdhhwgt_sa3 = "n", wehhwgt_sa3 = "n"))

raw$stops <- read_csv("data/Melbourne/raw/S_VISTA_1220_Coord.csv")

# INITIAL PREPARATION
# Trips dataset
trips <- raw$trips %>%
  arrange(hhid,persid,tripid) %>%
  mutate(persno = as.integer(stringr::str_extract(persid,"(?<=P).*")),
         startimeS = startime * 60,
         weekday = is.na(wdtripwgt_sa3),
         origGM = origsa4 %in% c(206:214),
         destGM = destsa4 %in% c(206:214),
         badOrigCoord = origlong == -2 | origlat == -1,
         badDestCoord = destlong == -2 | destlat == -1,
         origplace1 = ifelse(is.na(origplace1) & origpurp1 == "Work Related","Workplace",origplace1),
         destpurp1 = ifelse(is.na(destpurp1) & destplace1 == "Workplace","Work Related",destpurp1),
         origpurp1 = ifelse(is.na(origpurp1) & origplace1 == "Workplace","Work Related",origpurp1)) %>%
  group_by(persid) %>%
  mutate(trip_number=row_number()) %>%
  ungroup() %>%
  transform("origlong","origlat","origX","origY",4326,28355) %>%
  transform("destlong","destlat","destX","destY",4326,28355) %>%
  relocate(trip_number,.after = tripno) %>%
  relocate(origX,origY,destX,destY,.after = destlong) %>%
  relocate(persno,.after = persid) %>%
  relocate(startimeS,.after = startime)

# Households dataset
households <- raw$households %>% 
  mutate(homeGM = homesa4 %in% c(206:214))

# Persons dataset
persons <- raw$persons

# # FILTER CERTAIN BAD TRIPS
# # 400 trips don't increment properly, reduces trip count to 221238 (-0.3%) [this isn't in Carl's code]
# test <- trips %>% semi_join(select(filter(trips,!(tripno == trip_number)),persid))
# trips <- anti_join(trips,test, by = "persid")
# persons <- anti_join(persons,test, by = "persid")

# ASSIGN PURPOSE
# Modify workplace / work related
trips <- trips %>% rename(origin=origpurp1, destination=destpurp1)
trips <- trips %>% 
  mutate(
    purpose = case_when(
      origpurp2 == "Employer's Business" | destpurp2 == "Employer's Business" ~ "business",
      origin %in% c("Unknown Purpose (at start of day)", "Not Stated") | 
        destination %in% c("NA", "Not Stated") ~ "unknown",
      origin == "At Home" & destination == "At or Go Home" ~ "RRT",
      origin == "At Home" ~ case_when(
        destination == "Work Related" ~ "HBW",
        destination == "Education" ~ "HBE",
        destination == "Buy Something" ~ "HBS",
        destination == "Recreational" ~ "HBR",
        destination == "Other Purpose" ~ "HBO",
        destination %in% c("Accompany Someone","Pick-up or Drop-off Someone") ~ "HBA",
        # Classify remaining trips using place information if clearer than purpose
        destplace1 == "Workplace" ~ "HBW",
        destplace1 == "Place of Education" ~ "HBE",
        destplace1 == "Shops" ~ "HBS",
        destplace1 %in% c("Recreational Place","Natural Feature", "Social Place") ~ "HBR",
        destplace1 %in% c("Accommodation","Change Mode","Transport Feature", "Other") ~ "HBO"
      ),
      destination == "At or Go Home" ~ "NA",
      origin == "Work Related" | destination == "Work Related" ~ "NHBW",
      TRUE ~ "NHBO"
    ),
    full_purpose = case_when(
      purpose != "NA" ~ purpose,
      TRUE ~ case_when(
        origin == "Work Related" ~ "HBW",
        origin == "Education" ~ "HBE",
        origin == "Buy Something" ~ "HBS",
        origin == "Recreational" ~ "HBR",
        origin == "Other Purpose" ~ "HBO",
        origin %in% c("Accompany Someone","Pick-up or Drop-off Someone") ~ "HBA",
        # Classify remaining trips using place information if clearer than purpose
        origplace1 == "Workplace" ~ "HBW",
        origplace1 == "Place of Education" ~ "HBE",
        origplace1 == "Shops" ~ "HBS",
        origplace1 %in% c("Recreational Place","Natural Feature", "Social Place") ~ "HBR",
        origplace1 %in% c("Accommodation","Change Mode","Transport Feature", "Other") ~ "HBO"
      )
    )
  )

# Write trips to csv for routing
out <- select(trips,hhid,persno,tripno,origX,origY,destX,destY,startimeS,linkmode)
write_csv(out,"../../Documents/melbourne/vista_trips.csv")

# Save to .rds
VISTA <- list()
VISTA$raw = raw
VISTA$households = households
VISTA$persons = persons
VISTA$trips = trips
saveRDS(VISTA, "data/Melbourne/processed/VISTA.rds")