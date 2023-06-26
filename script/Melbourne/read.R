####### SETUP #######
library(tidyverse)
rm(list = ls())

####### FUNCTIONS #######
echo <- function(msg) {
cat(paste0(as.character(Sys.time()), ' | ', msg, "\n"))
}


labelLinkMode <- function(linkMode) {
factor(linkMode,
       levels=c("Bicycle","Jogging","Mobility Scooter","Motorcycle","Other","Public Bus","School Bus",
                "Taxi","Train","Tram","Vehicle Driver","Vehicle Passenger","Walking"),
       labels=c("bike","walk","walk","car_driver","other","bus","bus",
                "car_passenger","train","tram","car_driver","car_passenger","walk"))
}

####### LOAD SPATIAL DATA #######

# Load study area boundary
studyRegion <- sf::st_read("data/Melbourne/gis/absRegionsReprojected.sqlite",layer="GCCSA_2016_AUST") %>% 
  sf::st_buffer(1)

# Load SA1 regions
sa1 <- sf::st_read("data/Melbourne/gis/absRegionsReprojected.sqlite",layer="sa1_2016_aust") %>% 
  dplyr::select(sa1_maincode_2016,sa2_maincode_2016,sa3_code_2016)

####### LOAD VISTA DATA #######
raw <- list()
raw$trips <- read_csv("data/Melbourne/raw/T_VISTA_1220_Coord.csv", col_names = TRUE, na = c("N/A",""),
                        col_types = cols_only(tripid = "c", persid = "c", hhid = "c", tripno = "i", stops = "i",
                                              origplace1 = "c", origpurp1 = "c", startime = "i", origsa4 = "i", origlong = "n", origlat = "n",
                                              destplace1 = "c", destpurp1 = "c",  arrtime = "i", destsa4 = "i", destlong = "n", destlat = "n",
                                              linkmode = "c", cumdist = "n", duration = "i",
                                              mode1 = "c", mode2 = "c", mode3 = "c", mode4 = "c", mode5 = "c", mode6 = "c", mode7 = "c", mode8 = "c", mode9 = "c",
                                              time1 = "i", time2 = "i", time3 = "i", time4 = "i", time5 = "i", time6 = "i", time7 = "i", time8 = "i", time9 = "i",
                                              dist1 = "n", dist2 = "n", dist3 = "n", dist4 = "n", dist5 = "n", dist6 = "n", dist7 = "n", dist8 = "n", dist9 = "n",
                                              wdtripwgt_sa3 = "n", wetripwgt_sa3 = "n"))

raw$persons <- read_csv("data/Melbourne/raw/P_VISTA_1220_Coord.csv", col_names = TRUE, 
                        col_types = cols_only(hhid = "c", age = "i", sex = "c", carlicence = "c", 
                                              studying = "c", mainact = "c", wdperswgt_sa3 = "n", weperswgt_sa3 = "n"))

raw$households <- read_csv("data/Melbourne/raw/H_VISTA_1220_Coord.csv", col_names = TRUE, na = c("N/A",""),
                           col_types = cols_only(hhid = "c", surveyperiod = "c", homesa4 = "i", hhsize = "i", cars = "i", hhinc = "i",
                                                 wdhhwgt_sa3 = "n", wehhwgt_sa3 = "n"))

raw$stops <- read_csv("data/Melbourne/raw/S_VISTA_1220_Coord.csv")


# Trips dataset
trips <- raw$trips %>%
  arrange(hhid,persid,tripid) %>%
  rename(trip_id=tripid,person_id=persid,household_id=hhid,trip_number=tripno,number_of_stops=stops,
         orig_place_type=origplace1,orig_purpose=origpurp1,start_time=startime,orig_long=origlong,orig_lat=origlat,
         dest_place_type=destplace1,dest_purpose=destpurp1,end_time=arrtime,dest_long=destlong,dest_lat=destlat,
         link_mode=linkmode,distance=cumdist,
         mode_1=mode1,mode_2=mode2,mode_2=mode2,mode_3=mode3,mode_4=mode4,mode_5=mode5,mode_6=mode6,mode_7=mode7,mode_8=mode8,mode_9=mode9,
         time_1=time1,time_2=time2,time_2=time2,time_3=time3,time_4=time4,time_5=time5,time_6=time6,time_7=time7,time_8=time8,time_9=time9,
         dist_1=dist1,dist_2=dist2,dist_2=dist2,dist_3=dist3,dist_4=dist4,dist_5=dist5,dist_6=dist6,dist_7=dist7,dist_8=dist8,dist_9=dist9,
         weight_weekday=wdtripwgt_sa3,weight_weekend=wetripwgt_sa3) %>%
  mutate(weekday=is.na(weight_weekday),
         origGM = origsa4 %in% c(206:214),
         destGM = destsa4 %in% c(206:214),
         badOrigCoord = orig_long == -2 | orig_lat == -1,
         badDestCoord = dest_long == -2 | dest_lat == -1,
         across(c(link_mode,mode_1:mode_9), ~ labelLinkMode(.x))) %>%
  group_by(person_id) %>%
  # making sure the trip numbers increment properly (todo: figure out what it means when it doesn't)
  mutate(trip_number=row_number()) %>%
  ungroup()

# todo: calculate main mode, secondary mode etc.


# Households dataset
households <- raw$households %>% mutate(homeGM = homesa4 %in% c(206:214))

# Persons dataset
persons <- raw$persons

# Save to .rds
VISTA <- list()
VISTA$raw = raw
VISTA$households = households
VISTA$persons = persons
VISTA$trips = trips
saveRDS(VISTA, "data/Melbourne/processed/VISTA.rds")