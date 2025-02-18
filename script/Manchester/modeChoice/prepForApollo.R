### Clear memory
rm(list = ls())

library(tidyverse)
library(fastDummies)

################## Read RDS file ##################
TRADS <- read_rds("data/Manchester/processed/TRADS.rds")

################## Creating unique id for individuals and trips ##################
TRADS$indiv <- TRADS$indiv %>%
  unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = hh.id)
TRADS$trips <- TRADS$trips %>%
  unite("t.ID", c('hh.id', 'p.id',"t.id"), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(t.ID, .after = t.id)

################## Merging trip data with household and person data ##################
trips <- TRADS$trips %>% left_join(TRADS$indiv) %>% left_join(TRADS$households)


################## Assign Trip purpose for MITO ##################
trips <- trips %>% within({
  t.purpose = case_when(t.origin == "B" | t.destination == "B" ~ "business",
                      t.origin == "unknown" | t.destination == "unknown" ~ "unknown",
                      t.origin == "RRT" | t.destination == "RRT" ~ "RRT",
                      t.origin == "H" ~ case_when(t.destination == "W" ~ "HBW",
                                                  t.destination == "E" ~ "HBE",
                                                  t.destination == "A" ~ "HBA",
                                                  t.destination == "S" ~ "HBS",
                                                  t.destination == "R" ~ "HBR",
                                                  t.destination == "O" ~ "HBO"),
                      t.destination == "H" ~ "NA",
                      t.origin == "W" | t.destination == "W" ~ "NHBW",
                      TRUE ~ "NHBO")

  t.full_purpose = case_when(t.purpose == "NA" ~ case_when(t.origin == "W" ~ "HBW",
                                                         t.origin == "E" ~ "HBE",
                                                         t.origin == "A" ~ "HBA",
                                                         t.origin == "S" ~ "HBS",
                                                         t.origin == "R" ~ "HBR",
                                                         t.origin == "O" ~ "HBO"),
                           TRUE ~ t.purpose)
})

################## generating mode use frequency and mode combo ##################
frequencySet = c("1 day a week","2 days a week","3 or 4 days a week","5 or more days a week","At least once a fortnight")

trips = trips%>%
  mutate(p.mode_combo = paste(ifelse(p.freq_car%in%c(frequencySet),"Car",""),
                              ifelse(p.freq_bus%in%c(frequencySet)|p.freq_metro%in%c(frequencySet)|p.freq_train%in%c(frequencySet), "Pt",""),
                              ifelse(p.freq_bike%in%c(frequencySet), "Bike",""),
                              ifelse(p.freq_walk%in%c(frequencySet), "Walk",""),
                              ifelse(p.freq_other%in%c(frequencySet), "Other",""),sep = ""),
         t.m_main_agg = recode(t.m_main, `Walk` = "Walk", `Bicycle` = "Bike", `Motorcycle, scooter, moped` = "Car", 
                               `Car or van driver` = "Car", `Car or van passenger` = "Car", 
                               `Bus, minibus, coach` = "Pt", `Metrolink` = "Pt", 
                               `Train` = "Pt", `Taxi, minicab` = "Car", 
                               `Other` = "Other",.default = "unknown"))
trips$p.mode_combo[trips$p.mode_combo==""]="None"
trips$error_modeUseDismatch=FALSE

for (i in 1:nrow(trips)) {
  if (!grepl(trips$t.m_main_agg[i], trips$p.mode_combo[i], fixed = TRUE)){
    trips$error_modeUseDismatch[i]=TRUE
  }
}

error_modeUseDismatch_summary = trips%>%
  filter(error_modeUseDismatch)%>%
  group_by(t.m_main_agg,p.mode_combo)%>%count()

write.table(error_modeUseDismatch_summary,"clipboard",row.names=F,sep = "\t")

# For dismatch trips, add used mode into mode set
trips = trips%>%
  within({p.mode_combo_impute = case_when(error_modeUseDismatch & p.mode_combo!="None" ~ paste(p.mode_combo,t.m_main_agg,sep = ""),
                                         error_modeUseDismatch & p.mode_combo =="None" ~ as.character(t.m_main_agg),
                                         !error_modeUseDismatch ~ p.mode_combo)})

################## attach travel time ##################
routeData=read_csv("data/manchester/routed/routes.csv", col_types = "ciilllllcccinnnnn", na = c("","null")) %>%
  select(-cost) %>% 
  pivot_wider(names_from = "Route", values_from = c(time,dist)) %>%
  select(IDNumber,PersonNumber,TripNumber,OriginWithinBoundary,DestinationWithinBoundary,SameOrigAndDest,
         time_car_congested,dist_car_freespeed,dist_bike,dist_walk)

ptTravelTime=read_csv("data/manchester/routed/ptTravelTime_matsim.csv", col_names = TRUE, na = c("","null"),
                      col_types = cols_only(IDNumber = "c", PersonNumber = "i", TripNumber = "i", totalTravelTime = "n"))


trips = trips%>%
  left_join(routeData,by=c("hh.id"="IDNumber","p.id"="PersonNumber","t.id"="TripNumber"))%>%
  left_join(ptTravelTime,by=c("hh.id"="IDNumber","p.id"="PersonNumber","t.id"="TripNumber")) %>%
  rename(dist = dist_car_freespeed,
         carTravelTime_sec = time_car_congested,
         ptTravelTime_sec = totalTravelTime)

################## filter out invalid trips records ##################
# In total 31129 trips
# 1. filter out trips origin/destination outside Boundary (after filtering 30044 trips) 
trips = trips %>% filter(OriginWithinBoundary & DestinationWithinBoundary)

# 2. filter out trips with mode "Other" or "unknown" (after filtering 29932)
trips = trips %>% filter(!t.m_main_agg%in%c("Other","unknown"))

# 3. filter out trips with purpose "RRT", "business", and "unknown" and "return home" (after filtering 27952)
trips = trips %>% filter(!t.purpose%in%c("unknown","business"))


################### Deal with intrazonal trips ##################
# Estimate distance-dependent speed of car and pt
averageSpeed = trips %>%
  filter(!SameOrigAndDest & dist != 0 & carTravelTime_sec != 0 & ptTravelTime_sec != 0)%>%
  mutate(speed_car = as.numeric(dist)/carTravelTime_sec,
         speed_pt = as.numeric(dist)/ptTravelTime_sec)

lm_carSpeed=lm(speed_car ~ dist,averageSpeed)

summary(lm_carSpeed)

lm_ptSpeed=lm(speed_pt ~ sqrt(as.numeric(dist)),averageSpeed)

summary(lm_ptSpeed)


# Use reported trip length to impute travel time of alternative modes,
# If reported trip length == 0, then use reported travel time to impute travel time of alternative modes

intrazonalTrips=trips %>% filter(SameOrigAndDest)


intrazonalTrips=intrazonalTrips %>% 
  within({ dist = case_when(t.tripLength > 0 ~ t.tripLength * 1.2,
                            t.tripLength == 0 ~ case_when(t.m_main_agg == "Walk" ~ t.travelTime * 1.38, # 1.38 m/s average from modelled walk trips
                                                          t.m_main_agg == "Bike" ~ t.travelTime * 5.1, # 5.1 m/s average from modelled bike trips
                                                          t.m_main_agg == "Car" ~ t.travelTime * (30.0/3.6), #average car speed 30 km/h
                                                          t.m_main_agg == "Pt" ~ t.travelTime * (6.0/3.6)))}) #average pt speed km/h


intrazonalTrips=intrazonalTrips%>%
  mutate(carSpeed_impute = predict(lm_carSpeed,intrazonalTrips),
         ptSpeed_impute = predict(lm_ptSpeed,intrazonalTrips),
         carTravelTime_sec = dist/carSpeed_impute,
         ptTravelTime_sec = dist/ptSpeed_impute)


trips = trips%>%
  rows_update(intrazonalTrips %>%select(-carSpeed_impute, -ptSpeed_impute),by="t.ID")


## Remove the irregular intrazonal trips TODO: make more detailed based on zone sizes
unrealisticIntrazonalTrips <- intrazonalTrips %>% filter(dist >= 2500) %>% select(t.ID)
trips <- trips %>% anti_join(unrealisticIntrazonalTrips)


################### Deal with bike/walk dist 0 or NA ##################
## some are intrazonal trips, check why non-intrazonal trips also have dist as 0 or NA
trips = trips%>%
  mutate(dist_bike = case_when(dist_bike==0 | is.na(dist_bike) ~ dist,
                               TRUE ~ dist_bike),
         dist_walk = case_when(dist_walk==0 |is.na(dist_walk) ~ dist,
                               TRUE ~ dist_walk))


################### Set availability of modes ##################

trips = trips %>%
  mutate(av_carD = 1,
         av_carP = 1,
         av_pt = case_when(ptTravelTime_sec == 0 ~ 0,
                           TRUE ~ 1),
         av_bike = 1,
         av_walk = 1)

trips = trips %>% filter(!(trips$av_pt==0 & trips$t.m_main_agg=="Pt"))

################### Re-coding variables ##################
trips=trips %>% within({ 
  
  p.age_group_agg = recode(p.age_group, `5-9` = "5_14", `10-14` = "5_14", `15-19` = "15_24", 
                       `20-24` = "15_24", `25-29` = "25_34", `30-34` = "25_34", `35-39` = "35_44",
                       `40-44` = "35_44", `45-49` = "45_54", `50-54` = "45_54", `55-59` = "55_64",
                       `60-64` = "55_64", `65-69` = "65_74", `70-74` = "65_74", `75-79` = "75",
                       `80-84` = "75", `85+` = "75",.default = "NA")
  
  p.occupation = case_when(p.ws_studyFullTime  ~ "student",
                           p.ws_workUnder16h | p.ws_work16to30h | p.ws_workOver30h ~ "worker",
                           p.ws_retired ~ "retired",
                           p.ws_unemployed | p.ws_longTermDisabled  ~ "unemployed",
                           p.ws_studyPartTime  ~ "student",
                           p.ws_homeMaker ~ "unemployed",
                           TRUE ~ "other")
  
  
  hh.cars_gr = case_when(hh.cars >= 3 ~ 3,
                         TRUE ~ hh.cars)
  
  factor(hh.income, levels = c("less than £5000", "£5000 to £9999", "£10000 to £14999",
                                     "£15000 to £19999", "£20000 to £24999","£25000 to £34999",
                                     "£35000 to £49999", "£50000 to £74999", "£75000"))
  
  hh.income_agg = recode(hh.income, `less than £5000` = "low", `£5000 to £9999` = "low", `£10000 to £14999` = "low", 
                         `£15000 to £19999` = "medium", `£20000 to £24999` = "medium", `£25000 to £34999` = "medium", `£35000 to £49999` = "medium",
                         `£50000 to £74999` = "high", `£75000` = "high", .default = "NA")
  
  t.departureTime_gr = case_when(t.departureTime > 6*3600 & t.departureTime < 22*3600 ~ "time_6_22",
                               TRUE ~ "time_before6_after22")
  
})



saveRDS(trips, file = "data/Manchester/processed/tripsForApollo.rds")

