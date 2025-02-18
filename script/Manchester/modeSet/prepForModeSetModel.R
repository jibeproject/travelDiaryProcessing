### Clear memory
rm(list = ls())

library(tidyverse)
library(fastDummies)

########## Load Function ###########
TripSummaryVarsByPurpose <- function(prefix, ...) {
  trips %>%
    group_by(..., t.purpose, .drop = FALSE) %>%
    summarise(x.km = sum(t.tripLength,na.rm = T),
              x.km_mean = mean(t.tripLength,na.rm = T),
              x.TTB = sum(t.travelTime,na.rm = T),
              x.isMobile = ifelse(n() > 0,TRUE,FALSE),
              x.trips = n()) %>%
    pivot_wider(id_cols = c(...),
                names_from = t.purpose,
                values_from = starts_with("x.")) %>%
    rename_with( ~ gsub("^x.",prefix,.x))
}


################## Writing and reading csv ##################
TRADS <- read_rds("data/Manchester/processed/TRADS.rds")


################## Creating unique id for individuals and trips ##################
TRADS$indiv <- TRADS$indiv %>%
  unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = hh.id)

TRADS$trips <- TRADS$trips %>%
  unite("t.ID", c('hh.id', 'p.id',"t.id"), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(t.ID, .after = t.id)

trips <-TRADS$trips%>%
  mutate(  t.purpose = case_when(t.origin == "B" | t.destination == "B" ~ "business",
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
                                 TRUE ~ "NHBO"))
################## Merging person data with household data ##################
indiv <- TRADS$indiv %>% left_join(TRADS$households)
indiv <- left_join(indiv, TRADS$indiv %>% group_by(hh.id) %>% summarise(hh.adults = sum(p.age_group!="5-9"&p.age_group!="10-14"),
                                                                        hh.children5to9 = sum(p.age_group=="5-9"),
                                                                        hh.children10to14 = sum(p.age_group=="10-14"),
                                                                        hh.children15to19 = sum(p.age_group=="15-19"),
                                                                        hh.size = hh.adults + hh.children5to9 + hh.children10to14,
                                                                        hh.workers = sum(p.ws_workOver30h|p.ws_work16to30h|p.ws_workUnder16h) ))

                                                                                       

################## generating mode use frequency and mode combo ##################
frequencySet = c("1 day a week","2 days a week","3 or 4 days a week","5 or more days a week","At least once a fortnight")

indiv = indiv%>%
  mutate(p.mode_combo = paste(ifelse(p.freq_car%in%c(frequencySet),"Auto",""),
                              ifelse(p.freq_bus%in%c(frequencySet)|p.freq_metro%in%c(frequencySet)|p.freq_train%in%c(frequencySet), "Pt",""),
                              ifelse(p.freq_bike%in%c(frequencySet), "Cycle",""),
                              ifelse(p.freq_walk%in%c(frequencySet), "Walk",""),sep = ""))
indiv = indiv%>%
  mutate(p.mode_combo_full = paste(ifelse(p.freq_car%in%c(frequencySet),"Auto",""),
                              ifelse(p.freq_bus%in%c(frequencySet)|p.freq_metro%in%c(frequencySet)|p.freq_train%in%c(frequencySet), "Pt",""),
                              ifelse(p.freq_bike%in%c(frequencySet), "Cycle",""),
                              ifelse(p.freq_walk%in%c(frequencySet), "Walk",""),
                              ifelse(p.freq_other%in%c(frequencySet), "Other",""),sep = ""))

indiv = indiv%>%
  drop_na(-p.workOA, -p.studyOA, -p.travelIntegralToJob)
################### Attach built environment variables for workers ##################
corridor=read_csv("data/manchester/modeSet/AllShort92.csv")

corridor <- corridor %>%
  unite("indiv.id", c('IDNumber', 'PersonNumber'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = IDNumber)

indiv = indiv %>% left_join(corridor%>%select(-"car_time"))

################### Attach car/pt travel time  ##################
carTravelTime=read_csv("data/manchester/travelTime/carCongested_10perc.csv")%>%filter(Route=="carCongested")
ptTravelTime=read_csv("data/manchester/travelTime/ptTravelTime_matsim.csv")

carTravelTime_OA = carTravelTime%>%group_by(OriginZone,DestinationZone)%>%summarise(car_time=mean(as.numeric(time)))

ptTravelTime_OA = ptTravelTime%>%group_by(OriginZone,DestinationZone)%>%summarise(pt_time=mean(as.numeric(totalTravelTime)))

indiv = indiv%>%
  left_join(carTravelTime_OA,by=c("hh.OA"="OriginZone","p.workOA"="DestinationZone"))%>%
  left_join(ptTravelTime_OA,by=c("hh.OA"="OriginZone","p.workOA"="DestinationZone"))

################### Attach trip summary data ##################
indiv <- indiv %>% 
  left_join(TripSummaryVarsByPurpose("p.",hh.id,p.id))
  

################### Re-coding variables ##################
indiv=indiv %>% within({ 
  
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
  
  hh.bikes_gr = case_when(hh.bikes >= 3 ~ 3,
                         TRUE ~ hh.bikes)
  
  factor(hh.income, levels = c("less than £5000", "£5000 to £9999", "£10000 to £14999",
                               "£15000 to £19999", "£20000 to £24999","£25000 to £34999",
                               "£35000 to £49999", "£50000 to £74999", "£75000"))
  
  hh.income_agg = recode(hh.income, `less than £5000` = "low", `£5000 to £9999` = "low", `£10000 to £14999` = "low", 
                         `£15000 to £19999` = "medium", `£20000 to £24999` = "medium", `£25000 to £34999` = "medium", `£35000 to £49999` = "medium",
                         `£50000 to £74999` = "high", `£75000` = "high", .default = "NA")
  
})

################### Filter data ##################
OA_2011 = sf::st_read("data/manchester/gis/gm_oa.gpkg")
indiv = indiv%>%
  filter(hh.OA%in%OA_2011$geo_code)


################### Deal with bike/walk dist 0 or NA ##################
indiv <- indiv %>% 
  mutate_at(c(59:80), ~replace_na(.,0))

worker_hasBE=indiv%>%
  filter(p.occupation=="worker",)%>%
  filter(indiv.id%in%corridor$indiv.id)

worker_noBE=indiv%>%
  filter(p.occupation=="worker",)%>%
  filter(!indiv.id%in%corridor$indiv.id)

nrow(worker_noBE%>%filter(p.ws_workUnder16h))
nrow(worker_noBE%>%filter(p.ws_work16to30h))
nrow(worker_noBE%>%filter(p.ws_workOver30h))

write.csv(indiv,file = "data/manchester/processed/individualsForModeSet.csv",row.names=FALSE)


  
