#pacman::p_load(dplyr,tidyr,ggplot2,RColorBrewer,scales, pscl)

library(tidyverse)

# This script generates the purpose-based (HBW, HBE, ...) datasets required for the trip generation models (mandatory and non-mandatory trips).
# To be fixed: remove column X

######################## READ ALL DATASETS ########################

NTS=list()
NTS$persons<-read.csv(file = "datasets/persons.csv")
NTS$households<-read.csv(file = "datasets/households.csv")
NTS$trips<-read.csv(file = "datasets/trips.csv")

######################## READ ALL DATASETS ########################

# OPTIONAL
# Keep relevant variables for model estimation

NTS$persons <- NTS$persons %>%
  dplyr::select(c("hh.id","p.id", "p.ID","p.age_gr","p.female", "p.freqWFH", "p.weight", "p.occupationStatus", "p.ownBicycle", "p.driversLicence", "p.workMode"))

NTS$households<-NTS$households %>%
  dplyr::select(c("hh.id", "hh.weight", "hh.size", "hh.children", "hh.cars", "hh.income", "hh.urban"))

NTS$trips <- NTS$trips %>%
  dplyr::select(c("hh.id", "p.id", "p.ID", "t.day.id", "t.id", "t.weight", "t.purpose", "t.travelTime.wtd", "t.distance", "t.is_RRT"))

# TODO: should i keep number of cars per adult ?
NTS$households$hh.cars_per_adult <-NTS$households$hh.cars/(NTS$households$hh.size - NTS$households$hh.children)

cross_tab<-NTS$trips %>%
  group_by(p.ID, t.purpose) %>%
  tally() %>%
  spread(t.purpose, n) %>%
  mutate(across(
    .cols = everything(),  # Apply to all columns
    .fns = ~ ifelse(is.na(.), 0, .)
  ))

# Prepare trip distances
NTS$HBW_trip_lengths <- NTS$trips %>%
  filter(t.distance<40) %>%
  filter(t.purpose == "HBW") %>%
  group_by(p.ID) %>%
  summarise(p.mean_distance.hbw = mean(t.distance))

NTS$HBE_trip_lengths <- NTS$trips %>%
  filter(t.distance<40) %>%
  filter(t.purpose == "HBE") %>% 
  group_by(p.ID) %>%
  summarise(p.mean_distance.hbe = mean(t.distance))

NTS$data_for_model <- NTS$persons %>% 
  left_join(NTS$households, by=c("hh.id")) %>%
  left_join(NTS$HBE_trip_lengths, by=c("p.ID")) %>%
  left_join(NTS$HBW_trip_lengths, by=c("p.ID")) %>%
  left_join(cross_tab, by=c("p.ID"))

# Write results
write.table(NTS$data_for_model, file="datasets/dataTripGenManchester.csv", row.names = FALSE, sep=";")


######################## HH DATASET ########################

#hh_dataset <- NTS$trips %>%
#  group_by(hh.id, t.purpose) %>% 
#  summarise(n=n()) %>% 
#  pivot_wider(names_from = t.purpose, values_from = n, values_fill=0)

#NTS$data_for_model_HHlevel<-merge(NTS$households, hh_dataset, by='hh.id')

#write.table(NTS$data_for_model_HHlevel, file="HHdataTripGenManchester.csv", row.names = FALSE, sep=";")


######################## CLEAN DATA ########################
#NTS$data_for_model <- NTS$data_for_model[!(is.na(NTS$data_for_model$hh.income) | NTS$data_for_model$hh.income == -8), , drop = FALSE]
                                                                                         
######################## CHECK POTENTIAL INCONSISTENTIES ########################

#cross_tab_age_HBW<-NTS$data_for_model %>%
#  group_by(p.age_gr, HBW) %>%
#  tally() %>%
#  spread(HBW, n) %>%
#  mutate(across(
#    .cols = everything(),  # Apply to all columns
#    .fns = ~ ifelse(is.na(.), 0, .)
#  ))