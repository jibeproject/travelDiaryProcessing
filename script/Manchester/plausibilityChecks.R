####### PLAUSIBILITY CHECKS FOR TRADS DATASET (VERSION WITH LOCATION DATA ONLY) #######
library(tidyverse)
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")

####### CHECK FOR AND REMOVE BAD RECORDS #######
tests <- TRADS$trips %>% 
  group_by(hh.id,p.id) %>%
  mutate(test_PurposeMatch = t.startPurpose == lag(t.endPurpose),
         test_LocMatch = t.startOA == lag(t.endOA),
         test_FirstTrip1 = first(t.id) == 1,
         test_TripInc = t.id == lag(t.id) + 1,
         test_ArrivalTimeMatch = (t.departureTime + t.travelTime) %% (24*3600) - t.arrivalTime == 0,
         nextDay = t.departureTime < lag(t.arrivalTime),
         nextDayOccurences = sum(nextDay,na.rm = T),
         test_wrapAroundTime = 24*3600*(1 - nextDayOccurences) + first(t.departureTime) - last(t.arrivalTime) > 0,
         test_unknownOrigin = t.origin == "unknown",
         test_unknownDestination = t.destination == "unknown",
         test_neverHome = all(t.origin != "H"),
         test_startAtHome = first(t.origin) == "H",
         test_endAtHome = last(t.destination) == "H") %>%
  group_by(hh.id,p.id,t.id) %>%
  mutate(test_duplicateRecord = n() > 1) %>%
  ungroup() %>%
  left_join(select(TRADS$households,hh.id,hh.OA)) %>%
  left_join(select(TRADS$indiv,hh.id,p.id,p.workOA,p.studyOA)) %>%
  mutate(test_origHomeOA = case_when(t.origin == "H" ~ t.startOA == hh.OA),
         test_destHomeOA = case_when(t.destination == "H" ~ t.endOA == hh.OA),
         test_origWorkOA = case_when(t.origin == "W" ~ t.startOA == p.workOA),
         test_destWorkOA = case_when(t.destination == "W" ~ t.endOA == p.workOA),
         test_origStudyOA = case_when(t.origin == "E" ~ t.startOA == p.studyOA),
         test_destStudyOA = case_when(t.destination == "E" ~ t.endOA == p.studyOA))

tests %>% select(starts_with("test_")) %>% summary()

badTrips <- tests %>% 
  filter(!(test_PurposeMatch & test_LocMatch & test_TripInc & test_ArrivalTimeMatch & test_origHomeOA & test_destHomeOA & test_origWorkOA & 
             test_destWorkOA & test_origStudyOA & test_destStudyOA & test_startAtHome & test_endAtHome)) %>% 
  select(hh.id,p.id,t.id) %>% distinct()

# Remove bad individuals from dataset (but keep other persons in the same household... for now...)
indiv <- anti_join(TRADS$indiv, badTrips)
trips <- semi_join(TRADS$trips,select(indiv,hh.id,p.id))
rm(badTrips)