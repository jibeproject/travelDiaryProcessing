library(tidyverse)
rm(list = ls())

trips <- readRDS("data/Manchester/processed/TRADS_safe.rds")$trips %>%
  select(hh.id,p.id,t.id,t.origin,t.destination,t.departureTime,t.arrivalTime,t.travelTime,
         t.startSTUDYAREA,t.endSTUDYAREA,
         starts_with("t.check."))
  
####### CHECK FOR AND REMOVE BAD RECORDS #######
tests <- trips %>% 
  group_by(hh.id,p.id) %>%
  mutate(test_FirstTrip1 = first(t.id) == 1,
         test_TripInc = t.id == lag(t.id) + 1,
         test_ArrivalTimeMatch = (t.departureTime + t.travelTime) %% (24*3600) - t.arrivalTime == 0,
         nextDay = t.departureTime < lag(t.arrivalTime),
         nextDayOccurences = sum(nextDay,na.rm = T),
         test_validODlocations = (t.startSTUDYAREA %in% TRUE) & (t.endSTUDYAREA %in% TRUE),
         test_wrapAroundTime = 86400*(1 - nextDayOccurences) + first(t.departureTime) - last(t.arrivalTime) > 0,
         test_unknownOrigin = t.origin == "unknown",
         test_unknownDestination = t.destination == "unknown",
         test_neverHome = all(t.origin != "H"),
         test_startAtHome = first(t.origin) == "H",
         test_endAtHome = last(t.destination) == "H") %>%
  group_by(hh.id,p.id,t.id) %>%
  mutate(test_duplicateRecord = n() > 1) %>%
  ungroup()

tests %>% select(starts_with(c("test_","t.check."))) %>% summary()

persons_to_remove1 <- tests %>% 
  filter(!(test_TripInc & test_ArrivalTimeMatch & !test_unknownOrigin & t.check.purpose_matches_prev & t.check.OA_matches_prev & 
             t.check.OA_home_orig & t.check.OA_home_dest & t.check.OA_work_orig & t.check.OA_work_dest &
             t.check.OA_study_orig & t.check.OA_study_dest & !test_neverHome & test_validODlocations)) %>% 
  select(hh.id,p.id) %>% distinct()

# Remove bad individuals from dataset (but keep other persons in the same household... for now...)
trips <- trips %>% anti_join(persons_to_remove1) %>% select(-starts_with(c("test_","t.check.")),-ends_with(c("STUDYAREA")))

################ CREATE ACTIVITY-BASED DATASET ################
# test <- trips %>% group_by(hh.id,p.id) %>% summarise(firstAct = first(t.origin), lastAct = last(t.destination))

# Begin...
trips <- trips %>%
  filter(!(t.origin == "W" & t.destination == "W"),
         !(t.origin == "B" & t.destination == "W"),
         !(t.origin == "W" & t.destination == "B"),
         !(t.origin == "B" & t.destination == "B"),
         !(t.origin == "E" & t.destination == "E"),
         !(t.origin == "H" & t.destination == "H")) %>%
  group_by(hh.id,p.id) %>%
  mutate(t.first = row_number() == 1,
         t.last = row_number() == n()) %>%
  ungroup() %>%
  mutate(act.duration = case_when(!t.last ~ (lead(t.departureTime) - t.arrivalTime) %% 86400)) %>%
  group_by(hh.id,p.id) %>%
  mutate(tour.id = cumsum(t.origin == "H"))

# Duration of activities
first_trips = which(trips$t.first)
last_trips = which(trips$t.last)

trips$act.duration[last_trips] = (trips$t.departureTime[first_trips] - trips$t.arrivalTime[last_trips]) %% 86400

# Differentiate tours by type (depends on distance.... need processed location-based data...)
trips <- trips %>% 
  group_by(hh.id, p.id, tour.id) %>%
  mutate(tour.incomplete = any(t.last & t.destination != "H") | tour.id == 0,
         tour.w = !tour.incomplete & any(t.destination == "W"),
         tour.b = !tour.incomplete & any(t.destination == "B"),
         tour.e = !tour.incomplete & any(t.destination == "E"),
         tour.d = !tour.incomplete & !(tour.w | tour.e),
         tour.a = tour.d & any(t.destination == "A"),
         tour.s = tour.d & any(t.destination == "S"),
         tour.o = tour.d & any(t.destination == "O"),
         tour.r = tour.d & any(t.destination == "R"),
         tour.rrt = tour.d & any(t.destination == "RRT"),
         tour.purpose = case_when(tour.w ~ "work", 
                                  tour.e ~ "education", 
                                  tour.a ~ "accompany",
                                  tour.s ~ "shop",
                                  tour.o ~ "other",
                                  tour.r ~ "recreation",
                                  tour.rrt ~ "rrt",
                                  TRUE ~ "disregard"),
         act.id = 1:n(),
         subtour.w.id = cumsum(t.origin == "W" | t.origin == "B"),
         subtour.e.id = cumsum(t.origin == "E")) %>%
  ungroup()

# Remove individuals with business-only tours, mixed work/education tours, or incomplete tours (i.e. tours that didn't end at home)
persons_to_remove2 <- trips %>% filter((tour.b & !tour.w) | (tour.e & tour.w)) %>% select(hh.id,p.id) %>% distinct()
trips <- trips %>% anti_join(persons_to_remove2) %>% mutate(t.origin = recode(t.origin, `B` = "W"), t.destination = recode(t.destination, `B` = "W"))

# Determine subtour validity and rewrite subtour IDs to include only valid subtours
trips <- trips %>%
  group_by(hh.id, p.id, tour.id, subtour.w.id) %>%
  mutate(subtour.w = subtour.w.id > 0 & last(t.destination) == "W" & !any(t.destination == "H")) %>%
  group_by(hh.id, p.id, tour.id, subtour.e.id) %>%
  mutate(subtour.e = subtour.e.id > 0 & last(t.destination) == "E" & !any(t.destination == "H")) %>%
  group_by(hh.id, p.id, tour.id) %>%
  mutate(subtour = subtour.w | subtour.e,
         subtour.id = replace(cumsum(subtour & (t.origin == "W" | t.origin == "E")), !subtour, NA),
         subtour.type = case_when(subtour.w ~ "work", subtour.e ~ "education"),
         main.position = case_when(tour.w ~ match("W", t.destination),
                                   tour.e ~ match("E", t.destination),
                                   tour.a ~ match("A", t.destination),
                                   tour.s ~ match("S", t.destination),
                                   tour.o ~ match("O", t.destination),
                                   tour.r ~ match("R", t.destination),
                                   tour.rrt ~ match("RRT", t.destination))) %>%
  ungroup()

test <- trips %>% filter(is.na(main.position))

# Add activity details
trips <- trips %>%
  mutate(act.purpose = recode(t.destination, `H` = "home", `W` = "work", `A` = "accompany", `E` = "education", `S` = "shop", `R` = "recreation", `RRT` = "rrt", `O` = "other"),
         act.start = t.arrivalTime %% 86400,
         act.end = (t.arrivalTime + act.duration) %% 86400,
         act.type = case_when(act.purpose == "home" ~ "home",
                              tour.w & act.purpose == "work" ~ "main",
                              tour.e & act.purpose == "education" ~ "main",
                              tour.d & (act.id == main.position) ~ "main",
                              tour.incomplete & subtour & !(act.purpose == "work" | act.purpose == "education") ~ "subtour",
                              tour.incomplete ~ "disregard",
                              subtour ~ "subtour",
                              act.id < main.position ~ "stop_out",
                              TRUE ~ "stop_return"))

# Save tour details
trips_abm <- trips %>% select(hh.id, p.id, t.id, tour.id, tour.incomplete, tour.purpose, subtour.id, subtour.type, act.id, act.purpose, act.start, act.end, act.type)

# Main dataset
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")
TRADS$trips_abm <- trips_abm
saveRDS(TRADS,"data/Manchester/processed/TRADS.rds")

# Safe dataset
SAFE <- readRDS("data/Manchester/processed/TRADS_safe.rds")
SAFE$trips_abm <- trips_abm
saveRDS(SAFE,"data/Manchester/processed/TRADS_safe.rds")