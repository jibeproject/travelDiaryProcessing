## PREP FOR MODE CHOICE ##

VISTA <- readRDS("data/Melbourne/processed/VISTA.rds")
routed <- read_csv("../../Documents/melbourne/vista_routes.csv", col_types = "ciiclllcicnnniinnnccccnnn", na = c("null",""))

# Get routed 
routed <- routed %>%
  mutate(value = case_when(Route == "beeline" ~ dist,
                           Route == "car_freespeed" ~ time,
                           Route == "car_congested" ~ time,
                           Route == "bike_short" ~ dist,
                           Route == "bike_fast" ~ time,
                           Route == "walk_short" ~ dist,
                           Route == "walk_fast" ~ time,
                           Route == "pt" ~ totalTravelTime)) %>%
  select(hhid,persno,tripno,Mode,OriginWithinBoundary,DestinationWithinBoundary,SameOrigAndDest,Route,value) %>%
  pivot_wider(names_from = Route)
write_csv(select(routed,-Mode),"../../Documents/melbourne/vista_routed.csv")

# Get and filter trips (remove intrazonal for Melbourne as it's only a tiny minority)
trips <- VISTA$trips %>%
  left_join(select(VISTA$households,hhid,hhsize,cars)) %>%
  left_join(select(VISTA$persons,persid,hhid,age,sex,mainact)) %>%
  left_join(routed, by = c("hhid","persno","tripno")) %>%
  filter(OriginWithinBoundary,DestinationWithinBoundary,!SameOrigAndDest) %>%
  mutate(dist = beeline,
         mainmode = factor(case_match(linkmode,
                               "Walking" ~ "walk",
                               "Bicycle" ~ "bike",
                               "Vehicle Driver" ~ "carD",
                               "Vehicle Passenger" ~ "carP",
                               c("Public Bus","Train","Tram") ~ "pt",
                               .default = "other"),
                           levels = c("carD","carP","pt","bike","walk")),
         occupation = case_match(mainact,
                                 "Full-time Work" ~ "employed_full",
                                 c("Part-time Work","Casual Work") ~ "employed_part",
                                 c("Full-time TAFE/Uni","Part-time TAFE/Uni","Other Education","Primary School","Secondary School") ~ "student",
                                 "Retired" ~ "retired",
                                 .default = "other"),
         age_gr = factor(case_when(age < 16 ~ "under16",
                            age < 25 ~ "16_24",
                            age < 39 ~ "25_39",
                            age < 55 ~ "40_54",
                            age < 70 ~ "55_69",
                            TRUE ~ "70up"),
                         levels = c("under16","16_24","25_39","40_54","55_69","70up")),
         age_gr_under25 = ifelse(age < 25,1,0),
         age_gr_45_64 = ifelse(age >= 45 & age < 65,1,0),
         age_gr_65up = ifelse(age >= 65,1,0),
         hhcars = pmin(cars,3),
         female = ifelse(sex == "F",1,0),
         femaleAdult = ifelse(sex == "F" & age >= 16,1,0),
         av_carD = ifelse(age < 16,0,1),
         av_carP = 1,
         av_pt = ifelse(pt == 0,0,1),
         av_bike = 1,
         av_walk = 1) %>%
  filter(!(mainmode == "carD" & av_carD == 0),
         !(mainmode == "pt" & av_pt == 0),
         dist >= 9,
         purpose %in% c("HBW","HBE","HBA","HBS","HBR","HBO","NHBW","NHBO"),
         mainmode != "other")

# Checks
with(trips,table(age,mainmode))
with(trips,table(age_gr,mainmode))
with(trips,table(mainact,occupation))
with(trips,table(linkmode,mainmode))
with(trips,table(female,femaleAdult))

# Records by age group
testAge <- trips %>%
  count(full_purpose,mainmode,age_gr) %>%
  pivot_wider(names_from = mainmode, values_from = n, values_fill = 0) %>%
  arrange(full_purpose,age_gr)

# Records by gender
testGender <- trips %>%
  count(full_purpose,mainmode,female) %>%
  pivot_wider(names_from = mainmode, values_from = n, values_fill = 0) %>%
  arrange(full_purpose,female)

# Output for Java
tripsForJava <- trips %>%
  mutate(choice = recode(mainmode,"carD" = 0,"carP" = 1,"pt" = 2,"bike" = 3,"walk" = 4)) %>%
  select(tripid,choice,full_purpose,car_freespeed,car_congested,pt,occupation,age_gr,age_gr_under25,age_gr_45_64,age_gr_65up,female,femaleAdult,hhcars,starts_with("av_")) %>%
  dummy_cols(select_columns = c("occupation","age_gr","hhcars"), remove_selected_columns = TRUE) %>%
  mutate(age_gr_55up = age_gr_55_69 + age_gr_70up,
         age_gr_under24 = age_gr_under16 + age_gr_16_24,
         hhcars_23 = hhcars_2 + hhcars_3,
         shopping_trip = ifelse(full_purpose == "HBS",1,0),
         recreation_trip = ifelse(full_purpose == "HBR",1,0))

tripsForJavaHBW <- tripsForJava %>% filter(full_purpose == "HBW") %>% select(-full_purpose)
tripsForJavaHBE <- tripsForJava %>% filter(full_purpose == "HBE") %>% select(-full_purpose)
tripsForJavaHBR <- tripsForJava %>% filter(full_purpose == "HBR") %>% select(-full_purpose)
tripsForJavaHBSO <- tripsForJava %>% filter(full_purpose == "HBS" | full_purpose == "HBO") %>% select(-full_purpose)
tripsForJavaHBD <- tripsForJava %>% filter(full_purpose == "HBS" | full_purpose == "HBO" | full_purpose == "HBR") %>% select(-full_purpose)
tripsForJavaHBA <- tripsForJava %>% filter(full_purpose == "HBA") %>% select(-full_purpose)
tripsForJavaNHBW <- tripsForJava %>% filter(full_purpose == "NHBW") %>% select(-full_purpose)
tripsForJavaNHBO <- tripsForJava %>% filter(full_purpose == "NHBO") %>% select(-full_purpose)

write_csv(tripsForJavaHBW,file = "../../Documents/melbourne/estimation/data/HBW.csv")
write_csv(tripsForJavaHBE,file = "../../Documents/melbourne/estimation/data/HBE.csv")
write_csv(tripsForJavaHBA,file = "../../Documents/melbourne/estimation/data/HBA.csv")
write_csv(tripsForJavaHBR,file = "../../Documents/melbourne/estimation/data/HBR.csv")
write_csv(tripsForJavaHBSO,file = "../../Documents/melbourne/estimation/data/HBSO.csv")
write_csv(tripsForJavaHBD,file = "../../Documents/melbourne/estimation/data/HBD.csv")
write_csv(tripsForJavaNHBW,file = "../../Documents/melbourne/estimation/data/NHBW.csv")
write_csv(tripsForJavaNHBO,file = "../../Documents/melbourne/estimation/data/NHBO.csv")



######## DEBUG: EXAMINE ZERO-DISTANCE TRIPS ######## 
# Remove outliers
names(trips)
test <- filter(trips, walk_short == 0)

write_csv(select(test,origX,origY,destX,destY),"zero_dist_trips.csv")

# Plot PT trip distribution
ggplot(trips,aes(x = walk_short, color = as.factor(av_pt))) + geom_density()

