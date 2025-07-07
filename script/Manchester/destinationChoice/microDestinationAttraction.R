library(tidyverse)
library(bit64)

# Read output areas and attraction coefficients
OAs <- sf::st_read("../../manchester/zones/zonesShapefile/OA_2021_MCR.shp")
attractions <- read_csv("result/Manchester/destinationChoice/tripAttractionsCoefficients3.csv")
purposes <- names(attractions)[-1]

#### DESTINATIONS (NON-RESIDENTIAL) ###
buildings <- list()
buildings$community_health <- read_delim("../../manchester/destinations/Community_health_resources.csv", delim = ";", col_types = "cnnnic")
buildings$early_year_access <- read_delim("../../manchester/destinations/Early_year_access.csv", delim = ";", col_types = "cnnnic")
buildings$eating_establishments <- read_delim("../../manchester/destinations/Eating_establishments.csv", delim = ";", col_types = "cnnnic")
buildings$education <- read_delim("../../manchester/destinations/Education.csv", delim = ";", col_types = "cnnnic")
buildings$financial <- read_delim("../../manchester/destinations/Financial.csv", delim = ";", col_types = "cnnnic")
buildings$food_retail <- read_delim("../../manchester/destinations/Food_retail.csv", delim = ";", col_types = "cnnnic")
buildings$primary_health_care <- read_delim("../../manchester/destinations/Primary_health_care.csv", delim = ";", col_types = "cnnnic")
buildings$recreation <- read_delim("../../manchester/destinations/Recreational_sports_pitches_and_facilities.csv", delim = ";", col_types = "cnnnic")
buildings$services <- read_delim("../../manchester/destinations/Services.csv", delim = ";", col_types = "cnnnic")
buildings$social_and_culture <- read_delim("../../manchester/destinations/Social_and_culture_locations.csv", delim = ";", col_types = "cnnnic")
buildings$public_open_space <- read_delim("../../manchester/destinations/Public_open_space_v3.0.csv", delim = ";") %>%
  mutate(ref_no = strtoi(gsub("^B9FB5|-|-5E80-E053-A03BA40A915F$","",ID),16)) %>% 
  transmute(ID,X,Y,WEIGHT,ref_no,name = ID)

# Add OAs to building types and remove those not in study area
buildings <- lapply(buildings,FUN = function(df) {
  df %>% sf::st_as_sf(coords = c("X","Y"),remove = FALSE,crs = 27700) %>%
    sf::st_join(transmute(OAs,oaID = id),join = sf::st_intersects) %>%
    sf::st_drop_geometry() %>%
    filter(!is.na(oaID)) %>%
    mutate(ref_no = as.integer64(ref_no))
})


# #### RESIDENTIAL BUILDINGS (leave out for now) ####
# buildings$residential <- readr::read_csv("../../manchester/destinations/dwelling_building_2023.csv", col_types = "cicnn") %>%
#   transmute(ID = UPRN, X = coordX, Y = coordY, ref_no = as.integer64(UPRN), name = UPRN, oaID)
# 
# # Compare households vs. dwellings in each OA
# households <- readr::read_csv("../../manchester/destinations/hh_oa_2021.csv", col_types = "icccccccii") %>%
#   inner_join(count(buildings$residential,oaID,name = "dwellings")) %>%
#   transmute(oaID,households,dwellings,WEIGHT = households / dwellings)
# 
# ggplot(households,aes(x = households,y = dwellings)) + geom_point() + xlim(0,300) + ylim(0,300)
# 
# # Add residential weights based on household/dwelling ratio
# buildings$residential <- buildings$residential %>%
#   left_join(select(households,oaID,WEIGHT)) %>%
#   relocate(WEIGHT,.before = oaID)

#### COMBINE ALL BUILDING TYPES ####
all_buildings <- bind_rows(buildings, .id = "type") %>%
  rename(zone = oaID) %>%
  select(-ID) %>%
  mutate(id = 1:n(),
         WT = case_when(type == "public_open_space" ~ WEIGHT ^ 0.25,
                        type == "recreation" ~ WEIGHT ^ 0.25,
                        type == "social_and_culture" ~ WEIGHT ^ 0.25,
                        type == "education" ~ WEIGHT,
                        type == "residential" ~ WEIGHT,
                        TRUE ~ WEIGHT ^ 0.5),
         code = recode_factor(type,
                              # `residential` = "HH",
                              `social_and_culture` = "SCL",
                              `education` = "EDU",
                              `primary_health_care` = "PHC",
                              `community_health` = "CHR",
                              `recreation` = "RSPF",
                              `early_year_access` = "EYA",
                              `food_retail` = "FR",
                              `eating_establishments` = "EE",
                              `financial` = "FIN",
                              `services` = "SER",
                              `public_transport` = "PT",
                              `public_open_space` = "POS"),
         name = gsub(",","",name)) %>%
  group_by(ref_no) %>%
  mutate(WT = WT / n()) %>%
  ungroup() %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*WT))

write_csv(all_buildings,"result/Manchester/destinationChoice/microDestinationAttraction.csv")

#### TESTS ####
# Check uniqueness of reference numbers
all_buildings %>%
  group_by(type) %>%
  summarise(count = n(),
            dist_ID = n_distinct(ID),
            dist_no = n_distinct(ref_no))

# These should match. If not, need to change method for getting reference number
with(all_buildings, n_distinct(code,ID))
with(all_buildings, n_distinct(ref_no))

test <- all_buildings %>%
  group_by(zone,code) %>%
  summarise(sum_wt = sum(WT))

#### TEST DISTRIBUTION OF WEIGHTS OVER STUDY AREA ####
plot_data <- all_buildings %>%
  filter(code != "POS", code != "HH") %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WEIGHT)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p, y = TRUE, fill = code)) + geom_bar(position = "fill", stat = "identity")

test <- count(all_buildings,zone,code)


# Plot attraction results
plot_data <- all_buildings %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  group_by(purpose,code) %>%
  summarise(sum_wt = sum(value)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p,y = purpose, fill = code)) + 
  geom_bar(position = "fill",stat = "identity") + 
  xlab("share")

test <- all_buildings %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  filter(value > 0) %>%
  ggplot(aes(x = value)) + geom_density() + facet_wrap(~purpose) + xlim(0,0.2)

# Test aggregation before estimating total attraction
# (should match plot data, which estimates attraction before aggregating)
test <- all_buildings %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WT)) %>%
  left_join(attractions, by = c("code" = "IndependentVariable")) %>%
  mutate(across(all_of(purposes), ~.*sum_wt))
