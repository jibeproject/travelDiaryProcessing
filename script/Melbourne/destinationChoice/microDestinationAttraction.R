library(tidyverse)

# Read dwellings
dwellings <- readr::read_csv("../melbourne/microdata/dd_2018.csv") %>%
  transmute(type = "dwelling",
            code = "HH",
            id = row_number(),
            X = coordX,
            Y = coordY,
            zone)

# Compare households vs. dwellings in each zone (SA1)
# For Manchester this was evaluated as households/dwellings
# however, for Melbourne dwellings were not distinguished from households; 
# therefore, the weight of households/dwellings = 1
# So, we want the zone, household count, dwelling count, and household weight (a constant of 1)
households <- read_csv("../melbourne/microdata/hh_2018.csv") %>%
  group_by(zone) %>%
  summarise(households = n(),
            dwellings = n(), # assuming each household has one dwelling
            WT = 1) %>%
  ungroup()

# Add household weight attribute
dwellings <- dwellings %>%
  left_join(select(households,zone,WT)) %>%
  relocate(WT,.before = zone)

# Read output areas and attraction coefficients
zones <- sf::st_read(
    "../melbourne/input/zonesShapefile/SA1_2016_AUST_MEL.shp"
  ) %>%
  transmute(zone = SA1_MAIN16) %>%
  sf::st_transform(crs = 28355)

attractions <- read_csv("../melbourne/input/mito/tripAttractionsCoefficients.csv")
purposes <- names(attractions)[-1]

# all_destinations comes from destinations.r
all_destinations <- read_csv("../melbourne/preprocessing/poi/final/all_destinations.csv")
attr_destinations <- all_destinations %>% 
  filter(code != "PT") %>%
  group_by(code,id) %>%
  mutate(WT = WT / n()) %>%
  ungroup() %>%
  select(type,code,id,X,Y,WT) %>%
  sf::st_as_sf(coords = c("X","Y"),remove = FALSE,crs = 28355) %>%
  sf::st_join(zones,join = sf::st_intersects) %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(zone)) %>%
  rbind(dwellings) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*WT))


# Plot attraction results
plot_data <- attr_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  group_by(purpose,code) %>%
  summarise(sum_wt = sum(value)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p,y = purpose, fill = code)) + 
  geom_bar(position = "fill",stat = "identity") + 
  xlab("share")

# Test aggregation before estimating total attraction
# (should match plot data, which estimates attraction before aggregating)
test <- attr_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WT)) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*sum_wt))

## Reshape data for MITO/SILO microDestinationAttraction.csv
all_destinations_shaped <- all_destinations %>%
  mutate(type = recode(type,
    "Community health resources" = "community_health", 
    "Early year access" = "early_year_access",
    "Eating establishments" = "eating_establishments",
    "Education" = "education",
    "Financial" = "financial",
    "Food retail" = "food_retail",
    "Primary health care" = "primary_health_care",
    "Recreational, sports pitches and facilities" = "recreation",
    "Services" = "services",
    "Social and cultural locations" = "social_and_culture",
    "Public open space" = "public_open_space",
    "Public transport" = "public_transport"
  )) %>% 
  rename(WEIGHT = weight) %>%
  select(type, X, Y, WEIGHT, Attribute, id, WT, code)  %>%
  filter(type != "public_transport")
 

attr_destinations_shaped <- attr_destinations %>%
  mutate(type = recode(type,
    "Community health resources" = "community_health",
    "Early year access" = "early_year_access",
    "Eating establishments" = "eating_establishments",
    "Education" = "education",
    "Financial" = "financial",
    "Food retail" = "food_retail",
    "Primary health care" = "primary_health_care",
    "Recreational, sports pitches and facilities" = "recreation",
    "Services" = "services",
    "Social and cultural locations" = "social_and_culture",
    "Public open space" = "public_open_space"
  )) %>%
  select(code, id, zone, HBW, HBE, HBA, HBO, HBR, HBS, NHBO, NHBW) %>%
  filter(code != "HH")



microDestinationAttraction <- all_destinations_shaped %>%
  left_join(attr_destinations_shaped, by = c("code", "id"))

# > microDestinationAttraction$zone %>% is.na() %>% table()
# .
# FALSE  TRUE
# 41011  1859

# Omit NA zones
microDestinationAttraction <- microDestinationAttraction %>%
  filter(!is.na(zone))

write_csv(microDestinationAttraction,"../melbourne/input/mito/microDestinationAttraction.csv")
