library(tidyverse)

# Read output areas and attraction coefficients
zones <- sf::st_read(
    "../melbourne/input/zonesShapefile/SA1_2016_AUST_MEL.shp"
  ) %>%
  transmute(zone = SA1_MAIN16) %>%
  sf::st_transform(crs = 28355)

attractions <- read_csv("../melbourne/input/mito/tripAttractionsCoefficients.csv")
purposes <- names(attractions)[-1]

# all_destinations comes from destinations.r
all_destinations <- read_csv("../melbourne/preprocessing/poi/final/all_destinations.csv")  %>% 
  filter(code != "PT") %>%
  sf::st_as_sf(coords = c("X","Y"),remove = FALSE,crs = 28355) %>%
  sf::st_join(zones,join = sf::st_intersects) %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(zone))  %>%
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
  )) %>% 
  rename(WEIGHT = weight) %>%
  group_by(code,id) %>%
  mutate(WT = WT / n()) %>%
  ungroup() %>%
  select(type,X,Y,WEIGHT,Attribute,zone,id,WT,code) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*WT))


write_csv(all_destinations,"../melbourne/input/mito/microDestinationAttraction.csv")

#### TESTS ####
# Check uniqueness of reference numbers
all_destinations %>%
  group_by(type) %>%
  summarise(count = n(),
            dist_ID = n_distinct(id))

# These should match. If not, need to change method for getting reference number
with(all_destinations, n_distinct(code,id))


test <- all_destinations %>%
  group_by(zone,code) %>%
  summarise(sum_wt = sum(WT))

#### TEST DISTRIBUTION OF WEIGHTS OVER STUDY AREA ####
plot_data <- all_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WEIGHT)) %>%
  mutate(p = sum_wt / sum(sum_wt))
ggplot(plot_data, aes(x = p, y = TRUE, fill = code)) + geom_bar(position = "fill", stat = "identity")

test <- count(all_destinations,zone,code)

# Plot attraction results
plot_data <- all_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  group_by(purpose,code) %>%
  summarise(sum_wt = sum(value)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p,y = purpose, fill = code)) + 
  geom_bar(position = "fill",stat = "identity") + 
  xlab("share")


test <- all_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  filter(value > 0) %>%
  ggplot(aes(x = value)) + geom_density() + facet_wrap(~purpose) + xlim(0,0.2)


# Test aggregation before estimating total attraction
# (should match plot data, which estimates attraction before aggregating)
test <- all_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WT)) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*sum_wt))
