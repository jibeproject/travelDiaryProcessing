#### COMBINE DESTINATIONS ####

library(tidyverse)

destinations <- list()
destinations$community_health <- read_delim("data/Manchester/destinations/Community_health_resources.csv", delim = ";", col_types = "innnic")
destinations$early_year_access <- read_delim("data/Manchester/destinations/Early_year_access.csv", delim = ";", col_types = "innnic")
destinations$eating_establishments <- read_delim("data/Manchester/destinations/Eating_establishments.csv", delim = ";", col_types = "innnic")
destinations$education <- read_delim("data/Manchester/destinations/Education.csv", delim = ";", col_types = "innnic")
destinations$financial <- read_delim("data/Manchester/destinations/Financial.csv", delim = ";", col_types = "innnic")
destinations$food_retail <- read_delim("data/Manchester/destinations/Food_retail.csv", delim = ";", col_types = "innnic")
destinations$primary_health_care <- read_delim("data/Manchester/destinations/Primary_health_care.csv", delim = ";", col_types = "innnic")
destinations$recreation <- read_delim("data/Manchester/destinations/Recreational_sports_pitches_and_facilities.csv", delim = ";", col_types = "innnic")
destinations$services <- read_delim("data/Manchester/destinations/Services.csv", delim = ";", col_types = "innnic")
destinations$social_and_culture <- read_delim("data/Manchester/destinations/Social_and_culture_locations.csv", delim = ";", col_types = "innnic")

destinations$public_open_space <- read_delim("data/Manchester/destinations/Public_open_space_v4.0.csv", delim = ";") %>%
  group_by(ID) %>% 
  mutate(ID = cur_group_id()) %>% 
  ungroup() %>%
  transmute(ID,X,Y,WEIGHT,ref_no = as.integer(0),name = "unknown")

destinations$public_transport <- read_delim("data/Manchester/destinations/Public_transport.csv", delim = ";", col_types = "innni") %>% mutate(ref_no = as.integer(0),name = "unknown")

all_destinations <- bind_rows(destinations, .id = "type") %>% mutate(ID = paste(type,ID,sep = "_"))
write_delim(all_destinations,"data/Manchester/destinations/all.csv",delim = ";")

# USE REFORMATTED WEIGHT DISTRIBUTIONS
all_destinations %>%
  mutate(WEIGHT2 = WEIGHT ^ 0.5,
         WEIGHT4 = WEIGHT ^ 0.25) %>%
  write_delim("destinations/all.csv",delim = ";")

## PLOT WEIGHT DISTRIBUTIONS ##
all_destinations <- all_destinations %>% 
  mutate(title = recode_factor(type,
                               `social_and_culture` = "Social and cultural (α = 0.25)",
                               `education` = "Education (α = 1)",
                               `primary_health_care` = "Primary healthcare (α = 0.5)",
                               `community_health` = "Community health (α = 0.5)",
                               `recreation` = "Recreational facilities (α = 0.25)",
                               `early_year_access` = "Early years access (α = 0.5)",
                               `food_retail` = "Food retail (α = 0.5)",
                               `eating_establishments` = "Eating establishments (α = 0.5)",
                               `financial` = "Financial establishments (α = 0.5)",
                               `services` = "Services (α = 0.5)",
                               `public_transport` = "Public transport (α = 0.5)",
                               `public_open_space` = "Public open space (α = 0.25)",.ordered = TRUE),
         WT = case_when(type == "public_open_space" ~ WEIGHT ^ 0.25,
                        type == "recreation" ~ WEIGHT ^ 0.25,
                        type == "social_and_culture" ~ WEIGHT ^ 0.25,
                        type == "education" ~ WEIGHT,
                        TRUE ~ WEIGHT ^ 0.5)) 

ggplot(all_destinations,aes(x = WT)) + 
  geom_density() + facet_wrap(~title, scales = "free") + ylab("Density") + xlab("Weight ^ α") +
  ggtitle("Destination weights with power transform α", subtitle = "Kernel density estimations")

all_destinations <- all_destinations %>% group_by(type) %>% mutate(WT_NORM = WT / max(WT)) %>% ungroup()
write_delim(all_destinations,"data/Manchester/destinations/all.csv",delim = ";")
