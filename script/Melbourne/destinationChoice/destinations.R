#### COMBINE DESTINATIONS ####

library(tidyverse)

destinations <- list()
destinations$community_health <- read_delim("../../manchester/destinations/Community_health_resources.csv", delim = ";", col_types = "innnic")
destinations$early_year_access <- read_delim("../../manchester/destinations/Early_year_access.csv", delim = ";", col_types = "innnic")
destinations$eating_establishments <- read_delim("../../manchester/destinations/Eating_establishments.csv", delim = ";", col_types = "innnic")
destinations$education <- read_delim("../../manchester/destinations/Education.csv", delim = ";", col_types = "innnic")
destinations$financial <- read_delim("../../manchester/destinations/Financial.csv", delim = ";", col_types = "innnic")
destinations$food_retail <- read_delim("../../manchester/destinations/Food_retail.csv", delim = ";", col_types = "innnic")
destinations$primary_health_care <- read_delim("../../manchester/destinations/Primary_health_care.csv", delim = ";", col_types = "innnic")
destinations$recreation <- read_delim("../../manchester/destinations/Recreational_sports_pitches_and_facilities.csv", delim = ";", col_types = "innnic")
destinations$services <- read_delim("../../manchester/destinations/Services.csv", delim = ";", col_types = "innnic")
destinations$social_and_culture <- read_delim("../../manchester/destinations/Social_and_culture_locations.csv", delim = ";", col_types = "innnic")

destinations$public_open_space <- read_delim("../../manchester/destinations/Public_open_space_v4.0.csv", delim = ";") %>%
  group_by(ID) %>% 
  mutate(ID = cur_group_id()) %>% 
  ungroup() %>%
  transmute(ID,X,Y,WEIGHT,ref_no = as.integer(0),name = "unknown")

destinations$public_transport <- read_delim("../../manchester/destinations/Public_transport.csv", delim = ";", col_types = "innni") %>% mutate(ref_no = as.integer(0),name = "unknown")

all_destinations <- bind_rows(destinations, .id = "type") %>% mutate(ID = paste(type,ID,sep = "_"))

# # TEST & WRITE DIFFERENT TRANSFORMATIONS
# all_destinations %>%
#   mutate(WEIGHT2 = WEIGHT ^ 0.5,
#          WEIGHT4 = WEIGHT ^ 0.25) %>%
#   write_delim("../../manchester/destinations/all_test.csv",delim = ";")

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
         code = recode_factor(type,
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
         WT = case_when(type == "public_open_space" ~ WEIGHT ^ 0.25,
                        type == "recreation" ~ WEIGHT ^ 0.25,
                        type == "social_and_culture" ~ WEIGHT ^ 0.25,
                        type == "education" ~ WEIGHT,
                        TRUE ~ WEIGHT ^ 0.5)) %>%
  relocate(code,.after = "type") %>%
  relocate(title,.after = "code")

# Plot distribution of transformed weights
ggplot(all_destinations,aes(x = WT)) + 
  geom_density() + facet_wrap(~title, scales = "free") + ylab("Density") + xlab("Weight ^ α") +
  ggtitle("Destination weights with power transform α", subtitle = "Kernel density estimations")

# Write file with normalised weights for easier visualisation
norm_destinations <- all_destinations %>%
  group_by(type) %>% 
  mutate(WT_NORM = WT / max(WT)) %>% 
  ungroup()
  
write_delim(norm_destinations,"../../manchester/destinations/all.csv",delim = ";")
