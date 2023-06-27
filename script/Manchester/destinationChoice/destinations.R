#### COMBINE DESTINATIONS ####
destinations <- list()

destinations$community_health <- read_delim("data/Manchester/destinations/Community_health_resources.csv", delim = ";", col_types = "innnic")
destinations$early_year_access <- read_delim("data/Manchester/destinations/Early_year_access.csv", delim = ";", col_types = "innnic")
destinations$eating_establishments <- read_delim("data/Manchester/destinations/Eating_establishments.csv", delim = ";", col_types = "innnic")
destinations$education <- read_delim("data/Manchester/destinations/Education.csv", delim = ";", col_types = "innnic")
destinations$financial <- read_delim("data/Manchester/destinations/Financial.csv", delim = ";", col_types = "innnic")
destinations$food_retail <- read_delim("data/Manchester/destinations/Food_retail.csv", delim = ";", col_types = "innnic")
destinations$primary_health_care <- read_delim("data/Manchester/destinations/Food_retail.csv", delim = ";", col_types = "innnic")
destinations$public_open_space <- read_delim("data/Manchester/destinations/Primary_health_care.csv", delim = ";", col_types = "innnic")
destinations$recreation <- read_delim("data/Manchester/destinations/Recreational_sports_pitches_and_facilities.csv", delim = ";", col_types = "innnic")
destinations$services <- read_delim("data/Manchester/destinations/Services.csv", delim = ";", col_types = "innnic")
destinations$social_and_culture <- read_delim("data/Manchester/destinations/Social_and_culture_locations.csv", delim = ";", col_types = "innnic")

all_destinations <- bind_rows(destinations, .id = "type")
write_csv2(all_destinations,"data/Manchester/destinations/all.csv")

