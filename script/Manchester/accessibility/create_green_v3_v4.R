# SCRIPT TO CREATE V3 AND V4 OF PUBLIC OPEN SPACES DESTINATION TYPE #
# V4 IS THE RELEVANT VERSION FOR ACCESSIBILITY. V3 OR V4 COULD BE USED FOR MITO #

library(tidyverse)

# Read Irena's v2
publicOpenSpace2 <- readr::read_delim("data/Manchester/destinations/Public_open_space_v2.0.csv")

# Create v3 by updating the weight function to be the product of area and number of amenities
publicOpenSpace3 <- publicOpenSpace2 %>%
  mutate(WEIGHT_old = WEIGHT) %>%
  mutate(WEIGHT = prop_area * various) %>%
  filter(!is.na(WEIGHT))

# Create v4 by restricting to 5000m^2 based on WHO Guidance on high-quality greenspace
publicOpenSpace4 <- publicOpenSpace3 %>%
  filter(prop_area >= 5000)

write_delim(publicOpenSpace3,"Public_open_space_v3.0.csv",delim = ";")
write_delim(publicOpenSpace4,"Public_open_space_v4.0.csv",delim = ";")

# Visualise results according to a 1/4 power parameter
ggplot(publicOpenSpace3,aes(x = WEIGHT^(1/4))) + geom_density() + ggtitle("Greenspace weight distribution")
ggplot(publicOpenSpace4,aes(x = WEIGHT^(1/4))) + geom_density() + ggtitle("Greenspace weight distribution")