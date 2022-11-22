setwd("~/Documents/JIBE/travelDiaryProcessing")
source("script/Manchester/read.R")

# Spatial variation based on origin
cycleTripsByStartOA <- TRADS$trips %>% 
  filter(t.startOA != "         ") %>%
  group_by(t.startOA) %>% 
  summarise(cycleStart = sum(t.m_main == "Bicycle"),
            cycleStartShare = cycleStart / n())

cycleTripsByEndOA <- TRADS$trips %>% 
  filter(t.endOA != "         ") %>%
  group_by(t.endOA) %>% 
  summarise(cycleEnd = sum(t.m_main == "Bicycle"),
            cycleEndShare = cycleEnd / n())

cycleTripsByHomeOA <- TRADS$trips %>%
  left_join(TRADS$households) %>%
  group_by(hh.OA) %>%
  summarise(cycleHome = sum(t.m_main == "Bicycle"),
            cycleHomeShare = cycleHome / n())

OAs <- sf::read_sf("~/Documents/manchester/JIBE/accessibility/OAs/gm_oa.shp")

OAs <- OAs %>%
  left_join(cycleTripsByStartOA,by = c("geo_code" = "t.startOA")) %>%
  left_join(cycleTripsByEndOA,by = c("geo_code" = "t.endOA")) %>%
  left_join(cycleTripsByHomeOA,by = c("geo_code" = "hh.OA")) %>%
  mutate(across(starts_with("cycle"),function(x) replace_na(x,0)))

sf::write_sf(OAs,"result/bikeShares.shp")