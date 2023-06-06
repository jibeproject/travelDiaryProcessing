setwd("~/Documents/JIBE/travelDiaryProcessing")
source("script/Manchester/read.R")

OAs <- sf::st_read("~/Documents/manchester/zones/gm_oa.shp")
LSOAs <- sf::st_read("~/Documents/manchester/zones/LSOA_studyArea.shp")
MSOAs <- sf::st_read("~/Documents/manchester/zones/MSOA_studyArea.shp")

trips <- TRADS$trips %>% left_join(select(TRADS$households,hh.id,hh.OA))

# Spatial variation based on origin
getTripsAndShares <- function(var,suffix,areaType) {
  lookup <- OA_lookup[,c("OA11CD",areaType)]
  colnames(lookup)[1] <- var
  
  result <- trips %>% 
    left_join(lookup) %>% 
    group_by_(areaType) %>%
    summarise(cycleCount = sum(t.m_main == "Bicycle"),
              walkCount = sum(t.m_main == "Walk"),
              trips = n(),
              cycleShare = cycleCount / n(),
              walkShare = walkCount / n())
  
  colnames(result)[2:6] <- paste(colnames(result)[2:6],suffix,sep = "_")
  
  return(result)
}

OAs <- OAs %>%
  left_join(getTripsAndShares("t.startOA","start","OA11CD"),by = c("geo_code" = "OA11CD")) %>%
  left_join(getTripsAndShares("t.endOA","end","OA11CD"),by = c("geo_code" = "OA11CD")) %>%
  left_join(getTripsAndShares("hh.OA","hh","OA11CD"),by = c("geo_code" = "OA11CD")) %>%
  mutate(across(starts_with("cycle") | starts_with("walk") | starts_with("trip"),function(x) replace_na(x,0)))
  
MSOAs <- MSOAs %>%
  left_join(getTripsAndShares("t.startOA","start","MSOA11CD")) %>%
  left_join(getTripsAndShares("t.endOA","end","MSOA11CD")) %>%
  left_join(getTripsAndShares("hh.OA","hh","MSOA11CD")) %>%
  mutate(across(starts_with("cycle") | starts_with("walk") | starts_with("trip"),function(x) replace_na(x,0)))

LSOAs <- LSOAs %>%
  left_join(getTripsAndShares("t.startOA","start","LSOA11CD")) %>%
  left_join(getTripsAndShares("t.endOA","end","LSOA11CD")) %>%
  left_join(getTripsAndShares("hh.OA","hh","LSOA11CD")) %>%
  mutate(across(starts_with("cycle") | starts_with("walk") | starts_with("trip"),function(x) replace_na(x,0)))
  

sf::st_write(OAs,"result/activeSharesOA.gpkg",append = FALSE)
sf::st_write(MSOAs,"result/activeSharesMSOA.gpkg",append = FALSE)
sf::st_write(LSOAs,"result/activeSharesLSOA.gpkg",append = FALSE)