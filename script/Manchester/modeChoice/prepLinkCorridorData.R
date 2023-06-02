library(tidyverse)
setwd("~/Documents/manchester")

## MODE CHOICE JOINING / AGGREGATION CODE ##

# ESTIMATE DECAY PARAMETER
PERCENTILE = 0.9
DETOUR = 0.25

# estimate (from exponential distribution CDF)
-log(1-PERCENTILE)/DETOUR

ALPHA = 9.2

########### GET SHORTEST/FASTEST PATH INFO ###########
routesDist <- read_csv("TfGM/mandatory/routes/routesShort.csv")
routesTime <- read_csv("TfGM/mandatory/routes/routesFast.csv")

########### COMPUTE AND AGGREGATE LINK DATA BASED ON DETOUR ###########

# BRING IN ADDITIONAL LINK DATA FROM NETWORK GPKG
networkBike <- read_csv("TfGM/networkBike.csv")
networkWalk <- read_csv("TfGM/networkWalk.csv")

# READ IN LINK DATA
bikeLinksShort <- readr::read_csv("TfGM/mandatory/corridors/corridorsBikeShort.csv") %>% left_join(networkBike)
bikeLinksFast <- readr::read_csv("TfGM/mandatory/corridors/corridorsBikeFast.csv") %>% left_join(networkBike)

walkLinksShort <- readr::read_csv("TfGM/mandatory/corridors/corridorsWalkShort.csv") %>% left_join(networkWalk)
walkLinksFast <- readr::read_csv("TfGM/mandatory/corridors/corridorsWalkFast.csv") %>% left_join(networkWalk)

# GROUP AND AGGREGATE
aggregateBE <- function(linkData,costVar,modeName) {
  linkData %>%
    mutate(df = exp(-1 * ALPHA * detour),
           wt = df * !!enquo(costVar)) %>%
    group_by(IDNumber,PersonNumber,TripNumber) %>%
    summarise(sumWt = sum(wt),
              stressLink = sum(stressLink * wt) / sumWt,
              stressJct = sum(stressJct * df) / sumWt,
              vgvi = sum(vgvi * wt) / sumWt,
              shannon = sum(shannon * wt) / sumWt,
              POIs = sum(POIs * df) / sumWt,
              negPOIs = sum(negPOIs * df) / sumWt,
              crime = sum(crime * df) / sumWt,
              lights = sum(streetLights * df) / sumWt) %>%
    ungroup() %>%
    select(-sumWt) %>%
    rename_with(~paste(modeName,.x,sep="_"),.cols = c(stressLink,stressJct,vgvi,shannon,POIs,negPOIs,crime,lights))
}

bikeAggShort = bikeLinksShort %>% aggregateBE(length,"bike")
walkAggShort = walkLinksShort %>% aggregateBE(length,"walk")
bikeAggFast = bikeLinksFast %>% aggregateBE(time,"bike")
walkAggFast = bikeLinksFast %>% aggregateBE(time,"walk")

resultShort <- routesDist %>% inner_join(bikeAggShort) %>% inner_join(walkAggShort)
resultFast <- routesTime %>% inner_join(bikeAggFast) %>% inner_join(walkAggFast)

# Save to csv
write_csv(resultShort,"TfGM/mandatory/mandatoryShort92.csv")
write_csv(resultFast,"TfGM/mandatory/mandatoryFast92.csv")