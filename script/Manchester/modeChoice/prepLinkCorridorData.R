### Clear memory
rm(list = ls())
library(tidyverse)

## MODE CHOICE JOINING / AGGREGATION CODE ##

# ESTIMATE DECAY PARAMETER
PERCENTILE = 0.9
DETOUR = 0.25

# estimate (from exponential distribution CDF)
-log(1-PERCENTILE)/DETOUR

ALPHA = 9.2

########### GET SHORTEST/FASTEST PATH INFO ###########
routesDist <- read_csv("data/manchester/corridor/routesShort.csv")
routesTime <- read_csv("data/manchester/corridor/routesFast.csv")

########### COMPUTE AND AGGREGATE LINK DATA BASED ON DETOUR ###########

# BRING IN ADDITIONAL LINK DATA FROM NETWORK GPKG
networkBike <- read_csv("data/manchester/corridor/networkBike.csv")
networkWalk <- read_csv("data/manchester/corridor/networkWalk.csv")

networkWalk <- networkWalk %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

networkBike <- networkBike %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

# READ IN LINK DATA
bikeLinksShort <- rbind(read_csv("data/manchester/corridor/commuteBikeShort.csv"),
                        read_csv("data/manchester/corridor/intrazonalBike.csv"),
                        read_csv("data/manchester/corridor/discretionaryBikeShort.csv"))%>%left_join(networkBike)
bikeLinksFast <- rbind(read_csv("data/manchester/corridor/commuteBikeFast.csv"),
                       read_csv("data/manchester/corridor/intrazonalBike.csv"),
                       read_csv("data/manchester/corridor/discretionaryBikeFast.csv"))%>%left_join(networkBike)
walkLinksShort <- rbind(read_csv("data/manchester/corridor/commuteWalkShort.csv"),
                        read_csv("data/manchester/corridor/intrazonalWalk.csv"),
                        read_csv("data/manchester/corridor/discretionaryWalkShort.csv"))%>%left_join(networkWalk)
walkLinksFast <- rbind(read_csv("data/manchester/corridor/commuteWalkFast.csv"),
                       read_csv("data/manchester/corridor/intrazonalWalk.csv"),
                       read_csv("data/manchester/corridor/discretionaryWalkFast.csv"))%>%left_join(networkWalk)

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
              lights = sum(streetLights * df) / sumWt,
              lightsDensity = sum(streetLightsDensity * wt) / sumWt) %>%
    ungroup() %>%
    dplyr::select(-sumWt) %>%
    rename_with(~paste(modeName,.x,sep="_"),.cols = c(stressLink,stressJct,vgvi,shannon,POIs,negPOIs,crime,lights,lightsDensity))
}

bikeAggShort = bikeLinksShort %>% aggregateBE(length,"bike")
walkAggShort = walkLinksShort %>% aggregateBE(length,"walk")
bikeAggFast = bikeLinksFast %>% aggregateBE(time,"bike")
walkAggFast = walkLinksFast %>% aggregateBE(time,"walk")

resultShort <- routesDist %>% right_join(bikeAggShort) %>% right_join(walkAggShort)
resultFast <- routesTime %>% right_join(bikeAggFast) %>% right_join(walkAggFast)

# Save to csv
write_csv(resultShort,"data/manchester/corridor/AllShort92.csv")
write_csv(resultFast,"data/manchester/corridor/AllFast92.csv")
