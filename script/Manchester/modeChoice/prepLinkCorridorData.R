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
  mutate(streetLightsDensity = pmin(1/15,streetLights / length),
         crimeDensity=pmin(1/4,crime / length))

networkBike <- networkBike %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length),
         crimeDensity=pmin(1/4,crime / length))

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
    summarise(sumWt = sum(wt,na.rm = T),
              stressLink = sum(stressLink * wt,na.rm = T) / sumWt,
              stressJct = sum(stressJct * df,na.rm = T) / sumWt,
              vgvi = sum(vgvi * wt,na.rm = T) / sumWt,
              shannon = sum(shannon * wt,na.rm = T) / sumWt,
              POIs = sum(POIs * df,na.rm = T) / sumWt,
              negPOIs = sum(negPOIs * df,na.rm = T) / sumWt,
              crime = sum(crime * df,na.rm = T) / sumWt,
              lights = sum(streetLights * df,na.rm = T) / sumWt,
              lightsDensity = sum(streetLightsDensity * wt,na.rm = T) / sumWt) %>%
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
write_csv(resultShort,"data/manchester/corridor/tripsCorridorShort.csv")
write_csv(resultFast,"data/manchester/corridor/tripsCorridorFast.csv")
