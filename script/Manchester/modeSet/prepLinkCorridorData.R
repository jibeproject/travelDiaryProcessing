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

########### GET SHORTEST PATH INFO ###########
routesDist <- read_csv("data/manchester/modeSet/routesShort.csv")

########### COMPUTE AND AGGREGATE LINK DATA BASED ON DETOUR ###########

# BRING IN ADDITIONAL LINK DATA FROM NETWORK GPKG
networkBike <- read_csv("data/manchester/corridor/networkBike.csv")
networkWalk <- read_csv("data/manchester/corridor/networkWalk.csv")

networkWalk <- networkWalk %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

networkBike <- networkBike %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

# READ IN LINK DATA
bikeLinksShort <- rbind(read_csv("data/manchester/modeSet/linkDetoursHomeWorkBikeDist.csv"),
                        read_csv("data/manchester/modeSet/intrazonalBike.csv"))%>%left_join(networkBike)
walkLinksShort <- rbind(read_csv("data/manchester/modeSet/linkDetoursHomeWorkWalkDist.csv"),
                        read_csv("data/manchester/modeSet/intrazonalWalk.csv"))%>%left_join(networkWalk)


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

resultShort <- routesDist %>% left_join(bikeAggShort) %>% left_join(walkAggShort)

# Save to csv
write_csv(resultShort,"data/manchester/modeSet/AllShort92.csv")
