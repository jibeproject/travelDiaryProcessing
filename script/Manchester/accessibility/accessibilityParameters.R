# Read in routing costs
library(tidyverse)
TRADS <- readRDS("~/Documents/JIBE/travelDiaryProcessing/data/Manchester/processed/TRADS_safe_routed_v2.rds")$trips

## Cost vs. distance
summary(TRADS$t.route.walk_jibe_cost/TRADS$t.route.bike_short_dist)

lm(t.route.walk_jibe_cost ~ t.route.bike_short_dist + 0, data = TRADS)

## COSTS

walkTrips2 <- TRADS %>%
  filter(t.m_main == "Walk", t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  arrange(t.route.walk_jibe_cost) %>%
  transmute(t.cost = t.route.walk_jibe_cost,
            t.weight = t.expansionFactor_all,
            cum.pct = cumsum(t.weight) / sum(t.weight))

p80walk2 <- with(walkTrips2, first(t.cost[cum.pct >= 0.8]))

bikeTrips2 <- TRADS %>%
  filter(t.m_main == "Bicycle", t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  arrange(t.route.bike_jibe_cost) %>%
  transmute(t.cost = t.route.bike_jibe_cost,
            t.weight = t.expansionFactor_all,
            cum.pct = cumsum(t.weight) / sum(t.weight))

p80bike2 <- with(bikeTrips2, first(t.cost[cum.pct >= 0.8]))

# Get parameters
-log(1-0.8)/p80walk2
-log(1-0.8)/p80bike2

## DISTANCES
walkTrips2 <- TRADS %>%
  filter(t.m_main == "Walk", 
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  arrange(t.route.walk_short_dist) %>%
  transmute(t.cost = t.route.walk_short_dist,
            t.weight = t.expansionFactor_all,
            cum.pct = cumsum(t.weight) / sum(t.weight))

p80walk2 <- with(walkTrips2, first(t.cost[cum.pct >= 0.8]))

bikeTrips2 <- TRADS %>%
  filter(t.m_main == "Bicycle", t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  arrange(t.route.walk_short_dist) %>%
  transmute(t.cost = t.route.bike_short_dist,
            t.weight = t.expansionFactor_all,
            cum.pct = cumsum(t.weight) / sum(t.weight))

p80bike2 <- with(bikeTrips2, first(t.cost[cum.pct >= 0.8]))

# Get parameters
-log(1-0.8)/p80walk2
-log(1-0.8)/p80bike2


# Make a more general function
calcImpedanceParam <- function(data, lim, cost_var, weight_var) {
  processed <- data %>%
    arrange({{cost_var}}) %>%
    transmute(t.cost = {{cost_var}},
              t.weight = {{weight_var}},
              cum.pct = cumsum({{weight_var}}) / sum({{weight_var}}))
  
  pLim <- with(processed, first(t.cost[cum.pct >= lim]))
  
  impedanceParam <- -log(1-lim)/pLim
  
  print(paste(c(nrow(processed)," records."),collapse = ""))
  print(paste(c(lim*100,"th percentile cost = ",pLim),collapse = ""))
  print(paste(c("impedance parameter = ",impedanceParam),collapse = ""))
  
  return(impedanceParam)
}

# For Greenspace
TRADS %>% 
  filter(t.m_main == "Walk", t.endPurpose == "Social - Entertainment, recreation, Participate in sport, pub, restaurant", 
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  calcImpedanceParam(0.8,t.route.walk_short_dist,t.expansionFactor_all)

# Food
TRADS %>% 
  filter(t.m_main == "Walk", t.endPurpose == "Shopping Food",
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  calcImpedanceParam(0.8,t.route.walk_short_dist,t.expansionFactor_all) 

TRADS %>% 
  filter(t.m_main == "Walk", t.endPurpose == "Shopping Food",
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  calcImpedanceParam(0.8,t.route.walk_jibe_cost,t.expansionFactor_all) 

TRADS %>% 
  filter(t.m_main == "Bicycle", t.endPurpose == "Shopping Food",
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  calcImpedanceParam(0.8,t.route.bike_short_dist,t.expansionFactor_all) 

TRADS %>% 
  filter(t.m_main == "Bicycle", t.endPurpose == "Shopping Food",
         t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
  calcImpedanceParam(0.8,t.route.bike_jibe_cost,t.expansionFactor_all) 

