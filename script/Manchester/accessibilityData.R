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
  filter(t.m_main == "Walk", t.originWithinBoundary, t.destinationWithinBoundary, !t.sameOrigAndDest) %>%
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
