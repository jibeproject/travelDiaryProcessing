###### SCRIPT TO READ ROUTE ATTRIBUTES AND ADD TO TRADS DATASET #######
library(tidyverse)

routeAttributes <- readr::read_csv("data/Manchester/routing/output.csv") %>% 
  rename(hh.id = IDNumber, 
         p.id = PersonNumber, 
         t.id = TripNumber,
         t.originWithinBoundary = OriginWithinBoundary, 
         t.destinationWithinBoundary = DestinationWithinBoundary, 
         t.sameOrigAndDest = SameOrigAndDest)

# Add prefix "t.route." to all route attributes
names(routeAttributes)[-c(1:6)] <- paste0("t.route.",names(routeAttributes)[-c(1:6)])

###### INCLUDE ATTRIBUTES INTO TRADS DATASET ######
# Main dataset
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")
TRADS$trips <- left_join(TRADS$trips,routeAttributes)
saveRDS(TRADS,"data/Manchester/processed/TRADS_routed.rds")

# Safe dataset
SAFE <- readRDS("data/Manchester/processed/TRADS_safe.rds")
SAFE$trips <- left_join(SAFE$trips,routeAttributes)
saveRDS(SAFE,"data/Manchester/processed/TRADS_safe_routed.rds")