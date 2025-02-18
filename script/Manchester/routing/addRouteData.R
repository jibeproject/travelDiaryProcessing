###### SCRIPT TO READ ROUTE ATTRIBUTES AND ADD TO TRADS DATASET #######
library(tidyverse)
rm(list = ls())

routeAttributes <- readr::read_csv("~/Documents/manchester/TfGM/output11.csv", na = c("null",""),
                                   col_types = "ciilllllddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddiidddccccccd") %>% 
  rename(hh.id = IDNumber, 
         p.id = PersonNumber, 
         t.id = TripNumber,
         t.homeWithinBoundary = HomeWithinBoundary,
         t.originWithinBoundary = OriginWithinBoundary, 
         t.destinationWithinBoundary = DestinationWithinBoundary, 
         t.sameHomeAndDest = SameHomeAndDest,
         t.sameOrigAndDest = SameOrigAndDest) %>%
  select(-home_cost,-home_time)

# Add prefix "t.route." to all route attributes
names(routeAttributes)[-c(1:8)] <- paste0("t.route.",names(routeAttributes)[-c(1:8)])

###### INCLUDE ATTRIBUTES INTO TRADS DATASET ######
# Main dataset
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")
TRADS$trips <- left_join(TRADS$trips,routeAttributes)
saveRDS(TRADS,"data/Manchester/processed/TRADS_routed.rds")

# Safe dataset
SAFE <- readRDS("data/Manchester/processed/TRADS_safe.rds")
SAFE$trips <- left_join(SAFE$trips,routeAttributes)
saveRDS(SAFE,"data/Manchester/processed/TRADS_safe_routed.rds")