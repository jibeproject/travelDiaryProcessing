# Analyze rubber banding results
library(tidyverse)
rm(list = ls())

TRADS <- readRDS("data/Manchester/processed/TRADS_safe.rds")

stops <- readr::read_csv("data/manchester/processed/abm_stops.csv", na = c("null",""),
                                   col_types = "ciillllllccclddd") %>% 
  select(-PathId,-cost,-time) %>%
  pivot_wider(names_from = Route, values_from = dist) %>%
  rename(hh.id = IDNumber, 
         p.id = PersonNumber, 
         t.id = TripNumber,
         t.homeWithinBoundary = HomeWithinBoundary,
         t.originWithinBoundary = OriginWithinBoundary, 
         t.destinationWithinBoundary = DestinationWithinBoundary, 
         t.sameHomeAndDest = SameHomeAndDest,
         t.sameMainAndDest = SameMainAndDest,
         t.sameOrigAndDest = SameOrigAndDest) %>%
  left_join(TRADS$trips_abm)

# Set NAs to 0
stops$car_hs[stops$t.sameHomeAndDest] <- 0
stops$car_sm[stops$t.sameMainAndDest] <- 0

# Create safe version
stops_safe <- stops %>% 
  group_by(hh.id) %>%
  mutate(hh.id = cur_group_id()) %>%
  select(hh.id,p.id,tour.id,tour.purpose,act.id,act.purpose,act.type,starts_with("beeline"),starts_with("car"))

write_csv(stops_safe,file = "data/Manchester/processed/abm_stops_safe.csv")

# Define home and main dist
stops$home_stop = stops$car_hs
stops$stop_main = stops$car_sm
stops$home_main = stops$car_hm

# Get Home <-> Main Dist
stops <- stops %>% 
  filter(!act.purpose == "rrt") %>%
  mutate(p = (home_stop^2 + home_main^2 - stop_main^2) / (2 * home_main^2),
         h = sqrt(home_stop^2 - ((home_stop^2 + home_main^2 - stop_main^2) / (2 * home_main))^2) / home_main)
         

ggplot(stops, aes(x = p, colour = act.purpose)) + 
  geom_density() + ggtitle("Location of stop along home-main route") + 
  xlab("share of distance between home activity (x=0) and main activity (x=1)") + xlim(-1.5,2.5)

ggplot(stops, aes(x = h, colour = act.purpose)) + 
  geom_density(adjust = 2) + ggtitle("Detour from direct home-main route") + 
  xlab("detour as a proportion of home-main distance") + xlim(0,2)

ggplot(stops, aes(x = p, y = h, colour = act.purpose)) + geom_point(alpha = 0.3) + xlim(-0.3,1.3) + ylim(0,1) + 
  xlab("share of distance between home activity (x=0) and main activity (x=1)") + 
  ylab("detour as a proportion of home-main distance")
