# Analyze rubber banding results
library(tidyverse)
rm(list = ls())

TRADS <- readRDS("data/Manchester/processed/TRADS_safe_routed.rds")

trips <- readr::read_csv("~/Documents/manchester/TfGM/rubberBandingResults.csv", na = c("null",""),
                                   col_types = "ciilllllldddddddddddd") %>% 
  rename(hh.id = IDNumber, 
         p.id = PersonNumber, 
         t.id = TripNumber,
         t.homeWithinBoundary = HomeWithinBoundary,
         t.originWithinBoundary = OriginWithinBoundary, 
         t.destinationWithinBoundary = DestinationWithinBoundary, 
         t.sameHomeAndDest = SameHomeAndDest,
         t.sameMainAndDest = SameMainAndDest,
         t.sameOrigAndDest = SameOrigAndDest) %>%
  select(-ends_with("time"),-ends_with("cost")) %>%
  left_join(TRADS$trips_abm)

trips$home_dist[trips$t.sameHomeAndDest] <- 0
trips$main_dist[trips$t.sameMainAndDest] <- 0

# Get Home <-> Main Dist
trips <- trips %>% 
  group_by(hh.id,p.id,tour.id) %>% 
  mutate(home_main = first(home_dist[act.type == "main"]),
         home_stop = home_dist,
         main_stop = main_dist) %>% 
  ungroup()

stops <- trips %>% 
  filter(startsWith(act.type,"stop"),
         !act.purpose == "rrt") %>%
  mutate(p = (home_stop^2 + home_main^2 - main_stop^2) / (2 * home_main^2),
         h = sqrt(home_stop^2 - ((home_stop^2 + home_main^2 - main_stop^2) / (2 * home_main))^2) / home_main)
         

ggplot(stops, aes(x = p, colour = act.purpose)) + 
  geom_density() + ggtitle("Location of stop along home-main route") + 
  xlab("share of distance between home activity (x=0) and main activity (x=1)") + xlim(-1.5,2.5)

ggplot(stops, aes(x = h, colour = act.purpose)) + 
  geom_density(adjust = 2) + ggtitle("Detour from direct home-main route") + 
  xlab("detour as a proportion of home-main distance") + xlim(0,2)

ggplot(stops, aes(x = p, y = h, colour = act.purpose)) + geom_point(alpha = 0.3) + xlim(-0.3,1.3) + ylim(0,1) + 
  xlab("share of distance between home activity (x=0) and main activity (x=1)") + 
  ylab("detour as a proportion of home-main distance")
