library(tidyverse)

TRADS <- readRDS("data/Manchester/processed/TRADS.rds")

indiv <- TRADS$indiv

trips <- TRADS$trips %>% left_join(TRADS$indiv)

modelled <- readr::read_csv("../manchester/allModes.csv",na = c("","null")) %>%
  filter(Mode == "walk",
         Route == "walk_fast",
         OriginWithinBoundary,
         DestinationWithinBoundary,
         !SameOrigAndDest) %>%
  transmute(hh.id = IDNumber,
            p.id = PersonNumber,
            t.id = TripNumber,
            time,dist,
            OriginWithinBoundary,
            DestinationWithinBoundary,
            SameOrigAndDest)

walks <- trips %>% filter(t.m_main == "Walk",
                          !t.m_carDriver,
                          !t.m_carPassenger,
                          !t.m_metrolink,
                          !t.m_bus,
                          !t.m_taxi,
                          !is.na(p.age_group),
                          !is.na(p.female)) %>%
  select(hh.id,p.id,t.id,p.age_group,p.female,t.travelTime) %>%
  inner_join(modelled) %>%
  mutate(speed_mod = dist / time,
         speed_est = dist / t.travelTime) %>%
  filter(speed_est <= 5)

ggplot(walks,aes(x = speed_est)) + geom_density()



groups <- walks %>%
  group_by(p.age_group,p.female) %>%
  summarise(mean_speed = mean(speed_est),
            n = n())

ggplot(groups, aes(x = p.age_group, y = mean_speed)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~p.female)
