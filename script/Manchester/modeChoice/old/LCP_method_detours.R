# Evaluate ubserved detours 

model <- apollo_loadModel("result/Manchester/discretionary/LCP/good_discretionary_v3")
# model <- apollo_loadModel("result/Manchester/mandatory/tests/mnl_noModeUse_lcp_v1")
apollo_modelOutput(model)

interzonalBike <- database %>% 
  filter(!SameOrigAndDest, t.m_main_apollo == "bike") %>%
  left_join(routes$bike) %>%
  mutate(cost = travelTime + 
           model$estimate["gamma_bike_grad"] * gradient + 
           model$estimate["gamma_bike_grad"] * vgvi + 
           model$estimate["gamma_bike_stressLink"] * stressLink + 
           model$estimate["gamma_bike_stressJct"] * stressJct) %>%
  group_by(t.ID) %>%
  reframe(LCP_time = travelTime[which.min(cost)],
          min_time = min(travelTime)) %>%
  mutate(detour = LCP_time / min_time)

summary(interzonalBike$detour - 1)

interzonalWalk <- database %>% 
  filter(!SameOrigAndDest, t.m_main_apollo == "walk") %>%
  left_join(routes$walk) %>%
  mutate(cost = travelTime + 
           model$estimate["gamma_walk_grad"] * gradient + 
           model$estimate["gamma_walk_grad"] * vgvi + 
           model$estimate["gamma_walk_stressLink"] * stressLink + 
           model$estimate["gamma_walk_stressJct"] * stressJct) %>%
  group_by(t.ID) %>%
  reframe(LCP_time = travelTime[which.min(cost)],
          min_time = min(travelTime)) %>%
  mutate(detour = LCP_time / min_time)

summary(interzonalWalk$detour - 1)
