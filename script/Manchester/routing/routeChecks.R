results <- sf::read_sf("bikeRoutesSampled.gpkg")

results$cost_check = with(results, c_time + c_dist + c_grad + c_surf + c_attr + c_stress + c_jct)

test <- select(results,Route,cost,cost_check) %>% 
  filter(Route != "bike_fast" & Route != "bike_short") %>%
  mutate(test = cost_check - cost)

results <- sf::read_sf("percentileTest.csv")

test3 <- semi_join(RAW$trips,test)

