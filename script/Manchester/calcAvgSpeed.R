## Calculate Average Speed ## (for reference when working with marginal cost values)

processed <- readr::read_csv("TfGM/routes.csv", col_types = "ciilllllcccinnn", na = c("","null")) %>% 
  select(-PathId,-cost) %>%
  filter(OriginWithinBoundary & DestinationWithinBoundary & !SameOrigAndDest) %>%
  pivot_wider(names_from = "Route", values_from = c(time,dist))

meanBikeSpeed <- mean(routesOA$dist_bike_short / routesOA$time_bike_short, na.rm = TRUE)
meanWalkSpeed <- mean(routesOA$dist_walk_short / routesOA$time_walk_short, na.rm = TRUE)


MARGINAL_COST_TIME = 0.0067

### VGVI / LIGHTING / SHANNON / LINK_STRESS ###
# Equivalent bike Marginal cost of time (use 0.002)
MARGINAL_COST_TIME / meanBikeSpeed

# Equivalent walk marginal costs of tie (use 0.005)
MARGINAL_COST_TIME / meanBikeSpeed


### GRADIENT ###
# Read in 2-way network, 97th percentile gradient is 0.1. So multiply gradient by 10x

### JUNCTION_STRESS ###
# guess up to 5x link stress

# Median crossing width = 5.8. Mean crossing width = 6.5
