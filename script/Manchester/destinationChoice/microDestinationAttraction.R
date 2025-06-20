# Read dwellings
dwellings <- readr::read_csv("../../manchester/destinations/dwelling_building.csv", col_types = "nicnn") %>%
  transmute(type = "dwelling",
            code = "HH",
            ID = paste0(type,"_",1:n(),sep = ""),
            X = coordX,
            Y = coordY,
            oaID)

# Compare households vs. dwellings in each OA
households <- readr::read_csv("../../manchester/destinations/hh_oa_2021.csv", col_types = "icccccccii") %>%
  inner_join(count(dwellings,oaID,name = "dwellings")) %>%
  transmute(oaID,households,dwellings,WT = households / dwellings)

# Add household weight attribute
dwellings <- dwellings %>%
  left_join(select(households,oaID,WT)) %>%
  relocate(WT,.before = oaID)

# Read output areas and attraction coefficients
OAs <- sf::st_read("../../manchester/zones/zonesShapefile/OA_2021_MCR.shp") %>% transmute(oaID = id)
attractions <- read_csv("result/Manchester/destinationChoice/tripAttractionsCoefficients3.csv")
purposes <- names(attractions)[-1]

# all_destinations comes from destinations.r
attr_destinations <- all_destinations %>% 
  filter(code != "PT") %>%
  group_by(code,ID) %>%
  mutate(WT = WT / n()) %>%
  ungroup() %>%
  select(type,code,ID,X,Y,WT) %>%
  sf::st_as_sf(coords = c("X","Y"),remove = FALSE,crs = 27700) %>%
  sf::st_join(OAs,join = sf::st_intersects) %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(oaID)) %>%
  rbind(dwellings) %>%
  left_join(attractions, by = c("code" = "IndependentVariable")) %>%
  mutate(across(all_of(purposes), ~.*WT))

write_csv(attr_destinations,"result/Manchester/destinationChoice/destinationAttraction.csv")

# Plot attraction results
plot_data <- attr_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  group_by(purpose,code) %>%
  summarise(sum_wt = sum(value)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p,y = purpose, fill = code)) + 
  geom_bar(position = "fill",stat = "identity") + 
  xlab("share")

# Test aggregation before estimating total attraction
# (should match plot data, which estimates attraction before aggregating)
test <- attr_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WT)) %>%
  left_join(attractions, by = c("code" = "IndependentVariable")) %>%
  mutate(across(all_of(purposes), ~.*sum_wt))
