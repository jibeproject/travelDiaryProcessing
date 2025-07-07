#### COMBINE DESTINATIONS ####

library(tidyverse)

pos <-  sf::st_read("../melbourne/preprocessing/poi/final/baseline_weight.gpkg", layer = "park") %>%
  rename(Indicator = indicator, Attribute = attribute) %>%
  sf::st_transform(crs = 28355) %>%
  mutate(geometry = sf::st_centroid(geom)) %>%
  mutate(
    Indicator = "Public open space",
    Attribute = "Public open space",
    X = sf::st_coordinates(geometry)[,1],
    Y = sf::st_coordinates(geometry)[,2]
  ) %>%
  select(Indicator, X, Y, weight, Attribute) %>%
  sf::st_drop_geometry()

write_csv(pos, "../melbourne/preprocessing/poi/final/Public_open_space_derived_from_gpkg.csv")

pt <-  sf::st_read("../melbourne/preprocessing/poi/final/baseline_weight.gpkg", layer = "pt") %>%
  rename(Indicator = indicator, Attribute = attribute) %>%
  sf::st_transform(crs = 28355) %>%
  mutate(geometry = sf::st_centroid(geom)) %>%
  mutate(
    Indicator = "Public transport",
    X = sf::st_coordinates(geometry)[,1],
    Y = sf::st_coordinates(geometry)[,2]
  ) %>%
  select(Indicator, X, Y, weight, Attribute)%>%
  sf::st_drop_geometry()

write_csv(pt, "../melbourne/preprocessing/poi/final/Public_transport_derived_from_gpkg.csv")

destination_paths <- c(
  "../melbourne/preprocessing/poi/final/Community_health.csv",
  "../melbourne/preprocessing/poi/final/Early_year.csv",
  "../melbourne/preprocessing/poi/final/Eating_establishments.csv",
  "../melbourne/preprocessing/poi/final/Education.csv",
  "../melbourne/preprocessing/poi/final/Financial.csv",
  "../melbourne/preprocessing/poi/final/Food_retail.csv",
  "../melbourne/preprocessing/poi/final/Primary_health.csv",
  "../melbourne/preprocessing/poi/final/Recreational.csv",
  "../melbourne/preprocessing/poi/final/Services.csv",
  "../melbourne/preprocessing/poi/final/Social_cultural.csv",
  "../melbourne/preprocessing/poi/final/Public_open_space_derived_from_gpkg.csv",
  "../melbourne/preprocessing/poi/final/Public_transport_derived_from_gpkg.csv"
)

# Read and bind all destinations
all_destinations <- imap_dfr(
    destination_paths, ~ read_csv(.x) %>% rename(type = Indicator)
  ) %>%
  group_by(type) %>%
  mutate(id = row_number()) %>%
  ungroup() 
# # TEST & WRITE DIFFERENT TRANSFORMATIONS
# all_destinations %>%
#   mutate(weight2 = weight ^ 0.5,
#          weight4 = weight ^ 0.25) %>%
#   write_delim("../melbourne/preprocessing/poi/final/all_test.csv",delim = ";")

all_destinations %>%
  group_by(type) %>%
  summarise(
    na_wt = sum(is.na(weight)),
    n = n(),
    pct_na = na_wt / n * 100
    )

# As per https://github.com/jibeproject/silo/issues/41, destinations with NA weights will be omitted under assumption these were not used to calculate zonal trip attraction coefficients and therefore should not be considered for microdestination weightings that need to sum to match these.

all_destinations <- all_destinations %>%
  filter(!is.na(weight))

## PLOT weight DISTRIBUTIONS ##
all_destinations <- all_destinations %>% 
  mutate(title = recode_factor(type,
                               `Social and cultural locations` = "Social and cultural (α = 0.25)",
                               `Education` = "Education (α = 1)",
                               `Primary health care` = "Primary healthcare (α = 0.5)",
                               `Community health resources` = "Community health (α = 0.5)",
                               `Recreational, sports pitches and facilities` = "Recreational facilities (α = 0.25)",
                               `Early year access` = "Early years access (α = 0.5)",
                               `Food retail` = "Food retail (α = 0.5)",
                               `Eating establishments` = "Eating establishments (α = 0.5)",
                               `Financial` = "Financial establishments (α = 0.5)",
                               `Services` = "Services (α = 0.5)",
                               `Public transport` = "Public transport (α = 0.5)",
                               `Public open space` = "Public open space (α = 0.25)",.ordered = TRUE),
         code = recode_factor(type,
                              `Social and cultural locations` = "SCL",
                              `Education` = "EDU",
                              `Primary health care` = "PHC",
                              `Community health resources` = "CHR",
                              `Recreational, sports pitches and facilities` = "RSPF",
                              `Early year access` = "EYA",
                              `Food retail` = "FR",
                              `Eating establishments` = "EE",
                              `Financial` = "FIN",
                              `Services` = "SER",
                              `Public transport` = "PT",
                              `Public open space` = "POS"),
         WT = case_when(type == "Public open space" ~ weight ^ 0.25,
                        type == "Recreational, sports pitches and facilities" ~ weight ^ 0.25,
                        type == "Social and cultural locations" ~ weight ^ 0.25,
                        type == "Education" ~ weight,
                        TRUE ~ weight ^ 0.5)) %>%
  relocate(code,.after = "type") %>%
  relocate(title,.after = "code")

# Plot distribution of transformed weights
ggplot(all_destinations,aes(x = WT)) + 
  geom_density() + facet_wrap(~title, scales = "free") + ylab("Density") + xlab("Weight ^ α") +
  ggtitle("Destination weights with power transform α", subtitle = "Kernel density estimations")

# save plot
ggsave("../melbourne/preprocessing/poi/final/all_destinations_density_plot.png", width = 10, height = 6)

# Write file with normalised weights for easier visualisation
norm_destinations <- all_destinations %>%
  group_by(type) %>% 
  mutate(WT_NORM = WT / max(WT)) %>% 
  ungroup()
  
write_csv(all_destinations,"../melbourne/preprocessing/poi/final/all_destinations.csv")
write_csv(norm_destinations,"../melbourne/preprocessing/poi/final/all_destinations_normalised_weights.csv")
