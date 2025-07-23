library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)    

## Simulation outputs ----------------------------------------------------------
sim_trips <- fread("../melbourne/NoCalibrarion/scenOutput/base/2018/microData/trips.csv")       

# Clean simulation data
sim_clean <- sim_trips %>%
  mutate(
    scenario = "Simulation",
    mode = case_when(
      mode == "autoDriver"     ~ "Driving Car",
      mode == "autoPassenger"  ~ "Car Passenger",
      mode == "pt"             ~ "Public Transport",
      mode == "walk"           ~ "Walking",
      mode == "bicycle"        ~ "Cycling",
      TRUE                     ~ "Other"
    ),
    dist = case_when(mode=="Cycling"~t.distance_bike,
                     mode=="Walking"~t.distance_walk,
                     mode=="Public Transport"~t.distance_auto,
                     mode=="Driving Car"~t.distance_auto,
                     mode=="Car Passenger"~t.distance_auto),
  ) %>%
  select(mode, dist, scenario)

## VISTA data ------------------------------------------------------------------
vista_trips <- fread("./data/Melbourne/raw/T_VISTA_1220_Coord.csv") 

# Filter on trips in Greater Melbourne 
vista_trips_GM <- vista_trips %>%
  mutate(scenario = "VISTA") %>% 
  filter(!!sym("origsa4_name") %in% c("Melbourne - Inner", "Melbourne - Inner East",
                                   "Melbourne - Inner South", "Melbourne - North East",
                                   "Melbourne - North West", "Melbourne - Outer East",
                                   "Melbourne - South East", "Melbourne - West",
                                   "Mornington Peninsula") &
           !!sym("destsa4_name") %in% c("Melbourne - Inner", "Melbourne - Inner East",
                                          "Melbourne - Inner South", "Melbourne - North East",
                                          "Melbourne - North West", "Melbourne - Outer East",
                                          "Melbourne - South East", "Melbourne - West",
                                          "Mornington Peninsula"))

# Assign trip purpose
vista_trip_purpose <- vista_trips_GM %>%
  mutate(origplace1 = ifelse((is.na(origplace1) | str_detect(origplace1, "Unknown")) & 
                               origpurp1 == "Work Related",
                             "Workplace", 
                             origplace1),
         destplace1 = ifelse((is.na(destplace1) | str_detect(destplace1, "Unknown")) & 
                               destpurp1 == "Work Related",
                             "Workplace", 
                             destplace1),
         origpurp1 = ifelse((is.na(origpurp1) | str_detect(origpurp1, "Unknown")) & 
                              origplace1 == "Workplace",
                            "Work Related", 
                            origpurp1),
         destpurp1 = ifelse((is.na(destpurp1) | str_detect(destpurp1, "Unknown")) & 
                              destplace1 == "Workplace",
                            "Work Related", 
                            destpurp1))

vista_trip_purpose <- vista_trip_purpose %>%
  mutate(purpose = case_when(
    origpurp2 == "Employer's Business" | destpurp2 == "Employer's Business" ~ "business",
    origpurp1 %in% c("Unknown Purpose (at start of day)", "Not Stated") | 
      destpurp1 %in% c("NA", "Not Stated")                ~ "unknown",
    origpurp1 == "At Home" & destpurp1 == "At or Go Home" ~ "RRT",
    # trips from home are HBx, with 'x' based on destination
    origpurp1 == "At Home" ~ case_when(
      destpurp1 == "Work Related"                         ~ "HBW",
      destpurp1 == "Education"                            ~ "HBE",
      destpurp1 == "Buy Something"                        ~ "HBS",
      destpurp1 == "Recreational"                         ~ "HBR",
      destpurp1 == "Other Purpose"                        ~ "HBO",
      destpurp1 %in% c("Accompany Someone","Pick-up or Drop-off Someone") ~ "HBA",
      # classify remaining trips from home using place information if clearer than purpose
      destplace1 == "Workplace"                           ~ "HBW",
      destplace1 == "Place of Education"                  ~ "HBE",
      destplace1 == "Shops"                               ~ "HBS",
      destplace1 %in% c("Recreational Place","Natural Feature", "Social Place") ~ "HBR",
      # all other trips from home are HBO
      TRUE                                                ~ "HBO"
    ),
    # trips to home are NA
    destpurp1 == "At or Go Home"                          ~ "NA",
    origpurp1 == "Work Related" | destpurp1 == "Work Related" ~ "NHBW",
    # default for trips that are neither from home nor to home 
    TRUE                                                  ~ "NHBO"
  ))

vista_trip_clean <- vista_trip_purpose %>% 
  filter(!purpose%in%c("NA","business","unknown"))%>%
  mutate(
    mode = case_when(
      linkmode == "Vehicle Driver"                 ~ "Driving Car",
      linkmode == "Vehicle Passenger"              ~ "Car Passenger",
      linkmode %in% c("Public Bus", "School Bus", 
                      "Train", "Tram")             ~ "Public Transport",
      linkmode == "Walking"                        ~ "Walking",
      linkmode == "Bicycle"                        ~ "Cycling",
      TRUE                                         ~ "Other"),
    dist = cumdist) %>% 
  filter(!mode %in% c("Other"))%>%
  select(mode, dist, scenario)

## Visualise travel distance by mode -------------------------------------------
all_trips <- bind_rows(
  sim_clean,
  vista_trip_clean
)

all_trips <- all_trips %>% 
  mutate(dist= dist * 1000)

x_breaks <- c(50, 250, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
y_breaks <- c(0.00, 0.25, 0.50, 0.75, 1) 

plot <- ggplot(all_trips, aes(x = dist, colour = scenario)) +
  geom_density(
    aes(y = ..density..),
    size   = 0.5,
    adjust = 0.7,
    fill   = NA
  ) +
  facet_wrap(~ mode, ncol = 1) +
  scale_x_continuous(
    trans   = "log2",
    name    = "Distance (meters, log2sacale)",
    breaks  = x_breaks,
    labels  = label_comma(),         
    limits  = range(x_breaks),
    expand  = c(0, 0)               
  ) +
  scale_y_continuous(
    name = "Density",
    breaks  = y_breaks,
    limits  = range(y_breaks)  
    ) +
  labs(colour = "Scenario") +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing   = unit(1, "lines"),
    legend.position = "bottom"
  )

ggsave(
  filename = "plotDistance.png",
       plot = plot,
       width = 8,
       height = 12, 
       bg = "white"
  )

## Visualise mode share --------------------------------------------------------
mode_shares <- all_trips %>%
  count(scenario, mode) %>%
  group_by(scenario) %>%
  mutate(share = (n/ sum(n)) * 100) %>%
  ungroup()

plot <- ggplot(mode_shares, aes(x = mode, y = share, fill = scenario)) +
  geom_col(
    position = position_dodge(width = 0.8), 
    width = 0.7
    ) +
  geom_text(
    aes(label = sprintf("%.1f", share)),
    position = position_dodge(width = 0.8),
    vjust    = -0.3,
    size     = 3
  ) +
  scale_y_continuous(
    name   = "Mode share (%)",
    labels = label_number(accuracy = 0.1),
  ) +
  labs(
    x      = "Mode",
    fill   = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  filename = "plotShare.png",
  plot = plot,
  width = 8,
  height = 8, 
  bg = "white"
)


