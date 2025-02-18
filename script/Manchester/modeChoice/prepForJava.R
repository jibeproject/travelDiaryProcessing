# ################################################################# #
####       CLEAR MEMORY AND LOAD LIBRARY                #############
# ################################################################# #
rm(list = ls())

library(tidyverse)
library(fastDummies)
library(corrplot)

# ################################################################# #
####      LOAD DATA AND APPLY ANY TRANSFORMATIONS       #############
# ################################################################# #


# Urban / rural
RUC <- readr::read_csv("../manchester/zones/RUC11_OA11_EW.csv") %>%
  transmute(hh.OA = OA11CD, hh.urban = startsWith(RUC11,"Urban"))


trips = readRDS("data/Manchester/processed/tripsForApollo.rds") %>%
  mutate(t.m_main_apollo = as.character(recode(t.m_main, `Walk` = "walk", `Bicycle` = "bike", `Motorcycle, scooter, moped` = "X",
                                               `Car or van driver` = "carD", `Car or van passenger` = "carP",
                                               `Bus, minibus, coach` = "pt", `Metrolink` = "pt",
                                               `Train` = "pt", `Taxi, minicab` = "X")),
         p.age_group_agg = recode(p.age_group, `5-9` = "5_14", `10-14` = "5_14", `15-19` = "15_24",
                                  `20-24` = "15_24", `25-29` = "25_39", `30-34` = "25_39", `35-39` = "25_39",
                                  `40-44` = "40_54", `45-49` = "40_54", `50-54` = "40_54", `55-59` = "55_69",
                                  `60-64` = "55_69", `65-69` = "55_69", `70-74` = "70", `75-79` = "70",
                                  `80-84` = "70", `85+` = "70",.default = "NA"),
         t.departureTime_6_20 = ifelse(t.departureTime>6*3600 & t.departureTime<20*3600,1,0),
         t.departureTime_6_22 = ifelse(t.departureTime>6*3600 & t.departureTime<22*3600,1,0),
         car_time = carTravelTime_sec/60,
         pt_time = ptTravelTime_sec/60) %>%
  filter(t.m_main_apollo!="X") %>%
  dummy_cols(select_columns = c("p.age_group_agg","p.female","p.occupation",
                                "hh.cars_gr","hh.income_agg",
                                "t.departureTime_gr","t.full_purpose")) %>%
  left_join(RUC) %>%
  mutate(choice = recode(t.m_main_apollo,"carD" = 0,"carP" = 1,"pt" = 2,"bike" = 3,"walk" = 4),
         hh.urban = ifelse(hh.urban,1,0),
         p.age_group_agg_5_24 = p.age_group_agg_5_14 + p.age_group_agg_15_24,
         p.age_group_agg_40_69 = p.age_group_agg_40_54 + p.age_group_agg_55_69,
         p.age_55up = p.age_group_agg_55_69 + p.age_group_agg_70,
         p.age_65up = ifelse(p.age_group == "65-69",1,0) + p.age_group_agg_70,
         av_carD = ifelse(p.age_group_agg_5_14 == 1,0,1),
         p.female = ifelse(p.age_group_agg_5_14 == 1,0,as.numeric(p.female)),
         hh.cars_gr_23 = hh.cars_gr_2 + hh.cars_gr_3) %>%
  select(p.ID,t.ID,choice,starts_with("p.age_group_agg_"),p.age_55up,p.age_65up,p.female,p.female_FALSE,
         starts_with("p.occupation_"),starts_with("hh.cars_gr"),starts_with("hh.income_agg_"),hh.urban,
         starts_with("t.full_purpose_"),starts_with("av_"),"dist","dist_walk",
         car_time,pt_time) %>%
  na.omit()

# Setup for Java
tripsForJavaHBA <- trips %>% filter(t.full_purpose_HBA == 1)
tripsForJavaHBW <- trips %>% filter(t.full_purpose_HBW == 1)
tripsForJavaHBE <- trips %>% filter(t.full_purpose_HBE == 1)
tripsForJavaHBD <- trips %>% filter(t.full_purpose_HBS == 1 | t.full_purpose_HBR == 1 | t.full_purpose_HBO == 1)
tripsForJavaNHBW <- trips %>% filter(t.full_purpose_NHBW == 1)
tripsForJavaNHBO <- trips %>% filter(t.full_purpose_NHBO == 1) %>%
  mutate(av_carD = ifelse(hh.cars_gr_0 == 1,0,av_carD))
tripsForJavaRRT <- trips %>% filter(t.full_purpose_RRT == 1, choice >= 3)

write_csv(tripsForJavaHBD,"data/Manchester/processed/dynamicLCP/discretionary.csv")
write_csv(tripsForJavaHBW,"data/Manchester/processed/dynamicLCP/work.csv")
write_csv(tripsForJavaHBE,"data/Manchester/processed/dynamicLCP/education.csv")
write_csv(tripsForJavaHBA,"data/Manchester/processed/dynamicLCP/accompany.csv")
write_csv(tripsForJavaNHBO,"data/Manchester/processed/dynamicLCP/nhbo.csv")
write_csv(tripsForJavaNHBW,"data/Manchester/processed/dynamicLCP/nhbw.csv")
saveRDS(tripsForJavaRRT,"data/Manchester/processed/rrt.Rds")

# ################################################################# #
  ##################      RELATED ANALYSIS       ##################
# ################################################################# #

# Quick RRT model
RRTdata <- tripsForJavaRRT %>%
  mutate(cycle = choice == 3,
         dist_s = dist^2)
RRT <- glm(cycle ~ dist_s + p.age_group_agg_5_24 + p.age_65up + p.female +
             hh.cars_gr_3 + hh.income_agg_high, data = RRTdata, family = "binomial")

summary(RRT)
# CHECK CORRELATIONS
tripsForJavaHBA %>% select(-t.ID,-choice,-starts_with("t.full_purpose"),-starts_with("av_"),-p.age_group_agg_NA) %>%
  cor %>% corrplot(type = "upper", tl.pos = "td", method = "circle", tl.cglm(cycleTrip ~ hh.urban + hh.size_2 + hh.size_3 + hh.size_45 +
                                                                               p.age_gr_1 + p.age_gr_56 +
                                                                               p.female + t.distance_T, data = RRT_data, family = "binomial")ol = 'black', diag = FALSE)

with(tripsForJavaHBA,table(choice,hh.cars_gr))

test <- tripsForJavaHBA %>% filter(hh.cars_gr == 1)
summary(factor(test$choice))

# GET USEFUL COUNT CATEGORIES
getDetails <- function(tripData,i = -1) {
  if(i > -1) {
    tripData <- filter(tripData,choice == i)
  }
  print(paste("TOTAL =",nrow(tripData)))
  print(paste("FEMALES =",sum(tripData$p.female)))
  print(paste("UNDER15 =",sum(tripData$p.age_group_agg_5_14)))
  print(paste("OVER65 =",sum(tripData$p.age_65up)))
  print(paste("DISABLED_LITTLE =",sum(tripData$p.disability_little)))
  print(paste("DISABLED_LOT =",sum(tripData$p.disability_lot)))
}

getDetails(tripsForJavaHBD,4)

# OVERALL MODE SHARE
plot_data <- trips %>%
  group_by(choice) %>%
  tally() %>%
  mutate(p = n / sum(n))

trips$distance_bracket <- cut(
  trips$dist/1000,
  breaks = c(0, 1, 3, 5, 10, 20, 40, Inf),
  labels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+"),
  right = FALSE)

distance <- trips %>%
  group_by(distance_bracket, choice) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(distance_bracket) %>%
  mutate(percent = count / sum(count) * 100)

ggplot(distance, aes(x = distance_bracket, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = ifelse(percent > 1, paste0(round(percent, 1), "%"), "")), # Show label only if >= 1%
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  labs(
    title = "Transport Mode Share by Trip Distance (Observed in TRADS)",
    y = "Proportion (%)",
    x = "Distance (km)",
    fill = "Transport Mode"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )



# GET ROUTE DATA
route_data_bike <- read_csv("../manchester/estimation/StaticRouteData_bike.csv") %>%
  select(-ends_with("_c"),-ends_with("_f")) %>%
  mutate(across(starts_with("g_bike_"),function(x) x/.[["bike_time"]])) %>%
  rename_with(~str_remove(.,"g_bike_"))

route_data_walk <- read_csv("../manchester/estimation/StaticRouteData_walk.csv") %>%
  select(-ends_with("_c"),-ends_with("_f")) %>%
  mutate(across(starts_with("g_walk_"),function(x) x/.[["walk_time"]])) %>%
  rename_with(~str_remove(.,"g_walk_"))

# Correlation plots
route_data_bike %>% select(-ID) %>% cor %>% corrplot(type = "upper", tl.pos = "td", method = "circle", tl.col = 'black', diag = FALSE)
route_data_walk %>% select(-ID) %>% cor %>% corrplot(type = "upper", tl.pos = "td", method = "circle", tl.col = 'black', diag = FALSE)

######## PLOTS FOR COMPARISON ########
modeColoursL <- RColorBrewer::brewer.pal(12,"Set3")[c(5,7,10,6,4,9)]

# HBW and gender
plot_data <- tripsForJavaHBW %>%
  mutate(mode = factor(case_match(choice,0 ~ "autoDriver",1 ~ "autoPassenger",2 ~ "pt",3 ~ "bicycle",4 ~ "walk"),
                       levels = rev(c("autoDriver","autoPassenger","pt","bicycle","walk"))),
         gender = if_else(p.female == 1,"female","male")) %>%
  group_by(gender,mode) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n))

ggplot(plot_data,aes(x = gender, y = p, fill = mode)) + geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  coord_flip() + theme_minimal() + scale_fill_manual(values = modeColoursL[1:5]) +
  labs(fill = "Main mode") + guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(data = subset(plot_data, p > 0.005), aes(label = scales::label_percent(accuracy = 0.1)(p)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 2) +
  xlab("Model structure") + ylab("modal share of trips") + ggtitle("Observed modal split comparison (trip share), by gender",
                                                                   subtitle = "Home-based work trips")

# HBD and age
plot_data <- tripsForJavaHBD %>%
  mutate(mode = factor(case_match(choice,0 ~ "autoDriver",1 ~ "autoPassenger",2 ~ "pt",3 ~ "bicycle",4 ~ "walk"),
                       levels = rev(c("autoDriver","autoPassenger","pt","bicycle","walk"))),
         age = factor(case_when(p.age_group_agg_5_14 == 1 ~ "child",
                                p.age_65up == 1 ~ "elderly",
                                TRUE ~ "adult"),
                      levels = rev(c("child","adult","elderly")))) %>%
  group_by(age,mode) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n))

ggplot(plot_data,aes(x = age, y = p, fill = mode)) + geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  coord_flip() + theme_minimal() + scale_fill_manual(values = modeColoursL[1:5]) +
  labs(fill = "Main mode") + guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(data = subset(plot_data, p > 0.005), aes(label = scales::label_percent(accuracy = 0.1)(p)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 2) +
  xlab("Model structure") + ylab("modal share of trips") + ggtitle("Observed modal split comparison (trip share), by age",
