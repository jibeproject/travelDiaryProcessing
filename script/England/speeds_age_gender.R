# Travel times
library(tidyverse)

# Overall mean in the survey
NTS_walk_trips <- filter(NTS$trips,t.mode == "walk",!t.badSpeed,t.distance * 1.60934 <= 10)
NTS_bike_trips <- filter(NTS$trips,t.mode == "cycle",!t.badSpeed,t.distance * 1.60934 <= 23, t.full_purpose != "J")

# Population averages for age 5+ (not important)
NTS_walk_mean <- NTS_walk_trips %>%
  left_join(NTS$persons) %>%
  filter(p.age_gr2 >= 4) %>%
  with(weighted.mean(t.speed,t.weight))

NTS_bike_mean <- NTS_bike_trips %>%
  left_join(NTS$persons) %>%
  filter(p.age_gr2 >= 4) %>%
  with(weighted.mean(t.speed,t.weight))

# Modelled data using Tobler's hiking function (walk) and Ziemke 2017 (bike)
modelled_walk <- readr::read_csv("data/Manchester/routed/allModes.csv",na = c("","null")) %>%
  filter(Mode == "walk",
         Route == "walk_fast",
         OriginWithinBoundary,
         DestinationWithinBoundary,
         !SameOrigAndDest,!(time == 0)) %>%
  transmute(hh.id = IDNumber,
            p.id = PersonNumber,
            t.id = TripNumber,
            dist,time,
            speed = dist / time)

modelled_bike <- readr::read_csv("data/Manchester/routed/allModes.csv",na = c("","null")) %>%
  filter(Mode == "bike",
         Route == "bike_fast",
         OriginWithinBoundary,
         DestinationWithinBoundary,
         !SameOrigAndDest,!(time == 0)) %>%
  transmute(hh.id = IDNumber,
            p.id = PersonNumber,
            t.id = TripNumber,
            dist,time,
            speed = dist / time)

MOD_walk_mean = mean(modelled_walk$speed) * 3.6
MOD_bike_mean = mean(modelled_bike$speed) * 3.6

# Key factors
WALK_MAX_RATIO <- 6 / MOD_walk_mean
BIKE_MAX_RATIO <- 19.8 / MOD_bike_mean

# WALK - differentiated by age and sex
walks <- NTS_walk_trips %>%
    left_join(NTS$persons) %>%
  mutate(p.age_gr3 = case_match(p.age_gr2,
                                1 ~ "0-4",
                                2 ~ "0-4",
                                3 ~ "0-4",
                                4 ~ "5-10",
                                5 ~ "11-15",
                                6 ~ "16-17",
                                7 ~ "16-17",
                                8 ~ "18-20",
                                9 ~ "18-20",
                                10 ~ "18-20",
                                11 ~ "21-29",
                                12 ~ "21-29",
                                13 ~ "30-39",
                                14 ~ "40-49",
                                15 ~ "50-59",
                                16 ~ "60-64",
                                17 ~ "65-69",
                                18 ~ "70-74",
                                19 ~ "75-79",
                                20 ~ "80-84",
                                21 ~ "85-105"),
         p.age_gr3 = factor(p.age_gr3, levels = c("0-4","5-10","11-15","16-17",
                            "18-20","21-29","30-39","40-49","50-59",
                            "60-64","65-69","70-74","75-79","80-84","85-105"))) %>%
  group_by(p.age_gr3,p.female) %>%
  summarise(n = n(),
            sumwt = sum(t.weight),
            speed = weighted.mean(t.speed,t.weight)) %>%
  mutate(new_max = speed * WALK_MAX_RATIO,
         mode = "walk")


# BIKE - differentiated by age and sex
cycles <- NTS_bike_trips %>%
  left_join(NTS$persons) %>%
  mutate(p.age_gr3 = case_match(p.age_gr2,
                                1 ~ "0-10",
                                2 ~ "0-10",
                                3 ~ "0-10",
                                4 ~ "0-10",
                                5 ~ "11-15",
                                6 ~ "16-17",
                                7 ~ "16-17",
                                8 ~ "18-20",
                                9 ~ "18-20",
                                10 ~ "18-20",
                                11 ~ "21-29",
                                12 ~ "21-29",
                                13 ~ "30-39",
                                14 ~ "40-49",
                                15 ~ "50-59",
                                16 ~ "60-64",
                                17 ~ "65-74",
                                18 ~ "65-74",
                                19 ~ "75-105",
                                20 ~ "75-105",
                                21 ~ "75-105"),
         p.age_gr3 = factor(p.age_gr3, levels = c("0-10","11-15","16-17",
                                                  "18-20","21-29","30-39","40-49","50-59",
                                                  "60-64","65-74","75-105"))) %>%
  group_by(p.age_gr3,p.female) %>%
  summarise(n = n(),
            sumwt = sum(t.weight),
            speed = weighted.mean(t.speed,t.weight)) %>%
  mutate(new_max = speed * BIKE_MAX_RATIO,
         mode = "bicycle")

# Get factor
test <- rbind(walks,cycles)

# Create combined data frame
combined <- rbind(walks,cycles) %>%
  mutate(age_min = as.integer(sub("-.*","",p.age_gr3)),
         age_max = as.integer(sub(".*-","",p.age_gr3))) %>%
  group_by(mode,age_min,age_max,p.female) %>%
  slice(rep(1:n(), times = 1 + age_max - age_min)) %>%
  mutate(age = age_min + 1:n() - 1) %>%
  ungroup() %>%
  transmute(age,
            sex = ifelse(p.female,"FEMALE","MALE"),
            mode,
            speed,
            region = "England")

out <- combined %>%
  transmute(age, sex, mode, speed = new_max / 3.6)

write_csv(out,"result/England/maxSpeeds.csv")

# DENSITY PLOTS
test <- filter(combined, mode == "bicycle", sex == "FEMALE")

d <- smooth.spline(x = test$age, y = test$new_max)

test <- combined %>%
  group_by(sex,mode) %>%
  mutate(d = density(x = age, wt = new_max)[["y"]])

# Average walking speeds
ggplot(walks, aes(x = p.age_gr3, y = mean_speed,label = round(mean_speed,1))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~p.sex) + 
  geom_text(vjust = -0.5,
            size = 3) +
  ggtitle("Average walking speeds in National Travel Survey") + xlab("age group") + 
  theme(axis.text.x = element_text(angle = -70, vjust = 0.5, hjust=0))

# Adjusted maximum walking speed
ggplot(walks, aes(x = p.age_gr3, y = new_max,label = round(new_max,1))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~p.sex) + 
  geom_text(vjust = -0.5,
            size = 3) +
  ggtitle("Adjusted maximum walking speed for Tobler's hiking function") + xlab("age group") + 
  theme(axis.text.x = element_text(angle = -70, vjust = 0.5, hjust=0))

# Average cycling speeds
ggplot(cycles, aes(x = p.age_gr3, y = mean_speed,label = round(mean_speed,1))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~p.sex) + 
  geom_text(vjust = -0.5,
            size = 3) +
  ggtitle("Average cycling speeds in National Travel Survey") + xlab("age group") + 
  theme(axis.text.x = element_text(angle = -70, vjust = 0.5, hjust=0))

# Adjusted maximum walking speed
ggplot(cycles, aes(x = p.age_gr3, y = new_max,label = round(new_max,1))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~p.sex) + 
  geom_text(vjust = -0.5,
            size = 3) +
  ggtitle("Adjusted maximum cycling speed for MATSim bicycle extension") + xlab("age group") + 
  theme(axis.text.x = element_text(angle = -70, vjust = 0.5, hjust=0))

with(walks,weighted.mean(mean_speed_moving,sumwt))
with(walks,weighted.mean(mean_speed,sumwt))


summary(walks$mean_speed2)
