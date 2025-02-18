## Mode set analysis ##
library(tidyverse)
TRADS <- readRDS("data/Manchester/processed/TRADS.rds")
persons <- TRADS$indiv
trips <- TRADS$trips %>% 
  mutate(t.mode = recode_factor(t.m_main, 
                                `Walk` = "walk",
                                `Bicycle` = "bike",
                                `Car or van driver` = "car",
                                `Car or van passenger` = "car",
                                `Train` = "pt",
                                `Metrolink` = "pt",
                                `Bus, minibus, coach` = "pt",
                                .default = "other"))

# Summary data
summary(factor(persons$p.freq_walk))
summary(factor(persons$p.freq_bike))
summary(factor(persons$p.freq_bus))
summary(factor(persons$p.freq_metro))
summary(factor(persons$p.freq_train))
summary(factor(persons$p.freq_car))

recorded_data <- trips %>% 
  filter(t.mode != "other") %>%
  mutate(t.mode = factor(t.mode)) %>%
  group_by(hh.id,p.id,t.mode, .drop = FALSE) %>%
  summarise(x = n() > 0)

### Plot frequency by each mode type
freq_data <- persons %>%
  select(hh.id, p.id, p.weight, starts_with("p.freq_")) %>% 
  mutate(across(starts_with("p.freq_"), ~ ordered(.x, levels = rev(c("Never used",
                                                                "Not in the last 12 months",
                                                                "At least once a year",
                                                                "At least once a month",
                                                                "At least once a fortnight",
                                                                "1 day a week",
                                                                "2 days a week",
                                                                "3 or 4 days a week",
                                                                "5 or more days a week"))))) %>%
  rename_with(~ gsub("^p.freq_","",.x)) %>% 
  mutate(pt = pmin(train,metro,bus))

tot_wt = sum(freq_data$p.weight)

plot_data <- freq_data %>%
  select(-c(bus,metro,train,other)) %>%
  pivot_longer(cols = c(car,pt,bike,walk)) %>%
  group_by(name,value) %>%
  tally(wt = p.weight) %>%
  mutate(p = cumsum(n) / tot_wt) %>%
  ungroup() %>%
  filter(value < "Not in the last 12 months")

plot_data2 <- recorded_data %>%
  inner_join(select(freq_data,hh.id,p.id,p.weight)) %>%
  group_by(t.mode) %>%
  summarise(n = sum(p.weight * x)/tot_wt)
  
ggplot(plot_data, aes(x = value, y = p, colour = name)) + 
  geom_point() + geom_line(aes(group = name)) + coord_flip() + ylim(0,1) + 
  xlab("frequency") + ylab("proportion of population") + 
  geom_hline(data = plot_data2, aes(yintercept = n, colour = t.mode), linetype = "dashed") + 
  ggtitle("Stated mode use frequency (Manchester TRADS survey)", subtitle = "vs. recorded mode use (vertical lines)")

### Plot stated vs. recorded (binary)
plot_data <- recorded_data %>%
  full_join(select(freq_data,hh.id,p.id,p.weight,car,pt,bike,walk)) %>%
  pivot_longer(cols = c(car,pt,bike,walk)) %>%
  filter(is.na(t.mode) | t.mode == name) %>%
  mutate(t.mode = as.character(t.mode),
         t.mode = ifelse(is.na(t.mode),name,t.mode),
         x = ifelse(is.na(x),F,x)) %>%
  group_by(value,t.mode) %>%
  summarise(n = sum(p.weight * (x == T)) / sum(p.weight))

ggplot(plot_data, aes(x = value, y = n, colour = t.mode)) + 
  geom_point() + geom_line(aes(group = t.mode)) + coord_flip() + ylim(0,1) + 
  xlab("frequency") + ylab("proportion of individuals who stated frequency AND recorded mode in diary") + 
  ggtitle("Stated frequency vs. recorded mode use")

### Plot stated vs. recorded (share)
plot_data <- trips %>% 
  mutate(t.mode = factor(t.mode)) %>%
  group_by(hh.id,p.id,t.mode, .drop = FALSE) %>%
  tally() %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  left_join(select(freq_data,hh.id,p.id,p.weight,car,pt,bike,walk)) %>%
  pivot_longer(cols = c(car,pt,bike,walk)) %>%
  filter(t.mode == name) %>%
  mutate(t.mode = as.character(t.mode)) %>%
  group_by(value,t.mode) %>%
  summarise(n = sum(p * p.weight) / sum(p.weight))

ggplot(plot_data, aes(x = value, y = n, colour = t.mode)) + 
  geom_point() + geom_line(aes(group = t.mode)) + coord_flip() + ylim(0,1) + 
  xlab("frequency") + ylab("proportion of individuals who stated frequency AND recorded mode share in diary") + 
  ggtitle("Stated frequency vs. recorded mode share")
  

### Plot correlations w/ observed frequencies
# Walk vs. Pt (use same template for others)
plot_data <- freq_data %>%
  select(hh.id,p.id,p.weight,walk,pt) %>%
  group_by(walk,pt) %>%
  tally(wt = p.weight) %>%
  mutate(p = cumsum(n) / sum(n)) %>%
  ungroup() %>%
  filter(walk < "Not in the last 12 months", pt < "Not in the last 12 months")

ggplot(plot_data, aes(x = pt, y = p, colour = walk)) + 
  geom_point() + geom_line(aes(group = walk)) + coord_flip() + ylim(0,1) +
  xlab("pt frequency") + ylab("proportion of population") + 
  labs(colour = "walk frequency") +
  ggtitle("Stated mode use frequency (Manchester TRADS survey)")


