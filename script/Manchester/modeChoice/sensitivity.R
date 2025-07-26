##### MODE CHOICE SENSITIVITY TESTING FOR PUBLICATION ######
### Sensitivity to cases ##
library(tidyverse)
modeColoursL <- RColorBrewer::brewer.pal(12,"Set3")[c(5,7,10,6,4,9)]

HBD <-list()
HBD$base <- readr::read_csv("result/local/Manchester/modeChoicePrediction/discretionary/dynamic7new2_p.csv")
HBD$lowStress <- readr::read_csv("result/local/Manchester/modeChoicePrediction/discretionary/dynamic7new2_p_ls.csv")
HBD$green <- readr::read_csv("result/local/Manchester/modeChoicePrediction/discretionary/dynamic7new2_p_gn.csv")
HBD$both <- readr::read_csv("result/local/Manchester/modeChoicePrediction/discretionary/dynamic7new2_p_both.csv")
HBD <- bind_rows(HBD,.id = "SCEN") %>% mutate(purpose = "discretionary")


HBW <- list()
HBW$base <- readr::read_csv("result/local/Manchester/modeChoicePrediction/work/dynamic10new3_p.csv")
HBW$lowStress <- readr::read_csv("result/local/Manchester/modeChoicePrediction/work/dynamic10new3_p_ls.csv")
HBW$green <- readr::read_csv("result/local/Manchester/modeChoicePrediction/work/dynamic10new3_p_gn.csv")
HBW$both <- readr::read_csv("result/local/Manchester/modeChoicePrediction/work/dynamic10new3_p_both.csv")
HBW <- bind_rows(HBW,.id = "SCEN") %>% mutate(purpose = "work")


plot_data <- rbind(HBD,HBW) %>%
  select(carD,carP,pt,bike,walk,SCEN,purpose) %>%
  pivot_longer(cols = c(carD,carP,pt,bike,walk)) %>%
  mutate(SCEN = factor(SCEN,levels = rev(c("base","lowStress","green","both"))),
         purpose = factor(purpose, levels = c("work","discretionary")),
         name = factor(name,levels = rev(c("carD","carP","pt","bike","walk")))) %>%
  group_by(name,purpose,SCEN) %>%
  summarise(p = mean(value))

ggplot(plot_data,aes(x = SCEN, y = p, fill = name)) + geom_bar(stat = "identity", position = "stack", alpha = 0.5) + 
  coord_flip() + theme_minimal() + scale_fill_manual(values = modeColoursL[1:5]) + 
  labs(fill = "Selected mode") + guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(data = subset(plot_data, p > 0.005), aes(label = scales::label_percent(accuracy = 0.1)(p)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 3) + facet_wrap(~purpose,ncol = 1) +
  xlab("scenario") + ylab("modal share of trips") + ggtitle("Mode share comparison (Manchester)")
