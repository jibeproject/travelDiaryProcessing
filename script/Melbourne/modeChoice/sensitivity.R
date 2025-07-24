### Sensitivity to cases ##
library(tidyverse)
modeColoursL <- RColorBrewer::brewer.pal(12,"Set3")[c(5,7,10,6,4,9)]


base <- readr::read_csv("../../Documents/melbourne/estimation/results/HBD/HBD_dynamic8p.csv") %>% mutate(SCEN = "base")
ld <- readr::read_csv("../../Documents/melbourne/estimation/results/HBD/HBD_dynamic8p_ld.csv") %>% mutate(SCEN = "latentDemand")
pt <- readr::read_csv("../../Documents/melbourne/estimation/results/HBD/HBD_dynamic8p_pt.csv") %>% mutate(SCEN = "ptEquity")


plot_data <- rbind(base,ld,pt) %>%
  select(carD,carP,pt,bike,walk,SCEN) %>%
  pivot_longer(cols = c(carD,carP,pt,bike,walk)) %>%
  mutate(SCEN = factor(SCEN,levels = rev(c("base","latentDemand","ptEquity"))),
         name = factor(name,levels = rev(c("carD","carP","pt","bike","walk")))) %>%
  group_by(name,SCEN) %>%
  summarise(p = mean(value))


ggplot(plot_data,aes(x = SCEN, y = p, fill = name)) + geom_bar(stat = "identity", position = "stack", alpha = 0.5) + 
  coord_flip() + theme_minimal() + scale_fill_manual(values = modeColoursL[1:5]) + 
  labs(fill = "Selected mode") + guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(data = subset(plot_data, p > 0.005), aes(label = scales::label_percent(accuracy = 0.01)(p)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 3) + 
  xlab("scenario") + ylab("modal share of trips") + ggtitle("Mode share comparison (all trips)")



sum(base$bike) / nrow(base)
