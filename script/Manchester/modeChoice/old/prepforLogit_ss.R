## Prep for logit model ##
library(tidyverse)
raw <- readRDS("data/Manchester/processed/TRADS.rds")$raw
trips <- raw$trips %>% transmute(IDNumber,PersonNumber,TripNumber,
                                 isFemale = ifelse(Gender == "Female",1,0),
                              age = recode(Age,
                                           `5-9` = 7,
                                           `10-14` = 12,
                                           `15-19` = 17,
                                           `20-24` = 22,
                                           `25-29` = 27,
                                           `30-34` = 32,
                                           `35-39` = 37,
                                           `40-44` = 42,
                                           `45-49` = 47,
                                           `50-54` = 52,
                                           `55-59` = 57,
                                           `60-64` = 62,
                                           `65-69` = 67,
                                           `70-74` = 72,
                                           `75-79` = 77,
                                           `80-84` = 82,
                                           `85+` = 90),
                              ageUnder50 = ifelse(age < 50,1,0),
                              mode = case_when(MainMode == "Bicycle" ~ 1,
                                               MainMode == "Car or van driver" ~ 0,
                                               MainMode == "Car or van passenger" ~ 0)) %>%
  filter(!is.na(mode) & !is.na(age))

test <- trips %>% group_by(ageUnder50,mode) %>% tally() %>% mutate(p = n / sum(n))

processed <- readr::read_csv("~/Documents/manchester/TfGM/routes.csv", col_types = "ciilllllcccinnn") %>%
  select(-PathId,-cost,-time) %>%
  pivot_wider(names_from = "Route",values_from = "dist") %>% 
  filter(OriginWithinBoundary & DestinationWithinBoundary & !SameOrigAndDest)

logitData <- inner_join(trips,select(processed,IDNumber,PersonNumber,TripNumber,bike_short)) %>%
  select(-IDNumber)

write.table(logitData,file = "~/Documents/manchester/TfGM/logitData.csv", sep = ",", 
            row.names = FALSE, quote = FALSE)

