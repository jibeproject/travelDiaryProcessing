library(tidyverse)

# Left join with 0's in empty spaces
left_join0 <- function(x, y, fill = 0L){
  z <- left_join(x, y)
  tmp <- setdiff(names(z), names(x))
  z <- replace_na(z, setNames(as.list(rep(fill, length(tmp))), tmp))
  z
}

# Read trips file
trips <- foreign::read.spss("data/Manchester/Yrs 6,7,8 HouseholdPersonTrip Academic.sav", to.data.frame = T)

# Aggregate purposes
trips$purpose <- recode(trips$EndPurpose, 
                        `Home` = "home",
                        `Usual place of work` = "work",
                        `Work - Business, other` = "business",
                        `Moving people or goods in connection with employment` = "business",
                        `Education as pupil, student` = "education",
                        `Shopping Food` = "shop",
                        `Shopping Non food` = "shop",
                        `Social - Entertainment, recreation, Participate in sport, pub, restaurant` = "recreation",
                        `Tourism, sightseeing` = "recreation",
                        `Escorting to place of work, pick-up, drop-off` = "escort",
                        `Escorting to place of education, pick-up, drop-off` = "escort",
                        `Childcare  taking or collecting child to or from babysitter, nursery etc` = "escort",
                        `Accompanying or giving lift to other person, not school, or work` = "escort",
                        `Visit friends or relatives` = "other",
                        `Use Services, Personal Business, bank, hairdresser, library etc` = "other",
                        `Health or medical visit` = "other",
                        `Worship or religious observance` = "other",
                        `Unpaid, voluntary work` = "other",
                        `Staying at hotel or other temporary accommodation` = "other",
                        `Round trip walk, cycle, drive for enjoyment` = "rrt")

# Check totals
test <- trips %>% count(EndPurpose, purpose) %>% arrange(desc(n))

# Remove home/work/business destinations
trips <- filter(trips, purpose != "work" & purpose != "home" & purpose != "education" & purpose != "business")

# Get Attraction of each OA
rawAttractions <- trips %>%
  count(EndOutputArea,EndPurpose) %>%
  pivot_wider(names_from = EndPurpose,
              values_from = n) %>%
  filter(EndOutputArea != "         ")

zoneAttractions <- trips %>%
  count(EndOutputArea,purpose) %>%
  pivot_wider(names_from = purpose,
              values_from = n) %>%
  filter(EndOutputArea != "         ")

attractions <- full_join(rawAttractions,zoneAttractions)

# Attach attractions to OAs

all_OAs <- sf::st_read("~/Documents/TfGM/destinationChoice/gm_oa.gpkg")
OA_lookup <- readr::read_csv(paste0("data/Manchester/OA_lookup.csv"), col_select = c("OA11CD","LAD17CD","LAD17NM"))


gm_districts <- c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")
gm_oa_list <- OA_lookup %>% filter(LAD17NM %in% gm_districts) %>% pull(OA11CD)

gm_OAs <- all_OAs %>% 
  filter(geo_code %in% gm_oa_list) %>%
  left_join(attractions, by = c("geo_code" = "EndOutputArea")) %>%
  mutate(across(where(is.integer),function(x) replace_na(x,0)))

sf::st_write(gm_OAs,"result/attractions.gpkg",delete_layer = TRUE)