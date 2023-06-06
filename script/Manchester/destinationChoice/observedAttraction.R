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

# Add MSOA/LSOA
OA_lookup <- readr::read_csv(paste0("data/Manchester/OA_lookup.csv"), col_select = c("OA11CD","LSOA11CD","MSOA11CD"))

trips <- left_join(trips,OA_lookup,by = c("EndOutputArea" = "OA11CD")) %>% rename(EndMSOA = MSOA11CD, EndLSOA = LSOA11CD)

# Get Attraction of each OA

getAttractions <- function(regionType) {
  rawAttractions <- trips %>%
    count(!!enquo(regionType),EndPurpose) %>%
    pivot_wider(names_from = EndPurpose,
                values_from = n) %>%
    filter(!!enquo(regionType) != "         ")
  
  zoneAttractions <- trips %>%
    count(!!enquo(regionType),purpose) %>%
    pivot_wider(names_from = purpose,
                values_from = n) %>%
    filter(!!enquo(regionType) != "         ")
  
  return(full_join(rawAttractions,zoneAttractions))
}

attractions_OA <- getAttractions(EndOutputArea)
attractions_LSOA <- getAttractions(EndLSOA)
attractions_MSOA <- getAttractions(EndMSOA)



# Attach attractions to OAs

OAs <- sf::st_read("~/Documents/manchester/zones/gm_oa.shp")
LSOAs <- sf::st_read("~/Documents/manchester/zones/LSOA_studyArea.shp")
MSOAs <- sf::st_read("~/Documents/manchester/zones/MSOA_studyArea.shp")


attr_OAs <- OAs %>% select(-fid) %>%
  left_join(attractions_OA, by = c("OA11CD" = "EndOutputArea")) %>%
  mutate(across(where(is.integer),function(x) replace_na(x,0)))

attr_LSOAs <- LSOAs %>% 
  left_join(attractions_LSOA, by = c("LSOA11CD" = "EndLSOA")) %>%
  mutate(across(where(is.integer),function(x) replace_na(x,0)))

attr_MSOAs <- MSOAs %>% 
  left_join(attractions_MSOA, by = c("MSOA11CD" = "EndMSOA")) %>%
  mutate(across(where(is.integer),function(x) replace_na(x,0)))


# Write
sf::st_write(attr_OAs,"result/attractions_OA.gpkg",delete_layer = TRUE)
sf::st_write(attr_LSOAs,"result/attractions_LSOA.gpkg",delete_layer = TRUE)
sf::st_write(attr_MSOAs,"result/attractions_MSOA.gpkg",delete_layer = TRUE)

