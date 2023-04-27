############# JUNK FROM BEFROE #############

POIs <- sf::st_read("~/Documents/TfGM/destinationChoice/gm_poi.gpkg")


man_attr <- OAs %>% sf::st_drop_geometry() %>% select(geo_code) %>% left_join(zoneAttractions, by = c("geo_code" = "EndOutputArea"))

test <- as.vector(man_attr[,2:6])
test <- sum(man_attr[,2:6],na.rm = TRUE)

test2 <- trips %>% filter(purpose != "work" & purpose != "home" & purpose != "education" & purpose != "business") %>% 
  filter(EndOutputArea %in% OAs$geo_code)


readr::write_csv(man_attr,"~/Documents/TfGM/destinationChoice/attractions.csv", na = "0")

test <- man_attr$geo_code[is.na(man_attr$shop)]
test2 <- OAs$geo_code




test3 <-
  POIs$laura_cats <- with(POIs,
                          case_when(pointx_class %in% c(01020034,09470671) ~ "Licenced",
                                    categoryname %in% c("Eating and Drinking","Food, Drink and Multi Item Retail") ~ "Foodstores",
                                    pointx_class %in% c(06340456,06340459) ~ "Community centre",
                                    pointx_class == 03170813 ~ "Art gallery",
                                    pointx_class == 04250308 ~ "Theatre",
                                    pointx_class == 06340458 ~ "Libraries",
                                    pointx_class == 05280809 ~ "Childcare",
                                    pointx_class == 05310375 ~ "Primary",
                                    pointx_class == 05310379 ~ "Secondary",
                                    pointx_class == 05280373 ~ "Aged care",
                                    pointx_class %in% c(05280367,05280368) ~ "Dentist",
                                    pointx_class %in% c(05280365,05280812) ~ "General Practitioner",
                                    pointx_class %in% c(05280342,05290362) ~ "Maternal",
                                    pointx_class %in% c(05280780,05280330,05280815,05280337,05280333,
                                                        05280340,05280369,05280370,05280371,05280372,
                                                        05280344,05280345,05280352,05280354,05290356,
                                                        05290357,05290358,05290359,05290106,05290363) ~ "Other Health",
                                    pointx_class == 05280364 ~ "Pharmacy",
                                    pointx_class == 04240304 ~ "Pools",
                                    pointx_class %in% c(04240289,04240290,04240291,04240292,04240293,
                                                        04240294,04240297,04240298,04240299,04240300,
                                                        04240301,04240302,04240303,04240305,04240306) ~ "Sport",
                                    categoryname %in% c("Clothing and Accessories", "Household",
                                                        "Household, Office, Leisure and Garden",
                                                        "office", "leisure and games", "Motoring") ~ "Shops",
                                    pointx_class %in% c(10590732,10570738,10570794,10570794,10570794) ~ "Public transport"))

test <- POIs %>%
  sf::st_drop_geometry() %>%
  count(laura_cats, groupname, categoryname, classname, pointx_class)


OAs <- sf::st_drop_geometry(OAs) %>% select(geo_code,POIs)




# Convert POI data from RDS to GPKG
gm_pcarea <- readRDS("~/Documents/manchester/JIBE/POI/gm_pcarea.Rds")
gm_poi <- readRDS("~/Documents/manchester/JIBE/POI/gm_poi.Rds")
pc_bounds <- readRDS("~/Documents/manchester/JIBE/POI/pc_bounds.Rds")
pc_cents <- readRDS("~/Documents/manchester/JIBE/POI/pc_cents.Rds")

sf::st_write(gm_pcarea,"~/Documents/manchester/JIBE/POI/gm_pcarea.gpkg","gm_pcarea")
sf::st_write(gm_poi,"~/Documents/manchester/JIBE/POI/gm_poi.gpkg","gm_poi")
sf::st_write(pc_bounds,"~/Documents/manchester/JIBE/POI/pc_bounds.gpkg","pc_bounds")
sf::st_write(pc_cents,"~/Documents/manchester/JIBE/POI/pc_cents.gpkg","pc_cents")
