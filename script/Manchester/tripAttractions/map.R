library(sf)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

########################################
#==# READ IN DATASETS #================#
########################################

MAN_FLAG<- TRUE

if(MAN_FLAG){
  # Manchester
  geospatial_data <-st_read("manchesterData/geospatialData.gpkg")
  poi_data <- st_read("manchesterData/allPois.gpkg")
  trip_data<-read_csv("manchesterData/trips.csv")
  hh.lsoa2021<-read_csv("secondaryData/hh_lsoa_2021census.csv")
  #hh.oa2021<-read_csv("secondaryData/hh_oa_2021census.csv")
  #dwellings2023 <-read_excel("secondaryData/os_addressBase_RD_2023_aggregate2OA.xlsx")
  dwellings2023 <-read_csv("secondaryData/os_addressBase_RD_2023_aggregate2LSOA.csv")
}else{
  # Melbourne
  geospatial_data <-st_read("melbourneData/geospatialData.gpkg")
  poi_data <- st_read("melbourneData/allPois.gpkg")
  trip_data<-read_csv("melbourneData/trips.csv")
  hh.sa1<-read_csv("melbourneData/hh_sa1.csv")
  parks<-read_csv("melbourneData/parksAggregatedSA1.csv")
}

########################################
#==# TRANSFORM DATA #==================#
########################################

# indicator-based
poi_data_wide<- poi_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(IDspatial, Indicator, weight) %>% 
  pivot_wider(names_from = "Indicator", values_from= "weight", values_fn = sum, values_fill = 0)


if(MAN_FLAG){
  ## add househol info for Manchester
  hh_data<-hh.lsoa2021 %>%
    mutate(IDspatial= lsoa21cd) %>% mutate(HH= hh) %>%
    dplyr::select(IDspatial, HH) # counts or density 
    #pivot_wider(names_from = "purpose", values_from= "purpose", values_fn=length, values_fill = 0)
  
  ## add dwelling info for Manchester
  dw_data<-dwellings2023 %>%
    mutate(IDspatial= LSOA21CD) %>% mutate(DW= building_counts) %>%
    dplyr::select(IDspatial, DW) # counts or density 
  #pivot_wider(names_from = "purpose", values_from= "purpose", values_fn=length, values_fill = 0)
}else{
  # attribute-based (works for Melbourne only)
  poi_data_wide_spec<- poi_data %>% 
    st_drop_geometry() %>% 
    dplyr::select(IDspatial, Attribute, weight) %>% 
    pivot_wider(names_from = "Attribute", values_from= "weight", values_fn = sum, values_fill = 0)
  
  ## add househol info for Melbourne
  hh_data<-hh.sa1 %>%
    mutate(IDspatial= SA1_MAINCODE_2016) %>% mutate(HH= household) %>% #SA1_MAINCODE_2016 
    dplyr::select(IDspatial, HH) # counts or density
  # pivot_wider(names_from = "purpose", values_from= "purpose", values_fn=length, values_fill = 0)
  
  ## parks
  park_data_wide <- parks %>% dplyr::select(IDspatial, Indicator, weight) %>% pivot_wider(names_from = "Indicator", values_from= "weight", values_fn = sum, values_fill = 0)
  
}

trip_data_wide<-trip_data %>% 
  dplyr::select(IDspatial, purpose) %>% 
  pivot_wider(names_from = "purpose", values_from= "purpose", values_fn=length, values_fill = 0)

geoDataA<-left_join(geospatial_data, poi_data_wide, by = "IDspatial")

#if(!MAN_FLAG){
#  geoDataA<-left_join(geoDataA, poi_data_wide_spec, by = "IDspatial")
#}

# attach trip data, hh data, and parks (for Melbourne)
geoDataB<-left_join(geoDataA, trip_data_wide, by = "IDspatial")
geoDataB<-left_join(geoDataB, hh_data, by = "IDspatial")
geoDataB<-left_join(geoDataB, dw_data, by = "IDspatial")

if(!MAN_FLAG){
  geoDataB<-left_join(geoDataB, park_data_wide, by = "IDspatial")
}
# geoDataB$HH_density<- geoDataB$HH/(st_area(geoDataB)*1/1000000)
#geoDataB$HH_sqrt<- sqrt(geoDataB$HH)
geoDataB <- geoDataB %>% replace(is.na(.), 0)

########################################
#==# WRITE DATA #======================#
########################################

if(MAN_FLAG){
  parent_dir<- "manchesterData/"
}else{
  parent_dir<- "melbourneData/"
}

sf::st_write(geoDataB, paste0(parent_dir, "mappedAttractions.gpkg"), driver = "GPKG", append=FALSE)
agg1 <- geoDataB %>% 
  st_drop_geometry() %>% 
  # dplyr::select(-other, -recreation, -shop, -escort, -visit, -rrt)
  dplyr::select(-other, -recreation, -shop, -escort, -education, -work, -visit, -rrt, -DW, -HH)

# TODO: clean by removing non-relevant variables
write.csv(agg1,  file = paste0(parent_dir, "AggregatedPoiOA.csv"), quote = FALSE, row.names = FALSE)
