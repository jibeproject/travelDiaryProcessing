library(tidyverse)
library(fastDummies)


### Clear memory ###
rm(list = ls())


### load data ###
trips = read_csv("data/Manchester/processed/tripsForApollo.csv")

trips = trips%>%left_join(read_csv("data/Manchester/processed/tripsWithXY.csv")%>%
                            select(IDNumber, PersonNumber,TripNumber, StartOutputArea),
                          by=c("hh.id"="IDNumber", "p.id"="PersonNumber","t.id"="TripNumber"))

trips = trips%>%left_join(read_csv("F:/models/jibe/manchester/zones/Output_Area_to_Lower_layer_Super_Output_Area_to_Middle_layer_Super_Output_Area_to_Local_Authority_District_(December_2011)_Lookup_in_England_and_Wales.csv")%>%
                            select(OA11CD, LAD11CD,LAD11NM),
                          by=c("StartOutputArea"="OA11CD"))

### mode share by purpose by LAD ###
region=c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")

trips = trips%>%mutate(t.m_main_agg5 = case_when(t.m_main == "Car or van passenger"~ "autoPassenger",
                                                 t.m_main == "Car or van driver"~ "autoDriver",
                                                 t.m_main_agg == "Pt"~ "pt",
                                                 t.m_main_agg == "Walk"~ "walk",
                                                 t.m_main_agg == "Bike"~ "bicycle"))
modeShare = trips%>%
  filter(LAD11NM%in%region)%>%
  filter(!is.na(t.purpose))%>%
  filter(!is.na(t.m_main_agg5))%>%
  group_by(LAD11NM,t.purpose,t.m_main_agg5)%>%
  summarise(n=n())%>%
  mutate(share=n/sum(n))

ggplot(modeShare)+
  geom_bar(aes(x=LAD11NM,y=share,group=t.m_main_agg5,fill=t.m_main_agg5),stat="identity",position="fill")+
  facet_wrap(.~t.purpose,nrow  = 4)

modeShare = trips%>%
  filter(LAD11NM%in%region)%>%
  filter(!is.na(t.purpose))%>%
  filter(!is.na(t.m_main_agg5))%>%
  mutate(calibrationRegion=ifelse(LAD11NM=="Manchester","Manchester","Other"))%>%
  group_by(calibrationRegion,t.purpose,t.m_main_agg5)%>%
  summarise(n=n())%>%
  mutate(share=n/sum(n))

write_csv(modeShare,"result/Manchester/calibration/calibration_initial.csv")
