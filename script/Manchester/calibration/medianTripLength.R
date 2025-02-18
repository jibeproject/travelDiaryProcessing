library(tidyverse)
library(fastDummies)


### Clear memory ###
rm(list = ls())


### load data ###
trips = read_csv("data/Manchester/processed/tripsForApollo.csv")


### average trip length by purpose by person category ###
categories=c("NoCar","autosPerAdult_under1","autosPerAdult_over1")


trips = trips%>%
  filter(!is.na(hh.adults))%>%
  mutate(personCategory = case_when(hh.cars == 0 ~ "noCar",
                                    hh.cars/hh.adults < 1 ~ "autosPerAdult_under1",
                                    hh.cars/hh.adults >= 1 ~ "autosPerAdult_over1"))

tripLength = trips%>%
  group_by(t.full_purpose,personCategory)%>%
  summarise(medianTripLength = median(dist,na.rm = T)/1000)%>%
  spread(personCategory,medianTripLength)



ggplot(trips)+
  geom_density(aes(x=dist/1000,group=personCategory,color=personCategory))+
  facet_wrap(.~t.full_purpose,nrow  = 4)+
  xlim(0,20)




TRADS = readRDS("data/Manchester/processed/TRADS.rds")
indiv <- TRADS$indiv %>% left_join(TRADS$households)
indiv <- left_join(indiv, TRADS$indiv %>% group_by(hh.id) %>% summarise(hh.adults = sum(p.age_group!="5-9"&p.age_group!="10-14"),
                                                                        hh.children5to9 = sum(p.age_group=="5-9"),
                                                                        hh.children10to14 = sum(p.age_group=="10-14"),
                                                                        hh.children15to19 = sum(p.age_group=="15-19"),
                                                                        hh.size = hh.adults + hh.children5to9 + hh.children10to14,
                                                                        hh.workers = sum(p.ws_workOver30h|p.ws_work16to30h|p.ws_workUnder16h) ))



RRT=TRADS$trips%>%filter(t.origin == "RRT" | t.destination == "RRT" )
RRT<-left_join(RRT, indiv)

RRT = RRT%>%
  filter(!is.na(hh.adults))%>%
  mutate(personCategory = case_when(hh.cars == 0 ~ "noCar",
                                    hh.cars/hh.adults < 1 ~ "autosPerAdult_under1",
                                    hh.cars/hh.adults >= 1 ~ "autosPerAdult_over1"))


tripLengthRRT = RRT%>%
  group_by(personCategory)%>%
  summarise(medianTripLength = median(t.tripLength,na.rm = T)/1000)%>%
  spread(personCategory,medianTripLength)

tripLengthRRT$t.full_purpose="RRT"

write_csv(rbind(tripLength,tripLengthRRT),"result/Manchester/calibration/medianTripLength.csv")
