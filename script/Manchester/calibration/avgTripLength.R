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
  summarise(avgTripLength = mean(dist,na.rm = T)/1000)%>%
  spread(personCategory,avgTripLength)



ggplot(trips)+
  geom_density(aes(x=dist/1000,group=personCategory,color=personCategory))+
  facet_wrap(.~t.full_purpose,nrow  = 4)+
  xlim(0,20)


write_csv(tripLength,"result/Manchester/calibration/avgTripLength.csv")


RRT=trips%>%filter(t.purpose=="RRT")

RRT = RRT%>%
  filter(!is.na(hh.adults))%>%
  mutate(personCategory = case_when(hh.cars == 0 ~ "noCar",
                                    hh.cars/hh.adults < 1 ~ "autosPerAdult_under1",
                                    hh.cars/hh.adults >= 1 ~ "autosPerAdult_over1"))

tripLength = RRT%>%
  mutate(hasCar = ifelse(hh.cars>0,1,0))%>%
  group_by(hasCar)%>%
  summarise(avgTripLength = sum(t.tripLength*t.expansionFactor_all,na.rm = T)/sum(t.expansionFactor_all)/1000,
            n=n())
