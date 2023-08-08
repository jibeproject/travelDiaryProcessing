library(tidyverse)

indiv <- readxl::read_xlsx("Personas_ENMODO18.xlsx")
trips <- readxl::read_xlsx("Viajes_ENMODO18.xlsx")

radio <- sf::read_sf("admbound_arg_level_4/admbound_arg_level_4.shp") %>% mutate(REDCODE = as.numeric(REDCODE))


trips$sameOD <- trips$radio_origen == trips$radio_destino

test <- trips %>% filter(sameOD)

test <- trips %>% group_by(modo_des,sameOD) %>% tally() %>% mutate(p = n / sum(n))

codes <- radio %>% sf::st_drop_geometry() %>% transmute(REDCODE = as.numeric(REDCODE),DPTO)

trips <- trips %>% 
  left_join(codes, by = c("radio_origen" = "REDCODE")) %>% rename(dpto_origen = DPTO) %>%
  left_join(codes, by = c("radio_destino" = "REDCODE")) %>% rename(dpto_destino = DPTO)

test <- trips %>% filter(dpto_origen == "Capital")


origins <- trips %>% count(radio_origen, name = "origins")
destinations <- trips %>% count(radio_destino, name = "destinations")
intrazonal <- trips %>% filter(sameOD) %>% count(radio_origen, name = "sameOD")

radio <- radio %>% left_join(origins,by = c("REDCODE" = "radio_origen")) %>% 
  left_join(destinations, by = c("REDCODE" = "radio_destino")) %>%
  left_join(intrazonal, by = c("REDCODE" = "radio_origen"))

sf::write_sf(radio,"test.shp")


modeSplit <- trips %>% count(modo_des) %>% mutate(p = n / sum(n))
