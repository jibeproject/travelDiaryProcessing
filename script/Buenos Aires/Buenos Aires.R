library(tidyverse)

# Travel survey data
ENMODO <- list()

ENMODO$hogar <- readxl::read_xlsx("data/Buenos Aires/raw/Hogar_ENMODO18.xlsx")
ENMODO$personas <- readxl::read_xlsx("data/Buenos Aires/raw/Personas_ENMODO18.xlsx")
ENMODO$viajes <- readxl::read_xlsx("data/Buenos Aires/raw/Viajes_ENMODO18.xlsx",na = c("","-"))
ENMODO$etapas <- readxl::read_xlsx("data/Buenos Aires/raw/Etapas_ENMODO18.xlsx",na = "sd")

# Spatial data
radio <- sf::read_sf("data/Buenos Aires/gis/admbound_arg_level_4/admbound_arg_level_4.shp") %>% mutate(REDCODE = as.numeric(REDCODE))
codes <- radio %>% sf::st_drop_geometry() %>% select(REDCODE,DPTO,RADIOS_ARE)


# Transport mode
get_purpose <- function(x) {
  recode_factor(x, `1` = "hogar", `2` = "trabajo", `3` = "business", `4` = "estudios (lugar donde cursa)",
                `5` = "estudios (otros lugares)", `6` = "salud", `7` = "compras", `8` = "social", `9` = "familia", `10` = "deportes",
                `11` = "recreacion", `12` = "personal", `13` = "tramite personal", `14` = "buscar empleo", `15` = "acompañar (miembro de hogar - educativo)",
                `16` = "acompañar (miembro de hogar - otro)", `17` = "acompañar (NO miembro de hogar)",`22` = "otro")
}

viajes <- ENMODO$viajes %>% 
  mutate(mode = recode_factor(modo_des, `1` = "bicicleta", `2` = "pie", `3` = "taxi/remis", `4` = "pt", `5` = "auto", `6` = "charter",
                                 `7` = "multi", `8` = "multi-pt", `9` = "multi-nonpt", .ordered = TRUE),
         purpose.orig = get_purpose(VII_2_activida_en_el_origen),
         purpose.dest = get_purpose(VII_3_activida_en_el_destino),
         repeated = ifelse(VII_9_veces_por_semana_viaja == 88,0,VII_9_veces_por_semana_viaja),
         sameOD = radio_origen == radio_destino)

etapas <- ENMODO$etapas %>%
  mutate(blocks.to = ifelse(VII_28_cuadras_caminadas_hasta_el_medio == 88,0,VII_28_cuadras_caminadas_hasta_el_medio),
         blocks.from = ifelse(VII_30_cuadras_caminadas_al_bajar_del_m == 88,0,VII_30_cuadras_caminadas_al_bajar_del_m))

### INVESTIGATE REPETITION
rep_info <- viajes %>% mutate(expPurpose = factor(case_when(VII_2_activida_en_el_origen == 1 & VII_3_activida_en_el_destino == 2 ~ "home-work",
                                                   VII_2_activida_en_el_origen == 1 & VII_3_activida_en_el_destino == 4 ~ "home-education",
                                                   VII_2_activida_en_el_origen == 1 & VII_3_activida_en_el_destino == 7 ~ "home-shop",
                                                   VII_2_activida_en_el_origen == 1 & VII_3_activida_en_el_destino == 11 ~ "home-recreation"),
                                                  levels = c("home-work","home-education","home-shop","home-recreation"))) %>%
  filter(!is.na(expPurpose)) %>%
  group_by(id_hogar,id_persona,expPurpose) %>%
  summarise(nn = sum(repeated))

plot_data <- rep_info %>% 
  mutate(nn = pmin(nn,7)) %>%
  group_by(expPurpose,nn) %>%
  tally()

ggplot(plot_data, aes(x = nn, y = n)) + geom_bar(stat = "identity") + facet_wrap(~expPurpose, scales = "free_y") + 
  xlab("repetitions per week (0 means less than one per week)") + ylab("count") + scale_x_continuous(breaks = 0:7)

### SUITABILITY FOR ABMS
trips <- viajes %>% arrange(id_hogar,id_persona,id_viaje) %>% group_by(id_hogar,id_persona) %>% mutate(eqPurpose = VII_2_activida_en_el_origen == lag(VII_3_activida_en_el_destino),
                                                                                                       eqLoc = radio_origen == lag(radio_destino))
summary(trips$eqPurpose)
summary(trips$eqLoc)


check <- trips %>% semi_join(trips %>% filter(!eqLoc) %>% select(id_hogar,id_persona))

### SET UP DATA FOR GEPHI
nodes <-  radio %>% sf::st_drop_geometry() %>% transmute(id = as.numeric(REDCODE),latitude = RADIOS_LAT,longitude = RADIOS_LON)
write_csv(nodes,"data/Buenos Aires/gis/nodes.csv")

edges <- transmute(viajes,mode,source = as.numeric(radio_origen),target = as.numeric(radio_destino)) %>% drop_na()
write_csv(edges,"data/Buenos Aires/gis/edges.csv")

### Active travel at stage level\
test <- stages %>% group_by(id_hogar,id_persona,id_viaje) %>% summarise(has_walking = sum(VII_10_medio_transporte == 1) > 0, 
                                                                        has_other = sum(VII_10_medio_transporte > 1) > 0,
                                                                        walk_blocks = sum(VII_11_cuadras_caminadas))
test2 <- test %>% left_join(viajes)
test3 <- test2 %>% group_by(mode) %>% summraise(has_walking)

test4 <- viajes %>% filter(mode == "multi" | mode == "multi-pt") %>% left_join(etapas) %>% group_by(id_hogar,id_persona,id_viaje) %>% summarise(walk_blocks = sum(blocks.from+blocks.to))

# Some Analysis
test <- trips %>% filter(sameOD)

test <- viajes %>% group_by(mode,sameOD) %>% tally() %>% mutate(p = n / sum(n))

trips <- viajes %>% 
  left_join(codes, by = c("radio_origen" = "REDCODE")) %>% rename(dpto_origen = DPTO, area_origen = RADIOS_ARE) %>%
  left_join(codes, by = c("radio_destino" = "REDCODE")) %>% rename(dpto_destino = DPTO, area_destino = RADIOS_ARE)

ggplot(trips,aes(x = area_destino)) + geom_density() + xlim(0,1e06)

test <- c(trips$area_origen,trips$area_destino)



test <- trips %>% filter(dpto_origen == "Capital")

trips <- trips %>% filter(mode == "bicicleta")
origins <- trips %>% count(radio_origen, name = "origins")
destinations <- trips %>% count(radio_destino, name = "destinations")
intrazonal <- trips %>% filter(sameOD) %>% count(radio_origen, name = "sameOD")

radio <- radio %>% left_join(origins,by = c("REDCODE" = "radio_origen")) %>% 
  left_join(destinations, by = c("REDCODE" = "radio_destino")) %>%
  left_join(intrazonal, by = c("REDCODE" = "radio_origen"))

sf::write_sf(radio,"bike.shp")


modeSplit <- trips %>% count(modo_des) %>% mutate(p = n / sum(n))
