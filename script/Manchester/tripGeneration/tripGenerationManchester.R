rm(list=ls())

########## LOAD LIBRARIES ##########

library(tidyr)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(caret)
library(ISLR)
library(MASS)
library(pscl)
library(modelsummary)
library(purrr)
library(stringr)
library(openxlsx)


########## LOAD FUNCTIONS ##########

source("utils.R", encoding = "UTF-8")

########## READ & PROCESS DATA ##########

df<-read.csv("datasets/dataTripGenManchester.csv", sep=";")

# df <- df %>% select(-c("p.freqWFH"))
# df <- na.omit(df) # we loose 4 rows



##### Ensure that people <=15 have no HBW trips
df <- df %>%
  mutate(HBW = if_else(p.age_gr <= 3, 0, HBW)) %>%
  mutate(NHBW = if_else(p.age_gr <= 3, 0, NHBW))


#### Address issue of nan values in p.occupationStatus
#df <- df %>%
#  mutate(p.occupationStatus = ifelse(is.na(p.occupationStatus) & p.age_gr <= 3, "student", p.occupationStatus))



# HBW trip or not
df$p.has_HBW <- ifelse(df$HBW > 0, 1, 0)
df <- df %>%
  mutate(p.workTrips = case_when(
    HBW == 0 ~ 0,
    HBW == 1 ~ 1,
    HBW == 2 ~ 2,
    HBW == 3 ~ 3,
    HBW == 4 ~ 4,
    HBW >=5 ~ 5,
    TRUE ~ NA
  ))
df<-dummy_cols(df, select_columns= c('p.workTrips'))

df <- df %>%
  mutate(p.workTrips_15 = case_when(
    HBW > 0 ~ 1,
    HBW == 0 ~ 0,
    TRUE ~ NA
  ))

df <- df %>%
  mutate(p.workTrips_14 = case_when(
    (HBW > 0 & HBW < 5) ~ 1,
    (HBW == 0 | HBW >= 5) ~ 0,
    TRUE ~ NA
  ))

# HBE trip or not
df$p.has_HBE <- ifelse(df$HBE > 0, 1, 0)
df <- df %>%
  mutate(p.eduTrips = case_when(
    HBE == 0 ~ 0,
    HBE == 1 ~ 1,
    HBE == 2 ~ 2,
    HBE == 3 ~ 3,
    HBE == 4 ~ 4,
    HBE >=5 ~ 5,
    TRUE ~ NA
  ))
df<-dummy_cols(df, select_columns= c('p.eduTrips'))

df <- df %>%
  mutate(p.eduTrips_15 = case_when(
    HBE > 0 ~ 1,
    HBE == 0 ~ 0,
    TRUE ~ NA
  ))

df <- df %>%
  mutate(p.eduTrips_14 = case_when(
    (HBE > 0 & HBE < 5) ~ 1,
    (HBE == 0 | HBE >= 5) ~ 0,
    TRUE ~ NA
  ))

# Female or not
df$p.female <- as.integer(df$p.female)

# Urban area or not
df$hh.urban <- as.integer(df$hh.urban)
# df <- df %>%
#   filter(p.freqWFH>0)
#df<-dummy_cols(df, select_columns= c('p.freqWFH'))

# Household size
df <- df %>%
  mutate(hh.sized = case_when(
    hh.size == 1 ~ 1,
    hh.size == 2 ~ 2,
    hh.size == 3 ~ 3,
    hh.size == 4 ~ 4,
    hh.size >=5 ~ 5,
    TRUE ~ NA
  ))
df<-dummy_cols(df, select_columns= c('hh.sized'))

df <- df %>%
  mutate(hh.sized_45 = case_when(
    hh.size > 3 ~ 1,
    hh.size <= 3 ~ 0,
    TRUE ~ NA
  ))

df <- df %>%
  mutate(hh.sized_23 = case_when(
    (hh.size > 1 & hh.size < 4) ~ 1,
    (hh.size <= 1 | hh.size >=4) ~ 0,
    TRUE ~ NA
  ))

# Number of children
df <- df %>%
  mutate(hh.childrend = case_when(
    hh.children == 0 ~ 0,
    hh.children == 1 ~ 1,
    hh.children == 2 ~ 2,
    hh.children >=3 ~ 3
  ))
df<-dummy_cols(df, select_columns= c('hh.childrend'))

# Age (1 = 0-18, 2 = 19-29, 3 = 30-49, 4 = 50-59, 5 = 60-69, 6 = 70+)
df<-dummy_cols(df, select_columns= c('p.age_gr'))

df <- df %>%
  mutate(p.age_gr_23 = case_when(
    (p.age_gr > 1 & p.age_gr < 4) ~ 1,
    (p.age_gr == 1 | p.age_gr >=4) ~ 0,
    TRUE ~ NA
  )) %>%
  mutate(p.age_gr_78 = case_when(
    (p.age_gr >=7) ~ 1,
    (p.age_gr <7) ~ 0,
    TRUE ~ NA
  )) %>%
  mutate(p.age_gr_56 = case_when(
    (p.age_gr > 4 & p.age_gr < 7) ~ 1,
    (p.age_gr <= 4 | p.age_gr >= 7) ~ 0,
    TRUE ~ NA
  )) %>%
  mutate(p.age_gr_89 = case_when(
    (p.age_gr > 7 & p.age_gr < 10) ~ 1,
    (p.age_gr <= 7 | p.age_gr >= 10) ~ 0,
    TRUE ~ NA
  ))

# Recorded commute mode (optional)
df <- df %>%
  mutate(p.workModeRecoded = case_when(
    p.workMode == "bus" ~ "pt",
    p.workMode == "train" ~ "pt",
    TRUE ~ p.workMode
  ))
df<-dummy_cols(df, select_columns= c('p.workModeRecoded'))

# Has car or not
df$has_car <- as.integer(df$hh.cars>0)

# Remove the two obs. with NA values
df <- df %>% filter(!is.na(hh.cars))
df <- df %>%
  mutate(hh.carsd = case_when(
    hh.cars == 0 ~ 0,
    hh.cars == 1 ~ 1,
    hh.cars == 2 ~ 2,
    hh.cars >=3 ~ 3
  ))
df<-dummy_cols(df, select_columns= c('hh.carsd'))

df <- df %>%
  mutate(hh.carsd13 = case_when(
    (hh.cars > 0 ) ~ 1,
    (hh.cars == 0) ~ 0,
    TRUE ~ NA
  ))

# Income
df <- df %>%
  filter(hh.income !=-8)
df<-dummy_cols(df, select_columns= c('hh.income'))

# Occupation status
df <- df %>%
  mutate(p.status = case_when(
    is.na(p.occupationStatus) ~ "student", # I found that all NA values are under age groups <=3
    p.occupationStatus %in% c("full-time", "part-time") ~ "employed",
    p.occupationStatus %in% c("student") ~ "student",
    p.occupationStatus %in% c("unemployed") ~ "unemployed",
    TRUE ~ "retired" # Optional: To catch unexpected values
  ))


# df <- df[complete.cases(df$p.status), ] # 6005 observations lost after removing NA

df<-dummy_cols(df, select_columns= c('p.status'))

# Distances
df$p.log_km_mean_HBW <- log(df$p.mean_distance.hbw)
df$p.log_km_mean_HBE <- log(df$p.mean_distance.hbe)
df$p.log_km_mean_HBW[!df$p.has_HBW] <- 0
df$p.log_km_mean_HBE[!df$p.has_HBE] <- 0

df$p.mean_distance.hbw[!df$p.has_HBW] <- 0
df$p.mean_distance.hbe[!df$p.has_HBE] <- 0

########## MODELS ##########

m.tripGenYn <- list()
m.tripGenPolr <- list()
m.tripGenHurdle <-list()

# HBW yes/no model
# TODO: new model
m.tripGenYn$HBW <- glm(p.has_HBW ~  
                         hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                       + hh.sized_3 + hh.sized_45
                       + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                       + p.status_retired + p.status_student + p.status_unemployed
                       #+ p.driversLicence 
                       #+ p.ownBicycle
                       + p.female
                       + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                       #+ hh.income_2 # + hh.income_3  # hh.income_1 is the reference
                       + hh.urban, 
                       data = df, weights = p.weight, family = "quasibinomial")


summary(m.tripGenYn$HBW)

predicted_probabilities <- predict(m.tripGenYn$HBW, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
table(predicted_classes)
table(Predicted = predicted_classes, Actual = df$p.has_HBW)


# par(mfrow = c(2, 2))
# plot(m.tripGenYn$HBW)
# plot(m.tripGenYn$HBW, 1)

# HBW ordered logit model
df1 <- df %>%
  filter(p.has_HBW > 0) %>% 
  mutate(p.HBWbounded = factor(pmin(HBW, 7))) # I set to 10, it can be more or less

m.tripGenPolr$HBW <- MASS::polr(formula = p.HBWbounded ~ 
                                  hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                                #+ hh.sized_3 + hh.sized_4 + hh.sized_5 # + hh.sized_2
                                + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                                + p.status_retired + p.status_student + p.status_unemployed
                                # + p.driversLicence 
                                #+ p.ownBicycle
                                + p.female
                                # + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                                #+ hh.income_3 # + hh.income_2  # hh.income_1 is the reference
                                + hh.urban, 
                                data = df1, Hess=TRUE)

summary(m.tripGenPolr$HBW)
compare_MCsimulation(m.tripGenPolr$HBW)
compute_pvalues(m.tripGenPolr$HBW)

# HBE yes/no model
m.tripGenYn$HBE <- glm(p.has_HBE ~ 
                         #hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                       #+ hh.sized_4 + hh.sized_5 # + hh.sized_2 + hh.sized_3  
                       p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                       + p.status_retired + p.status_student, #+ p.status_unemployed #+ p.status_out 
                       #+ p.driversLicence 
                       #+ p.ownBicycle
                       #+ p.female,
                       # + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                       #+ hh.income_2 + hh.income_3,  # hh.income_1 is the reference
                       #+ hh.urban
                       data = df, weights = p.weight, family = "quasibinomial")

summary(m.tripGenYn$HBE)

predicted_probabilities <- predict(m.tripGenYn$HBE, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
table(predicted_classes)
table(Predicted = predicted_classes, Actual = df$p.has_HBE)


# HBE ordered logit model
df1 <- df %>%
  filter(p.has_HBE > 0) %>% 
  mutate(p.HBEbounded = factor(pmin(HBE, 7))) # I set to 7, it can be more or less

m.tripGenPolr$HBE <- MASS::polr(formula = p.HBEbounded ~ 
                                # hh.childrend_3 + hh.childrend_1 + hh.childrend_2 + 
                                # hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5 
                                + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                                + p.status_student #+ p.status_unemployed + p.status_retired 
                                #+ p.driversLicence 
                                # + p.ownBicycle
                                # + p.female
                                # + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                                # + hh.income_2 + hh.income_3  # hh.income_1 is the reference
                                + hh.urban, 
                                data = df1, Hess=TRUE)

summary(m.tripGenPolr$HBE)
#compare_MCsimulation(m.tripGenPolr$HBE) # Sometimes, it may not work as 7 is underrespresented.
compute_pvalues(m.tripGenPolr$HBE)

# df.pred_obs.HBE<-compute_MCsimulation(m.tripGenPolr$HBE)
# df1$diff <- df.pred_obs.HBE %>% mutate(diff= abs(MC.prediction-V1)>3) %>% select(c("diff"))
# df1<-df1 %>% filter(diff==TRUE)

# HBS
m.tripGenHurdle$HBS <- hurdle(HBS ~  
                                hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence 
                              #+ p.ownBicycle
                              #+ hh.cars_per_adult
                              + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5 # + p.workTrips_1
                              + p.eduTrips_2 + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 # + p.eduTrips_1  
                              + p.log_km_mean_HBW #+ p.log_km_mean_HBE 
                              |  hh.sized_45 # + hh.sized_5 + hh.sized_2 + hh.sized_3 + 
                              # + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence
                              # + hh.cars_per_adult
                              #+ p.ownBicycle
                              + p.workTrips_14 + p.workTrips_5 # + p.workTrips_2 + p.workTrips_3 + p.workTrips_4
                              + p.eduTrips_14 + p.eduTrips_5 # + p.eduTrips_4 + p.eduTrips_5 +p.eduTrips_2 
                              + p.log_km_mean_HBW+ p.log_km_mean_HBE, # + p.log_km_mean_HBE
                              dist = "negbin",data = df, weights = p.weight)
summary(m.tripGenHurdle$HBS)
#compare_MCsimulation(m.tripGenHurdle$HBS)

# HBR
m.tripGenHurdle$HBR <- hurdle(HBR ~  
                                hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              #+ hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              # + p.female
                              #+ p.driversLicence 
                              #+ p.ownBicycle
                              + hh.cars_per_adult
                              + p.workTrips_5 + p.workTrips_14 #+ p.workTrips_3 + p.workTrips_4 + p.workTrips_5 # + p.workTrips_1 + p.workTrips_2 
                              + p.eduTrips_5 + p.eduTrips_14 #+ p.eduTrips_4 + p.eduTrips_5 # + p.eduTrips_1 + p.eduTrips_2 + p.eduTrips_3  
                              + p.log_km_mean_HBW + p.log_km_mean_HBE # + p.log_km_mean_HBE 
                              | hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence
                              + hh.cars_per_adult
                              #+ p.ownBicycle
                              + p.workTrips_14 + p.workTrips_5 #+ p.workTrips_1  p.workTrips_4 
                              + p.eduTrips_14
                              + p.eduTrips_5 # + p.eduTrips_3 + p.eduTrips_4  
                              + p.log_km_mean_HBW + p.log_km_mean_HBE,
                              dist = "negbin",data = df, weights = p.weight)
summary(m.tripGenHurdle$HBR)
#compare_MCsimulation(m.tripGenHurdle$HBR)

# HBO
m.tripGenHurdle$HBO <- hurdle(HBO ~ 
                                hh.sized_4 + hh.sized_5 # hh.sized_2 + hh.sized_3 +   
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence 
                              #+ p.ownBicycle
                              + hh.cars_per_adult
                              + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5 # + p.workTrips_1 
                              # + p.eduTrips_1 + p.eduTrips_2 + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 / p.eduTrips_4 is significant
                              + p.log_km_mean_HBW + p.log_km_mean_HBE # + p.log_km_mean_HBE 
                              | hh.sized_3 + hh.sized_4 # + hh.sized_5 hh.sized_2 +  
                              # + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence
                              # + hh.cars_per_adult
                              #+ p.ownBicycle
                              + p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                              + p.eduTrips_14 + p.eduTrips_5  #+ p.eduTrips_3 + p.eduTrips_4 # + p.eduTrips_1 + p.eduTrips_2  
                              + p.log_km_mean_HBW 
                              + p.log_km_mean_HBE,
                              dist = "negbin",data = df, weights = p.weight)
summary(m.tripGenHurdle$HBO)
# compare_MCsimulation(m.tripGenHurdle$HBO)


# HBA
m.tripGenHurdle$HBA <- hurdle(HBA ~ 
                                hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9 
                              + hh.carsd_2 + hh.carsd_3 # + hh.carsd_1 
                              + p.female
                              #+ p.driversLicence 
                              #+ p.ownBicycle # significance low but I keep it as it makes sense to me
                              #+ hh.cars_per_adult
                              + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5 # + p.workTrips_1 
                              + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 + p.eduTrips_1 + p.eduTrips_2  
                              + p.log_km_mean_HBW 
                              #+ p.log_km_mean_HBE 
                              | hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9 
                              + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              #+ p.driversLicence
                              #+ hh.cars_per_adult
                              # + p.ownBicycle
                              + p.workTrips_4 + p.workTrips_5 # p.workTrips_3 + p.workTrips_1 + p.workTrips_2
                              + p.eduTrips_1 + p.eduTrips_2 + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5,
                              #+ p.log_km_mean_HBW 
                              #+ p.log_km_mean_HBE,
                              dist = "negbin",data = df, weights = p.weight)
summary(m.tripGenHurdle$HBA)

# compare_MCsimulation(m.tripGenHurdle$HBA)

# NHBW
m.tripGenHurdle$NHBW <- hurdle(NHBW ~ 
                                 # hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                               hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                               + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9 
                               # + hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                               # + p.female
                               #+ p.driversLicence 
                               # + p.ownBicycle
                               #+ hh.cars_per_adult
                               + p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                               + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 # p.eduTrips_1+ p.eduTrips_2 
                               + p.log_km_mean_HBW # + p.log_km_mean_HBE 
                               | hh.sized_3 + hh.sized_4 + hh.sized_5 # hh.sized_2 +   
                               + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                               + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                               #+ hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                               + p.female
                               #+ p.driversLicence
                               + hh.cars_per_adult
                               #+ p.ownBicycle
                               + p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                               #+ p.eduTrips_1 + p.eduTrips_2 + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5
                               + p.eduTrips_5,
                               #+ p.log_km_mean_HBW 
                               #+ p.log_km_mean_HBE,
                              dist = "negbin", data = df, weights = p.weight)
summary(m.tripGenHurdle$NHBW)

# NHBO
m.tripGenHurdle$NHBO <- hurdle(NHBO ~ 
                                 # hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                               + hh.childrend_2 + hh.childrend_3 # hh.childrend_1
                               + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                               + hh.carsd_1 + hh.carsd_2 #+ hh.carsd_3 # I kept hh.carsd_3 although it's not sign.
                               + p.female
                               #+ p.driversLicence 
                               #+ p.ownBicycle
                               + hh.cars_per_adult
                               + p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                               + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 # + p.eduTrips_1 + p.eduTrips_2  
                               #+ p.log_km_mean_HBW + p.log_km_mean_HBE 
                               | hh.sized_3 + hh.sized_4 + hh.sized_5 # hh.sized_2  
                               + hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                               + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9 
                               + p.female
                               #+ p.driversLicence
                               + hh.cars_per_adult
                               #+ p.ownBicycle
                               + p.workTrips_4 + p.workTrips_5 # + p.workTrips_1 + p.workTrips_2 + p.workTrips_3 
                               + p.eduTrips_14 + p.eduTrips_5 # + p.eduTrips_4 + p.eduTrips_5 # + p.eduTrips_1 
                               + p.log_km_mean_HBW,
                               #+ p.log_km_mean_HBE,
                               dist = "negbin",data = df, weights = p.weight)
summary(m.tripGenHurdle$NHBO)

# RRT
m.tripGenHurdle$RRT <- hurdle(RRT ~  
                                hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5 # hh.sized_2 + hh.sized_3 + 
                              #hh.childrend_3 + hh.childrend_1 + hh.childrend_2 
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9 
                              #+ hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              + p.female
                              + p.status_retired #+ p.status_student + p.status_unemployed
                              #+ p.driversLicence 
                              #+ p.ownBicycle
                              #+ hh.cars_per_adult
                              #+ hh.carsd_0 #
                              #+ hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              # + p.workTrips_5#+ p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                              + p.eduTrips_15 #+ p.eduTrips_1 + p.eduTrips_2 + p.eduTrips_3 + p.eduTrips_4 + p.eduTrips_5 
                              + p.log_km_mean_HBW 
                              #+ p.log_km_mean_HBE # + hh.urban
                              | hh.sized_2 + hh.sized_3 + hh.sized_4 + hh.sized_5  
                              #+ hh.childrend_1 + hh.childrend_2 + hh.childrend_3
                              + p.age_gr_1 + p.age_gr_2 + p.age_gr_3 + p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                              #+ p.status_retired + p.status_student + p.status_unemployed
                              #+ hh.carsd_1 + hh.carsd_2 + hh.carsd_3
                              #+ p.female
                              #+ p.driversLicence
                              + hh.cars_per_adult
                              #+ p.ownBicycle
                              + hh.urban
                              #+ p.workTrips_14 
                              + p.workTrips_5 #+ p.workTrips_1 + p.workTrips_2 + p.workTrips_3 + p.workTrips_4 + p.workTrips_5
                              + p.eduTrips_14 + p.eduTrips_5 
                              + p.log_km_mean_HBW
                              + p.log_km_mean_HBE,
                              dist = "negbin",data = df, weights = p.weight)

summary(m.tripGenHurdle$RRT)
#compare_MCsimulation(m.tripGenHurdle$RRT)

######################## WRITE COEFFICIENT TABLES TO CSV USING MODELSUMMARY ########################
gm <- modelsummary::gof_map
gm$omit <- TRUE
referenceVariables <- data.frame(variable= c("(Intercept)", "p.female", "hh.urban", "hh.cars_per_adult",
                                             "p.workTrips_0", "p.workTrips_1", "p.workTrips_2", "p.workTrips_3", "p.workTrips_4", "p.workTrips_5",   "p.workTrips_14", "p.workTrips_15",
                                             "p.eduTrips_0", "p.eduTrips_1", "p.eduTrips_2", "p.eduTrips_3", "p.eduTrips_4", "p.eduTrips_5",         "p.eduTrips_14", "p.eduTrips_15",
                                             "hh.sized_1", "hh.sized_2", "hh.sized_3", "hh.sized_4", "hh.sized_5",                                  "hh.sized_23", "hh.sized_45",
                                             "hh.childrend_0", "hh.childrend_1", "hh.childrend_2", "hh.childrend_3",
                                             "p.age_gr_1", "p.age_gr_2", "p.age_gr_3", "p.age_gr_4", "p.age_gr_5", "p.age_gr_6", "p.age_gr_7", "p.age_gr_8", "p.age_gr_9",   "p.age_gr_23", "p.age_gr_56", "p.age_gr_78",
                                             "hh.carsd_0", "hh.carsd_1", "hh.carsd_2", "hh.carsd_3",
                                             "p.status_employed", "p.status_retired", "p.status_student",
                                             "p.status_unemployed", "p.log_km_mean_HBW", "p.log_km_mean_HBE"))

# TODO: have been temporarily removed --> ,"p.driversLicenceTRUE", "p.ownBicycleTRUE", "hh.income_1", "hh.income_2", "hh.income_3",


modelSummaryHurdle<-modelsummary(m.tripGenHurdle, output = "results/CoefficientsNegBinCountV2.xlsx", fmt=8, statistic=NULL, gof_map = gm)
modelSummaryPolrCount<-modelsummary(m.tripGenPolr, output = "results/CoefficientsPolrCountV2.xlsx", fmt=8, statistic=NULL, gof_map = gm)
modelSummaryBinZero<-modelsummary(m.tripGenYn, output = "results/CoefficientsZeroBinV2.xlsx", fmt=8, statistic=NULL, gof_map = gm)

# Hurdle Count
modelSummaryHurdleCount<- modelSummaryHurdle %>%
  filter(str_detect(term, "^count_")) %>%
  mutate(variable= str_remove(term, "^count_")) %>% 
  dplyr::select(variable, HBS, HBR, HBO, HBA, NHBW, NHBO, RRT)

# Get the thetas
theta_values <- sapply(m.tripGenHurdle, function(x) x$theta)
theta_df <- data.frame(t(theta_values)) %>% mutate(variable = "theta")
theta_df<-theta_df %>% mutate(
  HBS = HBS.count,
  HBR = HBR.count,
  HBO = HBO.count,
  HBA = HBA.count,
  NHBW = NHBW.count,
  NHBO = NHBO.count,
  RRT = RRT.count
) %>% dplyr::select(variable, HBS, HBR, HBO, HBA, NHBW, NHBO, RRT)


# Hurdle Zero
modelSummaryHurdleZero<- modelSummaryHurdle %>%
  filter(str_detect(term, "^zero_")) %>%
  mutate(variable= str_remove(term, "^zero_")) %>% 
  dplyr::select(variable, HBS, HBR, HBO, HBA, NHBW, NHBO, RRT)

# Polr count
modelSummaryPolrCount<- modelSummaryPolrCount %>% 
  mutate(variable= term) %>% 
  dplyr::select(variable, HBW, HBE)

# Bin Zero
modelSummaryBinZero <- modelSummaryBinZero %>%
  mutate(variable= term) %>% 
  dplyr::select(variable, HBW, HBE)

### Rearrange everything
# Combine Bin Zero and Hurdle Zero
modelSummaryAllZero<- referenceVariables %>% 
  left_join(modelSummaryBinZero, by = "variable") %>%
  left_join(modelSummaryHurdleZero, by = "variable") %>%
  mutate_all(~ifelse(is.na(.), 0, ifelse(. == "", 0, .)))

# NegBin count
modelSummaryNegBinCount<- rbind(referenceVariables, "theta") %>% 
  left_join(rbind(modelSummaryHurdleCount, theta_df), by = "variable") %>%
  mutate_all(~ifelse(is.na(.), 0, ifelse(. == "", 0, .))) %>%
  mutate(HBW=0, HBE=0) %>%
  dplyr::select(variable, HBW, HBE, everything())

# Polr count
modelSummaryPolrCount<- rbind(referenceVariables, "1|2", "2|3", "3|4", "4|5", "5|6", "6|7") %>% 
  left_join(modelSummaryPolrCount, by = "variable") %>%
  mutate_all(~ifelse(is.na(.), 0, ifelse(. == "", 0, .)))


######################## =============================== ########################

groups <- list(
  list(base_row = "p.eduTrips_14", affected_rows = paste0("p.eduTrips_", 1:4)),
  list(base_row = "p.eduTrips_15", affected_rows = paste0("p.eduTrips_", 1:5)),
  list(base_row = "p.workTrips_14", affected_rows = paste0("p.workTrips_", 1:4)),
  list(base_row = "p.workTrips_15", affected_rows = paste0("p.workTrips_", 1:5)),
  list(base_row = "hh.sized_23", affected_rows = paste0("hh.size_", 2:3)),
  list(base_row = "hh.sized_45", affected_rows = paste0("hh.size_", 4:5)),
  list(base_row = "p.age_gr_23", affected_rows = paste0("p.age_gr_", 2:3)),
  list(base_row = "p.age_gr_56", affected_rows = paste0("p.age_gr_", 5:6)),
  list(base_row = "p.age_gr_78", affected_rows = paste0("p.age_gr_", 7:8))
)


# Update logic for grouped rows
update_values <- function(df, groups) {
  for (group in groups) {
    # Retrieve base row and affected rows
    base_row_name <- group$base_row
    affected_row_names <- group$affected_rows
    
    # Get index of base row
    base_row_index <- which(df$variable == base_row_name)
    
    # Proceed only if base row exists and has nonnegative values
    if (length(base_row_index) > 0) {
      for (col in colnames(df)[-1]) { # Exclude the 'variable' column
        if (df[base_row_index, col] != 0) {
          
          # Then do the update
          for (row_name in affected_row_names) {
            # Get the index of the affected row
            affected_row_index <- which(df$variable == row_name)
            if (length(affected_row_index) > 0) {
              df[affected_row_index, col] <- df[base_row_index, col]
            }
          }
          
        }
      }
    }
  }
  return(df)
}

modelSummaryAllZero <- update_values(modelSummaryAllZero, groups)
modelSummaryNegBinCount <- update_values(modelSummaryNegBinCount, groups)
modelSummaryPolrCount <- update_values(modelSummaryPolrCount, groups)


for(group in groups){
  modelSummaryAllZero <- modelSummaryAllZero %>% filter(variable != group$base_row)
  modelSummaryNegBinCount <- modelSummaryNegBinCount %>% filter(variable != group$base_row)
  modelSummaryPolrCount <- modelSummaryPolrCount %>% filter(variable != group$base_row)
}

######################## WRITE COEFFICIENT TABLES TO CSV ########################

write.csv(modelSummaryAllZero,  file = "results/CoefficientsZeroHurdleV2.csv", quote = FALSE, row.names = FALSE)
write.csv(modelSummaryNegBinCount,  file = "results/CoefficientsNegBinCountV2.csv", quote = FALSE, row.names = FALSE)
write.csv(modelSummaryPolrCount,  file = "results/CoefficientsPolrCountV2.csv", quote = FALSE, row.names = FALSE)