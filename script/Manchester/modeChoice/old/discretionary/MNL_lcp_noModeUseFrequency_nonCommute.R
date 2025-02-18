# ################################################################# #
####       CLEAR MEMORY AND LOAD LIBRARY                #############
# ################################################################# #
rm(list = ls())

library(tidyverse)
library(fastDummies)
#library(apollo)

# ################################################################# #
####      LOAD DATA AND APPLY ANY TRANSFORMATIONS       #############
# ################################################################# #

trips = readRDS("data/Manchester/processed/tripsForApollo.rds")

trips$asc=1

trips =  trips%>% within({ 
  t.m_main_apollo = as.character(recode(t.m_main, `Walk` = "walk", `Bicycle` = "bike", `Motorcycle, scooter, moped` = "X", 
                                        `Car or van driver` = "carD", `Car or van passenger` = "carP", 
                                        `Bus, minibus, coach` = "pt", `Metrolink` = "pt", 
                                        `Train` = "pt", `Taxi, minicab` = "X"))
  
  
  
  p.age_group_agg = recode(p.age_group, `5-9` = "5_14", `10-14` = "5_14", `15-19` = "15_24", 
                           `20-24` = "15_24", `25-29` = "25_39", `30-34` = "25_39", `35-39` = "25_39",
                           `40-44` = "40_69", `45-49` = "40_69", `50-54` = "40_69", `55-59` = "40_69",
                           `60-64` = "40_69", `65-69` = "40_69", `70-74` = "70", `75-79` = "70",
                           `80-84` = "70", `85+` = "70",.default = "NA")
  
  p.age_65up = (p.age_group == "65-69" | p.age_group == "70-74" | 
                  p.age_group == "75-79" | p.age_group == "80-84" | p.age_group == "85+")
  
  t.departureTime_day = case_when(t.departureTime/3600>6 & t.departureTime/3600 <20 ~ TRUE,
                                  TRUE ~ FALSE)
  
  car_time = carTravelTime_sec/60
  pt_time = ptTravelTime_sec/60
  walk_logDist = log(dist_walk)
  bike_logDist = log(dist_bike)
  
})

trips = trips%>%filter(t.m_main_apollo!="X")

trips = trips%>%dummy_cols(select_columns = c("p.age_group_agg","p.occupation","p.female",
                                              "hh.cars_gr","hh.income_agg",
                                              "t.departureTime_gr","t.full_purpose"))

tripsForJava = trips %>% filter(t.full_purpose%in%c("HBR","HBS","HBO")) %>%
  mutate(choice = recode(t.m_main_apollo,"carD" = 0,"carP" = 1,"pt" = 2,"bike" = 3,"walk" = 4),
         av_carD = ifelse(p.age_group_agg_5_14,0,1),
         p.female = ifelse(p.age_group_agg_5_14 == 1,0,as.numeric(p.female))) %>%
  select(t.ID,choice,starts_with("p.age_group_agg_"),p.age_65up,p.female,p.female_FALSE,
                                starts_with("p.occupation_"),starts_with("hh.cars_gr"),starts_with("hh.income_agg_"),
                                starts_with("t.departureTime_gr_"),starts_with("t.full_purpose_"),starts_with("av_"),"dist",
                                car_time,pt_time) %>% na.omit()
  
write_csv(tripsForJava,"data/Manchester/processed/dynamicLCP/discretionary.csv")

getDetails(tripsForJava)

# ################################################################# #
####                  CORRELATION TEST                  #############
# ################################################################# #

# walkCor = cor(trips%>%select('walk_dist','walk_logDist','walk_vgvi','walk_POIs','walk_negPOIs','walk_shannon',
#                        'walk_stressJct','walk_stressLink','walk_crime','walk_lights','walk_lightsDensity'),
#            use = "complete.obs")
# corrplot::corrplot(walkCor, method = 'number')
# 
# bikeCor = cor(trips%>%select('bike_dist','bike_logDist','bike_vgvi','bike_POIs','bike_negPOIs','bike_shannon',
#                        'bike_stressJct','bike_stressLink','bike_crime','bike_lights','bike_lightsDensity'),
#            use = "complete.obs")
# corrplot::corrplot(bikeCor, method = 'number')
# 

# ################################################################# #
####      LOAD LIBRARY AND DEFINE CORE SETTINGS                  ####
# ################################################################# #
outputDir <- "result/Manchester/discretionary/LCP/"
scenario = "mnl_noModeUse_lcp_v1"

### Load Apollo library
library(apollo)
library(dplyr)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = scenario,
  modelDescr = "MNL",
  panelData  = FALSE,
  indivID    = "t.ID",
  nCores     = 14,
  outputDirectory = outputDir
)

# ################################################################# #
####      DEFINE MODEL PARAMETERS                                ####
# ################################################################# #

options =                                                     c("carD" , "carP", "pt", "bike", "walk")
#parameter names should be the same as those in the database
beta_gv_matrix =  as.matrix(data.frame(asc                   = c( 1     , 0     , 0    , 0      ,0),
                                         p.age_group_agg_5_14  = c( 1     , 1     , 0    , 0      ,0),
                                         p.age_group_agg_15_24 = c( 1     , 0     , 0    , 0      ,0),
                                       # p.age_group_agg_25_39 = c( 1     , 0     , 0    , 0      ,0),
                                         p.age_group_agg_40_69 = c( 1     , 0     , 0    , 0      ,0),
                                         p.age_group_agg_70    = c( 1     , 0     , 0    , 0      ,0),
                                       p.female_FALSE        = c( 1     , 0     , 0    , 0      ,0),
                                       #p.occupation_worker   = c( 1     , 0     , 0    , 0      ,0),
                                       #p.occupation_student  = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_0          = c( 1     , 0     , 0    , 0      ,0),
                                       # hh.cars_gr_1          = c( 1     , 0     , 0    , 0      ,0),
                                       # hh.cars_gr_2          = c( 1     , 0     , 0    , 0      ,0),
                                       # hh.cars_gr_3          = c( 1     , 0     , 0    , 0      ,0),
                                       hh.income_agg_low     = c( 1     , 1     , 0    , 0      ,0),
                                       #hh.income_agg_medium  = c( 1     , 0     , 0    , 0      ,0),
                                       hh.income_agg_high    = c( 1     , 0     , 0    , 1      ,1),
                                       #t.departureTime_gr_time_before6_after22    = c( 1     , 0     , 0    , 0      ,0),
                                       #t.full_purpose_HBE     = c( 1     , 0     , 0    , 0      ,0),
                                       row.names = options))

beta_gv = apply(expand.grid(options,colnames(beta_gv_matrix)), 1, paste, collapse=".")


beta_av = c(beta_car_time = 0, beta_pt_time = 0, 
            beta_walk_cost = 0, gamma_walk_grad = 1, gamma_walk_vgvi = 0, gamma_walk_stressLink = 1, gamma_walk_stressJct = 0,
            beta_bike_cost = 0, gamma_bike_grad = 0, gamma_bike_vgvi = 1, gamma_bike_stressLink = 0, gamma_bike_stressJct = 1)

database = trips[,c("t.ID","t.m_main_apollo",paste("av",options,sep = "_"),colnames(beta_gv_matrix),
                    "car_time","pt_time","t.endOA","dist_walk","dist_bike","SameOrigAndDest")]


apollo_beta = c(rep(0,length(beta_gv)),rep(0,length(beta_av)))
names(apollo_beta) = c(beta_gv,names(beta_av))

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta,
###  use apollo_beta_fixed = c() if none
apollo_fixed = names(apollo_beta)[c(as.numeric(beta_gv_matrix) == 1,beta_av == 1)]


# # Eliminate data with NA results,#check if mode share pattern changed a lot after removing NA
# modeshare_before=database%>%
#   group_by(t.m_main_apollo)%>%
#   summarise(n=n())%>%
#   mutate(share=n/sum(n),data="before")
# 
# originalLength = nrow(database)

database = na.omit(database)

# newLength = nrow(database)
# print(paste(originalLength - newLength,"NA rows eliminated. New dataset has",newLength,"rows."))
# 
# modeshare_after=database%>%
#   group_by(t.m_main_apollo)%>%
#   summarise(n=n())%>%
#   mutate(share=n/sum(n),data="after")
# modeshare=rbind(modeshare_before,modeshare_after)
# ggplot(modeshare,aes(x=data,y=share,fill=t.m_main_apollo))+geom_bar(position ="fill", stat = "identity")
# 
# rm(originalLength,newLength, modeshare_before,modeshare_after,modeshare)

# ################################################################# #
#### READ IN PATHS & READ IN INTRAZONAL AND INTERZONAL TRIPS     ####
# ################################################################# #
interzonalBike <- read_csv("data/manchester/routed/multiRoutesBike7.csv", col_names = TRUE, 
                           col_types = cols_only(IDNumber = "c", PersonNumber = "i", TripNumber = "i",
                                                 travelTime = "n", gradient = "n", vgvi = "n", stressLink = "n", stressJct = "n")) %>% 
  mutate(t.ID = paste0(IDNumber,PersonNumber,TripNumber)) %>%
  relocate(t.ID) %>% select(-IDNumber,-PersonNumber,-TripNumber) %>%
  semi_join(database)

interzonalWalk <- read_csv("data/manchester/routed/multiRoutesWalk6.csv", col_names = TRUE, 
                           col_types = cols_only(IDNumber = "c", PersonNumber = "i", TripNumber = "i",
                                                 travelTime = "n", gradient = "n", vgvi = "n", stressLink = "n", stressJct = "n")) %>%
  mutate(t.ID = paste0(IDNumber,PersonNumber,TripNumber)) %>%
  relocate(t.ID) %>% select(-IDNumber,-PersonNumber,-TripNumber) %>%
  semi_join(database)

intrazonalTrips <- database %>% filter(SameOrigAndDest) %>% select(t.ID,t.endOA,dist_walk,dist_bike)

oaLinksBike <- read_csv("data/manchester/routed/outputAreaLinksBike.csv") %>%
  group_by(geo_code) %>%
  summarise(mGradient = sum(gradient * time) / sum(time),
            mVgvi = sum(vgvi * time) / sum(time),
            mStressLink = sum(stressLink * time) / sum(time),
            mStressJct = sum(stressJct * time) / sum(time))

oaLinksWalk <- read_csv("data/manchester/routed/outputAreaLinksWalk.csv") %>%
  group_by(geo_code) %>%
  summarise(mGradient = sum(gradient * time) / sum(time),
            mVgvi = sum(vgvi * time) / sum(time),
            mStressLink = sum(stressLink * time) / sum(time),
            mStressJct = sum(stressJct * time) / sum(time))

# Todo: get avg fastest travel time by walk/bicycle to be consistent with
intrazonalBike <- inner_join(intrazonalTrips,oaLinksBike, by = c("t.endOA" = "geo_code")) %>%
  transmute(t.ID,
            travelTime = dist_bike / 5.1,
            gradient = mGradient * travelTime,
            vgvi = mVgvi * travelTime,
            stressLink = mStressLink * travelTime,
            stressJct = mStressJct * travelTime)

intrazonalWalk <- inner_join(intrazonalTrips,oaLinksWalk, by = c("t.endOA" = "geo_code")) %>%
  transmute(t.ID,
            travelTime = dist_bike / 5.1,
            gradient = mGradient * travelTime,
            vgvi = mVgvi * travelTime,
            stressLink = mStressLink * travelTime,
            stressJct = mStressJct * travelTime)

routes <- list()
routes$bike <- rbind(interzonalBike,intrazonalBike)
routes$walk <- rbind(interzonalWalk,intrazonalWalk)

rm(interzonalBike,interzonalWalk,intrazonalTrips,oaLinksBike,oaLinksWalk,intrazonalBike,intrazonalWalk)
# 
# # Save inputs to RDS
# saveRDS(routes,"data/Manchester/routed/lcpRoutes.rds")

# ## ALT OPTION: LOAD FROM RDS
# routes <- readRDS("data/Manchester/routed/lcpRoutes.rds")

database <- database %>% semi_join(routes$bike) %>% semi_join(routes$walk)

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #
apollo_inputs = apollo_validateInputs()
apollo_inputs$bikeRoutes <- routes$bike
apollo_inputs$walkRoutes <- routes$walk

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #
calcV <- function(beta) {
  vars = gsub("^.*?\\.","",beta)
  
  val = 0
  for(n in 1:length(vars)) {
    val = val + get(beta[n]) * get(vars[n])
  }
  
  
  return(val)
}

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Get value for mcStress at current iteration
  
  # todo: calculate as a vector
  
  temp1 <- apollo_inputs$bikeRoutes %>% 
    mutate(cost = travelTime + gamma_bike_grad * gradient + gamma_bike_vgvi * vgvi + gamma_bike_stressLink * stressLink + gamma_bike_stressJct * stressJct) %>% 
    group_by(t.ID) %>% 
    summarise(minCost = min(cost))
  
  bike_cost <- left_join(apollo_inputs$database,temp1, by = "t.ID") %>% pull(minCost)
  
  temp2 <- apollo_inputs$walkRoutes %>% 
    mutate(cost = travelTime + gamma_walk_grad * gradient + gamma_walk_vgvi * vgvi + gamma_walk_stressLink * stressLink + gamma_walk_stressJct * stressJct) %>% 
    group_by(t.ID) %>% 
    summarise(minCost = min(cost))
  
  walk_cost <- left_join(apollo_inputs$database,temp2, by = "t.ID") %>% pull(minCost)
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['carD']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"carD.")]) + beta_car_time * car_time
  V[['carP']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"carP.")]) + beta_car_time * car_time
  V[['pt']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"pt.")]) + beta_pt_time * pt_time
  V[['bike']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"bike.")]) + beta_bike_cost * bike_cost
  V[['walk']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"walk.")]) + beta_walk_cost * walk_cost
  
  ### Availability
  avail = list(carD=av_carD, carP=av_carP,pt=av_pt, bike=av_bike,walk=av_walk)
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(carD = "carD", carP = "carP", pt = "pt", walk = "walk", bike = "bike"),
    avail         = avail, 
    choiceVar     = t.m_main_apollo,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Apply weights
  #P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                        estimate_settings=list(writeIter=FALSE,
                                               estimationRoutine = "bfgs", constraints = c("gamma_bike_stressLink > -0.001")))


# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILES)                               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, list(printClassical = TRUE, printPVal = TRUE,
                              printT1 = FALSE, printDataReport = TRUE, printModelStructure = TRUE,
                              printCovar = TRUE, printCorr = TRUE, printOutliers = TRUE,
                              printChange = TRUE, printFunctions = TRUE, saveEst = TRUE,
                              saveCov = FALSE, saveCorr = FALSE, saveModelObject = TRUE,
                              writeF12 = FALSE))



# Print estimates
library(dplyr)
model_estimates <- model$estimate
model_tTest <- model$estimate / model$se
model_pValue <- 2 * (1 - stats::pnorm(abs(model$estimate/model$se)))
model_sig <- case_when(model_pValue <= 0.001 ~ "***",
                       model_pValue <= 0.01 ~ "**",
                       model_pValue <= 0.05 ~ "*",
                       model_pValue <= 0.1 ~ ".",
                       TRUE ~ "")
estimateValues <- paste(round(model_estimates,3),"[",round(model_pValue,2),model_sig,"]")
write.table(estimateValues[(length(estimateValues)-length(beta_av)+1):length(estimateValues)],"clipboard",row.names = F,col.names = F)

names(estimateValues) = names(apollo_beta)


beta_av_full = apply(expand.grid(options,colnames(beta_av_matrix))%>%mutate(Var1 = paste(Var1,Var1,sep = ".")), 1, paste, collapse="_")
estimateValues_full <- c(rep("0",length(beta_gv)),rep("0",length(beta_av_full)))
names(estimateValues_full) = c(beta_gv,beta_av_full)
L <- list(as.list(estimateValues_full),as.list(estimateValues))
estimateValues_full =data.table::rbindlist(L, fill=TRUE)[2]

estimate_coeffsMatrix <- matrix(estimateValues_full,nrow = ncol(beta_gv_matrix)+ncol(beta_av_matrix), ncol = length(options), byrow = TRUE, dimnames = list(c(colnames(beta_gv_matrix),colnames(beta_av_matrix)),options))
r=1-model$LLout/model$LL0
estimate_coeffsMatrix = rbind(estimate_coeffsMatrix,c("r2",r,"","",""))
print(estimate_coeffsMatrix, quote = FALSE)
write.csv(estimate_coeffsMatrix,file=paste(outputDir, scenario,".csv",sep = ""),row.names=TRUE,quote = FALSE) 


#BE investigation
trips=trips%>% within({ 
  bike_stressLink_agg = case_when(bike_stressLink > 0 &bike_stressLink <= 0.2 ~ "bike_stressLink_0_2",
                                  bike_stressLink > 0.2 &bike_stressLink <= 0.4 ~ "bike_stressLink_0_4",
                                  bike_stressLink > 0.4 &bike_stressLink <= 0.6 ~ "bike_stressLink_0_6",
                                  bike_stressLink > 0.6 &bike_stressLink <= 0.8 ~ "bike_stressLink_0_8",
                                  TRUE ~ "bike_stressLink_1_0")
  
  walk_stressLink_agg = case_when(walk_stressLink > 0 &walk_stressLink <= 0.2 ~ "walk_stressLink_0_2",
                                  walk_stressLink > 0.2 &walk_stressLink <= 0.4 ~ "walk_stressLink_0_4",
                                  walk_stressLink > 0.4 &walk_stressLink <= 0.6 ~ "walk_stressLink_0_6",
                                  walk_stressLink > 0.6 &walk_stressLink <= 0.8 ~ "walk_stressLink_0_8",
                                  TRUE ~ "walk_stressLink_1_0")
  
  bike_stressJct_agg = case_when(bike_stressJct > 0 &bike_stressJct <= 0.02 ~ "bike_stressJct_00_2",
                                 bike_stressJct > 0.02 &bike_stressJct <= 0.04 ~ "bike_stressJct_00_4",
                                 bike_stressJct > 0.04 &bike_stressJct <= 0.06 ~ "bike_stressJct_00_6",
                                 bike_stressJct > 0.06 &bike_stressJct <= 0.08 ~ "bike_stressJct_00_8",
                                 TRUE ~ "bike_stressJct_0_1")
  
  walk_stressJct_agg = case_when(walk_stressJct > 0 &walk_stressJct <= 0.02 ~ "walk_stressJct_00_2",
                                 walk_stressJct > 0.02 &walk_stressJct <= 0.04 ~ "walk_stressJct_00_4",
                                 walk_stressJct > 0.04 &walk_stressJct <= 0.06 ~ "walk_stressJct_00_6",
                                 walk_stressJct > 0.06 &walk_stressJct <= 0.08 ~ "walk_stressJct_00_8",
                                 TRUE ~ "walk_stressJct_0_1")})
write.table(trips%>%
              group_by(bike_stressLink_agg,t.m_main_apollo)%>%
              summarise(n=n())%>%
              mutate(share=n/sum(n))%>%
              select(bike_stressLink_agg,t.m_main_apollo,share)%>%
              spread(t.m_main_apollo,share),
            "clipboard",row.names = F,sep = "\t")

write.table(trips%>%
              group_by(walk_stressLink_agg,t.m_main_apollo)%>%
              summarise(n=n())%>%
              mutate(share=n/sum(n))%>%
              select(walk_stressLink_agg,t.m_main_apollo,share)%>%
              spread(t.m_main_apollo,share),
            "clipboard",row.names = F,sep = "\t")

write.table(trips%>%
              group_by(walk_stressJct_agg,t.m_main_apollo)%>%
              summarise(n=n())%>%
              mutate(share=n/sum(n))%>%
              select(walk_stressJct_agg,t.m_main_apollo,share)%>%
              spread(t.m_main_apollo,share),
            "clipboard",row.names = F,sep = "\t")

write.table(trips%>%
              group_by(bike_stressJct_agg,t.m_main_apollo)%>%
              summarise(n=n())%>%
              mutate(share=n/sum(n))%>%
              select(bike_stressJct_agg,t.m_main_apollo,share)%>%
              spread(t.m_main_apollo,share),
            "clipboard",row.names = F,sep = "\t")
