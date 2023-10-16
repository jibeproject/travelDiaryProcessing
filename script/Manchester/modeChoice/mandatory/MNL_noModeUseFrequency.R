# ################################################################# #
####       CLEAR MEMORY AND LOAD LIBRARY                #############
# ################################################################# #
rm(list = ls())

library(tidyverse)
library(fastDummies)

# ################################################################# #
####      LOAD DATA AND APPLY ANY TRANSFORMATIONS       #############
# ################################################################# #

trips = read_csv("data/Manchester/processed/tripsForApollo.csv")

trips$asc=1

trips =  trips%>% within({ 
  t.m_main_apollo = recode(t.m_main, `Walk` = "walk", `Bicycle` = "bike", `Motorcycle, scooter, moped` = "X", 
                           `Car or van driver` = "carD", `Car or van passenger` = "carP", 
                           `Bus, minibus, coach` = "pt", `Metrolink` = "pt", 
                           `Train` = "pt", `Taxi, minicab` = "X")
  
  
  
  p.age_group_agg = recode(p.age_group, `5-9` = "5_24", `10-14` = "5_24", `15-19` = "5_24", 
                           `20-24` = "5_24", `25-29` = "25_39", `30-34` = "25_39", `35-39` = "25_39",
                           `40-44` = "40_69", `45-49` = "40_69", `50-54` = "40_69", `55-59` = "40_69",
                           `60-64` = "40_69", `65-69` = "40_69", `70-74` = "70", `75-79` = "70",
                           `80-84` = "70", `85+` = "70",.default = "NA")
  
  t.departureTime_day = case_when(t.departureTime/3600>6 & t.departureTime/3600 <20 ~ TRUE,
                                  TRUE ~ FALSE)
  
  carD_time = carTravelTime_sec/60
  carP_time = carTravelTime_sec/60
  pt_time = ptTravelTime_sec/60
  walk_logDist = log(walk_dist)
  bike_logDist = log(bike_dist)
  
  #interaction
  carD_timeHBW = case_when(t.full_purpose=="HBW" ~ carD_time, TRUE ~ 0)
  carD_timeHBE = case_when(t.full_purpose=="HBE" ~ carD_time, TRUE ~ 0)
  carP_timeHBW = case_when(t.full_purpose=="HBW" ~ carP_time, TRUE ~ 0)
  carP_timeHBE = case_when(t.full_purpose=="HBE" ~ carP_time, TRUE ~ 0)
  pt_timeHBW = case_when(t.full_purpose=="HBW" ~ pt_time, TRUE ~ 0)
  pt_timeHBE = case_when(t.full_purpose=="HBE" ~ pt_time, TRUE ~ 0)
  
  bike_vgviDay = case_when(t.departureTime_day ~ bike_vgvi, TRUE ~ 0)
  walk_vgviDay = case_when(t.departureTime_day ~ walk_vgvi, TRUE ~ 0)
  bike_lightNight = case_when(!t.departureTime_day ~ bike_lightsDensity, TRUE ~ 0)
  walk_lightNight = case_when(!t.departureTime_day ~ walk_lightsDensity, TRUE ~ 0)
  
  bike_stressLinkLogDistance = bike_stressLink * bike_logDist
  
  bike_stressJctDistance = bike_stressJct * bike_dist
  
  bike_crimeDistance = bike_crime * bike_dist
  
  bike_lightNightLogDistance = bike_lightNight * bike_logDist
  
  walk_vgviDay = case_when(t.departureTime_day ~ walk_vgvi, TRUE ~ 0)
  walk_lightNight = case_when(!t.departureTime_day ~ walk_lightsDensity, TRUE ~ 0)
  
  walk_stressLinkDistance2km = case_when(walk_dist <= 2000 ~ walk_stressLink, TRUE ~ 0)
  walk_stressLinkDistance = walk_stressLink * walk_dist
  
  walk_stressJctDistance = walk_stressJct * walk_dist
  
  walk_crimeDistance = walk_crime * walk_dist
  
  walk_vgviDayDistance1km = case_when(walk_dist <= 1000 ~ walk_vgviDay, TRUE ~ 0)
})

trips = trips%>%filter(t.m_main_apollo!="X")

trips = trips%>%dummy_cols(select_columns = c("p.age_group_agg","p.female","p.occupation",
                                              "hh.cars_gr","hh.income_agg",
                                              "t.departureTime_gr","t.full_purpose"))

trips = trips%>%filter(t.full_purpose%in%c("HBW","HBE"))

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
outputDir <- "result/Manchester/mandatory/tests/"
scenario = "mnl_noModeUse_v4_newCorridor"

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
  nCores     = 16,
  outputDirectory = outputDir
)

# ################################################################# #
####      DEFINE MODEL PARAMETERS                                ####
# ################################################################# #

options =                                                     c("carD" , "carP", "pt", "bike", "walk")
#parameter names should be the same as those in the database
beta_gv_matrix =  as.matrix(data.frame(asc                   = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_5_14  = c( 1     , 0     , 0    , 0      ,0),
                                       p.age_group_agg_5_24 = c( 1     , 0     , 0    , 0      ,0),
                                       p.age_group_agg_25_39 = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_40_69 = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_70    = c( 1     , 0     , 0    , 0      ,0),
                                       p.female_FALSE        = c( 1     , 0     , 0    , 0      ,1),
                                       #p.occupation_worker   = c( 1     , 0     , 0    , 0      ,0),
                                       #p.occupation_student  = c( 1     , 0     , 0    , 0      ,0),
                                       hh.cars_gr_0          = c( 1     , 0     , 0    , 0      ,0),
                                       hh.cars_gr_1          = c( 1     , 0     , 0    , 0      ,0),
                                       hh.cars_gr_2          = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_3          = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.income_agg_low     = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.income_agg_medium  = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.income_agg_high    = c( 1     , 0     , 0    , 0      ,0),
                                       #t.departureTime_gr_time_before6_after22    = c( 1     , 0     , 0    , 0      ,0),
                                       #t.full_purpose_HBE     = c( 1     , 0     , 0    , 0      ,0),
                                       row.names = options))

beta_av_matrix =  as.matrix(data.frame(time                  = c( 1     , 1     , 1     , 0     ,0),
                                       timeHBW               = c( 1     , 1     , 0     , 0     ,0),
                                       #timeHBE               = c( 1     , 1     , 0     , 0     ,0),
                                       logDist               = c( 0     , 0     , 0     , 1     ,1),
                                       #stressLink            = c( 0     , 0     , 0     , 1     ,1),
                                       stressLinkLogDistance = c( 0     , 0     , 0     , 1     ,0),
                                       stressLinkDistance    = c( 0     , 0     , 0     , 0     ,1),
                                       stressJctDistance     = c( 0     , 0     , 0     , 1     ,1),
                                       shannon               = c( 0     , 0     , 0     , 1     ,1),
                                       #POIs                  = c( 0     , 0     , 0     , 1     ,1),
                                       # negPOIs              = c( 0     , 0     , 0     , 1     ,1),
                                       #vgviDay               = c( 0     , 0     , 0     , 0     ,1),
                                       crimeDistance         = c( 0     , 0     , 0     , 0     ,1),
                                       lightNightLogDistance = c( 0     , 0     , 0     , 1     ,0),
                                       row.names = options))

beta_gv = apply(expand.grid(options,colnames(beta_gv_matrix)), 1, paste, collapse=".")
beta_av = apply(expand.grid(options,colnames(beta_av_matrix))%>%mutate(Var1 = paste(Var1,Var1,sep = ".")), 1, paste, collapse="_")[as.numeric(beta_av_matrix) == 1]

apollo_beta = c(rep(0,length(beta_gv)),rep(0,length(beta_av)))
names(apollo_beta) = c(beta_gv,beta_av)

database = trips[,c("t.ID","t.m_main_apollo",paste("av",options,sep = "_"),gsub("^.*?\\.","",beta_av),colnames(beta_gv_matrix))]



# Eliminate data with NA results,#check if mode share pattern changed a lot after removing NA
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

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta,
###  use apollo_beta_fixed = c() if none
apollo_fixed = c(names(apollo_beta)[c(as.numeric(beta_gv_matrix) == 1,beta_av==FALSE)])


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #
apollo_inputs = apollo_validateInputs()

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

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate_settings$bootstrapSE=TRUE"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()

  V[['carD']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"carD.")])
  V[['carP']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"carP.")])
  V[['pt']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"pt.")])
  V[['bike']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"bike.")])
  V[['walk']] = calcV(names(apollo_beta)[startsWith(names(apollo_beta),"walk.")])
  
  nlNests      = list(root=1)
  
  ### Specify tree structure for NL model
  nlStructure = list()
  nlStructure[["root"]]   = c("carD","carP","pt","walk","bike")
  
  
  ### Availability
  avail = list(carD=av_carD, carP=av_carP,pt=av_pt, bike=av_bike,walk=av_walk)
  
  
  ### Define settings for MNL model component
  nl_settings = list(
    alternatives  = c(carD = "carD", carP = "carP", pt = "pt", walk = "walk", bike = "bike"),
    avail         = avail, 
    choiceVar     = t.m_main_apollo,
    V             = V,
    nlNests       = nlNests,
    nlStructure   = nlStructure
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_nl(nl_settings, functionality)
  
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
                        estimate_settings=list(writeIter=FALSE))


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
