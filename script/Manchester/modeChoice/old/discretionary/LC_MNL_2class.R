# ################################################################# #
####       CLEAR MEMORY AND LOAD LIBRARY                #############
# ################################################################# #
rm(list = ls())

library(tidyverse)
library(fastDummies)
library(purrr)
library(caret)

# ################################################################# #
####      LOAD DATA AND APPLY ANY TRANSFORMATIONS       #############
# ################################################################# #

trips = read_csv("data/Manchester/processed/tripsForApollo.csv")

trips$asc = 1
trips$carD_asc=1
trips$carP_asc=1
trips$pt_asc=1
trips$bike_asc=1
trips$walk_asc=1

trips =  trips%>% within({ 
  
  t.m_main_apollo = recode(t.m_main, `Walk` = "walk", `Bicycle` = "bike", `Motorcycle, scooter, moped` = "X", 
                           `Car or van driver` = "carD", `Car or van passenger` = "carP", 
                           `Bus, minibus, coach` = "pt", `Metrolink` = "pt", 
                           `Train` = "pt", `Taxi, minicab` = "X")
  
  
  
  p.age_group_agg = recode(p.age_group, `5-9` = "5_14", `10-14` = "5_14", `15-19` = "15_24", 
                           `20-24` = "15_24", `25-29` = "25_39", `30-34` = "25_39", `35-39` = "25_39",
                           `40-44` = "40_54", `45-49` = "40_54", `50-54` = "40_54", `55-59` = "55",
                           `60-64` = "55", `65-69` = "55", `70-74` = "55", `75-79` = "55",
                           `80-84` = "55", `85+` = "55",.default = "NA")
  
  hh.cars_agg = case_when(hh.cars >3 ~3,
                          TRUE ~ hh.cars)
  
  hh.carsPerAdult = hh.cars/hh.adults
  
  hh.children = hh.children10to14 +hh.children5to9
  
  hh.children_yes = case_when(hh.children > 0 ~ 1,
                              TRUE ~ 0)
  
  hh.children10to14_yes = case_when(hh.children10to14 > 0 ~ 1,
                                    TRUE ~ 0)
  
  hh.children5to9_yes = case_when(hh.children5to9 > 0 ~ 1,
                                  TRUE ~ 0)
  
  t.departureTime_day = case_when(t.departureTime/3600>6 & t.departureTime/3600 <20 ~ TRUE,
                                  TRUE ~ FALSE)
  
  carD_time = carTravelTime_sec/60
  carP_time = carTravelTime_sec/60
  pt_time = ptTravelTime_sec/60
  
  walk_distkm = walk_dist/1000
  bike_distkm = bike_dist/1000
  
  walk_logDist = log(walk_dist)
  bike_logDist = log(bike_dist)
  walk_logDistkm = log(walk_dist/1000)
  bike_logDistkm = log(bike_dist/1000)
  
  walk_cubeDistkm = (walk_dist/1000)^(1/3)
  bike_cubeDistkm = (bike_dist/1000)^(1/3)
  
  walk_logDistOver1km = case_when(walk_dist>1000 ~ log(walk_dist/1000),
                                  TRUE ~ 0)
  walk_distUnder1km = case_when(walk_dist>1000 ~ walk_dist/1000,
                                TRUE ~ 0)
  
  #interaction
  bike_vgviDay = case_when(t.departureTime_day ~ bike_vgvi, TRUE ~ 0)
  walk_vgviDay = case_when(t.departureTime_day ~ walk_vgvi, TRUE ~ 0)
  bike_lightNight = case_when(!t.departureTime_day ~ bike_lightsDensity, TRUE ~ 0)
  walk_lightNight = case_when(!t.departureTime_day ~ walk_lightsDensity, TRUE ~ 0)
  
  bike_ambience = case_when(t.departureTime_day ~ bike_vgvi, TRUE ~ bike_lightsDensity)
  walk_ambience = case_when(t.departureTime_day ~ walk_vgvi, TRUE ~ walk_lightsDensity)
  
  bike_stressLinkLogDistance = bike_stressLink * bike_logDistkm
  
  bike_stressJctDistance = bike_stressJct * bike_distkm
  
  bike_crimeDistance = bike_crime * bike_distkm
  
  bike_lightNightLogDistance = bike_lightNight * bike_logDistkm
  
  bike_ambienceLogDistance = bike_ambience * bike_logDistkm
  
  walk_vgviDay = case_when(t.departureTime_day ~ walk_vgvi, TRUE ~ 0)
  walk_lightNight = case_when(!t.departureTime_day ~ walk_lightsDensity, TRUE ~ 0)
  
  walk_stressLinkDistance = walk_stressLink * walk_distkm
  
  walk_stressLinkLogDistance = walk_stressLink * walk_logDistkm
  
  walk_stressJctDistance = walk_stressJct * walk_distkm
  
  walk_crimeDistance = walk_crime * walk_distkm
  
  walk_lightNightLogDistance = walk_lightNight * walk_logDistkm
  
  walk_ambienceLogDistance = walk_ambience * walk_logDistkm
})

trips = trips%>%filter(t.m_main_apollo!="X")

trips = trips%>%filter(bike_dist>0)

trips = trips%>%dummy_cols(select_columns = c("p.age_group_agg","p.female","p.occupation",
                                              "hh.cars_gr","hh.income_agg",
                                              "t.departureTime_day","t.full_purpose"))

trips = trips%>%filter(!t.full_purpose%in%c("HBW","HBE"))

# ################################################################# #
####                  CORRELATION TEST                  #############
# ################################################################# #

# sdCor = cor(trips%>%select('hh.cars','hh.bikes','hh.adults','hh.children5to9',"hh.carsPerAdult",
#                            'hh.children10to14',"hh.children",'hh.size','hh.workers'),
#            use = "complete.obs")
# corrplot::corrplot(sdCor, method = 'number')

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
outputDir <- "result/Manchester/discretionary/lcmnl/"
scenario = "lcmnl_v17"

### Load Apollo library
library(apollo)
library(dplyr)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = scenario,
  modelDescr = "LC_MNL",
  panelData  = FALSE,
  indivID    = "t.ID",
  nCores     = 16,
  outputDirectory = outputDir
)

# ################################################################# #
####      DEFINE MODEL PARAMETERS                                ####
# ################################################################# #

class =                                                     c("c1" , "c2")
#Define beta in class membership model 
beta_lc_matrix <- as.matrix(data.frame(
  asc                     = c(1, 0),
  p.age_group_agg_15_24   = c(1, 0),
  p.age_group_agg_25_39   = c(1, 0),
  p.female_FALSE          = c(1, 0),
  p.occupation_worker     = c(1, 0),
  hh.cars_gr_0            = c(1, 0),
  hh.cars_gr_1            = c(1, 0),
  hh.cars_gr_2            = c(1, 0),   
  hh.size                 = c(1, 0),
  hh.children5to9_yes     = c(1, 0),
  hh.children10to14_yes   = c(1, 0),
  t.full_purpose_HBR      = c(1, 0),
  t.full_purpose_HBS      = c(1, 0),
  t.full_purpose_HBA      = c(1, 0),
  t.full_purpose_HBO      = c(1, 0),
  t.departureTime_day_TRUE= c(1, 0),
  # hh.cars_agg             = c(1, 0),
  # Uncomment the lines below and replace 0 with the desired values
  # p.age_group_agg_5_14    = c(1, 0),
  # p.age_group_agg_40_54   = c(1, 0),
  # p.age_group_agg_55      = c(1, 0),
  # p.occupation_student    = c(1, 0),
  # hh.cars_gr_3            = c(1, 0),
  # hh.carsPerAdult         = c(1, 0),
  # hh.children             = c(1, 0),
  # hh.children_yes         = c(1, 0),
  row.names = class
))



#Define beta_gv in mode choice model 
options =                                                     c("carD" , "carP", "pt", "bike", "walk")
beta_gv_matrix =  as.matrix(data.frame(asc                   = c( 1     , 1     , 1    , 1      ,1),
                                       #p.age_group_agg_5_14  = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_5_24 = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_25_39 = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_40_69 = c( 1     , 0     , 0    , 0      ,0),
                                       #p.age_group_agg_70    = c( 1     , 0     , 0    , 0      ,0),
                                       #p.female_FALSE        = c( 1     , 0     , 0    , 0      ,0),
                                       #p.occupation_worker   = c( 1     , 0     , 0    , 0      ,0),
                                       #p.occupation_student  = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.carsPerAdult           = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_0          = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_1          = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_2          = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.cars_gr_3          = c( 1     , 0     , 0    , 0      ,0),
                                       #t.departureTime_gr_time_before6_after22    = c( 1     , 0     , 0    , 0      ,0),
                                       #t.full_purpose_HBE     = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.children5to9       = c( 1     , 0     , 0    , 0      ,0),
                                       #hh.children10to14     = c( 1     , 0     , 0    , 0      ,0),
                                       row.names = options))

#Define class-specific beta_av in mode choice model
beta_av_matrix =  as.matrix(data.frame(asc                     = c( 1     , 1     , 1    , 1      ,1),
                                       time                    = c( 1     , 1     , 1     , 0     ,0),
                                       logDistkm               = c( 0     , 0     , 0     , 1     ,1),
                                       stressLink              = c( 0     , 0     , 0     , 1     ,1),
                                       stressJctDistance               = c( 0     , 0     , 0     , 1     ,1),
                                       crimeDistance                   = c( 0     , 0     , 0     , 1     ,1),
                                       #shannon                 = c( 0     , 0     , 0     , 1     ,1),
                                       #ambience                = c( 0     , 0     , 0     , 1     ,1),
                                       #POIs                    = c( 0     , 0     , 0     , 1     ,1),
                                       vgviDay                 = c( 0     , 0     , 0     , 1     ,1),
                                       #lightNight              = c( 0     , 0     , 0     , 1     ,1),
                                       row.names = options))

beta_lc = apply(expand.grid(class,colnames(beta_lc_matrix))%>%mutate(Var2 = paste("gamma",Var2,sep = "_")), 1, paste, collapse="_")
beta_gv = apply(expand.grid(options,colnames(beta_gv_matrix)), 1, paste, collapse="gv.")

beta_av = apply(expand.grid(options,colnames(beta_av_matrix))%>%mutate(Var1 = paste(Var1,Var1,sep = "av.")), 1, paste, collapse="_")[as.numeric(beta_av_matrix) == 1]

beta_av_class=list()
for(n in class){
  beta_av_class = c(beta_av_class,paste0(beta_av,"_",n))
}

apollo_beta = c(rep(0,length(beta_lc)),rep(0,length(beta_gv)),rep(0,length(beta_av_class)))
names(apollo_beta) = c(beta_lc,beta_gv,beta_av_class)

apollo_beta = apollo_beta[names(apollo_beta) %in% c("carDav.carD_asc_c1","carDav.carD_time_c1",
                                                    "ptav.pt_asc_c2","ptav.pt_time_c2",
                                                    "bikeav.bike_asc_c2","bikeav.bike_logDistkm_c2","bikeav.bike_stressLink_c2",
                                                    "bikeav.bike_stressJctDistance_c2","bikeav.bike_crimeDistance_c2","bikeav.bike_vgviDay_c2"
                                                    ) == FALSE]

database = trips[,c("t.ID","t.m_main_apollo",paste("av",options,sep = "_"),
                    gsub("^.*?\\.","",beta_av),unique(c(colnames(beta_gv_matrix),colnames(beta_lc_matrix))))]


# Eliminate data with NA results,#check if mode share pattern changed a lot after removing NA
originalLength = nrow(database)
database = na.omit(database)
newLength = nrow(database)
print(paste(originalLength - newLength,"NA rows eliminated. New dataset has",newLength,"rows."))

database.x = database[,-c(1:12,which(colnames(database)=="asc"))]

nzv = nearZeroVar(database.x)
names(database.x)[nzv]
train.x_cor = cor(database.x)  # computes correlation
data_highCor = findCorrelation(train.x_cor, cutoff = .6)
names(database.x)[data_highCor]
# eventually remove linear dependencies (i.e. column is a multiple of another column)
lindep = findLinearCombos(database.x)
names(database.x)[lindep$remove]

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta,
###  use apollo_beta_fixed = c() if none
apollo_fixed = c(names(apollo_beta)[c(as.numeric(beta_lc_matrix) == 1,as.numeric(beta_gv_matrix) == 1,unlist(beta_av_class)==FALSE)],
                 "carPav.carP_asc_c1",
                 "carPav.carP_asc_c2")




# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #


apollo_lcPars = function(apollo_beta, apollo_inputs){
  
  lc = names(apollo_beta)[endsWith(names(apollo_beta),"_c1")|endsWith(names(apollo_beta),"_c2")]
  vars = unique(substr(lc,1,nchar(lc)-3))
  
  lcpars = list()

  for (var in vars) {
    varslc=names(apollo_beta)[startsWith(names(apollo_beta),var)]
    #lcpars[[var]] = list(get(paste0(var,"_c1")),get(paste0(var,"_c2")))
    
    var1 = get("c1_gamma_asc") ##fake object
    var2 = get("c1_gamma_asc")
    
    for(varlc in varslc){
      class = substr(varlc,nchar(varlc)-1,nchar(varlc))
      if(class=="c1"){
        var1=get(varlc)
      }else if(class=="c2"){
        var2=get(varlc)
      }
    } 
    
    lcpars[[var]] = list(var1,var2)
   
  }
  
  V=list()
  
  for(s in c("c1","c2")){
    beta = names(apollo_beta)[startsWith(names(apollo_beta),paste0(s,"_"))]
    val = 0
    for(n in 1:length(beta)) {
      val = val + get(beta[n]) * get(substr(beta[n],10,nchar(beta[n])))
    }
    
    V[[paste0("Class_",s)]] = val
  }
  
  
  classAlloc_settings = list(
    classes      = c(Class_c1=1, Class_c2=2), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #
apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #
#betaGv=names(apollo_beta)[startsWith(names(apollo_beta),"carDgv.")]
#betaAv=names(apollo_beta)[startsWith(names(apollo_beta),"ptav")&endsWith(names(apollo_beta),paste0("_","c1"))]
calcV <- function(i,betaGv,betaAv) {
  
  varsGv = gsub("^.*?\\.","",betaGv)
  varsAv = gsub("^.*?\\.","",betaAv)
  val = 0
  
  if(length(betaGv)>0){
    for(n in 1:length(varsGv)) {
      val = val + get(betaGv[n]) * get(varsGv[n])
    }
  }
  
  
  if(length(betaAv)>0){
    for(n in 1:length(betaAv)) {
      val = val + get(substr(betaAv[n], 1,nchar(betaAv[n])-3))[[i]] * get(substr(varsAv[n], 1,nchar(varsAv[n])-3))
    }
  }
  
  return(val)
}

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate_settings$bootstrapSE=TRUE"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  availa = list()
  availa[["c1"]] = list(carD=0, carP=1,pt=1, bike=1,walk=1)
  availa[["c2"]] = list(carD=av_carD, carP=1,pt=0, bike=0,walk=1)
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  ### Loop over classes
  for(i in 1:2){
    
    s = case_when(i == 1 ~ "c1",
                  i == 2 ~ "c2")
    
    ### Compute class-specific utilities
    V = list()
    
    V[['carD']] = calcV(i,names(apollo_beta)[startsWith(names(apollo_beta),"carDgv.")],names(apollo_beta)[startsWith(names(apollo_beta),"carDav.")&endsWith(names(apollo_beta),paste0("_",s))])
    V[['carP']] = calcV(i,names(apollo_beta)[startsWith(names(apollo_beta),"carPgv.")],names(apollo_beta)[startsWith(names(apollo_beta),"carPav.")&endsWith(names(apollo_beta),paste0("_",s))])
    V[['pt']]   = calcV(i,names(apollo_beta)[startsWith(names(apollo_beta),"ptgv.")],names(apollo_beta)[startsWith(names(apollo_beta),"ptav.")&endsWith(names(apollo_beta),paste0("_",s))])
    V[['bike']] = calcV(i,names(apollo_beta)[startsWith(names(apollo_beta),"bikegv.")],names(apollo_beta)[startsWith(names(apollo_beta),"bikeav.")&endsWith(names(apollo_beta),paste0("_",s))])
    V[['walk']] = calcV(i,names(apollo_beta)[startsWith(names(apollo_beta),"walkgv.")],names(apollo_beta)[startsWith(names(apollo_beta),"walkav.")&endsWith(names(apollo_beta),paste0("_",s))])
    
    
    mnl_settings = list(
      alternatives  = c(carD = "carD", carP = "carP", pt = "pt", walk = "walk", bike = "bike"),
      choiceVar     = t.m_main_apollo,
      avail         = availa[[s]],
      utilities     = V,
      componentName = paste0("Class_",s)
    )
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
  }
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #



model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                        estimate_settings=list(writeIter=FALSE,maxIterations=500))


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


r=1-model$LLout/model$LL0
#model=apollo_loadModel("lcmnl_v10.2_set")
predict = apollo_prediction(
  model,
  apollo_probabilities,
  apollo_inputs,
  prediction_settings = list(),
  modelComponent = NA
)

expectedValue = apollo_lcConditionals(model,
                                      apollo_probabilities,
                                      apollo_inputs)
indicators=trips[,c("t.ID","p.age_group_agg_5_14","p.age_group_agg_15_24",
                    "p.age_group_agg_25_39","p.age_group_agg_40_54","p.age_group_agg_55",
                    "p.female_FALSE","p.female_TRUE","p.occupation_worker","p.occupation_student",
                    "hh.cars","hh.children5to9","hh.children10to14","hh.size")]
expectedValue = expectedValue%>%
  gather(class,weight,2:3)%>%
  left_join(indicators,by=c("ID"="t.ID"))

summary=expectedValue[,-1]%>%
  group_by(class)%>%
  summarise(across(everything(), 
                   list(weighted = ~weighted.mean(., w = weight))))

summary=summary%>%gather(indicator, ev, 2:15)%>%spread(class,ev)
write.table(summary,"clipboard",row.names = F,sep = ";")

predict = predict$model %>% mutate(
  maxProb = pmax(carD,carP,pt,walk,bike),
  predictMode = case_when(maxProb == carD ~ "carD",
                          maxProb == carP ~ "carP",
                          maxProb == pt ~ "pt",
                          maxProb == walk ~ "walk",
                          TRUE ~ "bike")
)

database$predict = predict$predictMode

database%>%group_by(t.m_main_apollo,predict)%>%count()%>%spread(predict,n)




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

estimate_coeffsMatrix = rbind(estimate_coeffsMatrix,c("r2",r,"","",""))
print(estimate_coeffsMatrix, quote = FALSE)
write.csv(estimate_coeffsMatrix,file=paste(outputDir, scenario,".csv",sep = ""),row.names=TRUE,quote = FALSE) 
