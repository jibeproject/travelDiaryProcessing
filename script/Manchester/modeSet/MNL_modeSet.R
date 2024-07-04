library(tidyverse)
library(fastDummies)


### Clear memory
rm(list = ls())

outputDir <- "result/Manchester/modeSet/"
scenario = "modeSet_MNL_v6_sg"


# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
    modelName  = scenario,
    modelDescr ="modeSet",
    indivID    ="indiv.id",
    panelData  = FALSE,
    #weights    ="hh.expansionFactor",
    nCores     = 10,
    outputDirectory = outputDir
)




# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read_csv("data/manchester/modeSet/individualsForModeSet.csv")

database$INTERCEPT = 1
database = database %>% filter(!is.na(p.mode_combo))
database = database %>% mutate(hh.cars_agg = case_when(hh.cars > 3 ~ 3,
                                                       TRUE ~ hh.cars))
database = database %>% mutate(hh.bikes_agg = case_when(hh.bikes > 3 ~ 3,
                                                       TRUE ~ hh.bikes))
database = database %>% mutate(p.disability = case_when(p.disability =="a lot" ~ "lot",
                                                        p.disability =="a little" ~ "little",
                                                        TRUE ~ "none"))
database = database %>% mutate(hh.carPerAdult = case_when(hh.adults == 0 ~ 0,
                                                        TRUE ~ hh.cars/hh.adults))

database = database %>% mutate(hh.children = hh.children5to9+hh.children10to14)

database = database%>%dummy_cols(select_columns = c("p.age_group_agg","p.female","p.occupation",
                                                    "p.ethnicity","p.seasonTicket","p.disability",
                                                    "hh.cars_gr","hh.income_agg","hh.children5to9",
                                                    "hh.children10to14","hh.size","hh.workers",
                                                    "hh.bikes_gr","hh.children"))

#### DEFINE MODEL PARAMETERS
### Vector of parameters, including any that are kept fixed in estimation
options = unique(database$p.mode_combo)
ASCs = rep(0,length(options))
names(ASCs) = paste("asc",options, sep = "_")

modes  =                                                    c("auto", "pt", "cycle", "walk")

coeff =  as.matrix(data.frame(INTERCEPT             = c( 0     , 0     , 0   , 0),
                                       p.age_group_agg_5_14  = c( 1     , 0     , 0    , 1),
                                       p.age_group_agg_15_24 = c( 0     , 0     , 0    , 0),
                                       p.age_group_agg_25_34 = c( 1     , 0     , 0    , 0),
                                       p.age_group_agg_35_44 = c( 1     , 1     , 0    , 0),
                                       #p.age_group_agg_45_64 = c( 0     , 0     , 0    , 0),
                                       p.age_group_agg_65_74    = c( 0     , 0     , 0    , 0),
                                       p.age_group_agg_75    = c( 0     , 0     , 0    , 0),
                                       p.female_TRUE        = c(0      , 0     , 0   , 0),
                                       p.occupation_worker   = c( 0     , 1     , 0    , 0),
                                       p.occupation_student  = c( 1     , 0     , 0    , 0),
                                       hh.cars_gr_0          = c( 0     , 0     , 0    , 0),
                                       #hh.cars_gr_1          = c( 0     , 0     , 0    , 0),
                                       hh.cars_gr_2          = c( 0     , 0     , 1    , 1),
                                       hh.cars_gr_3          = c( 1     , 0     , 0    , 0),
                                       hh.carPerAdult        = c( 0     , 0     , 0    , 0),
                                       #hh.bikes_agg          = c( 0     , 0     , 0    , 0),
                                       #hh.income_agg_low     = c( 0     , 0     , 0    , 0),
                                       #hh.income_agg_medium  = c( 0     , 0     , 0    , 0),
                                       #hh.income_agg_high    = c( 0     , 0     , 0    , 0),
                                       hh.children       = c( 1     , 0     , 1    , 1),
                                       #hh.size               = c( 0     , 0     , 0    , 0),
                                       #hh.workers            = c( 0     , 0     , 0    , 0),
                                       # p.isMobile_HBW        = c( 0     , 0     , 0    , 0),
                                       # p.log_km_mean_HBW     = c( 0     , 0     , 0    , 0),
                                       # p.isMobile_HBE        = c( 0     , 0     , 0    , 0),
                                       # p.log_km_mean_HBE     = c( 0     , 0     , 0    , 0),
                                       # p.isMobile_RRT        = c( 1     , 1     , 0    , 0),
                                       row.names = modes))

# coeff_av =  as.matrix(data.frame(      time                  = c( 1     , 1     , 0     ,0),
#                                        dist                  = c( 0     , 0     , 1     ,1),
#                                        stressLink            = c( 0     , 0     , 1     ,1),
#                                        stressJct             = c( 0     , 0     , 1     ,1),
#                                        shannon               = c( 0     , 0     , 1     ,1),
#                                        POIs                  = c( 0     , 0     , 1     ,1),
#                                        vgviDay               = c( 0     , 0     , 1     ,1),
#                                        crime                 = c( 0     , 0     , 1     ,1),
#                                        lightNight            = c( 0     , 0     , 1     ,1),
#                                        row.names = options))


coeffs = rep(0,length(coeff))

names(coeffs) = apply(expand.grid(modes,colnames(coeff)), 1, paste, collapse=".")


apollo_beta = c(ASCs, coeffs)
apollo_fixed = c("asc_Auto","asc_Pt","asc_Walk","asc_Cycle","asc_AutoPtCycleWalk",names(coeffs)[coeff==1])


database = database[,c("hh.id","indiv.id","p.mode_combo",colnames(coeff))]%>%arrange(hh.id)

# Eliminate data with NA results
originalLength = nrow(database)
database = na.omit(database)
newLength = nrow(database)
print(paste(originalLength - newLength,"NA rows eliminated. New dataset has",newLength,"rows."))
rm(originalLength,newLength)

# Print Useful information
str(database)

#### GROUP AND VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION
calcV <- function(beta_obj) {
  vars = gsub("^.*?\\.","",beta_obj)
  
  val = 0
  for(n in 1:length(vars)) {
    val = val + get(beta_obj[n]) * get(vars[n])
  }
  return(val)
}

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['Auto']]                        = asc_Auto + calcV(names(apollo_beta)[grepl("^auto\\.",names(apollo_beta))])
  V[['AutoPt']]                    = asc_AutoPt + calcV(names(apollo_beta)[grepl("^auto\\.|^pt\\.",names(apollo_beta))])
  V[['AutoPtCycle']]          = asc_AutoPtCycle + calcV(names(apollo_beta)[grepl("^auto\\.|^pt\\.|^cycle\\.",names(apollo_beta))])
  V[['AutoPtCycleWalk']]  = asc_AutoPtCycleWalk + calcV(names(apollo_beta)[grepl("^auto\\.|^pt\\.|^cycle\\.|^walk\\.",names(apollo_beta))])
  V[['AutoPtWalk']]            = asc_AutoPtWalk + calcV(names(apollo_beta)[grepl("^auto\\.|^pt\\.|^walk\\.",names(apollo_beta))])
  V[['AutoCycle']]              = asc_AutoCycle + calcV(names(apollo_beta)[grepl("^auto\\.|^cycle\\.",names(apollo_beta))])
  V[['AutoCycleWalk']]      = asc_AutoCycleWalk + calcV(names(apollo_beta)[grepl("^auto\\.|^cycle\\.|^walk\\.",names(apollo_beta))])
  V[['AutoWalk']]                = asc_AutoWalk + calcV(names(apollo_beta)[grepl("^auto\\.|^walk\\.",names(apollo_beta))])
  V[['Pt']]                            = asc_Pt + calcV(names(apollo_beta)[grepl("^pt\\.",names(apollo_beta))])
  V[['PtCycle']]                  = asc_PtCycle + calcV(names(apollo_beta)[grepl("^pt\\.|^cycle\\.",names(apollo_beta))])
  V[['PtCycleWalk']]          = asc_PtCycleWalk + calcV(names(apollo_beta)[grepl("^pt\\.|^cycle\\.|^walk\\.",names(apollo_beta))])
  V[['PtWalk']]                    = asc_PtWalk + calcV(names(apollo_beta)[grepl("^pt\\.|^walk\\.",names(apollo_beta))])
  V[['Cycle']]                      = asc_Cycle + calcV(names(apollo_beta)[grepl("^cycle\\.",names(apollo_beta))])
  V[['CycleWalk']]              = asc_CycleWalk + calcV(names(apollo_beta)[grepl("^cycle\\.|^walk\\.",names(apollo_beta))])
  V[['Walk']]                        = asc_Walk + calcV(names(apollo_beta)[grepl("^walk\\.",names(apollo_beta))])
  
  
  ### Define settings for MNL model component
  avail = list(Auto = 1,
               AutoPt = 1,
               AutoPtCycle = 1,
               AutoPtCycleWalk = 1,
               AutoPtWalk = 1,
               AutoCycle = 1,
               AutoCycleWalk = 1,
               AutoWalk = 1,
               Pt = 1,
               PtCycle = 1,
               PtCycleWalk = 1,
               PtWalk = 1,
               Cycle = 1,
               CycleWalk = 1,
               Walk = 1)
  
  alternatives = names(V)
  names(alternatives) = alternatives
  
  mnl_settings = list(
    alternatives  = alternatives,
    avail         = avail, 
    choiceVar     = p.mode_combo,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  #P = apollo_weighting(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

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

apollo_saveOutput(model)


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
estimateValues <- paste(round(model_estimates,3),"[",round(model_pValue,3),model_sig,"]")
estimateValues[model_estimates == 0] <- NA
names(estimateValues) = names(apollo_beta)

estimate_ASCs <- estimateValues[startsWith(names(apollo_beta),"asc")]
estimate_coeffs <- estimateValues[grepl("\\.",names(apollo_beta))]

estimate_coeffsMatrix <- matrix(estimate_coeffs,nrow = ncol(coeff), ncol = length(modes), byrow = TRUE, dimnames = list(colnames(coeff),modes))
r=1-model$LLout/model$LL0
print(estimate_ASCs)
print(estimate_coeffsMatrix, quote = FALSE)
