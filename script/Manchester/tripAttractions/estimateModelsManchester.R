# This script estimates the trip attractions for the destination choice model without applying power transforms to the POI weights.


rm(list=ls())

# TODO : remove spaces from the labels

library(sf)
library(MASS)
library(modelsummary)
library(dplyr)
library(readr)

source("functions.R")

########################################
#==# READ IN DATASETS #================#
########################################

dataset <- sf::st_read("manchesterData/mappedAttractions.gpkg")
dataset <- dataset %>% st_drop_geometry()

table <- read_csv("manchesterData/table.csv")


## Apply transform
FLG_TR<-FALSE
n<-2
if(FLG_TR){
  #dataset$SCL <- dataset$SCL^(1/4)
  #dataset$EDU <- dataset$EDU^(1/1)
  #dataset$PHC <- dataset$PHC^(1/2)
  #dataset$CHR <- dataset$CHR^(1/2)
  
  #dataset$RSPF <- dataset$RSPF^(1/4)
  #dataset$EYA <- dataset$EYA^(1/2)
  #dataset$FR <- dataset$FR^(1/2)
  #dataset$EE <- dataset$EE^(1/2)
  
  #dataset$FIN <- dataset$FIN^(1/2)
  #dataset$SER <- dataset$SER^(1/2)
  # PT
  #dataset$POS <- dataset$POS^(1/4)
  
  #dataset$HH <- dataset$HH^(1/2)
  
  dataset$shop <- dataset$shop^(1/n)
  dataset$work <- dataset$work^(1/n)
  dataset$education <- dataset$education^(1/n)
  dataset$escort <- dataset$escort^(1/n)
  dataset$recreation <- dataset$recreation^(1/n)
  dataset$other <- dataset$other^(1/n)
}


########################################
#==# ESTIMATE EMPIRICAL MODELS #=======#
########################################

models= list()

models$work <- lm(work ~
                    0+
                    #EYA + 
                    #EDU + 
                    #FR + 
                    RSPF + 
                    #SCL + 
                    CHR +
                    EE + 
                    FIN +  
                    PHC + 
                    #POS + 
                    SER,
                    #HH,
                    data= dataset)

summary(models$work)

models$education <- lm(education ~
                    0+
                    EYA + 
                    EDU + 
                    #FR + 
                    #RSPF + 
                    #SCL + 
                    #CHR +
                    #EE + 
                    #FIN +  
                    #PHC + 
                    #POS + 
                    #SER +
                    HH,
                  data= dataset)

summary(models$education)

models$shop <- lm(shop ~
                    0+
                    # EYA + 
                    # EDU + 
                    FR + 
                    RSPF + 
                    #SCL + 
                    CHR +
                    EE +  
                    FIN #+hh  
                    # PHC + 
                    # POS + 
                    # SER,
                    , data= dataset %>% filter(shop>0))

summary(models$shop)

models$escort <- lm(escort ~
                      0+
                      EYA + 
                      EDU + 
                      FR + 
                      #RSPF + 
                      #SCL + 
                      CHR +
                      #EE + 
                      #FIN + 
                      #PHC + 
                      #POS +  is significant & positive if hh removed
                      HH  
                      #SER
                      , data= dataset %>% filter(escort>0))

summary(models$escort) # TODO: remove trip attractions to airport / %>% filter(IDspatial != "E01005316")

models$recreation <- lm(recreation ~
                          0+
                          #EYA + 
                          #EDU + 
                          #FR + 
                          RSPF + 
                          #SCL + 
                          #CHR +
                          EE + 
                          #FIN + 
                          PHC + 
                          POS + HH, 
                          #SER, 
                        data= dataset  %>% filter(recreation>0))

summary(models$recreation)

models$other <- lm(other ~
                     0+
                     #EYA + 
                     EDU + 
                     FR + 
                     #RSPF + 
                     #SCL + 
                     CHR +
                     EE + 
                     #FIN + 
                     PHC + 
                     #POS + 
                     HH 
                     #SER
                     , data= dataset %>% filter(other>0))

summary(models$other)

models$rrt <- lm(rrt ~
                     0+
                     #EYA + 
                     #EDU + 
                     #FR + 
                     RSPF + 
                     #SCL + 
                     #CHR +
                     #EE + 
                     #FIN + 
                     PHC + 
                     POS + 
                     HH, 
                   #SER
                   , data= dataset %>% filter(rrt>0))

summary(models$rrt)

##

if(FLG_TR){
  modelsummary(models, output = "manchesterData/tripAttractionsCoefficients_withStats_with_PowerTransforms.png")
}else{
  modelsummary(models, output = "manchesterData/tripAttractionsNO_transformations.png")
}

gm <- modelsummary::gof_map
gm$omit <- TRUE
modelSummaryAtttractions<-modelsummary(models, output = "manchesterData/tripAttractionsCoefficients1.csv", fmt=6, statistic=NULL, gof_map = gm)
modelSummaryAtttractions<-modelSummaryAtttractions %>%
  mutate(Variable=term) %>% 
  dplyr::select(Variable, work, education, shop, escort, recreation, other)

## could be added somewhere else
#modelSummaryAtttractions <- modelSummaryAtttractions %>%
#  mutate(
#    Variable = case_when(
#      Variable == "Early.year.access" ~ "EYA",
#      Variable == "Education" ~ "EDU",
#      Variable == "Food.retail" ~ "FR",
#      Variable == "Recreational.sports.pitches.and.facilities" ~ "RSPF",
#      Variable == "Social.and.culture.locations" ~ "SCL",
#      Variable == "Community.health.resources" ~ "CHR",
#      Variable == "Eating.establishments" ~ "EE",
#      Variable == "Financial" ~ "FIN",
#      Variable == "Primary.health.care" ~ "PHC",
#      Variable == "Public.open.space" ~ "POS",
#      Variable == "Services" ~ "SER",
#      TRUE ~ Variable  # Keep other values unchanged
#    )
#  )

write.csv(modelSummaryAtttractions,  file = "manchesterData/tripAttractionsCoefficients2.csv", quote = FALSE, row.names = FALSE)

####
rowNames<- modelSummaryAtttractions$Variable
colNames<- table$Var2
A <- modelSummaryAtttractions %>%
  dplyr::select(work, education, shop, escort, other, recreation) %>%
  mutate(across(everything(), as.numeric)) %>%
  replace(is.na(.), 0)
B <- table %>% dplyr::select(work, education, shop, escort, other, recreation)
A<- as.matrix(A)
B <- t(as.matrix(B))
C<-A%*%B
df <- data.frame(C)
colnames(df) <- colNames
df$poi <- rowNames
df <- df %>% dplyr::select(poi, HBW, HBE, HBA, HBO, HBR, HBS, NHBO, NHBW)
# adjust SCL as it is rejected from all models
# Create a new row with zeros and poi as "SCL"
new_row <- data.frame(
  poi = "SCL",
  HBW = 0, HBE = 0, HBA = 0, HBO = 0, HBR = 0, HBS = 0, NHBO = 0, NHBW = 0
)
# Add the new row to the data frame
df <- bind_rows(df, new_row)
write.csv(df,  file = "manchesterData/tripAttractionsCoefficients3.csv", quote = FALSE, row.names = FALSE)
# TODO check that the multiplication is ok
