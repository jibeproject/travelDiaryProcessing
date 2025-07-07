# Setup
library(tidyverse)

# ################################################################# #
#####################      FUNCTIONS       #####################
# ################################################################# #

sig <- function(pVal) {
  return(case_when(pVal <= 0.001 ~ "***",
                   pVal <= 0.01 ~ "**",
                   pVal <= 0.05 ~ "*",
                   pVal <= 0.1 ~ "\`",
                   pVal <= 0.15 ~ ".",
                   TRUE ~ ""))
}

get_asc <- function(c) {
  asc <- c %>%
    select(starts_with("asc_")) %>%
    mutate(variable = "asc") %>%
    relocate(variable,.before = "asc_carD")
  names(asc) <- gsub("asc_","",names(asc))
  rownames(asc) <- 0
  return(asc)
}

get_MITO_sig <- function(coeff) {
  result <- coeff %>%
    select(starts_with("b_") | starts_with("g_")) %>%
    pivot_longer(cols = everything(),
                 cols_vary = "slowest",
                 names_to = c(".value","variable"),
                 names_pattern = "([^_]+)_(.*)",
                 names_prefix = "^b_|^g_") %>%
    mutate(carD = ifelse(is.na(carD) & !is.na(car),car,carD),
           carP = ifelse(is.na(carP) & !is.na(car),car,carP)) %>%
    select(-car) %>%
    rbind(get_asc(coeff),.) %>%
    transmute(variable,
              autoDriver = carD,
              autoPassenger = carP,
              pt,
              bicycle = bike,
              walk)
  
  result[is.na(result)] <- 0
  return(result)
}

get_MITO <- function(c) {
  c %>% 
    select(starts_with("b_") | starts_with("g_")) %>%
    pivot_longer(cols = everything(),
                 cols_vary = "slowest",
                 names_to = c(".value","variable"),
                 names_pattern = "([^_]+)_(.*)",
                 names_prefix = "^b_|^g_") %>%
    mutate(carD = ifelse(is.na(carD) & !is.na(car),car,carD),
           carP = ifelse(is.na(carP) & !is.na(car),car,carP)) %>%
    select(-car) %>%
    rbind(get_asc(c),.) %>%
    transmute(variable = gsub("^p.|^hh|^t.|group_|agg_|gr_","",variable),
              autoDriver = carD,
              autoPassenger = carP,
              pt,
              bicycle = bike,
              walk)
}


# Read in csv
getMITO <- function(path) {
  
  estimation_data <- read_csv(path) %>%
    filter((as.integer(Iteration) == max(as.integer(Iteration),na.rm = T)) | Iteration == "p.val") %>%
    select(-LL,-Iteration)
  colnames(estimation_data) <- gsub("time","cost",colnames(estimation_data))
  
  coeff <- as.data.frame(rbind(round(estimation_data[1,],10)))
  
  return(get_MITO(coeff))
}

getMITOsig <- function(path) {
  estimation_data <- read_csv(path) %>%
    filter((as.integer(Iteration) == max(as.integer(Iteration),na.rm = T)) | Iteration == "p.val") %>%
    select(-LL,-Iteration)
  colnames(estimation_data) <- gsub("time","cost",colnames(estimation_data))
  
  # Prepare result vector
  result = rep("",ncol(estimation_data))
  names(result) <- names(estimation_data)
  for(i in 1:ncol(estimation_data)) {
    result[i] <- paste0(round(estimation_data[1,i],3),sig(estimation_data[2,i]))
  }
  
  coeff <- as.data.frame(rbind(result))
  
  return(get_MITO_sig(coeff))
}

rewrite <- function(coeffTable) {
  if(!all(names(coeffTable) == c("variable","autoDriver","autoPassenger","pt","bicycle","walk"))) {
    stop("Row headers don't match!")
  }
  
  df = data.frame(variable = all_vars, autoDriver = 0,autoPassenger = 0,pt = 0,bicycle = 0,walk = 0)
  
  for(i in 1:nrow(coeffTable)) {
    var <- coeffTable[[i,1]]
    if(var %in% all_vars) {
      m <- match(var,all_vars)
      df[m,2:6] <- coeffTable[i,2:6]
    } else if (var %in% names(combinedVars)) {
      for(sep_var in combinedVars[[var]]) {
        m <- match(sep_var,all_vars)
        if(is.na(m)) {
          stop(paste0("Variable \"",sep_var,"\" from combined variable \"",var,"\" not found!"))
        }
        df[m,2:6] <- coeffTable[i,2:6]
      }
    } else {
      stop(paste0("Variable not found: ",var))
    }
  }
  
  df[df == 0] <- NA
  
  return(df)
}


# ################################################################# #
###############      FORMAT RESULTS FOR MITO       ##############
# ################################################################# #

results <- list()
results$HBW <- getMITO("result/Melbourne/modeChoice/HBW/HBW_dynamic9.csv")
results$HBE <- getMITO("result/Melbourne/modeChoice/HBE/HBE_dynamic3.csv")
results$HBR <- getMITO("result/Melbourne/modeChoice/HBR/HBR_dynamic14.csv")
results$HBSO <- getMITO("result/Melbourne/modeChoice/HBSO/HBSO_dynamic11.csv")
results$HBA <- getMITO("result/Melbourne/modeChoice/HBA/HBA_dynamic8.csv")
results$NHBW <- getMITO("result/Melbourne/modeChoice/NHBW/NHBW_dynamic2.csv")
results$NHBO <- getMITO("result/Melbourne/modeChoice/NHBO/NHBO_dynamic3.csv")

all_vars = c("asc",
             "age_under16","age_16_24","age_40_44","age_45_54","age_55_64","age_65up",
             "cars_0","cars_1","cars_2","cars_3",
             "female",
             "shopping_trip",
             "cost",
             "grad","grad_f","grad_c","stressLink","stressLink_f","stressLink_c",
             "vgvi","speed","speed_c")

combinedVars = list("cars_23" = c("cars_2","cars_3"),
                    "age_under25" = c("age_under16","age_16_24"),
                    "age_55up" = c("age_55_64","age_65up"),
                    "age_40_54" = c("age_40_44","age_45_54"),
                    "age_45_64" = c("age_45_54","age_55_64"))

results2 <- lapply(results,rewrite)

for(purpose in names(results2)) {
  write_csv(results2[[purpose]],paste0("MITO/withCostCoeffs/mc_coefficients_",tolower(purpose),".csv"),na = "0")
}

# ################################################################# #
######      FORMAT RESULTS FOR PUBLICATION/PRESENTATION       ###
# ################################################################# #

# Dynamic approach
setwd("~/Downloads/results")

results <- list()
results$HBW <- getMITOsig("HBW/HBW_dynamic9.csv")
results$HBE <- getMITOsig("HBE/HBE_dynamic3.csv")
results$HBR <- getMITOsig("HBR/HBR_dynamic14.csv")
results$HBSO <- getMITOsig("HBSO/HBSO_dynamic11.csv")
results$HBA <- getMITOsig("HBA/HBA_dynamic8.csv")
results$NHBW <- getMITOsig("NHBW/NHBW_dynamic2.csv")
results$NHBO <- getMITOsig("NHBO/NHBO_dynamic3.csv")

all_vars = c("asc",
             "age_gr_under16","age_gr_16_24","age_gr_40_44","age_gr_45_54","age_gr_55_64","age_gr_65up",
             "hhcars_0","hhcars_1","hhcars_2","hhcars_3",
             "female",
             "shopping_trip",
             "cost","grad","grad_f","grad_c","stressLink","stressLink_f","stressLink_c",
             "vgvi","stressJct","speed","speed_c","speed_o")

combinedVars = list("hhcars_23" = c("hhcars_2","hhcars_3"),
                    "age_gr_under25" = c("age_gr_under16","age_gr_16_24"),
                    "age_gr_55up" = c("age_gr_55_64","age_gr_65up"),
                    "age_gr_40_54" = c("age_gr_40_44","age_gr_45_54"),
                    "age_gr_45_64" = c("age_gr_45_54","age_gr_55_64"))


results3 <- lapply(results,rewrite)

for(name in names(results)) {
  xlsx::write.xlsx(results[[name]], file= "results.xlsx",sheetName = name, append = TRUE, row.names = FALSE, showNA = FALSE)
  # if(name %in% names(results2)) {
  #   xlsx::write.xlsx(results4[[name]], file= "results.xlsx",sheetName = paste0(name,"s"), append = TRUE, row.names = FALSE, showNA = FALSE)
  # }
}

for(purpose in names(results3)) {
  write_csv(results3[[purpose]],paste0("MITO/mc_coefficients_",tolower(purpose),".csv"))
}

# ################################################################# #
########################      MISC/JUNK       #####################
# ################################################################# #

# Sociodemographic AND Built Environment Attributes (for presentation)
estimation_data <- read_csv("NHBO.csv") %>%
  filter((as.integer(Iteration) == max(as.integer(Iteration),na.rm = T)) | Iteration == "p.val") %>%
  select(-LL,-Iteration)
colnames(estimation_data) <- gsub("time","cost",colnames(estimation_data))


result = rep("",ncol(estimation_data))
names(result) <- names(estimation_data)
for(i in 1:ncol(estimation_data)) {
  result[i] <- paste0(round(estimation_data[1,i],3),sig(estimation_data[2,i]))
}
coeffSig <- as.data.frame(rbind(result))
coeff <- as.data.frame(rbind(estimation_data[1,]))


