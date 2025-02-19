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
    transmute(variable = gsub("^p.|^hh.|^t.|group_|agg_|gr_","",
                              gsub("full_purpose_HBR","recreation_trip",
                                   gsub("full_purpose_HBO","other_trip",variable))),
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
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(),
                 cols_vary = "slowest",
                 names_to = c(".value","variable"),
                 names_pattern = "([^_]+)_(.*)",
                 names_prefix = "b_") %>%
    mutate(carD = ifelse(is.na(carD) & !is.na(car),car,carD),
           carP = ifelse(is.na(carP) & !is.na(car),car,carP)) %>%
    select(-car) %>%
    rbind(get_asc(c),.) %>%
    transmute(variable = gsub("^p.|^hh.|^t.|group_|agg_|gr_","",
                              gsub("full_purpose_HBR","recreation_trip",
                                   gsub("full_purpose_HBO","other_trip",variable))),
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
  
  coeff <- as.data.frame(rbind(estimation_data[1,]))
  
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
results$HBW <- getMITO("result/Manchester/modeChoice/work/dynamic10.csv")
results$HBE <- getMITO("result/Manchester/modeChoice/education/dynamic13.csv")
results$HBD <- getMITO("result/Manchester/modeChoice/discretionary/dynamic7.csv")
results$HBA <- getMITO("result/Manchester/modeChoice/accompany/dynamic6_final.csv")
results$NHBW <- getMITO("result/Manchester/modeChoice/nhbw/static1_final.csv")
results$NHBO <- getMITO("result/Manchester/modeChoice/nhbo/dynamic6_final.csv")

all_vars = c("asc","age_5_14","age_15_24","age_40_54","age_55_69","age_70",
             "female","occupation_worker","income_low","income_high",
             "recreation_trip","other_trip",
             "cars_0","cars_2","cars_3",
             "cost","grad","stressLink","stressLink_f","stressLink_c","vgvi","stressJct","speed","speed_c","speed_o")

combinedVars = list(cars_23 = c("cars_2","cars_3"),
                    age_5_24 = c("age_5_14","age_15_24"),
                    age_40_69 = c("age_40_54","age_55_69"),
                    age_55 = c("age_55_69","age_70"))


results2 <- lapply(results,rewrite)

for(purpose in names(results2)) {
  write_csv(results2[[purpose]],paste0("result/Manchester/modeChoice/MITO/mc_coefficients_",tolower(purpose),".csv"))
}

# ################################################################# #
  ######      FORMAT RESULTS FOR PUBLICATION/PRESENTATION       ###
# ################################################################# #

# Dynamic approach
test <- getMITOsig("result/Manchester/modeChoice/final/work_static.csv")
test2 <- getMITOsig("result/Manchester/modeChoice/work/dynamic10.csv")

results <- list()
results$HBW <- getMITOsig("result/Manchester/modeChoice/work/dynamic10.csv")
results$HBE <- getMITOsig("result/Manchester/modeChoice/education/dynamic13.csv")
results$HBD <- getMITOsig("result/Manchester/modeChoice/discretionary/dynamic7.csv")
results$HBA <- getMITOsig("result/Manchester/modeChoice/accompany/dynamic6_final.csv")
results$NHBW <- getMITOsig("result/Manchester/modeChoice/nhbw/static1_final.csv")
results$NHBO <- getMITOsig("result/Manchester/modeChoice/nhbo/dynamic6_final.csv")

# Static approach
results2 <- list()
results2$HBW <- getMITOsig("result/Manchester/modeChoice/work/static11.csv")
results2$HBE <- getMITOsig("result/Manchester/modeChoice/final/education_static.csv") # Previously had inconsistent starting LL.
results2$HBD <- getMITOsig("result/Manchester/modeChoice/final/discretionary_static.csv") # Previously had inconsistent starting LL.
results2$HBA <- getMITOsig("result/Manchester/modeChoice/final/accompany_static.csv") # Previously had inconsistent starting LL.
results2$NHBO <- getMITOsig("result/Manchester/modeChoice/final/nhbo_static.csv")

all_vars = c("asc",
             "cars_0","cars_1","cars_2","cars_3",
             "income_low","income_med","income_high",
             "age_5_14","age_15_24","age_25_39","age_40_54","age_55_69","age_70",
             "male","female",
             "occupation_worker","occupation_other",
             "recreation_trip","shopping_trip","other_trip",
             "cost","grad","stressLink","stressLink_f","stressLink_c",
             "vgvi","stressJct","speed","speed_c","speed_o")

results3 <- lapply(results,rewrite)
results4 <- lapply(results2,rewrite)

for(name in names(results)) {
  xlsx::write.xlsx(results3[[name]], file= "results.xlsx",sheetName = name, append = TRUE, row.names = FALSE, showNA = FALSE)
  if(name %in% names(results2)) {
    xlsx::write.xlsx(results4[[name]], file= "results.xlsx",sheetName = paste0(name,"s"), append = TRUE, row.names = FALSE, showNA = FALSE)
  }
}

# ################################################################# #
    ########################      MISC       #####################
# ################################################################# #

# Sociodemographic AND Built Environment Attributes (for presentation)
estimation_data <- read_csv("NHBO.csv") %>%
  filter((as.integer(Iteration) == max(as.integer(Iteration),na.rm = T)) | Iteration == "p.val") %>%
  select(-LL,-Iteration)
colnames(estimation_data) <- gsub("time","cost",colnames(estimation_data))

# Prepare result vector
test <- getMITOsig("result/Manchester/modeChoice/work/dynamic10.csv")


result = rep("",ncol(estimation_data))
names(result) <- names(estimation_data)
for(i in 1:ncol(estimation_data)) {
  result[i] <- paste0(round(estimation_data[1,i],3),sig(estimation_data[2,i]))
}
coeffSig <- as.data.frame(rbind(result))
coeff <- as.data.frame(rbind(estimation_data[1,]))


