library(reshape)
library(reshape2)
library('plot.matrix')

##########

# To compute the p-values
compute_pvalues<-function(m){
  # Compute p-values HBW
  ctable <- coef(summary(m))
  
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  
  ## combined table
  return(cbind(ctable, "p value" = p, "significance" = p<0.05))
}

# Combined variables for person model data
combinedVars = list(hh.size_12  = c("hh.size_1","hh.size_2"),
                    hh.size_123 = c("hh.size_1","hh.size_2","hh.size_3"),
                    hh.size_23  = c("hh.size_2","hh.size_3"),
                    hh.size_234 = c("hh.size_2","hh.size_3","hh.size_4"),
                    hh.size_2345 = c("hh.size_2","hh.size_3","hh.size_4","hh.size_5"),
                    hh.size_34  = c("hh.size_3","hh.size_4"),
                    hh.size_345 = c("hh.size_3","hh.size_4","hh.size_5"),
                    hh.size_45 = c("hh.size_4","hh.size_5"),
                    hh.adults_34 = c("hh.adults_3", "hh.adults_4"),
                    hh.children_12 = c("hh.children_1","hh.children_2"),
                    hh.children_23 = c("hh.children_2","hh.children_3"),
                    hh.children_123 = c("hh.children_1","hh.children_2","hh.children_3"),
                    hh.econStatus_23 = c("hh.econStatus_2","hh.econStatus_3"),
                    hh.econStatus_34 = c("hh.econStatus_3","hh.econStatus_4"),
                    hh.econStatus_234 = c("hh.econStatus_2","hh.econStatus_3","hh.econStatus_4"),
                    hh.cars_12 = c("hh.cars_1","hh.cars_2"),
                    hh.cars_23 = c("hh.cars_2","hh.cars_3"),
                    hh.cars_123 = c("hh.cars_1","hh.cars_2","hh.cars_3"),
                    p.age_gr_12 = c("p.age_gr_1","p.age_gr_2"),
                    p.age_gr_23 = c("p.age_gr_2","p.age_gr_3"),
                    p.age_gr_34 = c("p.age_gr_3","p.age_gr_4"),
                    p.age_gr_45 = c("p.age_gr_4","p.age_gr_5"),
                    p.age_gr_56 = c("p.age_gr_5","p.age_gr_6"),
                    p.age_gr_456 = c("p.age_gr_4","p.age_gr_5","p.age_gr_6"),
                    p.workTrips_12345 = c("p.workTrips_1","p.workTrips_2","p.workTrips_3","p.workTrips_4","p.workTrips_5"),
                    p.workTrips_1234 = c("p.workTrips_1","p.workTrips_2","p.workTrips_3","p.workTrips_4"),
                    p.eduTrips_12345 = c("p.eduTrips_1","p.eduTrips_2","p.eduTrips_3","p.eduTrips_4","p.eduTrips_5"),
                    p.eduTrips_1234 = c("p.eduTrips_1","p.eduTrips_2","p.eduTrips_3","p.eduTrips_4"))

integerVars = list(hh.size = 1:10,
                   hh.children = 1:3,
                   #hh.cars = 1:3,
                   p.workTrips = 1:5,
                   p.eduTrips = 1:5,
                   p.age_gr = 1:6,
                   hh.income = 1:3)

##########
compare_MCsimulation<-function(fitted_model){
  modelData <- fitted_model$model # data
  tripGen.var <- as.character(fitted_model[["call"]][["formula"]][[2]]) # outcome
  
  probability.matrix <- predict(fitted_model, type = "p")
  MC.prediction <- rep(NA, nrow(modelData))
  for(n in c(1:nrow(modelData))) {
    MC.prediction[n] <- sample(as.numeric(colnames(probability.matrix)),1,replace = TRUE,probability.matrix[n,])
  }
  
  
  dfi<-as.data.frame(cbind(modelData[,tripGen.var],MC.prediction))
  
  binwidth=1.
  alpha=1.
  
  Observed <- as.vector(table(dfi$V1))
  Predicted <- as.vector(table(dfi$MC.prediction))
  variable_z <- c(1:length(unique(modelData[,tripGen.var])))
  
  dfp <- data.frame(Observed, Predicted, variable_z)
  
  # plot((dfi$V1-dfi$MC.prediction)**2, main = "",
  #      xlab = "Survey", ylab = "OL Model",
  #      pch = 19, frame = FALSE)
  
  dfp <- melt(dfp, id.vars='variable_z')
  
  dfp %>%
    ggplot(aes(x=variable_z, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') +
    theme_classic() +
    labs(title = "HBW",
         y = "Frequency",
         x = "Number of trips")
  
  
  #print(table(dfi$V1, dfi$MC.prediction))
  # print(table(abs(dfi$V1-dfi$MC.prediction)))
  
  #plot(as.matrix(table(dfi$V1, dfi$MC.prediction)))
}

# This function returns the observed and predicted (MC simulation) weekly number of trips per person
compute_MCsimulation<-function(fitted_model){
  modelData <- fitted_model$model # data
  tripGen.var <- as.character(fitted_model[["call"]][["formula"]][[2]]) # outcome
  
  probability.matrix <- predict(fitted_model, type = "p")
  MC.prediction <- rep(NA, nrow(modelData))
  for(n in c(1:nrow(modelData))) {
    MC.prediction[n] <- sample(as.numeric(colnames(probability.matrix)),1,replace = TRUE,probability.matrix[n,])
  }
  
  
  return(as.data.frame(cbind(modelData[,tripGen.var],MC.prediction)))
}

# Print Polr model coefficients
printPolrCoeffTable <- function(listOfPolrModels, rowNames) {
  interceptNames <- c("1|2","2|3","3|4","4|5","5|6","6|7")
  coeffTable <- matrix(0, nrow = length(rowNames)+length(interceptNames), ncol = length(listOfPolrModels), dimnames = list(c(rowNames,interceptNames), names(listOfPolrModels)))
  
  for(purpose in names(listOfPolrModels)) {
    coefficients = listOfPolrModels[[purpose]][["coefficients"]]
    names(coefficients) = gsub("TRUE$","",names(coefficients))
    for(varName in names(coefficients)) {
      if(varName %in% rowNames) {
        coeffTable[varName,purpose] = coeffTable[varName,purpose] + coefficients[varName]
      }
      else if(varName %in% names(combinedVars)) {
        for(combinedVarName in combinedVars[[varName]]) {
          coeffTable[combinedVarName,purpose] = coeffTable[combinedVarName,purpose] + coefficients[varName]
        }
      }
      else if(varName %in% names(integerVars)) {
        for(integerVarNumber in integerVars[[varName]]) {
          integerVarName = paste(varName,integerVarNumber,sep = "_")
          coeffTable[integerVarName,purpose] = coeffTable[integerVarName,purpose] + coefficients[varName] * integerVarNumber
        }
      }
      else {
        stop(paste("Error!",varName, "not included!"))
      }
    }
    intercepts = listOfPolrModels[[purpose]][["zeta"]]
    interceptsNames = names(intercepts)
    for(n in 1:length(intercepts)) {
      coeffTable[interceptsNames[[n]],purpose] = intercepts[[n]];
    }
  }
  coeffTable <- tibble::rownames_to_column(as.data.frame(coeffTable), "variable")
  return(coeffTable)
}

# Print Hurdle model coefficients
printHurdleCoeffTable <- function(listOfHurdleModels, rowNames) {
  coeffTable <- list(zero  = matrix(0, nrow = length(rowNames)  , ncol = length(listOfHurdleModels), dimnames = list(rowNames,            names(listOfHurdleModels))),
                     count = matrix(0, nrow = length(rowNames)+1, ncol = length(listOfHurdleModels), dimnames = list(c(rowNames,"theta"), names(listOfHurdleModels))))
  
  for(hurdlePart in names(coeffTable)) {
    for(purpose in names(listOfHurdleModels)) {
      coefficients = listOfHurdleModels[[purpose]][["coefficients"]][[hurdlePart]]
      names(coefficients) = gsub("TRUE$","",names(coefficients))
      for(varName in names(coefficients)) {
        if(varName %in% rowNames) {
          coeffTable[[hurdlePart]][varName,purpose] = coeffTable[[hurdlePart]][varName,purpose] + coefficients[varName]
        }
        else if(varName %in% names(combinedVars)) {
          for(combinedVarName in combinedVars[[varName]]) {
            coeffTable[[hurdlePart]][combinedVarName,purpose] = coeffTable[[hurdlePart]][combinedVarName,purpose] + coefficients[varName]
          }
        }
        else if(varName %in% names(integerVars)) {
          for(integerVarNumber in integerVars[[varName]]) {
            integerVarName = paste(varName,integerVarNumber,sep = "_")
            coeffTable[[hurdlePart]][integerVarName,purpose] = coeffTable[[hurdlePart]][integerVarName,purpose] + coefficients[varName] * integerVarNumber
          }
        }
        else {
          stop(paste("Error!",varName, "not included!"))
        }
      }
      if(hurdlePart == "count") {
        coeffTable[[hurdlePart]]["theta",purpose] = listOfHurdleModels[[purpose]][["theta"]][[hurdlePart]]
      }
    }
    coeffTable[[hurdlePart]] <- tibble::rownames_to_column(as.data.frame(coeffTable[[hurdlePart]]), "variable")
  }
  return(coeffTable)
}