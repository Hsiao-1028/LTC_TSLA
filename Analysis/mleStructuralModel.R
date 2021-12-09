# Estimate the structural model -----

source("Analysis/TLSADataPreparation.R")
library(maxLik)
library(matrixStats)
# Prepare for the likelihood sample ----

likelihoodVar <- c("relCate","age","lnPredictedWage","edu","unmarried","male",
                   "rage","rsex","nADL","nIADL","SubSameSex",
                   "primaryADL")


dtSample <- fmlymmb.dt[nADL > 0 & !is.na(careArng) & !is.na(primaryADL) ,c(likelihoodVar,"careArng","survey_year","unif","qser_no"), with = FALSE]

toBeDuplicated <- unique(dtSample[, c("qser_no","survey_year","rage","rsex","nADL","careArng","nIADL")])
toBeDuplicated <- rbind(copy(toBeDuplicated)[, relCate := "Institute"], copy(toBeDuplicated)[, relCate := "HiredCaregiver"]) 
toBeDuplicated[, primaryADL := as.numeric(relCate == careArng)]

toBeDuplicated[, age := 0]
toBeDuplicated[, lnPredictedWage := 0]
toBeDuplicated[, edu := 0]
toBeDuplicated[, unmarried := 0]
toBeDuplicated[, SubSameSex := 0]
toBeDuplicated[, male := 0]
toBeDuplicated[, unmarried := 0]




dtSample <- rbind(dtSample,toBeDuplicated, fill = TRUE)

dtSample[, careDaughter := relCate == "Daughter"]
dtSample[, careSon := relCate == "Son"]
dtSample[, careDaughterInLaw := relCate == "Daughter-In-Law"]
dtSample[, careInstitute := relCate == "Institute"]
dtSample[, careHiredCaregiver := relCate == "HiredCaregiver"]
dtSample[, careFamily := careDaughter+careSon+careDaughterInLaw]


# gamma[1]: Daughter Dummy, 
# gamma[2]: Son Dummy
# gamma[3]: Daughter-In-Law Dummy, 
# 
 gamma <-runif(26)
 dtSample <- dtSample[!is.na(unmarried)]
dt= copy(dtSample)
logLik <- function(gamma, dt){
  
  dt[, utility := gamma[1]*careDaughter + gamma[2]*careSon + gamma[3]*careDaughterInLaw + 
             gamma[4]*careInstitute + gamma[5]*careHiredCaregiver + 
       careFamily*(gamma[6]* lnPredictedWage + gamma[7]*0.1*age + gamma[8]*0.01*(age^2) + gamma[9]*edu + gamma[10]*SubSameSex + gamma[11]*unmarried) +
       careFamily        * (gamma[12]*0.1*rage +  gamma[13]*0.01*(rage^2) +  gamma[14]*rsex + gamma[15]*nADL + gamma[16]*nIADL ) +
       careInstitute     * (gamma[17]*0.1*rage +  gamma[18]*0.01*(rage^2) +  gamma[19]*rsex + gamma[20]*nADL + gamma[21]*nIADL ) +
       careHiredCaregiver* (gamma[22]*0.1*rage +  gamma[23]*0.01*(rage^2) +  gamma[24]*rsex + gamma[25]*nADL + gamma[26]*nIADL ) ]

  
  lnlike <- sum(dt[, sum(utility*primaryADL)    - logSumExp(utility) , by = .(qser_no, survey_year)]$V1)
  
  #cat(lnlike, ", ")
  return(lnlike)
}


print(Sys.time())
gg <- maxLik(logLik, start  = rep(0,26), dt= copy(dtSample),control = list(iterlim= 1000))
# gg <- maxLik(logLik, start  = c(-23.17213,-781.757,109.7304,52.79335,255.1432,-191.503,122.121,-12.38107,
#                                 12.30895,817.4588,-12.37746,-283.3043,-97.09345,100.8297,315.1771,153.6512,
#                                 -442.5574,-86.7559,119.8829,315.9471,153.2705,-499.5803,-82.89013,128.1817,316.6188,152.5982)
#              , dt= copy(dtSample),control = list(iterlim= 1000))
print(Sys.time())


summary(gg)

saveRDS(gg, "mleEstimate.rds")


# Think about how to put spouse. Currently it's in the baseline group. 

gammaEst <- runif(26)
dt[, utility := gammaEst[1]*careDaughter + gammaEst[2]*careSon + gammaEst[3]*careDaughterInLaw + 
     gammaEst[4]*careInstitute + gammaEst[5]*careHiredCaregiver + 
     careFamily*(gammaEst[6]* lnPredictedWage + gammaEst[7]*0.1*age + gammaEst[8]*0.01*(age^2) + gammaEst[9]*edu + gammaEst[10]*SubSameSex + gammaEst[11]*unmarried) +
     careFamily        * (gammaEst[12]*0.1*rage +  gammaEst[13]*0.01*(rage^2) +  gammaEst[14]*rsex + gammaEst[15]*nADL + gammaEst[16]*nIADL ) +
     careInstitute     * (gammaEst[17]*0.1*rage +  gammaEst[18]*0.01*(rage^2) +  gammaEst[19]*rsex + gammaEst[20]*nADL + gammaEst[21]*nIADL ) +
     careHiredCaregiver* (gammaEst[22]*0.1*rage +  gammaEst[23]*0.01*(rage^2) +  gammaEst[24]*rsex + gammaEst[25]*nADL + gammaEst[26]*nIADL ) ]


dt[, denominator := logSumExp(utility), by = .(qser_no, survey_year)]
dt[, probEst :=  exp(utility - denominator)]


#logLik(c(-345.86588, -203.45051,-1912.43483,50.86263,447.59113), dt)

## Marginal Effects -----
dt[primaryADL==1 & relCate == "Son", mean(probEst*(1-probEst))] * gammaEst
dt[primaryADL==1 & relCate == "Daughter", mean(probEst*(1-probEst))] * gammaEst
dt[primaryADL==1 & relCate == "Daughter-In-Law", mean(probEst*(1-probEst))] * gammaEst
dt[primaryADL==1 & relCate == "HiredCaregiver", mean(probEst*(1-probEst))] * gammaEst
dt[primaryADL==1 & relCate == "Institute", mean(probEst*(1-probEst))] * gammaEst

#head(sort(table(dtSample[,paste0(sort(relCate),collapse = " ") , by = qser_no]), decreasing = TRUE))


#sort(c(1,3,5,2,0),decreasing = TRUE)