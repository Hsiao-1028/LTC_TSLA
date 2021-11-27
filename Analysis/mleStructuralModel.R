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
       careFamily*(gamma[6]* lnPredictedWage + gamma[7]*age + gamma[8]*(age^2) + gamma[9]*edu + gamma[10]*male + gamma[11]*unmarried) +
       
       careFamily        * (gamma[12]*rage +  gamma[13]*(rage^2) +  gamma[14]*rsex + gamma[15]*nADL + gamma[16]*nIADL ) +
       careInstitute     * (gamma[17]*rage +  gamma[18]*(rage^2) +  gamma[19]*rsex + gamma[20]*nADL + gamma[21]*nIADL ) +
       careHiredCaregiver* (gamma[22]*rage +  gamma[23]*(rage^2) +  gamma[24]*rsex + gamma[25]*nADL + gamma[26]*nIADL ) ]

  sum(dt[, sum(utility*primaryADL)    - logSumExp(utility) , by = .(qser_no, survey_year)]$V1)
}

gg <- maxLik(logLik, start  = rep(0,26), dt= copy(dtSample))
summary(gg)


logLik(c(-345.86588, -203.45051,-1912.43483,50.86263,447.59113), dt)
