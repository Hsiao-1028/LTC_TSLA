# Estimate the structural model -----

source("Analysis/TLSADataPreparation.R")
library(maxLik)
library(matrixStats)
# Prepare for the likelihood sample ----

likelihoodVar <- c("relCate","age","lnPredictedWage","edu","unmarried",
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
 gamma <-runif(5)
dt= copy(dtSample)
logLik <- function(gamma, dt){
  
  dt[, utility := gamma[1]*careDaughter + gamma[2]*careSon + gamma[3]*careDaughterInLaw + 
             gamma[4]*careInstitute + gamma[5]*careHiredCaregiver]

  sum(dt[, sum(utility*primaryADL)    - logSumExp(utility) , by = .(qser_no, survey_year)]$V1)
}

gg <- maxLik(logLik, start  = c(0,0,0,0,0), dt= copy(dtSample))
summary(gg)


logLik(c(-345.86588, -203.45051,-1912.43483,50.86263,447.59113), dt)
