source("Analysis/TLSADataPreparation.R")

## Summary statistics ----

# Something like this, where 
## G1: Conditional on ADL, 
## G2: Conditional on ADL and having a spouse
## G3: Conditional on ADL and not having a spouse

## Columns (pre-determined): N, Age, sex, edu, married, children
## Columns (health status): ADL
## Columns (choices): Live together, work
## Columns (care-arrangement): share of each option
  
#     |N         |Age       | Sex       | Work      |
#     |N Unique N| Sub |Fam   |Sub |Fam   |Sub |Fam   |  
# All |
# G1  |
# G2  |



## Generate rows of the data ------

valueN <- c(subject.dt[,.N], subject.dt[nADL>0,.N], subject.dt[nADL>0 & marstat == 1,.N], subject.dt[nADL>0 & marstat == 0,.N])
valueN_Unique <- c(subject.dt[,length(unique(qser_no))], subject.dt[nADL>0,length(unique(qser_no))],
                   subject.dt[nADL>0 & marstat == 1,length(unique(qser_no))], subject.dt[nADL>0 & marstat == 0,length(unique(qser_no))])
valueNFam <- c(fmlymmb.dt[,.N], fmlymmb.dt[nADL>0,.N], fmlymmb.dt[nADL>0 & marstat.y == 1,.N], fmlymmb.dt[nADL>0 & marstat.y == 0,.N])
valueNFam_Unique <- c(fmlymmb.dt[,length(unique(unif))], fmlymmb.dt[nADL>0,length(unique(unif))],
                      fmlymmb.dt[nADL>0 & marstat.y == 1,length(unique(unif))], fmlymmb.dt[nADL>0 & marstat.y == 0,length(unique(unif))])

valueSex <- c(subject.dt[,mean(rsex)], subject.dt[nADL>0,mean(rsex)], subject.dt[nADL>0 & marstat == 1,mean(rsex)], subject.dt[nADL>0 & marstat == 0,mean(rsex)])
valueSexFam <- c(fmlymmb.dt[,mean(male)], fmlymmb.dt[nADL>0,mean(male)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(male)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(male)])

valueAge <- c(subject.dt[,mean(rage)], subject.dt[nADL>0,mean(rage)], subject.dt[nADL>0 & marstat == 1,mean(rage)], subject.dt[nADL>0 & marstat == 0,mean(rage)])
valueAgeFam <- c(fmlymmb.dt[,mean(age)], fmlymmb.dt[nADL>0,mean(age)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(age)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(age)])

valueEdu <- c(subject.dt[,mean(reduc)], subject.dt[nADL>0,mean(reduc)], subject.dt[nADL>0 & marstat == 1,mean(reduc)], subject.dt[nADL>0 & marstat == 0,mean(reduc)])
valueEduFam <- c(fmlymmb.dt[,mean(edu)], fmlymmb.dt[nADL>0,mean(edu)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(edu)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(edu)])

valueMarried <- c(subject.dt[,mean(married)], subject.dt[nADL>0,mean(married)], subject.dt[nADL>0 & marstat == 1,mean(married)], subject.dt[nADL>0 & marstat == 0,mean(married)])
valueMarriedFam <- c(fmlymmb.dt[,mean(married.x)], fmlymmb.dt[nADL>0,mean(married.x)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(married.x)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(married.x)])

valueChild <- c(subject.dt[,mean(have_child)], subject.dt[nADL>0,mean(have_child)], subject.dt[nADL>0 & marstat == 1,mean(have_child)], subject.dt[nADL>0 & marstat == 0,mean(have_child)])
valueChildFam <- c(fmlymmb.dt[,mean(have_child.x)], fmlymmb.dt[nADL>0,mean(have_child.x)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(have_child.x)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(have_child.x)])

valueADL <- c(subject.dt[,mean(nADL)], subject.dt[nADL>0,mean(nADL)], subject.dt[nADL>0 & marstat == 1,mean(nADL)], subject.dt[nADL>0 & marstat == 0,mean(nADL)])
valueIADL <- c(subject.dt[,mean(nIADL)], subject.dt[nADL>0,mean(nIADL)], subject.dt[nADL>0 & marstat == 1,mean(nIADL)], subject.dt[nADL>0 & marstat == 0,mean(nIADL)])


valueWork <- c(subject.dt[,mean(rwstat==1)], subject.dt[nADL>0,mean(rwstat==1)], subject.dt[nADL>0 & marstat == 1,mean(rwstat==1)], subject.dt[nADL>0 & marstat == 0,mean(rwstat==1)])
valueWorkFam <- c(fmlymmb.dt[,mean(work==1)], fmlymmb.dt[nADL>0,mean(work==1)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(work==1)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(work==1)])

valueColiveFam <- c(fmlymmb.dt[,mean(livstat==0)], fmlymmb.dt[nADL>0,mean(livstat==0)], fmlymmb.dt[nADL>0 & marstat.y == 1,mean(livstat==0)], fmlymmb.dt[nADL>0 & marstat.y == 0,mean(livstat==0)])
valueCareOption <- c(subject.dt[,mean(dwell1==2)], subject.dt[nADL>0,mean(dwell1==2)], subject.dt[nADL>0 & marstat == 1,mean(dwell1==2)], subject.dt[nADL>0 & marstat == 0,mean(dwell1==2)])

# Generate table ------

TR(valueSex, dec=2) + TR(valueSexFam, dec=2) + TR(valueAge, dec=2) + TR(valueAgeFam, dec=2) + 
  TR(valueEdu, dec=2) + TR(valueEduFam, dec=2) + TR(valueMarried, dec=2) + TR(valueMarriedFam, dec=2) + 
  TR(valueChild, dec=2) + TR(valueChildFam, dec=2) + TR(valueADL, dec=2) + TR(valueIADL, dec=2) + 
  TR(valueWork, dec=2) + TR(valueWorkFam, dec=2) + TR(valueColiveFam, dec=2) + TR(valueCareOption, dec=2) + midrule() +
  TR(valueN, dec=0) + TR(valueN_Unique, dec=0) + TR(valueNFam, dec=0) + TR(valueNFam_Unique, dec=0) 
  
dtPrint <- as.data.table(rbind(valueSex, valueSexFam, valueAge, valueAgeFam,
                valueEdu, valueEduFam, valueMarried, valueMarriedFam,
                valueChild, valueChildFam, valueADL, valueIADL, 
                valueWork, valueWorkFam, valueColiveFam, valueCareOption,
                valueN, valueN_Unique,valueNFam,valueNFam_Unique),keep.rownames = TRUE)
dtPrint[,V0 := c(rep(c("Ind", "Fam"),5),"Ind","Ind","Ind","Fam","Fam","Ind","Ind","Ind","Fam","Fam")]
dtPrint[,rn := gsub(rn,pattern = "value",replacement = "")]
setcolorder(dtPrint,c("rn","V0","V1","V2","V3","V4"))
print(xtable(dtPrint,digits = 2),booktabs = TRUE,include.rownames = FALSE)

## Classification trees ----

simple.tree <- rpart(primaryADL ~ male + age + age**2 + rsex + rage + rage**2 + 
                       edu + partner + SubSameSex + nADL + nIADL + 
                       unmarried , data = fmlymmb.dt[!is.na(primaryADL)], cp=.008)
rpart.plot(simple.tree, faclen=0, 
           box.palette="RdBu", 
           fallen.leaves=TRUE, 
           shadow.col="gray", 
           nn=TRUE)


simple.tree <- rpart(primaryADL ~ male + age + age**2 + rsex + rage + rage**2 + 
                       edu + partner + SubSameSex + 
                       unmarried , data = fmlymmb.dt[!is.na(primaryADL)], cp=.008)
rpart.plot(simple.tree, faclen=0, 
           box.palette="RdBu", 
           fallen.leaves=TRUE, 
           shadow.col="gray", 
           nn=TRUE)

## Look at G2 and G3, and then the group that having a children as primary caregiver. 

# The classification tree by one's base might not be the most informative way. 
## What we are really thinking of is the family level specification there. 
simple.tree <- rpart(careArngUpperLevel ~ nADL + nIADL + rage + rsex + married, data = subject.dt)
simple.tree <- rpart(careArngGrand ~ nADL + nIADL + rage + rsex + married, data = subject.dt[careArngUpperLevel == "ChildrenGen"])
rpart.plot(simple.tree, faclen=0, 
           box.palette="RdBu", 
           fallen.leaves=TRUE, 
           shadow.col="gray", 
           nn=TRUE)


## So should look at the tree at the family level then. But then the family specifciation is different. 

# There should be some parametric way of controlling the family structure in a parametric way:
# We should conditional on not being spouse and ask the following question:
#   1. Whether a spouse alive (by subject. married.x)
#   2. Whether you are the only kid (by subject. nChildren)
#   3. Whether you are the only daughter ()
#   4. Whether you are the only son
#   5. Whether you are the oldest kid 
#   6. Whether you are the youngest kid

  
fmlymmb.dt[, onlySon := sum(relCate == "Son") == 1, by = .(qser_no,survey_year)]
fmlymmb.dt[, onlyDaughter := sum(relCate == "Daughter") == 1, by = .(qser_no,survey_year)]
fmlymmb.dt[, oldestKid := age == max(age[relCate %in% c("Son", "Daughter")],na.rm=TRUE) & relCate %in% c("Son", "Daughter") , by = .(qser_no,survey_year)]
fmlymmb.dt[, youngestKid := age == min(age[relCate %in% c("Son", "Daughter")],na.rm=TRUE) & relCate %in% c("Son", "Daughter") , by = .(qser_no,survey_year)]


fmlymmb.dt[, sumAllPossible := sum(relCate == "Son") + sum(relCate == "Daughter") + sum(relCate == "Daughter-In-Law"), by = .(qser_no,survey_year)]

  # Regression model:
## The gender model. 
## (Let me first estimate potential wage from other places for the female.)

est <- feols(primaryADL ~ male + I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
        edu + partner + onlySon +onlyDaughter + oldestKid + youngestKid + SubSameSex + 
        unmarried|survey_year, data = fmlymmb.dt[nADL >0 ])


est0 <- feols(primaryADL ~  I(relCate=="Daughter") + I(relCate == "Daughter-In-Law") +  I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
                edu + unmarried|survey_year, data = fmlymmb.dt[nADL >0 & relCate %in% c("Son", "Daughter","Daughter-In-Law") ])

est1 <- feols(primaryADL ~  I(relCate=="Daughter") + I(relCate == "Daughter-In-Law") +  I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
               edu + onlySon +onlyDaughter + oldestKid + youngestKid + SubSameSex + 
               unmarried|survey_year, data = fmlymmb.dt[nADL >0 & relCate %in% c("Son", "Daughter","Daughter-In-Law") ])

est2 <- feols(primaryADL ~  I(relCate=="Daughter") + I(relCate == "Daughter-In-Law") +  
               I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
               I(relCate=="Daughter")*unmarried + married.y +
               edu + SubSameSex + 
               unmarried|survey_year, data = fmlymmb.dt[nADL >0 & relCate %in% c("Son", "Daughter","Daughter-In-Law") ])
est3 <- feols(primaryADL ~  I(relCate=="Daughter") + I(relCate == "Daughter-In-Law") +  
                I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
                I(relCate=="Daughter")*unmarried + married.y +
                edu + SubSameSex + 
                unmarried + sumAllPossible|survey_year, data = fmlymmb.dt[nADL >0 & relCate %in% c("Son", "Daughter","Daughter-In-Law") ])
cwpm <- readRDS("correctedWagePredictionModel.rds")
fmlymmb.dt[, lnPredictedWage := cwpm["(Intercept)"] + male* cwpm["maleTRUE"]  +
             age*cwpm["age"] + age^2 * cwpm["I(age^2)"] + edu* cwpm["eduYr"] + married.x * cwpm["marriedTRUE"] + 
             married.x * male * cwpm["maleTRUE:marriedTRUE"]]


est4 <- feols(primaryADL ~   I(relCate=="Daughter") + I(relCate == "Daughter-In-Law") +  
                I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
                I(relCate=="Daughter")*unmarried + married.y +
                edu + SubSameSex + 
                unmarried + sumAllPossible+lnPredictedWage|survey_year, data = fmlymmb.dt[nADL >0 & relCate %in% c("Son", "Daughter","Daughter-In-Law") ])

# Add number of daughters/sons/in-laws
etable(est0, est1, est2, est3, est4 ,tex = TRUE)
summary(est)

## Look at dynamic concern and then replicate the models by Stern. 

## Examine Stability -----
setorder(subject.dt, qser_no, survey_year)
subject.dt[, lagged_help:= shift(adl_who_help), by = qser_no]


subject.dt[, mean(adl_who_help==lagged_help,na.rm=TRUE)]
subject.dt[lagged_help <= 60 & adl_who_help <= 60& lagged_help != 0 & adl_who_help != 0, table(adl_who_help,lagged_help)]
subject.dt[lagged_help <= 60 & adl_who_help <= 60& lagged_help != 0 & adl_who_help != 0, mean(adl_who_help==lagged_help,na.rm=TRUE)]

