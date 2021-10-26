library(data.table)
library(ggplot2)
library(textables)
library(xtable)
library(rpart)
library(rpart.plot)
library(fixest)

subject.dt <- fread("main_subject_1026.csv") 
fmlymmb.dt <- fread("family_member_1010.csv")

## Define ADL -----
subject.dt[bath==9, bath := 3]
subject.dt[clothes==9, clothes := 3]
subject.dt[dine==9, dine := 3]
subject.dt[wakeup==9, wakeup := 3]
subject.dt[walk_indoor==9, walk_indoor := 3]
subject.dt[WC==9, WC := 3]

subject.dt[grocery==9, grocery := 3]
subject.dt[financial==9, financial := 3]
subject.dt[transport==9, transport := 3]
subject.dt[heavywork==9, heavywork := 3]
subject.dt[housework==9, housework := 3]
subject.dt[telehphone==9, telehphone := 3]


subject.dt[, nADL  := bath + clothes + dine + wakeup + walk_indoor + WC]
subject.dt[, nIADL := grocery + financial + transport + heavywork + housework + telehphone]

## Process Relationships ----
fmlymmb.dt$male = ifelse(fmlymmb.dt$male == 1, 1, 0)  # male 0 = females
fmlymmb.dt$partner = ifelse(fmlymmb.dt$told_relation == 2, 1, 0) 
fmlymmb.dt$working = ifelse(fmlymmb.dt$work == 1, 1, 0)
fmlymmb.dt$unmarried = ifelse(fmlymmb.dt$marstat == 6, 1, 0) 
fmlymmb.dt$attend_at_least_uni = ifelse(fmlymmb.dt$edu >= 13, 1, 0) 
fmlymmb.dt$attend_at_least_high_school = ifelse(fmlymmb.dt$edu >=10, 1, 0) 
# age above 15 & unmarry -> 6, if under 15 -> NA
fmlymmb.dt$livTogether = ifelse(fmlymmb.dt$livstat == 0, 1, 0)
fmlymmb.dt$primaryADL = ifelse(fmlymmb.dt$adl_who_help == fmlymmb.dt$told_relation, 1, 0)
fmlymmb.dt$primaryIADL = ifelse(fmlymmb.dt$iadl_who_help == 
                                  fmlymmb.dt$told_relation, 1, 0)
fmlymmb.dt$subject_residency = as.character(fmlymmb.dt$subject_residency)
subject.dt$rsex = ifelse(subject.dt$rsex == 1, 1, 0)

# Category of relationships ------
fmlymmb.dt[told_relation == 2, relCate := "Spouse"]
fmlymmb.dt[told_relation %in% 3:19 , relCate := "SeniorGen"]
fmlymmb.dt[told_relation %in% 21:29, relCate := "SiblingGen"]
fmlymmb.dt[told_relation == 30, relCate := "Son-In-Law"]
fmlymmb.dt[told_relation %in% 31:38 & male==1, relCate := "Son"]
fmlymmb.dt[told_relation %in% c(38,41:48) & male==0, relCate := "Daughter"]
fmlymmb.dt[told_relation %in% c(39,49), relCate := "JuniorGen"]
fmlymmb.dt[told_relation %in% 91:97, relCate := "Daughter-In-Law"]
fmlymmb.dt[told_relation %in% 51:62, relCate := "GrandChildGen"]
fmlymmb.dt[told_relation %in% c(73,77), relCate := "HiredCaregiver"]
fmlymmb.dt[is.na(relCate), relCate := "Other"]


# Classify care arrangement ------
subject.dt[adl_who_help == 0, careArng := "Self"]
subject.dt[adl_who_help == 2, careArng := "Spouse"]
subject.dt[adl_who_help %in% 3:19 , careArng := "SeniorGen"]
subject.dt[adl_who_help %in% 21:29, careArng := "SiblingGen"]
subject.dt[adl_who_help == 30, careArng := "Son-In-Law"]
subject.dt[adl_who_help %in% 31:37, careArng := "Son"]
subject.dt[adl_who_help %in% 41:48, careArng := "Daughter"]
subject.dt[adl_who_help %in% c(39,49), careArng := "JuniorGen"]
subject.dt[adl_who_help %in% 91:97, careArng := "Daughter-In-Law"]
subject.dt[adl_who_help %in% 51:62, careArng := "GrandChildGen"]
subject.dt[adl_who_help %in% c(73,77), careArng := "HiredCaregiver"]
subject.dt[is.na(careArng) & !is.na(adl_who_help), careArng := "Other"]
subject.dt[adl_who_help %in% 88, careArng := NA]
subject.dt[dwell1 == 2, careArng := "Institute"]
subject.dt[nADL == 0 & nIADL == 0, careArng := NA]

# ------ Care Arrangement Grand Category -------

subject.dt[,careArngGrand := careArng]
subject.dt[careArng %in% c("JuniorGen","Other","SeniorGen","SiblingGen","Son-In-Law","GrandChildGen"), careArngGrand := "Other"]
subject.dt[,careArngUpperLevel := careArngGrand]
subject.dt[careArngGrand %in% c("Daughter","Son","Daughter-In-Law"), careArngUpperLevel := "ChildrenGen"]
subject.dt[,careArngGrand := as.factor(careArngGrand)]
subject.dt[,careArngUpperLevel := as.factor(careArngUpperLevel)]



# Unify education, marital status, and children ----
subject.dt[reduc == 90, reduc := 6]
subject.dt[reduc == 90, reduc := NA]

fmlymmb.dt[is.na(marstat) & age <= 15, marstat := 6]

fmlymmb.dt[is.na(have_child) & age <= 15, have_child := 0]
fmlymmb.dt[is.na(number_of_children) & age <= 15, number_of_children := 0]
fmlymmb.dt[is.na(have_child) & marstat == 6, have_child := 0]
fmlymmb.dt[is.na(number_of_children) & marstat == 6, number_of_children := 0]

subject.dt[, have_child := nchldrn > 0]

subject.dt[, married := marstat==1]
fmlymmb.dt[, married := marstat==1]

fmlymmb.dt[,SubSameSex :=  male == rsex]

# Throw incomplete information

fmlymmb.dt[is.na(work), work := 8]
fmlymmb.dt[is.na(livstat), livstat := 0]
subject.dt <- subject.dt[!is.na(rage) & !is.na(reduc)]
fmlymmb.dt <- fmlymmb.dt[!is.na(male) & !is.na(age) & !is.na(edu) & !is.na(have_child) & !is.na(married)]


## Merge information ------
fmlymmb.dt <- merge(fmlymmb.dt, 
                    subject.dt, 
                    all.x = TRUE, 
                    by = c("qser_no", "survey_year"))

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

  # Regression model:
## The gender model. 
## (Let me first estimate potential wage from other places for the female.)

est <- feols(primaryADL ~ male + I(age <= 65) + age + age**2 + rsex + rage + rage**2 + nADL + nIADL + 
        edu + partner + onlySon +onlyDaughter + oldestKid + youngestKid + SubSameSex + 
        unmarried|survey_year, data = fmlymmb.dt[nADL >0 ])

summary(est)
## Look at dynamic concern and then replicate the models by Stern. 
