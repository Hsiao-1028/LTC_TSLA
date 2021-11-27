library(data.table)
library(ggplot2)
#library(textables)
library(xtable)
library(rpart)
library(rpart.plot)
library(fixest)
library(mlogit)

subject.dt <- fread("main_subject_1111.csv") 
fmlymmb.dt <- fread("family_member_1111.csv")

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
fmlymmb.dt[,SubSameSex :=  male == rsex]

cwpm <- readRDS("correctedWagePredictionModel.rds")
fmlymmb.dt[, lnPredictedWage := cwpm["(Intercept)"] + male* cwpm["maleTRUE"]  +
             age*cwpm["age"] + age^2 * cwpm["I(age^2)"] + edu* cwpm["eduYr"] + married.x * cwpm["marriedTRUE"] + 
             married.x * male * cwpm["maleTRUE:marriedTRUE"]]


