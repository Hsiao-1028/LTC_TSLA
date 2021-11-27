library(data.table)
library(texreg)

library(sampleSelection)

fromStr <- substr(95:116, nchar(95:116)-1, nchar(95:116))
toStr <- substr(96:117, nchar(96:117)-1, nchar(96:117))

dt <- mapply(SIMPLIFY = FALSE, fromStr, toStr, FUN = function(fromFile, toFile){
  cat(fromFile,"\n")
  dt <- fread(paste0("~/Documents/ltcTW/data/mus/MUS_stata/mergemp",fromFile,"_",toFile,".csv"))
  dt <- dt[sampleid_1 == sampleid_2 & memberid_1 == memberid_2]
  dt[, indID := paste0(sampleid_1, ".", memberid_1)]
  
  
  if(fromFile %in% c("95")){
    setnames(dt, c("b40_a1_1","b40_a2_1","b40_a3_1","b40_a4_1","b40_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b40_a1_2","b40_a2_2","b40_a3_2","b40_a4_2","b40_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("96")){
    setnames(dt, c("b40_a1_1","b40_a2_1","b40_a3_1","b40_a4_1","b40_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b38_a1_2","b38_a2_2","b38_a3_2","b38_a4_2","b38_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("97")){
    setnames(dt, c("b38_a1_1","b38_a2_1","b38_a3_1","b38_a4_1","b38_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b38_a1_2","b38_a2_2","b38_a3_2","b38_a4_2","b38_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("98")){
    setnames(dt, c("b38_a1_1","b38_a2_1","b38_a3_1","b38_a4_1","b38_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b17_a1_2","b17_a2_2","b17_a3_2","b17_a4_2","b17_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("06","07","08","09","10","11")){
    setnames(dt, c("b18_a1_1","b18_a2_1","b18_a3_1","b18_a4_1","b18_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b18_a1_2","b18_a2_2","b18_a3_2","b18_a4_2","b18_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("12")){
    setnames(dt, c("b18_a1_1","b18_a2_1","b18_a3_1","b18_a4_1","b18_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b19_a1_2","b19_a2_2","b19_a3_2","b19_a4_2","b19_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else if(fromFile %in% c("13","14","15","16")){
    setnames(dt, c("b19_a1_1","b19_a2_1","b19_a3_1","b19_a4_1","b19_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b19_a1_2","b19_a2_2","b19_a3_2","b19_a4_2","b19_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }else{
    setnames(dt, c("b17_a1_1","b17_a2_1","b17_a3_1","b17_a4_1","b17_a5_1"), 
             c("nChildren1_1", "nChildren2_1", "nChildren3_1", "nChildren4_1", "nChildren5_1"))
    setnames(dt, c("b17_a1_2","b17_a2_2","b17_a3_2","b17_a4_2","b17_a5_2"), 
             c("nChildren1_2", "nChildren2_2", "nChildren3_2", "nChildren4_2", "nChildren5_2"))
    
  }

  var <- c("sampleid","year","sex","age","edu","marital","work","workhour","hussize",
           "income","nChildren1","nChildren2","nChildren3","nChildren4","nChildren5")
  
  selectedVar <- c(paste0(var, "_1"), paste0(var, "_2"))
  
  
  dt <- dt[, c("indID", selectedVar), with = FALSE]
  
  dt <- reshape(dt, direction = "long", idvar="indID", 
          varying = 2:ncol(dt), sep = "_")
  dt[is.na(nChildren1), nChildren1 := 0]
  dt[is.na(nChildren2), nChildren2 := 0]
  dt[is.na(nChildren3), nChildren3 := 0]
  dt[is.na(nChildren4), nChildren4 := 0]
  dt[is.na(nChildren5), nChildren5 := 0]
  dt[, nChildren := nChildren1 + nChildren2 + nChildren3 + nChildren4 + nChildren5]
  dt[, c("nChildren1","nChildren2","nChildren3","nChildren4","nChildren5") := NULL]
  return(dt)
  
})

dt <- rbindlist(dt, use.names = TRUE,fill = TRUE)


dt[, time := NULL]
dt[, male := sex == "Male"]
dt[, married := marital == "Married and cohabited"]

dt[, working := work %in% c("Undertaking a kind of work","Undertaking some kind of work")]
dt[is.na(workhour), workhour := 0]

dt[, income := as.integer(income)]
dt[is.na(income), income := 0]

dt[edu == "Illiterate", eduYr := 0]
dt[edu ==  "Junior college", eduYr := 14]
dt[edu == "Junior high school", eduYr := 9]
dt[edu == "Master's", eduYr := 18]
dt[edu == "Ph. D's", eduYr := 21]
dt[edu == "Primary school", eduYr := 6]
dt[edu == "Self-educated", eduYr := 6]
dt[edu == "Senior high school", eduYr := 12]
dt[edu == "University", eduYr := 16]
dt[edu == "Vocational school", eduYr := 12]

dt[, wageRate := income/(workhour*4.5)]
dt[, HHOtherIncome := sum(income) - income, by = .(sampleid,year)]

feols(log(wageRate) ~ male + age + age^2 + eduYr + married + married*male|year, data = dt[age <= 65 & age >= 25])
feols(working ~ male + age + age^2 + eduYr + married + married*male + nChildren + hussize + HHOtherIncome|year, data = dt[age <= 65 & age >= 25])

# Merge CPI and put up real income. 
# Create the table. 
dtCPI <- readRDS("~/Documents/ltcTW/data/CPI.rds")



dt[dtCPI, on = c(year= "Year"), CPI := i.CPI]
dt <- dt[!(working == 1 & is.na(wageRate))]
dt[, wageRate := wageRate/CPI*100]
dt[, lnW := log(wageRate)]

dt[lnW == Inf|lnW == -Inf | working ==0, lnW := NA]
dt[, ageSq := age^2]

dt[, exper := age - eduYr]

fitInner <- lm(lnW ~ male + age + I(age^2) + eduYr + married + married*male, data = dt[age <= 65 & age >= 25 ])
fitOuter <- glm(working ~ male + age + I(age^2) + eduYr + married + married*male + nChildren + hussize + HHOtherIncome, data = dt[age <= 65 & age >= 25], family = binomial(link = "probit"))


twostage_fit <- heckit(
  selection = working ~ male + age+I( age^2 )+ eduYr + married + married*male + nChildren + hussize + HHOtherIncome,
  outcome = lnW ~ male + age + I( age^2 )   + eduYr + married + married*male,
  method = "ml",
  data = dt[age <= 65 & age >= 25]
)
summary(twostage_fit)


texreg(list(fitInner, fitOuter),booktabs = TRUE)
texreg(list(twostage_fit),booktabs = TRUE)


# 2016 price level log-hourly wage
correctedWagePredictionModel <- twostage_fit$estimate[11:17]
saveRDS(correctedWagePredictionModel,"correctedWagePredictionModel.rds")
