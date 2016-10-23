library(XLConnect)
library(foreign)
library(stats)
library(plyr)

bd_dhs <- read.dta("BDIR70FL.DTA")
drc_dhs <- read.dta("CDIR61FL.DTA")
et_dhs <- read.dta("ETIR61FL.DTA")
id_dhs <- read.dta("IDIR63FL.DTA")
mz_dhs <- read.dta("MZIR62FL.DTA")
ng_dhs <- read.dta("NGIR6AFL.DTA")
pk_dhs <- read.dta("PKIR61FL.DTA")

# This Function takes in a DHS Dataset and returns a an excel file sheet with a contingency table of initial careseeking for
# fever for the given country, breaking down facilities visited by urban/rural, wealth quintile, and region. 

initial.fever <- function (DHSdata){
  idlong <- deparse(substitute(DHSdata))
  id <- gsub("_dhs", "", idlong)
  careseeking.fever <- DHSdata[, c("h46a_1", "v190", "v024", "v025", "v005"), drop = FALSE]
  careseeking.fever$`v005`<- careseeking.fever$`v005`/1000000
  colnames(careseeking.fever) <- c("child 1", "wealth quintile", "region", "residence", "sample weight")
  
    if (id == "et") {
      careseeking.fever$"child 1" <- as.factor(careseeking.fever$"child 1")
      levels(careseeking.fever$"child 1") <- c("11"="Government hospital", "12"="Government health center", 
                                             "13"="Government health station/clinic", "14"="Government health post/hew", 
                                             "21"="Private hospital", "22"="Private clinic", 
                                             "23"="Pharmacy", "24"="NGO health facility", "25"="NGO VCHW", 
                                             "31"="Drug vendor/store", "32"="Shop", "33"="Traditional healer", "96"="Other", 
                                             "99"="Missing")
    }
  
    if (id == "ng") {
      careseeking.fever$"child 1" <- as.factor(careseeking.fever$"child 1")
      levels(careseeking.fever$"child 1") <- c("11"="Government hospital", "12"="Government health center", 
                                             "13"="Government health post", "14"="Mobile Clinic", "15" = "Fieldworker", 
                                             "16" = "Other public sector", "21"="Private hospital, clinic", "22"="Pharmacy", 
                                             "23"="Private Doctor", "24"="Chemist/PMS", "25"="Private Mobile Clinic", 
                                             "26"="Private Fieldworker", "27"="Other Private Medical Sector",
                                             "31"="Shop", "32"="Traditional Practitioner", "33"="Market", "96"="Other", 
                                             "99"="Missing")
    }
  
  total <- xtabs(careseeking.fever$`sample weight`~ careseeking.fever$`child 1`, data = careseeking.fever)
  
  urban.data <- careseeking.fever[careseeking.fever$residence=="urban", ]
  urban <- xtabs(urban.data$`sample weight`~ urban.data$`child 1`, data = urban.data)
  
  rural.data <- careseeking.fever[careseeking.fever$residence=="rural", ]
  rural <- xtabs(rural.data$`sample weight`~ rural.data$`child 1`, data = rural.data)
  
  poorest.data <- careseeking.fever[careseeking.fever$"wealth quintile"=="poorest", ]
  poorest <- xtabs(poorest.data$`sample weight`~ poorest.data$`child 1`, data = poorest.data)
  
  poorer.data <- careseeking.fever[careseeking.fever$"wealth quintile"=="poorer", ]
  poorer <- xtabs(poorer.data$`sample weight`~ poorer.data$`child 1`, data = poorer.data)
  
  middle.data <- careseeking.fever[careseeking.fever$"wealth quintile"=="middle", ]
  middle <- xtabs(middle.data$`sample weight`~ middle.data$`child 1`, data = middle.data)
  
  richer.data <- careseeking.fever[careseeking.fever$"wealth quintile"=="richer", ]
  richer <- xtabs(richer.data$`sample weight`~ richer.data$`child 1`, data = richer.data)
  
  richest.data <- careseeking.fever[careseeking.fever$"wealth quintile"=="richest", ]
  richest <- xtabs(richest.data$`sample weight`~ richest.data$`child 1`, data = richest.data)
  
  numbersDF <- as.data.frame(rbind(total, urban, rural, poorest, poorer, middle, richer, richest))
  
  regions <- names(summary(careseeking.fever$region))
  regionsDF <- data.frame()
  
  for (region in regions) {
    assign(region, get(("regions")))
    region.data <- careseeking.fever[careseeking.fever$"region"== region, ]
    region <- xtabs(region.data$`sample weight`~ region.data$`child 1`, data = region.data)
    regionsDF <- as.data.frame(rbind(regionsDF, region))
  }
  
  colnames(regionsDF) <- colnames(numbersDF)
  
  allDF <- as.data.frame(rbind(numbersDF, regionsDF))
  allDF$`NA's`<- NULL
  allDF$total <- rowSums(allDF)
  
    percenter <- function(x){
      100*(x/allDF$total)
    }
  
  percentDF <- as.data.frame(apply(allDF, 2, percenter))
  percentDF <- round(percentDF, digits = 1)
  group <- c(c("total", "urban", "rural", "poorest", "poorer", "middle", "richer", "richest"), regions)
  percentDF <- cbind(group, percentDF)
  
  writeWorksheetToFile("initial.fever.xlsx", data = percentDF, sheet = paste(id, "initial fever"))
}

initial.fever(bd_dhs)
initial.fever(drc_dhs)
initial.fever(et_dhs)
initial.fever(id_dhs)
initial.fever(mz_dhs)
initial.fever(ng_dhs)

 
 


