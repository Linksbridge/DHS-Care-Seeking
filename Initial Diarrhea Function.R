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
# Diarrhea for the given country, breaking down facilities visited by urban/rural, wealth quintile, and region. 

initial.diarrhea <- function (DHSdata){
  idlong <- deparse(substitute(DHSdata))
  id <- gsub("_dhs", "", idlong)
  careseeking.diarrhea <- DHSdata[, c("h44a_1", "v190", "v024", "v025", "v005"), drop = FALSE]
  careseeking.diarrhea$`v005`<- careseeking.diarrhea$`v005`/1000000
  colnames(careseeking.diarrhea) <- c("child 1", "wealth quintile", "region", "residence", "sample weight")
  
  if (id == "et") {
    careseeking.diarrhea$"child 1" <- as.factor(careseeking.diarrhea$"child 1")
    levels(careseeking.diarrhea$"child 1") <- c("11"="Government hospital", "12"="Government health center", 
                                             "13"="Government health station/clinic", "14"="Government health post/hew", 
                                             "15"="Other Public",
                                             "21"="Private hospital", "22"="Private clinic", 
                                             "23"="Pharmacy", "24"="NGO health facility", "25"="NGO VCHW", 
                                             "31"="Drug vendor/store", "32"="Shop", "33"="Traditional healer", "96"="Other", 
                                             "99"="Missing")
  }
  
  if (id == "ng") {
    careseeking.diarrhea$"child 1" <- as.factor(careseeking.diarrhea$"child 1")
    levels(careseeking.diarrhea$"child 1") <- c("11"="Government hospital", "12"="Government health center", 
                                             "13"="Government health post", "14"="Mobile Clinic", "15" = "Fieldworker", 
                                             "16" = "Other public sector", "21"="Private hospital, clinic", "22"="Pharmacy", 
                                             "23"="Private Doctor", "24"="Chemist/PMS", "25"="Private Mobile Clinic", 
                                             "26"="Private Fieldworker", "27"="Other Private Medical Sector",
                                             "31"="Shop", "32"="Traditional Practitioner", "33"="Market", "96"="Other", 
                                             "99"="Missing")
  }
  
  total <- xtabs(careseeking.diarrhea$`sample weight`~ careseeking.diarrhea$`child 1`, data = careseeking.diarrhea)
  
  urban.data <- careseeking.diarrhea[careseeking.diarrhea$residence=="urban", ]
  urban <- xtabs(urban.data$`sample weight`~ urban.data$`child 1`, data = urban.data)
  
  rural.data <- careseeking.diarrhea[careseeking.diarrhea$residence=="rural", ]
  rural <- xtabs(rural.data$`sample weight`~ rural.data$`child 1`, data = rural.data)
  
  poorest.data <- careseeking.diarrhea[careseeking.diarrhea$"wealth quintile"=="poorest", ]
  poorest <- xtabs(poorest.data$`sample weight`~ poorest.data$`child 1`, data = poorest.data)
  
  poorer.data <- careseeking.diarrhea[careseeking.diarrhea$"wealth quintile"=="poorer", ]
  poorer <- xtabs(poorer.data$`sample weight`~ poorer.data$`child 1`, data = poorer.data)
  
  middle.data <- careseeking.diarrhea[careseeking.diarrhea$"wealth quintile"=="middle", ]
  middle <- xtabs(middle.data$`sample weight`~ middle.data$`child 1`, data = middle.data)
  
  richer.data <- careseeking.diarrhea[careseeking.diarrhea$"wealth quintile"=="richer", ]
  richer <- xtabs(richer.data$`sample weight`~ richer.data$`child 1`, data = richer.data)
  
  richest.data <- careseeking.diarrhea[careseeking.diarrhea$"wealth quintile"=="richest", ]
  richest <- xtabs(richest.data$`sample weight`~ richest.data$`child 1`, data = richest.data)
  
  numbersDF <- as.data.frame(rbind(total, urban, rural, poorest, poorer, middle, richer, richest))
  
  regions <- names(summary(careseeking.diarrhea$region))
  regionsDF <- data.frame()
  
  for (region in regions) {
    assign(region, get(("regions")))
    region.data <- careseeking.diarrhea[careseeking.diarrhea$"region"== region, ]
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
  
  writeWorksheetToFile("initial.diarrhea.xlsx", data = percentDF, sheet = paste(id, "initial diarrhea"))
}

initial.diarrhea(drc_dhs)
initial.diarrhea(et_dhs)
initial.diarrhea(id_dhs)
initial.diarrhea(mz_dhs)
initial.diarrhea(ng_dhs)

