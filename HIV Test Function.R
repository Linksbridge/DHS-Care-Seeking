#Note that the XLConnect Package requires Java. If you need to download Java, make sure Java and R have matching architectures (i.e., if you have the 64 bit version of 
# R you will need the 64 bit version of Java)

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


hiv.test <- function (DHSdata){
  idlong <- deparse(substitute(DHSdata))
  id <- gsub("_dhs", "", idlong)
  careseeking.hiv <- DHSdata[, c("v829", "v190", "v024", "v025", "v005"), drop = FALSE]
  careseeking.hiv$`v005`<- careseeking.hiv$`v005`/1000000
  colnames(careseeking.hiv) <- c("facility", "wealth quintile", "region", "residence", "sample weight")
  
  if (id == "et") {
    careseeking.hiv$"facility" <- as.factor(careseeking.hiv$"facility")
    levels(careseeking.hiv$"facility") <- c("11"="Government hospital", "12"="Government health center",
                                                      "13"="Government health station/clinic", "14"="Stand-alone VCT center","16"="Other Public",
                                                      "21"="Private hospital", "22"="Private clinic", "23"="NGO health facility",
                                                      "24" = "Stand-alone VCT center - private", "25"="Mobile", "26" ="Other NGO",  
                                                      "27" = "Other private medical", "96"="Other",
                                                      "99"="Missing")
  }
  
  if (id == "ng") {
    careseeking.hiv$"facility" <- as.factor(careseeking.hiv$"facility")
    levels(careseeking.hiv$"facility") <- c("11"="Government hospital", "12"="Government health center",
                                                      "13"="Stand-alone VCT center", "14"="Family Planning Clinic", "15" = "Mobile Clinic", "16" = "Fieldworker",
                                                      "17" = "School based clinic", "18"= "Other Public Sector", "21"="Private hospital, clinic",
                                                      "22" = "private stand-alone VCT center", "23"="Pharmacy", "24"="Private Mobile Clinic",
                                                      "25"="Private Fieldworker", "26" = "Private School-based clinic", "27"="Other Private Medical Sector",
                                                      "31"="home", "32" = "correctional facility", "96"="other", "99"="Missing")
  }
  
  total <- xtabs(careseeking.hiv$`sample weight`~ careseeking.hiv$`facility`, data = careseeking.hiv)
  
  urban.data <- careseeking.hiv[careseeking.hiv$residence=="urban", ]
  urban <- xtabs(urban.data$`sample weight`~ urban.data$`facility`, data = urban.data)
  
  rural.data <- careseeking.hiv[careseeking.hiv$residence=="rural", ]
  rural <- xtabs(rural.data$`sample weight`~ rural.data$`facility`, data = rural.data)
  
  poorest.data <- careseeking.hiv[careseeking.hiv$"wealth quintile"=="poorest", ]
  poorest <- xtabs(poorest.data$`sample weight`~ poorest.data$`facility`, data = poorest.data)
  
  poorer.data <- careseeking.hiv[careseeking.hiv$"wealth quintile"=="poorer", ]
  poorer <- xtabs(poorer.data$`sample weight`~ poorer.data$`facility`, data = poorer.data)
  
  middle.data <- careseeking.hiv[careseeking.hiv$"wealth quintile"=="middle", ]
  middle <- xtabs(middle.data$`sample weight`~ middle.data$`facility`, data = middle.data)
  
  richer.data <- careseeking.hiv[careseeking.hiv$"wealth quintile"=="richer", ]
  richer <- xtabs(richer.data$`sample weight`~ richer.data$`facility`, data = richer.data)
  
  richest.data <- careseeking.hiv[careseeking.hiv$"wealth quintile"=="richest", ]
  richest <- xtabs(richest.data$`sample weight`~ richest.data$`facility`, data = richest.data)
  
  numbersDF <- as.data.frame(rbind(total, urban, rural, poorest, poorer, middle, richer, richest))
  
  regions <- names(summary(careseeking.hiv$region))
  regionsDF <- data.frame()
  
  for (region in regions) {
    assign(region, get(("regions")))
    region.data <- careseeking.hiv[careseeking.hiv$"region"== region, ]
    region <- xtabs(region.data$`sample weight`~ region.data$`facility`, data = region.data)
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
  
  writeWorksheetToFile("hiv.test.xlsx", data = percentDF, sheet = paste(id, "hiv"))
}


hiv.test(drc_dhs)
hiv.test(et_dhs)
hiv.test(mz_dhs)
hiv.test(ng_dhs)