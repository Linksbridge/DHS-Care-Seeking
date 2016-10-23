library(XLConnect)
library(foreign)
library(stats)
library(plyr)

drc_dhs <- read.dta("CDIR61FL.DTA")
et_dhs <- read.dta("ETIR61FL.DTA")
id_dhs <- read.dta("IDIR63FL.DTA")
mz_dhs <- read.dta("MZIR62FL.DTA")
ng_dhs <- read.dta("NGIR6AFL.DTA")
pk_dhs <- read.dta("PKIR61FL.DTA")


barriers <- function (DHSdata){
  idlong <- deparse(substitute(DHSdata))
  id <- gsub("_dhs", "", idlong)
  barriers.care <- DHSdata[, c("v467a","v467b","v467c","v467d","v467e","v467f","v467g","v467h","v467i","v467j", "v190", "v024", "v025", "v005"), drop = FALSE]
  
  colnames(barriers.care) <- c("know where to go", "getting permission to go", "getting money for treatment", "distance to health facility", 
                               "having to take transport", "not wanting to go alone", "concern no female health provider", "concern no provider available", 
                               "concern no drugs available", "country specific", "wealth quintile", "region", "residence", "sample weight")
  
  if (id == "et") {
    barriers.care$"getting permission to go" <- as.factor(barriers.care$"getting permission to go")
    levels(barriers.care$"getting permission to go") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"getting money for treatment" <- as.factor(barriers.care$"getting money for treatment")
    levels(barriers.care$"getting money for treatment") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"distance to health facility" <- as.factor(barriers.care$"distance to health facility")
    levels(barriers.care$"distance to health facility") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"having to take transport" <- as.factor(barriers.care$"having to take transport")
    levels(barriers.care$"having to take transport") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"not wanting to go alone" <- as.factor(barriers.care$"not wanting to go alone")
    levels(barriers.care$"not wanting to go alone") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"concern no female health provider" <- as.factor(barriers.care$"concern no female health provider")
    levels(barriers.care$"concern no female health provider") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"concern no provider available" <- as.factor(barriers.care$"concern no provider available")
    levels(barriers.care$"concern no provider available") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"concern no drugs available" <- as.factor(barriers.care$"concern no drugs available")
    levels(barriers.care$"concern no drugs available") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    names(barriers.care)[names(barriers.care)=="country specific"] <- "workload inside or outside home"
    barriers.care$"workload inside or outside home" <- as.factor(barriers.care$"workload inside or outside home")
    levels(barriers.care$"workload inside or outside home") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
  }
  
  if (id == "ng") {
   barriers.care$"getting permission to go" <- as.factor(barriers.care$"getting permission to go")
   levels(barriers.care$"getting permission to go") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
   barriers.care$"getting money for treatment" <- as.factor(barriers.care$"getting money for treatment")
   levels(barriers.care$"getting money for treatment") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
   barriers.care$"distance to health facility" <- as.factor(barriers.care$"distance to health facility")
   levels(barriers.care$"distance to health facility") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
   barriers.care$"not wanting to go alone" <- as.factor(barriers.care$"not wanting to go alone")
   levels(barriers.care$"not wanting to go alone") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
   names(barriers.care)[names(barriers.care)=="country specific"] <- "attitude of health worker"
   barriers.care$"attitude of health worker" <- as.factor(barriers.care$"attitude of health worker")
   levels(barriers.care$"attitude of health worker") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
  }
  
  if (id == "pk") {
    barriers.care$"getting permission to go" <- as.factor(barriers.care$"getting permission to go")
    levels(barriers.care$"getting permission to go") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"getting money for treatment" <- as.factor(barriers.care$"getting money for treatment")
    levels(barriers.care$"getting money for treatment") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"distance to health facility" <- as.factor(barriers.care$"distance to health facility")
    levels(barriers.care$"distance to health facility") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"having to take transport" <- as.factor(barriers.care$"having to take transport")
    levels(barriers.care$"having to take transport") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
    barriers.care$"not wanting to go alone" <- as.factor(barriers.care$"not wanting to go alone")
    levels(barriers.care$"not wanting to go alone") <- c("1"="big problem", "2"="not big problem", "9" = "NA")
  }
  
  total <- c()
  
    for (column in barriers.care[, 1:10]) {
    data <- sum(!is.na(column))
    sample.weight <- barriers.care$`sample weight`/1000000
    total.x <- xtabs(sample.weight~column,data=barriers.care)
    
    if  (data != 0 & "no problem" %in% names(total.x)){
      total <- c(total, total.x[2]/(total.x[2] + total.x[3]), total.x[3]/(total.x[2] + total.x[3]))
      
    } else if (data != 0){
      total <- c(total, total.x[1]/(total.x[1]+total.x[2]), total.x[2]/(total.x[1] + total.x[2]))
    }
  }
  
  urban.data <- barriers.care[barriers.care$residence=="urban", ]
  urban <- c()
  
    for (column in urban.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- urban.data$`sample weight`/1000000
    urban.x <- xtabs(sample.weight~column,data=urban.data)
    
    if  (data != 0 & "no problem" %in% names(urban.x)){
      urban <- c(urban, urban.x[2]/(urban.x[2] + urban.x[3]), urban.x[3]/(urban.x[2] + urban.x[3]))
      
    } else if (data != 0){
      urban <- c(urban, urban.x[1]/(urban.x[1]+urban.x[2]), urban.x[2]/(urban.x[1] + urban.x[2]))
    }
  }
  
  rural.data <- barriers.care[barriers.care$residence=="rural", ]
  rural <- c()
  
  for (column in rural.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- rural.data$`sample weight`/1000000
    rural.x <- xtabs(sample.weight~column,data=rural.data)
    
    if  (data != 0 & "no problem" %in% names(rural.x)){
      rural <- c(rural, rural.x[2]/(rural.x[2] + rural.x[3]), rural.x[3]/(rural.x[2] + rural.x[3]))
      
    } else if (data != 0){
      rural <- c(rural, rural.x[1]/(rural.x[1]+rural.x[2]), rural.x[2]/(rural.x[1] + rural.x[2]))
    }
  }
  
  poorest.data <- barriers.care[barriers.care$`wealth quintile`=="poorest", ]
  poorest <- c()
  
  for (column in poorest.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- poorest.data$`sample weight`/1000000
    poorest.x <- xtabs(sample.weight~column,data=poorest.data)
    
    if  (data != 0 & "no problem" %in% names(poorest.x)){
      poorest <- c(poorest, poorest.x[2]/(poorest.x[2] + poorest.x[3]), poorest.x[3]/(poorest.x[2] + poorest.x[3]))
      
    } else if (data != 0){
      poorest <- c(poorest, poorest.x[1]/(poorest.x[1]+poorest.x[2]), poorest.x[2]/(poorest.x[1] + poorest.x[2]))
    }
  }
  
  poorer.data <- barriers.care[barriers.care$`wealth quintile`=="poorer", ]
  poorer <- c()
  
  for (column in poorer.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- poorer.data$`sample weight`/1000000
    poorer.x <- xtabs(sample.weight~column,data=poorer.data)
    
    if  (data != 0 & "no problem" %in% names(poorer.x)){
      poorer <- c(poorer, poorer.x[2]/(poorer.x[2] + poorer.x[3]), poorer.x[3]/(poorer.x[2] + poorer.x[3]))
      
    } else if (data != 0){
      poorer <- c(poorer, poorer.x[1]/(poorer.x[1]+poorer.x[2]), poorer.x[2]/(poorer.x[1] + poorer.x[2]))
    }
  }
  
  middle.data <- barriers.care[barriers.care$`wealth quintile`=="middle", ]
  middle <- c()
  
  for (column in middle.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- middle.data$`sample weight`/1000000
    middle.x <- xtabs(sample.weight~column,data=middle.data)
    
    if  (data != 0 & "no problem" %in% names(middle.x)){
      middle <- c(middle, middle.x[2]/(middle.x[2] + middle.x[3]), middle.x[3]/(middle.x[2] + middle.x[3]))
      
    } else if (data != 0){
      middle <- c(middle, middle.x[1]/(middle.x[1]+middle.x[2]), middle.x[2]/(middle.x[1] + middle.x[2]))
    }
  }
  
  richer.data <- barriers.care[barriers.care$`wealth quintile`=="richer", ]
  richer <- c()
  
  for (column in richer.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- richer.data$`sample weight`/1000000
    richer.x <- xtabs(sample.weight~column,data=richer.data)
    
    if  (data != 0 & "no problem" %in% names(richer.x)){
      richer <- c(richer, richer.x[2]/(richer.x[2] + richer.x[3]), richer.x[3]/(richer.x[2] + richer.x[3]))
      
    } else if (data != 0){
      richer <- c(richer, richer.x[1]/(richer.x[1]+richer.x[2]), richer.x[2]/(richer.x[1] + richer.x[2]))
    }
  }
  
  richest.data <- barriers.care[barriers.care$`wealth quintile`=="richest", ]
  richest <- c()
  
  for (column in richest.data[, 1:10]){
    data <- sum(!is.na(column))
    sample.weight <- richest.data$`sample weight`/1000000
    richest.x <- xtabs(sample.weight~column,data=richest.data)
    
    if  (data != 0 & "no problem" %in% names(richest.x)){
      richest <- c(richest, richest.x[2]/(richest.x[2] + richest.x[3]), richest.x[3]/(richest.x[2] + richest.x[3]))
      
    } else if (data != 0){
      richest <- c(richest, richest.x[1]/(richest.x[1]+richest.x[2]), richest.x[2]/(richest.x[1] + richest.x[2]))
    }
  }

regions <- names(summary(barriers.care$region))
regionsDF <- data.frame()
  
  for (region in regions) {
    assign(region, get(("regions")))
    region.data <- barriers.care[barriers.care$"region"== region, ]
    row <- c()
    
    for (column in region.data[, 1:10]){
      data <- sum(!is.na(column))
      sample.weight <- region.data$`sample weight`/1000000
      region.x <- xtabs(sample.weight~column,data=region.data)
      
      if  (data != 0 & "no problem" %in% names(region.x)){
        row <- c(row, region.x[2]/(region.x[2] + region.x[3]), region.x[3]/(region.x[2] + region.x[3]))
         #print(row)
         #print(str(row))
        
      } else if (data != 0){
        row <- c(row, region.x[1]/(region.x[1]+region.x[2]), region.x[2]/(region.x[1] + region.x[2]))
         #print(row)
         #print(str(row))
      }
      
    }
    regionsDF <- as.data.frame(rbind(regionsDF, row))
  }

barrier.columns <- (barriers.care[,1:10])
barrier.columns <- barrier.columns[,colSums(is.na(barrier.columns))<nrow(barrier.columns)]
problem <- names(barrier.columns)
problem <- rep(problem, each = 2)
colnames(regionsDF) <- rep_len(c("big problem", "not a big problem"), length.out = length(problem))

barriersDF <- as.data.frame(rbind(total, urban, rural, poorest, poorer, middle, richer, richest))
colnames(barriersDF) <- colnames(regionsDF)
barriersDF <- rbind(barriersDF, regionsDF)
barriersDF <- 100*(round(barriersDF, digits = 3))

barriersDF <- rbind(problem, barriersDF)
group <- c(c("problem", "total", "urban", "rural", "poorest", "poorer", "middle", "richer", "richest"), regions)
barriersDF <- cbind(group, barriersDF)

writeWorksheetToFile("barriers.to.care.xlsx", data = barriersDF, sheet = paste(id, "barriers"))

}


barriers(drc_dhs)
barriers(et_dhs)
barriers(id_dhs)
barriers(mz_dhs)
barriers(ng_dhs)
barriers(pk_dhs)

 


