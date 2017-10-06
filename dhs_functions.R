# dHS_functions.R
# 6-26-17
# Anna Farrell-Sherman

#This file contains functions used to pull data from DHS files, for use in the DHS 
#Careseeking Analysis Walkthrough. 

##################################### READ ONLY ######################################################
##   Do not modfiy this file except to customize the analysis perfomred by the analyzeDHS script    ##
##################################### READ ONLY ######################################################


#Load libraries
suppressPackageStartupMessages(suppressWarnings(library(dplyr))) #provides group_by!
library(foreign) #provides read.dta
library(tidyr) #used for gather and drop_na
suppressPackageStartupMessages(library(data.table)) #used to change the names of the variables in readDHSdata (with setnames())


# ---------- readDHSdata function --------------------------------------------------------------------
# Creates a dataframe from the DHS stata
# Arguments: file name
# Returns: Country code (as a string)

readDHSdata <- function (filename, analysis_vars = c(), name_vars = c()) {
  #load the data from the stata file
  dhsData <- read.dta(filename, convert.factors = FALSE)
  dhsData$v005<- dhsData$v005/1000000
  
  #Select analysis variables if they have been provided
  if (length(analysis_vars) > 0) {
    #determine which columns contain the desired variables
    matches <- grep(paste(analysis_vars, collapse="$|^"), names(dhsData), value=TRUE)
    
    #subset the larger data set and remove participants who did not repsond
    dhsData <- dhsData[matches]
    #dhsData <- dhsData[complete.cases(dhsData), ]
    
    #Rename the columns with provided names
    #check to make sure the correct number of variable names were provided
    if (length(name_vars) > 0) {
      if (length(name_vars) == length(matches)) {
        setnames(dhsData, old = analysis_vars, new = name_vars)
      }
    }
  }
  
  #check that all variables were found
  if (0 %in% colSums(dhsData, na.rm = TRUE)) {
    warning("NOTE: The variable (shown under \"Analyzing:\" above) was not found in the dataset.  Please check that your country of interest collected these data.") 
  }
  
  dhsData
}

# ---------- loadlabelData function ----------------------------
# Creates a list of lists containing the coding information for the variables in analysis_vars
# Arguments: labelDOfile: the .do file provided in the stata folder from DHS
#            analysis_vars: the variables to create the table with
# Returns: a list of lists referencing variable:number:name

loadlabelData <- function(labelDOfile, analysis_vars, name_vars){
  con <- file(labelDOfile, "r") #connect with .DO file
  filelines <- readLines(con)
  
  varLabels <- list() #initiate list to contain names
  analysis_vars <- toupper(gsub("_[[:digit:]]", "", analysis_vars)) #remove "_1" from variables
  
  #read through the .do file line by line
  for (i in 1:length(filelines)) {
    if (filelines[i] == ";" | filelines[i] == "#delimit ;") {
      i <- i+1
      var = gsub('label define | ', '', filelines[i])
      #check to see if var was requested
      if (var %in% analysis_vars) {
        i <- i+1
        nameTable <- list() #create a list to contain levels of variable
        cat = "" #set up blank categry used if variable has none
        while(filelines[i] != ";") {
          varNum <- gsub("^ *| *\"[[:print:]]*\"", "", filelines[i]) #extract the number
          varName <- gsub("^ *[[:digit:]]* *\"|\"", "", filelines[i]) #extract variable name
          #check if this list item is a category and reset "cat" if so
          if (grepl("^[^a-z]*[A-Z]+$", varName)) cat = varName
          if (nchar(cat) > 0) varName <- paste(cat, varName, sep = ": ")
          #put this variable in list
          nameTable[[varNum]] <- varName
          i <- i+1
        }
        #add the variable to the table
        varLabels[[var]] <- nameTable
      }
      i <- i-1
    }
  }
  close(con)
  
  #for loop to rename the variables using the names provided
  varNameList <- list()
  for (var in names(varLabels)) {
    varNameList <- c(varNameList, name_vars[match(var, analysis_vars)])
  }
  names(varLabels) <- varNameList
  
  varLabels
}

# ---------- analyzeDHS function ------------------------------------
# creates the data frame
# Arguments: dataset: the dataframe containing the raw DHS data
#            var_list: a list of the variables (assumed to be the vNumber labels)
# Returns: data frame with selected country data

analyseDHS <- function (dataset, analysis_vars, all_facility_vars, facility_var) {
  
  #clean up dataset to remove other careseeking variables
  c_dataset <- dataset %>% drop_na(facility_var)
  colnames(c_dataset)[match(facility_var, colnames(c_dataset))] <- "facility"
  c_dataset <- c_dataset[! colnames(c_dataset) %in% all_facility_vars]
  
  #reorder and flip the data to get facility careseeking data for each analysis
  facilityUseData <- group_by(c_dataset, facility) %>%
    mutate(`Facility Total` = sum(sample_weight)) %>%
    gather(key = "category", value = "number", analysis_vars) %>%
    mutate(subgroup = paste0(category, number)) %>%
    group_by(facility, subgroup, `Facility Total`) %>%
    summarize(subgroup_total = sum(sample_weight)) %>%
    spread(key = subgroup, value = subgroup_total) %>%
    mutate_all(function (x) ifelse(is.na(x), 0, x)) %>%
    t()
  
  #put the facility numbers as the column names
  colnames(facilityUseData) <- facilityUseData[1,] #move the facility data (in first row) into the column names
  facilityUseData <- as.data.frame(facilityUseData[-1,]) #and remove the first row

  #take the percentage of all values based on the total in each category
  facilityUseData$total <- rowSums(facilityUseData)
  facilityUseData <- apply(facilityUseData, 2, function(x) x/facilityUseData$total)
  facilityUseData <- round(facilityUseData, digits = 4)

  #make two new columns containing the category of the variable
  facilityUseData <- mutate(as.data.frame(facilityUseData),
                            category = gsub('[0-9]+', '', rownames(facilityUseData)),
                            category_values = rownames(facilityUseData))

  #move the catagory naming variables to the first two rows of the table
  facilityUseData <- facilityUseData[,c(ncol(facilityUseData)-1,
                                            ncol(facilityUseData),
                                            1:(ncol(facilityUseData)-2))]

  as.data.frame(facilityUseData)
}

#test <- analyseDHS(filtered_data, analysis_var_names, careseeking_var_list, "h46a_1")

# ---------- labelVar function --------------------------------
# converts a string containing a variable category name and number into the decoded variable string

labelVar <- function (varCode, varNameTable, analysis_var) {
  #separate the name and number of the variable
  varType <- gsub("[[:digit:]]", "", varCode)
  varNumber <- gsub("[[:alpha:]]|[[:punct:]]", "", varCode)
  
  #add facility so that the function will work for the column names as well
  if (varType == "") varType <- analysis_var
  
  #if the variable is in the table then re-name it with the table, otherwise keep the name
  if (varType %in% names(varNameTable)) {
    if (varNumber %in% names(varNameTable[[varType]])) { #check for non-coded reponses
      return(varNameTable[[varType]][varNumber])
    } else {return("Other")}
  } else {return(varCode)}
}


# ---------- renameDataFrame function ------------------------
# Renames a dataframe based on provided variable name table.  This is called
# by analyzeDHSdata rather than used on its own!!
# Arguments: DHSdataframe: a dataframe creat
renameDataFrame <- function (DHSdataframe, varNameTable, analysis_var) {
  #rename rows
  for(n in rownames(DHSdataframe)) {
    DHSdataframe[n,"category_values"] <- labelVar(DHSdataframe[n,"category_values"], varNameTable, analysis_var) 
    }
  #rename columns
  for(n in colnames(DHSdataframe)) {
    colnames(DHSdataframe)[match(n, colnames(DHSdataframe))] <- labelVar(n, varNameTable, analysis_var) 
  }
  
  DHSdataframe
}

# ---------- write_dhs_file ----------------------------------------------------
# Arguments: country code, list of variables to analyze
# Returns: TRUE if successful, FALSE if failed

write_dhs_file <- function (formated_data, filename) {
  print(paste("Formatted DHS file saved as", filename, sep=": "))
  write.csv(formated_data, file = filename, row.names = FALSE)
}

# end file 