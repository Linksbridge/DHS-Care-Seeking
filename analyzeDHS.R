#analyzeDHSdata.R
#7-11-17
#Anna Farrell-Sherman

# Use this script to create a careseeking analysis data table from a DHS dataset. 

#################################### INSTALLING PACKAGES ####################################

# This script reqires the attchment of the following libraries: dplyr, foreign, tidyr, 
# and data.table. If you do not have these libraries installed, uncomment 
# (remove the pound signs from) the lines below and run the commands to to install them.  
# You can check if they are installed by running the command -- library(dplyr) --.

# install.packages("dplyr")
# install.packages("foreign")
# install.packages("tidyr")
# install.packages("data.table")

################################### WORKING DIRECTORY ###################################

# Using the command below, set your working directory to the 
# folder containing both this file, and the dhs_functions.R script.  The path to the file
# must be in quotes and contain only forward slashes.  This will be the folder in which 
# the DHS careseeking .cvs files created by the script will be stored.

setwd("C:/Dropbox (Linksbridge)/Anna Summer Internship Files/Final R Script")

##################################### INSTRUCTIONS #####################################

# As described in the DHS Careseeking Analysis Walkthrough, there are six variables that
# you must change to run this script on your data.  They include:
#
#       infilename: The full path to the file storing your downloaded DHS data ( which must be in stata format with a .dta file extension)
#               example: "~/Documents/DHS Data/Tanzania/tzir63dt/TZIR63FL"
#               note: Do NOT include the file extension (ie: ".dta")
#
#       country: The name of the country you are analyzing
#               example: "Tanzania"
#
#       careseeking_var_list: A list of the careseeking variables you want to generate files for
#               example: c("h46a_1", "h44a_1", "v829", "v842")
#
#       careseeking_var_names: The names of the careseeking variables you are using  
#               example: c("fever", "diarrhea", "hiv.test", "antenatal.hiv.test")
#               note: make sure they are in the same order as they are listed in careseeking_var_list
#
#       analysis_var_list: The variables you want to analyze your careseeking variables with
#               example: c("v190", "v024", "v025")
#
#       analysis_var_names: The variables you want to analyze your careseeking variables with
#               example: c("wealth_quintile", "region", "residence")
#               note: make sure they are in the same order as they are listed in analysis_var_list
#
# The examples described above are the defaults in the assignment commands below.  Modify these
# assignments (by changing the text after the <- ) before running the file.

infilename <- "C:/Dropbox (Linksbridge)/Gates country scoping/Data Sources/DHS Data/Ghana/ghir72dt"
country <- "Ghana"
careseeking_var_list <- c("h44a_1")
careseeking_var_names <- c("fever")
analysis_var_list <- c("v024", "v190", "v025")
analysis_var_names <- c("Region", "Wealth_Quintile", "Residence")

# Now you are ready to run the file! Click the "Source" button at the top right of this pane.
# Clear the global environment (window pane in the top right corner) before running again.



##################################### READ ONLY ######################################################
##             The following lines should only be changed to customize the analysis                 ##
##################################### READ ONLY ######################################################

#Load DHS analysis functions
source("dhs_functions.R")

# STEP 0: Create lists of variable names and numbers -----------------------------------------
var_numbers <- c(careseeking_var_list, analysis_var_list, "v005")
var_names <- c(careseeking_var_list, analysis_var_names, "sample_weight")

# STEP 1: LOAD DATA --------------------------------------------------------------------------
print("Loading DHS data...")
staFile <- paste(infilename, "dta", sep = ".")
filtered_data <- readDHSdata(staFile, var_numbers, var_names)


# STEP 2: LOAD VARIABLE LABELS ---------------------------------------------------------------
print("Loading variable naming data...")
doFile <- paste(infilename, "do", sep = ".")
label_data <- loadlabelData(doFile, var_numbers, var_names)

# STEP 3: CREATE AND SAVE FACILITY TABLES ----------------------------------------------------
for (a_var in careseeking_var_list) {
  
  print(paste("Analyzing:", a_var))
  
  # Step 3a: Format data as facility vs. analysis variables
  formatted_data <- analyseDHS(filtered_data, analysis_var_names, careseeking_var_list, a_var)
  
  #Step 3b: Name the data with the .do file info
  named_data <- renameDataFrame(formatted_data, label_data, a_var)
  
  # Step 3c: Write the datatable to a csv file
  facility_var <- careseeking_var_names[match(a_var, careseeking_var_list)]
  outfile <- paste(country, facility_var, "csv", sep=".")
  write_dhs_file(named_data, outfile)
}

#end script