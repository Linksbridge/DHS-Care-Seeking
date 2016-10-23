# DHS Care-Seeking
This repo provides R code examples for extracting and summarizing data on care-seeking from DHS data sets. Each country's DHS will be slightly different. The codes for the variables are the same across countries. However, not all countries have data for all variables. Also, the values the variables can take are sometimes different from one country to another. For example, among the countries in our analysis, there are optional country-specific values for the "barriers to care" variable in addition to possible values that apply to all countries. Also, the possible values for "place of careseeking" are different from one country to the next. Consult the steps below, and the examples provided, keeping in mind you wlil need to tailor the R code to your country of interest and your specific research questions. 

1.	Register on dhsprogram.com
2.	Request access to respective survey data
3.	You will want to download the individual recode file (which is typically the women’s questionnaire) as this contains the questions related to care seeking for fever and cough
4.	Download data
5.	The downloaded folder will contain a number of files, including:
    a.	DCT File - This indicates what data type each variable is. We did not use it in our analysis.
    b.	DO File – Stata file that applies labels to each of the variables in the data set
    c.	Word Document – Gives an overview of the data set, number of records, variables names and descriptions
    d.	DTA File – This is the data file; contains all variables and all records. Needs to be opened with Stata or R
    e.	FRQ File – Contains frequencies for each possible value of each variable. Unweighted. 
    f.	FRW File – Contains frequencies for each possible value of each variable. Weighted. 
    g.	MAP File – Maps the value labels of each variable value. 
6.	Open the data. You will need statistical software to open and analyze data. We have included R code in this repo as an example. 
7.	Pull the data needed for care seeking patterns. The variable numbers are:
    a.	Barriers to Care – v467a-m
    b.	Place of initial care-seeking for child's diarrhea: h44a_1
    c.	Place of initial care-seeking for child's fever:  h46a_1
    d.	Place of HIV test (adult): v829
    e.	Place of antenatal HIV test: v842
8. The files "Barrier to Care Function," "Antenatal HIV Test Function," "HIV Test Function," "Initial Diarrhea Function," and "Initial Fever Function" may serve as examples to help get you started on your own analysis. These R functions utilize the XLConnect package in order to export data to an excel file. In the examples, each function results in an excel workbook wherein each country has its own tab. If you desire a similar output, you may also wish to utilize the XLConnect package, as we have done. If so, you will need to have Java installed on your machine, as XLConnect depends on Java. If you need to download Java, make sure Java and R have matching architectures (i.e., if you have the 64 bit version of R you will require the 64 bit version of Java). 
