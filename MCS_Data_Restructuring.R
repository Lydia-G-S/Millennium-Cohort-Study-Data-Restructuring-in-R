#Restructuring data to end up with a longitudinally merged data set structured as 1 row per cohort member. See README file for further information. 
#Authors: Lydia Gabriela Speyer (@LydiaSpeyer) & Hildigunnur Anna Hall (@_annahall)

#Millennium Cohort Study data were downloaded from the UK Data Service (https://ukdataservice.ac.uk/)

#Load required packages and set working directory----
library(tidyverse)
library(haven)
library(dplyr)
library(utils)
library(data.table)

# 1. Reading in Datasets----
setwd("C:/Users/s1771508/OneDrive - University of Edinburgh/PhD/Projects/Millennium Cohort Study/MCS Working Files/data/used_datasets")
temp = list.files(pattern="*.sav")
list <- lapply(setNames(temp, make.names(gsub("*.sav$", "", temp))), 
               read_sav)

# 2. Creating Subsets----
#A CSV file containing the list of variable names of interest was created. This was formatted as 1 column per original SPSS file. **Note that this file was created outside of R**
##Reading in list of variable names
setwd("C:/Users/s1771508/OneDrive - University of Edinburgh/PhD/Projects/Millennium Cohort Study/MCS Working Files")

selected_vars <- read_csv("data/selected_variables/selected_vars_updated.csv", col_names = T)

##Subsetting each data set to only include varaibles of interest from the selected_vars data frame
list_sub <- Map(function(x,y) x[na.omit(y)], list[names(selected_vars)], selected_vars)
names(list_sub) <-  paste(names(list_sub), "sub",  sep = "_")
list2env(list_sub, envir = .GlobalEnv)


#Checking whether data sets need to be restructured----
##The data sets are structured differently. Some have 1 row per household, some have 1 row per cohort member and others 1 row per parent respondent (e.g. main and partner). 

##Checking structure of data sets by looking for multiple instances of mcsid to see whether the data are structured in one line per child, household or parent. 
###To do this, we created a new data frame containing only thea mcsid column and searched for duplicates/triplicates there. 

#Hospital records----
#1 line per cohort member -> no resturcturing neccessary


##Wave 1----
 
#mcs1_derived_variables_sub
id_list_mcs1_derived_variables_sub <- data.frame(table(mcs1_derived_variables_sub$MCSID))
id_list_mcs1_derived_variables_sub[id_list_mcs1_derived_variables_sub$Freq > 1,] #needs restructuring

#mcs1_geographically_linked_data_sub 
id_list_mcs1_geographically_linked_data_sub <- data.frame(table(mcs1_geographically_linked_data_sub$mcsid))
id_list_mcs1_geographically_linked_data_sub[id_list_mcs1_geographically_linked_data_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging. 

#mcs1_parent_interview_sub 
id_list_mcs1_parent_interview_sub <- data.frame(table(mcs1_parent_interview_sub$mcsid))
id_list_mcs1_parent_interview_sub[id_list_mcs1_parent_interview_sub$Freq > 1,] #needs restructuring

##Wave 2----

#mcs2_derived_variables_sub 
id_list_mcs2_derived_variables_sub <- data.frame(table(mcs2_derived_variables_sub$MCSID))
id_list_mcs2_derived_variables_sub[id_list_mcs2_derived_variables_sub$Freq > 1,] #needs restructuring

#mcs2_geographically_linked_data_sub 
id_list_mcs2_geographically_linked_data_sub <- data.frame(table(mcs2_geographically_linked_data_sub$mcsid))
id_list_mcs2_geographically_linked_data_sub[id_list_mcs2_geographically_linked_data_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging. 

#mcs2_parent_interview_sub
id_list_mcs2_parent_interview_sub <- data.frame(table(mcs2_parent_interview_sub$mcsid))
id_list_mcs2_parent_interview_sub[id_list_mcs2_parent_interview_sub$Freq > 1,] #needs restructuring

##Wave 3----

#mcs3_derived_variables_sub
id_list_mcs3_derived_variables_sub <- data.frame(table(mcs3_derived_variables_sub$MCSID))
id_list_mcs3_derived_variables_sub[id_list_mcs3_derived_variables_sub$Freq > 1,] #needs restructuring

#mcs3_geographically_linked_data_sub
id_list_mcs3_geographically_linked_data_sub <- data.frame(table(mcs3_geographically_linked_data_sub$mcsid))
id_list_mcs3_geographically_linked_data_sub[id_list_mcs3_geographically_linked_data_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging. 

#mcs3_parent_interview_sub
id_list_mcs3_parent_interview_sub <- data.frame(table(mcs3_parent_interview_sub$mcsid))
id_list_mcs3_parent_interview_sub[id_list_mcs3_parent_interview_sub$Freq > 1,] #needs restructuring

##Wave 4----

#mcs4_cm_self_completion_final_sub
id_list_mcs4_cm_self_completion_final_sub <- data.frame(table(mcs4_cm_self_completion_final_sub$mcsid))
id_list_mcs4_cm_self_completion_final_sub[id_list_mcs4_cm_self_completion_final_sub$Freq > 1,] #doesn't need restructuring 

#mcs4_derived_variables_sub
id_list_mcs4_derived_variables_sub <- data.frame(table(mcs4_derived_variables_sub$MCSID))
id_list_mcs4_derived_variables_sub[id_list_mcs4_derived_variables_sub$Freq > 1,] #needs restructuring

#mcs4_geographically_linked_data_sub
id_list_mcs4_geographically_linked_data_sub <- data.frame(table(mcs4_geographically_linked_data_sub$mcsid))
id_list_mcs4_geographically_linked_data_sub[id_list_mcs4_geographically_linked_data_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging. 

#mcs4_parent_interview_sub
id_list_mcs4_parent_interview_sub <- data.frame(table(mcs4_parent_interview_sub$mcsid))
id_list_mcs4_parent_interview_sub[id_list_mcs4_parent_interview_sub$Freq > 1,] #needs restructuring

#mcs4_teacher_sub
id_list_mcs4_teacher_sub <- data.frame(table(mcs4_teacher_sub$MCSID))
id_list_mcs4_teacher_sub[id_list_mcs4_teacher_sub$Freq > 1,] #doesn't need restructuring

##Wave 5----

#mcs5_cm_derived_sub
id_list_mcs5_cm_derived_sub <- data.frame(table(mcs5_cm_derived_sub$MCSID))
id_list_mcs5_cm_derived_sub[id_list_mcs5_cm_derived_sub$Freq > 1,] #doesn't need restructuring

#mcs5_cm_interview_sub
id_list_mcs5_cm_interview_sub <- data.frame(table(mcs5_cm_interview_sub$MCSID))
id_list_mcs5_cm_interview_sub[id_list_mcs5_cm_interview_sub$Freq > 1,] #doesn't need restructuring

#mcs5_cm_teacher_survey_sub
id_list_mcs5_cm_teacher_survey_sub <- data.frame(table(mcs5_cm_teacher_survey_sub$MCSID))
id_list_mcs5_cm_teacher_survey_sub[id_list_mcs5_cm_teacher_survey_sub$Freq > 1,] #doesn't need restructuring

#mcs5_geographically_linked_data_sub
id_list_mcs5_geographically_linked_data_sub <- data.frame(table(mcs5_geographically_linked_data_sub$MCSID))
id_list_mcs5_geographically_linked_data_sub[id_list_mcs5_geographically_linked_data_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging. 

#mcs5_parent_cm_interview_sub
id_list_mcs5_parent_cm_interview_sub <- data.frame(table(mcs5_parent_cm_interview_sub$MCSID))
id_list_mcs5_parent_cm_interview_sub[id_list_mcs5_parent_cm_interview_sub$Freq > 1,] #needs restructuring

#mcs5_parent_derived_sub
id_list_mcs5_parent_derived_sub <- data.frame(table(mcs5_parent_derived_sub$MCSID))
id_list_mcs5_parent_derived_sub[id_list_mcs5_parent_derived_sub$Freq > 1,] #doesn't need restructuring

#mcs5_parent_interview_sub
id_list_mcs5_parent_interview_sub <- data.frame(table(mcs5_parent_interview_sub$MCSID))
id_list_mcs5_parent_interview_sub[id_list_mcs5_parent_interview_sub$Freq > 1,] #doesn't need restructuring

#mcs5_family_derived_sub 
id_list_mcs5_family_derived_sub <- data.frame(table(mcs5_family_derived_sub$MCSID))
id_list_mcs5_family_derived_sub[id_list_mcs5_family_derived_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging.

##Wave 6----

#mcs6_cm_derived_sub 
id_list_mcs6_cm_derived_sub <- data.frame(table(mcs6_cm_derived_sub$MCSID))
id_list_mcs6_cm_derived_sub[id_list_mcs6_cm_derived_sub$Freq > 1,] #doesn't need restructuring

#mcs6_cm_interview_sub 
id_list_mcs6_cm_interview_sub <- data.frame(table(mcs6_cm_interview_sub$MCSID))
id_list_mcs6_cm_interview_sub[id_list_mcs6_cm_interview_sub$Freq > 1,] #doesn't need restructuring

#mcs6_family_derived_sub 
id_list_mcs6_family_derived_sub <- data.frame(table(mcs6_family_derived_sub$MCSID))
id_list_mcs6_family_derived_sub[id_list_mcs6_family_derived_sub$Freq > 1,] #only one data point per family, so the information will be repeated for each cohort member. Does not need restructuring as data will be copied for additional cohort members when merging.

#mcs6_parent_cm_interview_sub 
id_list_mcs6_parent_cm_interview_sub <- data.frame(table(mcs6_parent_cm_interview_sub$MCSID))
id_list_mcs6_parent_cm_interview_sub[id_list_mcs6_parent_cm_interview_sub$Freq > 1,] #needs restructuring

#mcs6_parent_derived_sub
id_list_mcs6_parent_derived_sub <- data.frame(table(mcs6_parent_derived_sub$MCSID))
id_list_mcs6_parent_derived_sub[id_list_mcs6_parent_derived_sub$Freq > 1,] #doesn't need restructuring

#mcs6_parent_interview_sub 
id_list_mcs6_parent_interview_sub <- data.frame(table(mcs6_parent_interview_sub$MCSID))
id_list_mcs6_parent_interview_sub[id_list_mcs6_parent_interview_sub$Freq > 1,] #doesn't need restructuring

#Restructuring Datasets----

#Wave 1---- 
##mcs1_derived_variables
###mcs1_derived_variables is structured as 1 row per household, with repeated variables for different cohort members

###Restructuring varaibles which are repeated for each cohort member. For variables that are on a household level, data will be imputed for each cohort member. 
i <- c(grep("(......)(A)0", colnames(mcs1_derived_variables_sub)), grep("(......)(B)0", colnames(mcs1_derived_variables_sub)), grep("(......)(C)0", colnames(mcs1_derived_variables_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs1_derived_variables_sub) #get vector containing all column names 
c <- endsWith(x, "A0") | endsWith(x, "B0") | endsWith(x,"C0") #check whether these names end with either "A0", "B0" or "C0". Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't end with either "A0", "B0" or "C0" with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) == 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs1_derived_variables_sub_tidy_final <- 
  mcs1_derived_variables_sub %>% 
  gather(key, value, s, na.rm = T) %>% 
  extract(key, c("variable", "cnum"), "(......)(.)0") %>% 
  spread(variable, value)

##mcs1_parent_interview
###mcs1_parent_interview is structured at 1 row per household, with repeated variables for different cohort members and multicodes. 

###Creating subsets because of memory issues
mcs1_parent_interview_sub_1 <- mcs1_parent_interview_sub[1:6000,]
mcs1_parent_interview_sub_2 <- mcs1_parent_interview_sub[6001:12000,]
mcs1_parent_interview_sub_3 <- mcs1_parent_interview_sub[12001:18552,]

i <- c(grep("(......)(a)(.)", colnames(mcs1_parent_interview_sub)), grep("(......)(b)(.)", colnames(mcs1_parent_interview_sub)), grep("(......)(c)(.)", colnames(mcs1_parent_interview_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs1_parent_interview_sub) #get vector containing all column names 
c <- endsWith(substring(x, 1, 7), "a") | endsWith(substring(x, 1, 7), "b") | endsWith(substring(x, 1, 7),"c") #check whether these names have either "a", "b" or "c" as  their next to last character. Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't have either "a", "b" or "c" as their next to last character with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) >= 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

###Restructuring variables that are repeated for cohort member and multicode
mcs1_parent_interview_sub_tidy_all_1 <- 
  mcs1_parent_interview_sub_1 %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)


mcs1_parent_interview_sub_tidy_all_2 <- 
  mcs1_parent_interview_sub_2 %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)

mcs1_parent_interview_sub_tidy_all_3 <- 
  mcs1_parent_interview_sub_3 %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)


###Recombining subsets
mcs1_parent_interview_sub_tidy_all<- rbind(mcs1_parent_interview_sub_tidy_all_1, mcs1_parent_interview_sub_tidy_all_2, mcs1_parent_interview_sub_tidy_all_3)

###Removing rows that represent nobody (cnum = b, c when there is only 1 child per family and multicode = a-i when question did not have multiple possibilities. 
###Indexing selects multiple columns that should all be NA for households which only have one cohort member (e.g. age, gender)
mcs1_parent_interview_sub_tidy_all_final <- mcs1_parent_interview_sub_tidy_all[rowSums(!is.na(mcs1_parent_interview_sub_tidy_all[319:322])) > 0, ]

#Wave 2----
##mcs2_derived_variables
###mcs2_derived_variables is structured as 1 row per household, with repeated variables for different cohort members

###Restructuring varaibles which are repeated for each cohort member. 
i <- c(grep("(......)(A)0", colnames(mcs2_derived_variables_sub)), grep("(......)(B)0", colnames(mcs2_derived_variables_sub)), grep("(......)(C)0", colnames(mcs2_derived_variables_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs2_derived_variables_sub) #get vector containing all column names 
c <- endsWith(x, "A0") | endsWith(x, "B0") | endsWith(x,"C0") #check whether these names end with either "A0", "B0" or "C0". Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't end with either "A0", "B0" or "C0" with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) == 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs2_derived_variables_sub_tidy_final <- 
  mcs2_derived_variables_sub %>% 
  gather(key, value, s, na.rm = T) %>% 
  extract(key, c("variable", "cnum"), "(......)(.)0") %>% 
  spread(variable, value)

##mcs2_parent_interview
###mcs2_parent_interview is structured at 1 row per household, with repeated variables for different cohort members and multicodes. 

###Restructuring variables that are repeated for cohort member and multicode
i <- c(grep("(......)(a)(.)", colnames(mcs2_parent_interview_sub)), grep("(......)(b)(.)", colnames(mcs2_parent_interview_sub)), grep("(......)(c)(.)", colnames(mcs2_parent_interview_sub))) #grab all column names that fit criterion of regular expression

x <- colnames(mcs2_parent_interview_sub) #get vector containing all column names 
c <- endsWith(substring(x, 1, 7), "a") | endsWith(substring(x, 1, 7), "b") | endsWith(substring(x, 1, 7),"c") #check whether these names have either "a", "b" or "c" as  their next to last character. Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't have either "a", "b" or "c" as their next to last character with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) >= 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

#restructure
mcs2_parent_interview_sub_tidy<- 
  mcs2_parent_interview_sub %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)

###Removing rows that represent nobody (cnum = b, c when there is only 1 child per family and multicode = a-i when question did not have multiple possibilities. 
###Indexing selects multiple columns that should all be NA for households which only have one cohort member (e.g. age, gender)
mcs2_parent_interview_sub_tidy_final <- mcs2_parent_interview_sub_tidy[rowSums(!is.na(mcs2_parent_interview_sub_tidy[52:55])) > 0, ] 

#Wave 3----
##mcs3_derived_variables
###mcs3_derived_variables is structured as 1 row per household, with repeated variables for different cohort members

###Restructuring varaibles which are repeated for each cohort member. 
i <- c(grep("(......)(A)0", colnames(mcs3_derived_variables_sub)), grep("(......)(B)0", colnames(mcs3_derived_variables_sub)), grep("(......)(C)0", colnames(mcs3_derived_variables_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs3_derived_variables_sub) #get vector containing all column names 
c <- endsWith(x, "A0") | endsWith(x, "B0") | endsWith(x,"C0") #check whether these names end with either "A0", "B0" or "C0". Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't end with either "A0", "B0" or "C0" with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) == 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs3_derived_variables_sub_tidy_final <- 
  mcs3_derived_variables_sub %>% 
  gather(key, value, s, na.rm = T) %>% 
  extract(key, c("variable", "cnum"), "(......)(.)0") %>% 
  spread(variable, value)

##mcs3_parent_interview
###mcs3_parent_interview is structured at 1 row per household, with repeated variables for different cohort members and multicodes.

###Restructuring variables that are repeated for cohort member and multicode
i <- c(grep("(......)(a)(.)", colnames(mcs3_parent_interview_sub)), grep("(......)(b)(.)", colnames(mcs3_parent_interview_sub)), grep("(......)(c)(.)", colnames(mcs3_parent_interview_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs3_parent_interview_sub) #get vector containing all column names 
c <- endsWith(substring(x, 1, 7), "a") | endsWith(substring(x, 1, 7), "b") | endsWith(substring(x, 1, 7),"c") #check whether these names have either "a", "b" or "c" as  their next to last character. Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't have either "a", "b" or "c" as their next to last character with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) >= 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs3_parent_interview_sub_tidy_all <- 
  mcs3_parent_interview_sub %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)

###Removing rows that represent nobody (cnum = b, c when there is only 1 child per family and multicode = a-i when question did not have multiple possibilities. 
###Indexing selects multiple columns that should all be NA for households which only have one cohort member (e.g. age, gender)
mcs3_parent_interview_sub_tidy_all_final <- mcs3_parent_interview_sub_tidy_all[rowSums(!is.na(mcs3_parent_interview_sub_tidy_all[57:61])) > 0, ] 

#Wave 4----
##mcs4_derived_variables
###mcs4_derived_variables is structured as 1 row per household, with repeated variables for different cohort members

###Restructuring varaibles which are repeated for each cohort member. 
i <- c(grep("(......)(A)0", colnames(mcs4_derived_variables_sub)), grep("(......)(B)0", colnames(mcs4_derived_variables_sub)), grep("(......)(C)0", colnames(mcs4_derived_variables_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs4_derived_variables_sub) #get vector containing all column names 
c <- endsWith(x, "A0") | endsWith(x, "B0") | endsWith(x,"C0") #check whether these names end with either "A0", "B0" or "C0". Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't end with either "A0", "B0" or "C0" with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) == 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs4_derived_variables_sub_tidy_final <- 
  mcs4_derived_variables_sub %>% 
  gather(key, value, s, na.rm = T) %>% 
  extract(key, c("variable", "cnum"), "(......)(.)0") %>% 
  spread(variable, value)

##mcs4_parent_interview
###mcs3_parent_interview is structured at 1 row per household, with repeated variables for different cohort members and multicodes.

###Restructuring variables that are repeated for cohort member and multicode
i <- c(grep("(......)(a)(.)", colnames(mcs4_parent_interview_sub)), grep("(......)(b)(.)", colnames(mcs4_parent_interview_sub)), grep("(......)(c)(.)", colnames(mcs4_parent_interview_sub))) #grab all column names that fit criterion of regular expression
x <- colnames(mcs4_parent_interview_sub) #get vector containing all column names 
c <- endsWith(substring(x, 1, 7), "a") | endsWith(substring(x, 1, 7), "b") | endsWith(substring(x, 1, 7),"c") #check whether these names have either "a", "b" or "c" as  their next to last character. Return True if yes. 
e <- ifelse(c == F, NA, substring(x, 1, 6)) #Replace column names that don't have either "a", "b" or "c" as their next to last character with NA
y <- substring(x, 1, 6) %in% names(table(e)[table(e) >= 3]) # count how often the variable name minus suffix occurs in e 
s <- na.omit(ifelse(y[i] == T, i, NA)) #create a vector containing column name indicies based on i if True in y. 

mcs4_parent_interview_sub_tidy_all <- 
  mcs4_parent_interview_sub %>% 
  gather(key, value, s) %>% 
  extract(key, c("variable", "cnum", "multi"), "(......)(.)(.)") %>% 
  unite(var, c(variable, multi), sep = "_", remove = TRUE) %>%
  spread(var, value)

###Removing rows that represent nobody (cnum = b, c when there is only 1 child per family and multicode = a-i when question did not have multiple possibilities. 
###Indexing selects multiple columns that should all be NA for households which only have one cohort member (e.g. age, gender)
mcs4_parent_interview_sub_tidy_all_final <- mcs4_parent_interview_sub_tidy_all[rowSums(!is.na(mcs4_parent_interview_sub_tidy_all[57:61])) > 0, ] 

#Wave 5----
##mcs5_derived_variables
###mcs5_parent_derived is structured as 1 row per parent respondent. It does not contain any repeated variables for cohort member and no multicoded itmes. It needs to be restructured into one line per household. When merging, data will be copied for additional cohort members where appropriate.   

###bring EPNUM00 to front
mcs5_parent_derived_sub <- mcs5_parent_derived_sub %>% select(MCSID, EPNUM00, everything())

###Restructuring all data at once. Data are repeated for every cohort member and multicode. 
###Each respondent was assigned an ID (a or b). 
num_observ <- nrow(mcs5_parent_derived_sub)
mcs5_parent_derived_sub$respondent <- rep_len(c("a", "b"), num_observ)
mcs5_parent_derived_sub_tidy<-dcast(melt(mcs5_parent_derived_sub, id.vars=c("MCSID", "respondent")), MCSID~variable+respondent)

##MCS5_Parent_Cohort-Member_Interview

###mcs5_parent_cm_interview is organised as one line per parent respondent, it needs to be restructured into one line per family

###bring EPNUM00 and ECNUM00 to front
mcs5_parent_cm_interview_sub <- mcs5_parent_cm_interview_sub %>% select(MCSID, EPNUM00, ECNUM00, everything())

###Restructuring all data at once. Data are repeated for every cohort member and multicode. 
num_observ <- nrow(mcs5_parent_cm_interview_sub)
mcs5_parent_cm_interview_sub$respondent <- rep_len(c("a", "b"), num_observ)
mcs5_parent_cm_interview_sub_tidy<-dcast(melt(mcs5_parent_cm_interview_sub, id.vars=c("MCSID", "ECNUM00", "respondent")), ECNUM00+MCSID~variable+respondent)

##MCS_Parent_Interview
###mcs5_parent_interview is organised as one line per parent respondent, it needs to be restructured into one line per family

###bring EPNUM00 to front
mcs5_parent_interview_sub <- mcs5_parent_interview_sub %>% select(MCSID, EPNUM00, everything())

###Restructuring all data at once. Data are repeated for every cohort member and multicode. 
mcs5_parent_interview_sub$respondent <- c("a", "b")

mcs5_parent_interview_sub_tidy<-dcast(melt(mcs5_parent_interview_sub, id.vars=c("MCSID", "respondent")), MCSID~variable+respondent)

#Wave 6----
##MCS6_Parent_Derived_Variables
###mcs6_parent_derived is organised as one line per parent respondent, it needs to be restructured into one line per famil

###bring FPNUM00 to front
mcs6_parent_derived_sub <- mcs6_parent_derived_sub %>% select(MCSID, FPNUM00, everything())

###Restructuring all data at once. Data are repeated for every cohort member and multicode. 
num_observ <- nrow(mcs6_parent_derived_sub)
mcs6_parent_derived_sub$respondent <- rep_len(c("a", "b"), num_observ)
mcs6_parent_derived_sub_tidy<-dcast(melt(mcs6_parent_derived_sub, id.vars=c("MCSID", "respondent")), MCSID~variable+respondent)

##MCS6_Parent_Cohort-Member_Interview
###mcs6_parent_cm_interview is organised as one line per parent respondent, it needs to be restructured into one line per family

###bring FPNUM00 and FCNUM00 to front
mcs6_parent_cm_interview_sub <- mcs6_parent_cm_interview_sub %>% select(MCSID, FPNUM00, FCNUM00, everything())


###Restructuring all data at once. Data are repeated for every cohort member and multicode.
num_observ <- nrow(mcs6_parent_cm_interview_sub)
mcs6_parent_cm_interview_sub$respondent <- rep_len(c("a", "b"), num_observ)
mcs6_parent_cm_interview_sub_tidy<-dcast(melt(mcs6_parent_cm_interview_sub, id.vars=c("MCSID", "FCNUM00", "respondent")), FCNUM00+MCSID~variable+respondent)

##MCS6_Parent_Interview
###mcs6_parent_interview is organised as one line per parent respondent, it needs to be restructured into one line per family

###bring FPNUM00 to front
mcs6_parent_interview_sub <- mcs6_parent_interview_sub %>% select(MCSID, FPNUM00, everything())

###Restructuring all data at once. Data are repeated for every cohort member and multicode. 
mcs6_parent_interview_sub$respondent <- c("a", "b")
mcs6_parent_interview_sub_tidy<-dcast(melt(mcs6_parent_interview_sub, id.vars=c("MCSID", "respondent")), MCSID~variable+respondent)

#Merge Datasets by wave----
#first merge datasets by wave (waves can be identified by first letter of variable), in addition, one letter will be added bedind each variable to code for original dataset

##Hospital records----
#hospital records data cannot be identified by first letter, hence "h" will be added in front of all hosp datasets
index_cnum <- which(colnames(hosp_eng_ns_sub)=="cnum" | colnames(hosp_eng_ns_sub)=="CNUM") 
index_mcsid <- which(colnames(hosp_eng_ns_sub)=="mcsid" | colnames(hosp_eng_ns_sub)=="MCSID") 
names(hosp_eng_ns_sub) <- paste0("h", names(hosp_eng_ns_sub))
names(hosp_eng_ns_sub) <- paste0(names(hosp_eng_ns_sub), "_e") #e for england
#mcsid and cnum have to be changed back to original
colnames(hosp_eng_ns_sub)[index_cnum] <- "cnum" 
colnames(hosp_eng_ns_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(hosp_ni_ns_sub)=="cnum" | colnames(hosp_ni_ns_sub)=="CNUM") 
index_mcsid <- which(colnames(hosp_ni_ns_sub)=="mcsid" | colnames(hosp_ni_ns_sub)=="MCSID") 
names(hosp_ni_ns_sub) <- paste0("h", names(hosp_ni_ns_sub))
names(hosp_ni_ns_sub) <- paste0(names(hosp_ni_ns_sub), "_n") #n for northern irland
#mcsid and cnum have to be changed back to original
colnames(hosp_ni_ns_sub)[index_cnum] <- "cnum" 
colnames(hosp_ni_ns_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(hosp_scot_ns_sub)=="cnum" | colnames(hosp_scot_ns_sub)=="CNUM") 
index_mcsid <- which(colnames(hosp_scot_ns_sub)=="mcsid" | colnames(hosp_scot_ns_sub)=="MCSID") 
names(hosp_scot_ns_sub) <- paste0("h", names(hosp_scot_ns_sub))
names(hosp_scot_ns_sub) <- paste0(names(hosp_scot_ns_sub), "_s") #s for scotland
#mcsid and cnum have to be changed back to original
colnames(hosp_scot_ns_sub)[index_cnum] <- "cnum" 
colnames(hosp_scot_ns_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(hosp_wales_ns_sub)=="cnum" | colnames(hosp_wales_ns_sub)=="CNUM") 
index_mcsid <- which(colnames(hosp_wales_ns_sub)=="mcsid" | colnames(hosp_wales_ns_sub)=="MCSID") 
names(hosp_wales_ns_sub) <- paste0("h", names(hosp_wales_ns_sub))
names(hosp_wales_ns_sub) <- paste0(names(hosp_wales_ns_sub), "_w") #w for wales
#mcsid and cnum have to be changed back to original
colnames(hosp_wales_ns_sub)[index_cnum] <- "cnum" 
colnames(hosp_wales_ns_sub)[index_mcsid] <- "mcsid"

hosp_merged_eng_ni <- merge(hosp_eng_ns_sub, hosp_ni_ns_sub, by=c("mcsid","cnum"), all=TRUE)
hosp_merged_eng_ni_scot <- merge(hosp_merged_eng_ni, hosp_scot_ns_sub, by=c("mcsid","cnum"), all=TRUE)
hosp_merged_final <- merge(hosp_merged_eng_ni_scot, hosp_wales_ns_sub,  by=c("mcsid","cnum"), all=TRUE)


##Wave 1----
##in mcs1_derived_variables_sub_tidy_final cnum is coded as uppercase letters, this has to be changed to lowercase in order to be matched with parent cnum
mcs1_derived_variables_sub_tidy_final$cnum <- as.character(mcs1_derived_variables_sub_tidy_final$cnum) #change class from integer to character
mcs1_derived_variables_sub_tidy_final$cnum <- tolower(mcs1_derived_variables_sub_tidy_final$cnum) #change upper to lowercase characters
index_cnum <- which(colnames(mcs1_derived_variables_sub_tidy_final)=="cnum" | colnames(mcs1_derived_variables_sub_tidy_final)=="CNUM") 
index_mcsid <- which(colnames(mcs1_derived_variables_sub_tidy_final)=="mcsid" | colnames(mcs1_derived_variables_sub_tidy_final)=="MCSID") 
names(mcs1_derived_variables_sub_tidy_final) <- paste0(names(mcs1_derived_variables_sub_tidy_final), "_d") #d for derived variable
#mcsid and cnum have to be changed back to original
colnames(mcs1_derived_variables_sub_tidy_final)[index_cnum] <- "cnum" 
colnames(mcs1_derived_variables_sub_tidy_final)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- as.numeric(which(colnames(mcs1_geographically_linked_data_sub)=="mcsid" | colnames(mcs1_geographically_linked_data_sub)=="MCSID")) 
names(mcs1_geographically_linked_data_sub) <- paste0(names(mcs1_geographically_linked_data_sub), "_g") #g for geographically linked data 
#mcsid has to be changed back to original
colnames(mcs1_geographically_linked_data_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs1_parent_interview_sub_tidy_all_final)=="cnum" | colnames(mcs1_parent_interview_sub_tidy_all_final)=="CNUM") 
index_mcsid <- which(colnames(mcs1_parent_interview_sub_tidy_all_final)=="mcsid" | colnames(mcs1_parent_interview_sub_tidy_all_final)=="MCSID") 
names(mcs1_parent_interview_sub_tidy_all_final) <- paste0(names(mcs1_parent_interview_sub_tidy_all_final), "_p") #p for parent interview
#mcsid and cnum have to be changed back to original
colnames(mcs1_parent_interview_sub_tidy_all_final)[index_cnum] <- "cnum" 
colnames(mcs1_parent_interview_sub_tidy_all_final)[index_mcsid] <- "mcsid"

wave_1_derived_parent <- merge(mcs1_derived_variables_sub_tidy_final, mcs1_parent_interview_sub_tidy_all_final,  by=c("mcsid","cnum"), all=TRUE) #datasets have to be merged seperately, only two can be merged at once
wave_1_merged_final <- merge(wave_1_derived_parent, mcs1_geographically_linked_data_sub,  by=c("mcsid"), all=TRUE)

##Wave 2----
mcs2_derived_variables_sub_tidy_final$cnum <- as.character(mcs2_derived_variables_sub_tidy_final$cnum) #change class from integer to character
mcs2_derived_variables_sub_tidy_final$cnum <- tolower(mcs2_derived_variables_sub_tidy_final$cnum) #change upper to lowercase characters
index_cnum <- which(colnames(mcs2_derived_variables_sub_tidy_final)=="cnum" | colnames(mcs2_derived_variables_sub_tidy_final)=="CNUM") 
index_mcsid <- which(colnames(mcs2_derived_variables_sub_tidy_final)=="mcsid" | colnames(mcs2_derived_variables_sub_tidy_final)=="MCSID") 
names(mcs2_derived_variables_sub_tidy_final) <- paste0(names(mcs2_derived_variables_sub_tidy_final), "_d") #d for derived variable
#mcsid and cnum have to be changed back to original
colnames(mcs2_derived_variables_sub_tidy_final)[index_cnum] <- "cnum" 
colnames(mcs2_derived_variables_sub_tidy_final)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- which(colnames(mcs2_geographically_linked_data_sub)=="mcsid" | colnames(mcs2_geographically_linked_data_sub)=="MCSID") 
names(mcs2_geographically_linked_data_sub) <- paste0(names(mcs2_geographically_linked_data_sub), "_g") #g for geographically linked data 
#mcsid has to be changed back to original
colnames(mcs2_geographically_linked_data_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs2_parent_interview_sub_tidy_final)=="cnum" | colnames(mcs2_parent_interview_sub_tidy_final)=="CNUM") 
index_mcsid <- which(colnames(mcs2_parent_interview_sub_tidy_final)=="mcsid" | colnames(mcs2_parent_interview_sub_tidy_final)=="MCSID") 
names(mcs2_parent_interview_sub_tidy_final) <- paste0(names(mcs2_parent_interview_sub_tidy_final), "_p") #p for parent interview
#mcsid and cnum have to be changed back to original
colnames(mcs2_parent_interview_sub_tidy_final)[index_cnum] <- "cnum" 
colnames(mcs2_parent_interview_sub_tidy_final)[index_mcsid] <- "mcsid"

wave_2_derived_parent <- merge(mcs2_derived_variables_sub_tidy_final, mcs2_parent_interview_sub_tidy_final,  by=c("mcsid","cnum"), all=TRUE) #datasets have to be merged seperately, only two can be merged at once
wave_2_merged_final <- merge(wave_2_derived_parent, mcs2_geographically_linked_data_sub,  by=c("mcsid"), all=TRUE)

##Wave 3----
mcs3_derived_variables_sub_tidy_final$cnum <- as.character(mcs3_derived_variables_sub_tidy_final$cnum) #change class from integer to character
mcs3_derived_variables_sub_tidy_final$cnum <- tolower(mcs3_derived_variables_sub_tidy_final$cnum) #change upper to lowercase characters
index_cnum <- which(colnames(mcs3_derived_variables_sub_tidy_final)=="cnum" | colnames(mcs3_derived_variables_sub_tidy_final)=="CNUM") 
index_mcsid <- which(colnames(mcs3_derived_variables_sub_tidy_final)=="mcsid" | colnames(mcs3_derived_variables_sub_tidy_final)=="MCSID") 
names(mcs3_derived_variables_sub_tidy_final) <- paste0(names(mcs3_derived_variables_sub_tidy_final), "_d") #d for derived variable
#mcsid and cnum have to be changed back to original
colnames(mcs3_derived_variables_sub_tidy_final)[index_cnum] <- "cnum" 
colnames(mcs3_derived_variables_sub_tidy_final)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- which(colnames(mcs3_geographically_linked_data_sub)=="mcsid" | colnames(mcs3_geographically_linked_data_sub)=="MCSID") 
names(mcs3_geographically_linked_data_sub) <- paste0(names(mcs3_geographically_linked_data_sub), "_g") #g for geographically linked data 
#mcsid has to be changed back to original
colnames(mcs3_geographically_linked_data_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs3_parent_interview_sub_tidy_all_final)=="cnum" | colnames(mcs3_parent_interview_sub_tidy_all_final)=="CNUM") 
index_mcsid <- which(colnames(mcs3_parent_interview_sub_tidy_all_final)=="mcsid" | colnames(mcs3_parent_interview_sub_tidy_all_final)=="MCSID") 
names(mcs3_parent_interview_sub_tidy_all_final) <- paste0(names(mcs3_parent_interview_sub_tidy_all_final), "_p") #p for parent interview
#mcsid and cnum have to be changed back to original
colnames(mcs3_parent_interview_sub_tidy_all_final)[index_cnum] <- "cnum" 
colnames(mcs3_parent_interview_sub_tidy_all_final)[index_mcsid] <- "mcsid"

wave_3_derived_parent <- merge(mcs3_derived_variables_sub_tidy_final, mcs3_parent_interview_sub_tidy_all_final,  by=c("mcsid","cnum"), all=TRUE) #datasets have to be merged seperately, only two can be merged at once
wave_3_merged_final <- merge(wave_3_derived_parent, mcs3_geographically_linked_data_sub,  by=c("mcsid"), all=TRUE)

##Wave 4----
mcs4_derived_variables_sub_tidy_final$cnum <- as.character(mcs4_derived_variables_sub_tidy_final$cnum) #change class from integer to character
mcs4_derived_variables_sub_tidy_final$cnum <- tolower(mcs4_derived_variables_sub_tidy_final$cnum) #change upper to lowercase characters
index_cnum <- which(colnames(mcs4_derived_variables_sub_tidy_final)=="cnum" | colnames(mcs4_derived_variables_sub_tidy_final)=="CNUM") 
index_mcsid <- which(colnames(mcs4_derived_variables_sub_tidy_final)=="mcsid" | colnames(mcs4_derived_variables_sub_tidy_final)=="MCSID") 
names(mcs4_derived_variables_sub_tidy_final) <- paste0(names(mcs4_derived_variables_sub_tidy_final), "_d") #d for derived variable
#mcsid and cnum have to be changed back to original
colnames(mcs4_derived_variables_sub_tidy_final)[index_cnum] <- "cnum" 
colnames(mcs4_derived_variables_sub_tidy_final)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- which(colnames(mcs4_geographically_linked_data_sub)=="mcsid" | colnames(mcs4_geographically_linked_data_sub)=="MCSID")
names(mcs4_geographically_linked_data_sub) <- paste0(names(mcs4_geographically_linked_data_sub), "_g") #g for geographically linked data 
#mcsid has to be changed back to original
colnames(mcs4_geographically_linked_data_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs4_parent_interview_sub_tidy_all_final)=="cnum" | colnames(mcs4_parent_interview_sub_tidy_all_final)=="CNUM") 
index_mcsid <- which(colnames(mcs4_parent_interview_sub_tidy_all_final)=="mcsid" | colnames(mcs4_parent_interview_sub_tidy_all_final)=="MCSID")
names(mcs4_parent_interview_sub_tidy_all_final) <- paste0(names(mcs4_parent_interview_sub_tidy_all_final), "_p") #t for parent interview
#mcsid and cnum have to be changed back to original
colnames(mcs4_parent_interview_sub_tidy_all_final)[index_cnum] <- "cnum" 
colnames(mcs4_parent_interview_sub_tidy_all_final)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs4_teacher_sub)=="DCCNUM00" | colnames(mcs4_teacher_sub)=="dccnum00") 
index_mcsid <- which(colnames(mcs4_teacher_sub)=="mcsid" | colnames(mcs4_teacher_sub)=="MCSID")
names(mcs4_teacher_sub) <- paste0(names(mcs4_teacher_sub), "_t") #t for teacher
#in teacher dataset, cnum is coded as DCCNUM00, in order to match participants later on, this column also has to be renamed to cnum
#in addition, cnum is coded as 1, 2, 3, whereas in the other datasets it is coded as a, b, c. This has to be changed too!
#mcsid and cnum have to be changed back to original
colnames(mcs4_teacher_sub)[index_cnum] <- "cnum" 
mcs4_teacher_sub$cnum[mcs4_teacher_sub$cnum == "1"] <- "a"
mcs4_teacher_sub$cnum[mcs4_teacher_sub$cnum == "2"] <- "b"
mcs4_teacher_sub$cnum[mcs4_teacher_sub$cnum == "3"] <- "c"
colnames(mcs4_teacher_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs4_cm_self_completion_final_sub)=="dccnum00" | colnames(mcs4_cm_self_completion_final_sub)=="DCCNUM00") 
index_mcsid <- which(colnames(mcs4_cm_self_completion_final_sub)=="mcsid" | colnames(mcs4_cm_self_completion_final_sub)=="MCSID")
names(mcs4_cm_self_completion_final_sub) <- paste0(names(mcs4_cm_self_completion_final_sub), "_c") #c for child
#in self_completion dataset, cnum is coded as dccnum00, in order to match participants later on, this column also has to be renamed to cnum
#in addition, cnum is coded as 1, 2, 3, whereas in the other datasets it is coded as a, b, c. This has to be changed too!
#mcsid and cnum have to be changed back to original
colnames(mcs4_cm_self_completion_final_sub)[index_cnum] <- "cnum" 
mcs4_cm_self_completion_final_sub$cnum[mcs4_cm_self_completion_final_sub$cnum == "1"] <- "a"
mcs4_cm_self_completion_final_sub$cnum[mcs4_cm_self_completion_final_sub$cnum == "2"] <- "b"
mcs4_cm_self_completion_final_sub$cnum[mcs4_cm_self_completion_final_sub$cnum == "3"] <- "c"
colnames(mcs4_cm_self_completion_final_sub)[index_mcsid] <- "mcsid"

wave_4_derived_parent <- merge(mcs4_derived_variables_sub_tidy_final, mcs4_parent_interview_sub_tidy_all_final,  by=c("mcsid","cnum"), all=TRUE) #datasets have to be merged seperately, only two can be merged at once
wave_4_derived_parent_geo <- merge(wave_4_derived_parent, mcs4_geographically_linked_data_sub,  by=c("mcsid"), all=TRUE)
wave_4_derived_parent_geo_teacher <- merge(wave_4_derived_parent_geo, mcs4_teacher_sub,  by=c("mcsid", "cnum"), all=TRUE)
wave_4_merged_final <- merge(wave_4_derived_parent_geo_teacher, mcs4_cm_self_completion_final_sub,  by=c("mcsid", "cnum"), all=TRUE)


##Wave 5----
#in wave 5, datasets are already in one-cm-per-line format (exception, parent interview, parent derived and parent cm interview where there are two rows per family, one for main and one for partner or one for main and one for child), this means that cnum is always coded as 1, 2, 3, (before merging this wave with the previous waves, this has to be changed to a, b, c). Also, within each dataset, the cnum variable is still coded as ..cnum00 To allow merging, this variable has to be renamed to cnum within each dataset.
index_cnum <- which(colnames(mcs5_cm_derived_sub)=="ecnum00" | colnames(mcs5_cm_derived_sub)=="ECNUM00") 
index_mcsid <- which(colnames(mcs5_cm_derived_sub)=="mcsid" | colnames(mcs5_cm_derived_sub)=="MCSID")
names(mcs5_cm_derived_sub) <- paste0(names(mcs5_cm_derived_sub), "_cd") #cd for child derived 
colnames(mcs5_cm_derived_sub)[index_cnum] <- "cnum" 
colnames(mcs5_cm_derived_sub)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- which(colnames(mcs5_geographically_linked_data_sub)=="mcsid" | colnames(mcs5_geographically_linked_data_sub)=="MCSID")
names(mcs5_geographically_linked_data_sub) <- paste0(names(mcs5_geographically_linked_data_sub), "_g") #g for geographically linked data 
colnames(mcs5_geographically_linked_data_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs5_cm_teacher_survey_sub)=="ecnum00" | colnames(mcs5_cm_teacher_survey_sub)=="ECNUM00") 
index_mcsid <- which(colnames(mcs5_cm_teacher_survey_sub)=="mcsid" | colnames(mcs5_cm_teacher_survey_sub)=="MCSID")
names(mcs5_cm_teacher_survey_sub) <- paste0(names(mcs5_cm_teacher_survey_sub), "_t") #t for teacher
colnames(mcs5_cm_teacher_survey_sub)[index_cnum] <- "cnum" 
colnames(mcs5_cm_teacher_survey_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs5_cm_interview_sub)=="ecnum00" | colnames(mcs5_cm_interview_sub)=="ECNUM00") 
index_mcsid <- which(colnames(mcs5_cm_interview_sub)=="mcsid" | colnames(mcs5_cm_interview_sub)=="MCSID")
names(mcs5_cm_interview_sub) <- paste0(names(mcs5_cm_interview_sub), "_c") #c for child
colnames(mcs5_cm_interview_sub)[index_cnum] <- "cnum" 
colnames(mcs5_cm_interview_sub)[index_mcsid] <- "mcsid"

index_mcsid <- which(colnames(mcs5_family_derived_sub)=="mcsid" | colnames(mcs5_family_derived_sub)=="MCSID") 
names(mcs5_family_derived_sub) <- paste0(names(mcs5_family_derived_sub), "_t") #f for teacher
colnames(mcs5_family_derived_sub)[index_mcsid] <- "mcsid"

#parent interview, parent derived and parent cm interview sometimes have  two rows per family, 1 for main and one for partner, these have to be matched based on pnum
index_pnum <- which(colnames(mcs5_parent_interview_sub)=="epnum00" | colnames(mcs5_parent_interview_sub)=="EPNUM00") 
index_mcsid <- which(colnames(mcs5_parent_interview_sub)=="mcsid" | colnames(mcs5_parent_interview_sub)=="MCSID")
names(mcs5_parent_interview_sub) <- paste0(names(mcs5_parent_interview_sub), "_p") #p for parent interview
colnames(mcs5_parent_interview_sub)[index_pnum] <- "pnum"
colnames(mcs5_parent_interview_sub)[index_mcsid] <- "mcsid"

index_pnum <- which(colnames(mcs5_parent_derived_sub)=="epnum00" | colnames(mcs5_parent_derived_sub)=="EPNUM00") 
index_mcsid <- which(colnames(mcs5_parent_derived_sub)=="mcsid" | colnames(mcs5_parent_derived_sub)=="MCSID")
names(mcs5_parent_derived_sub) <- paste0(names(mcs5_parent_derived_sub), "_pd") #pd for parent derived
colnames(mcs5_parent_derived_sub)[index_pnum] <- "pnum"
colnames(mcs5_parent_derived_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs5_parent_cm_interview_sub)=="ecnum00" | colnames(mcs5_parent_cm_interview_sub)=="ECNUM00") 
index_pnum <- which(colnames(mcs5_parent_cm_interview_sub)=="epnum00" | colnames(mcs5_parent_cm_interview_sub)=="EPNUM00") 
index_mcsid <- which(colnames(mcs5_parent_cm_interview_sub)=="mcsid" | colnames(mcs5_parent_cm_interview_sub)=="MCSID")
names(mcs5_parent_cm_interview_sub) <- paste0(names(mcs5_parent_cm_interview_sub), "_b") #b for both child and parent
colnames(mcs5_parent_cm_interview_sub)[index_cnum] <- "cnum" 
colnames(mcs5_parent_cm_interview_sub)[index_pnum] <- "pnum" 
colnames(mcs5_parent_cm_interview_sub)[index_mcsid] <- "mcsid"

wave_5_cmderived_teacher <- merge(mcs5_cm_derived_sub,  mcs5_cm_teacher_survey_sub, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm <- merge(wave_5_cmderived_teacher,  mcs5_cm_interview_sub, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm_geo <- merge(wave_5_cmderived_teacher_cm,  mcs5_geographically_linked_data_sub, by=c("mcsid"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm <- merge(wave_5_cmderived_teacher_cm_geo,  mcs5_parent_cm_interview_sub, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm_pderived <- merge(wave_5_cmderived_teacher_cm_geo_parentcm, mcs5_parent_derived_sub, by=c("mcsid",  "pnum"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm_pderived_family <- merge(wave_5_cmderived_teacher_cm_geo_parentcm_pderived, mcs5_family_derived_sub, by=c("mcsid"), all=TRUE)
wave_5_merged_final <- merge(wave_5_cmderived_teacher_cm_geo_parentcm_pderived_family, mcs5_parent_interview_sub, by=c("mcsid",  "pnum"), all=TRUE)

#parent interview, parent derived and parent child intervied had to be changed to one-child-per row level. Now, they can be merged together on a child level. 
index_mcsid <- which(colnames(mcs5_parent_interview_sub_tidy)=="mcsid" | colnames(mcs5_parent_interview_sub_tidy)=="MCSID")
names(mcs5_parent_interview_sub_tidy) <- paste0(names(mcs5_parent_interview_sub_tidy), "_p") #p for parent interview
colnames(mcs5_parent_interview_sub_tidy)[index_mcsid]<- "mcsid"

index_mcsid <- which(colnames(mcs5_parent_derived_sub_tidy)=="mcsid" | colnames(mcs5_parent_derived_sub_tidy)=="MCSID")
names(mcs5_parent_derived_sub_tidy) <- paste0(names(mcs5_parent_derived_sub_tidy), "_pd") #pd for parent derived
colnames(mcs5_parent_derived_sub_tidy)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs5_parent_cm_interview_sub_tidy)=="ecnum00" | colnames(mcs5_parent_cm_interview_sub_tidy)=="ECNUM00") 
index_mcsid <- which(colnames(mcs5_parent_cm_interview_sub_tidy)=="mcsid" | colnames(mcs5_parent_cm_interview_sub_tidy)=="MCSID")
names(mcs5_parent_cm_interview_sub_tidy) <- paste0(names(mcs5_parent_cm_interview_sub_tidy), "_b") #b for both child and parent
colnames(mcs5_parent_cm_interview_sub_tidy)[index_cnum] <- "cnum" 
colnames(mcs5_parent_cm_interview_sub_tidy)[index_mcsid] <- "mcsid"

#merge the child level dataset together with the other wave5 dataset to create one child level wave 5 dataset
wave_5_cmderived_teacher_cl <- merge(mcs5_cm_derived_sub,  mcs5_cm_teacher_survey_sub, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm_cl <- merge(wave_5_cmderived_teacher_cl,  mcs5_cm_interview_sub, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_cl <- merge(wave_5_cmderived_teacher_cm_cl,  mcs5_geographically_linked_data_sub, by=c("mcsid"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm_cl <- merge(wave_5_cmderived_teacher_cm_geo_cl,  mcs5_parent_cm_interview_sub_tidy, by=c("mcsid","cnum"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm_pderived_cl <- merge(wave_5_cmderived_teacher_cm_geo_parentcm_cl, mcs5_parent_derived_sub_tidy, by=c("mcsid"), all=TRUE)
wave_5_cmderived_teacher_cm_geo_parentcm_pderived_family_cl <- merge(wave_5_cmderived_teacher_cm_geo_parentcm_pderived_cl, mcs5_family_derived_sub, by=c("mcsid"), all=TRUE)
wave_5_merged_final_child_level <- merge(wave_5_cmderived_teacher_cm_geo_parentcm_pderived_family_cl, mcs5_parent_interview_sub_tidy, by=c("mcsid"), all=TRUE)


##Wave 6----
index_cnum <- which(colnames(mcs6_cm_derived_sub)=="fcnum00" | colnames(mcs6_cm_derived_sub)=="FCNUM00") 
index_mcsid <- which(colnames(mcs6_cm_derived_sub)=="mcsid" | colnames(mcs6_cm_derived_sub)=="MCSID") 
names(mcs6_cm_derived_sub) <- paste0(names(mcs6_cm_derived_sub), "_cd") #cd for child derived 
colnames(mcs6_cm_derived_sub)[index_cnum] <- "cnum" 
colnames(mcs6_cm_derived_sub)[index_mcsid] <- "mcsid" #also change from capital letters to small letters

index_mcsid <- which(colnames(mcs6_family_derived_sub)=="mcsid" | colnames(mcs6_family_derived_sub)=="MCSID") 
names(mcs6_family_derived_sub) <- paste0(names(mcs6_family_derived_sub), "_t") #f for teacher
colnames(mcs6_family_derived_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs6_cm_interview_sub)=="fcnum00" | colnames(mcs6_cm_interview_sub)=="FCNUM00") 
index_mcsid <- which(colnames(mcs6_cm_interview_sub)=="mcsid" | colnames(mcs6_cm_interview_sub)=="MCSID") 
names(mcs6_cm_interview_sub) <- paste0(names(mcs6_cm_interview_sub), "_c") #c for child
colnames(mcs6_cm_interview_sub)[index_cnum] <- "cnum" 
colnames(mcs6_cm_interview_sub)[index_mcsid] <- "mcsid"

#parent interview, parent derived and parent cm interview sometimes have two rows per family, 1 for main and one for partner, these have to be matched based on pnum
index_pnum <- which(colnames(mcs6_parent_interview_sub)=="fpnum00" | colnames(mcs6_parent_interview_sub)=="FPNUM00") 
index_mcsid <- which(colnames(mcs6_parent_interview_sub)=="mcsid" | colnames(mcs6_parent_interview_sub)=="MCSID") 
names(mcs6_parent_interview_sub) <- paste0(names(mcs6_parent_interview_sub), "_p") #p for parent interview
colnames(mcs6_parent_interview_sub)[index_pnum] <- "pnum"
colnames(mcs6_parent_interview_sub)[index_mcsid] <- "mcsid"

index_pnum <- which(colnames(mcs6_parent_derived_sub)=="fpnum00" | colnames(mcs6_parent_derived_sub)=="FPNUM00") 
index_mcsid <- which(colnames(mcs6_parent_derived_sub)=="mcsid" | colnames(mcs6_parent_derived_sub)=="MCSID") 
names(mcs6_parent_derived_sub) <- paste0(names(mcs6_parent_derived_sub), "_pd") #pd for parent derived
colnames(mcs6_parent_derived_sub)[index_pnum] <- "pnum"
colnames(mcs6_parent_derived_sub)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs6_parent_cm_interview_sub)=="fcnum00" | colnames(mcs6_parent_cm_interview_sub)=="FCNUM00") 
index_pnum <- which(colnames(mcs6_parent_cm_interview_sub)=="fpnum00" | colnames(mcs6_parent_cm_interview_sub)=="FPNUM00") 
index_mcsid <- which(colnames(mcs6_parent_cm_interview_sub)=="mcsid" | colnames(mcs6_parent_cm_interview_sub)=="MCSID") 
names(mcs6_parent_cm_interview_sub) <- paste0(names(mcs6_parent_cm_interview_sub), "_b") #b for both child and parent
colnames(mcs6_parent_cm_interview_sub)[index_cnum] <- "cnum" 
colnames(mcs6_parent_cm_interview_sub)[index_pnum] <- "pnum" 
colnames(mcs6_parent_cm_interview_sub)[index_mcsid] <- "mcsid"

wave_6_cmderived_cm <- merge(mcs6_cm_derived_sub,  mcs6_cm_interview_sub, by=c("mcsid","cnum"), all=TRUE)
wave_6_cmderived_cm_family <- merge(wave_6_cmderived_cm, mcs6_family_derived_sub, by=c("mcsid"), all=TRUE)
wave_6_cmderived_cm_family_parentcm <- merge(wave_6_cmderived_cm_family, mcs6_parent_cm_interview_sub, by=c("mcsid", "cnum"), all=TRUE)
wave_6_cmderived_cm_family_parentcm_pderived <- merge(wave_6_cmderived_cm_family_parentcm, mcs6_parent_derived_sub, by=c("mcsid",  "pnum"), all=TRUE)
wave_6_merged_final <- merge(wave_6_cmderived_cm_family_parentcm_pderived, mcs6_parent_interview_sub, by=c("mcsid",  "pnum"), all=TRUE)

#parent interview, parent derived and parent child intervied had to be changed to one-child-per row level. Now, they can be merged together on a child level. 
index_mcsid <- which(colnames(mcs6_parent_interview_sub_tidy)=="mcsid" | colnames(mcs6_parent_interview_sub_tidy)=="MCSID") 
names(mcs6_parent_interview_sub_tidy) <- paste0(names(mcs6_parent_interview_sub_tidy), "_p") #p for parent interview
colnames(mcs6_parent_interview_sub_tidy)[index_mcsid]<- "mcsid"

index_mcsid <- which(colnames(mcs6_parent_derived_sub_tidy)=="mcsid" | colnames(mcs6_parent_derived_sub_tidy)=="MCSID") 
names(mcs6_parent_derived_sub_tidy) <- paste0(names(mcs6_parent_derived_sub_tidy), "_pd") #pd for parent derived
colnames(mcs6_parent_derived_sub_tidy)[index_mcsid] <- "mcsid"

index_cnum <- which(colnames(mcs6_parent_cm_interview_sub_tidy)=="fcnum00" | colnames(mcs6_parent_cm_interview_sub_tidy)=="FCNUM00") 
index_mcsid <- which(colnames(mcs6_parent_cm_interview_sub_tidy)=="mcsid" | colnames(mcs6_parent_cm_interview_sub_tidy)=="MCSID") 
names(mcs6_parent_cm_interview_sub_tidy) <- paste0(names(mcs6_parent_cm_interview_sub_tidy), "_b") #b for both child and parent
colnames(mcs6_parent_cm_interview_sub_tidy)[index_cnum] <- "cnum" 
colnames(mcs6_parent_cm_interview_sub_tidy)[index_mcsid] <- "mcsid"

#merge the child level dataset together with the other wave5 dataset to create one child level wave 6 dataset
wave_6_cmderived_cm_cl <- merge(mcs6_cm_derived_sub,  mcs6_cm_interview_sub, by=c("mcsid","cnum"), all=TRUE)
wave_6_cmderived_cm_family_cl <- merge(wave_6_cmderived_cm_cl, mcs6_family_derived_sub, by=c("mcsid"), all=TRUE)
wave_6_cmderived_cm_family_parentcm_cl <- merge(wave_6_cmderived_cm_family_cl, mcs6_parent_cm_interview_sub_tidy, by=c("mcsid", "cnum"), all=TRUE)
wave_6_cmderived_cm_family_parentcm_pderived_cl <- merge(wave_6_cmderived_cm_family_parentcm_cl, mcs6_parent_derived_sub_tidy, by=c("mcsid"), all=TRUE)
wave_6_merged_final_child_level <- merge(wave_6_cmderived_cm_family_parentcm_pderived_cl, mcs6_parent_interview_sub_tidy, by=c("mcsid"), all=TRUE)

#Merge all datasets together (keeping parent level: main and partner)----
#before merging all datasets together, cnum has to be coded consistently, at the moment it is coded as 1, 2, 3 in hosp and wave 5 and 6, but as a, b, c, in wave 1,2,3,4. Wave 1, 2, 3, 4, will now be changed to 1, 2, 3 too.

wave_1_merged_final$cnum[wave_1_merged_final$cnum == "a"] <- "1"
wave_1_merged_final$cnum[wave_1_merged_final$cnum == "b"] <- "2"
wave_1_merged_final$cnum[wave_1_merged_final$cnum == "c"] <- "3"

wave_2_merged_final$cnum[wave_2_merged_final$cnum == "a"] <- "1"
wave_2_merged_final$cnum[wave_2_merged_final$cnum == "b"] <- "2"
wave_2_merged_final$cnum[wave_2_merged_final$cnum == "c"] <- "3"

wave_3_merged_final$cnum[wave_3_merged_final$cnum == "a"] <- "1"
wave_3_merged_final$cnum[wave_3_merged_final$cnum == "b"] <- "2"
wave_3_merged_final$cnum[wave_3_merged_final$cnum == "c"] <- "3"

wave_4_merged_final$cnum[wave_4_merged_final$cnum == "a"] <- "1"
wave_4_merged_final$cnum[wave_4_merged_final$cnum == "b"] <- "2"
wave_4_merged_final$cnum[wave_4_merged_final$cnum == "c"] <- "3"

hosp_w1<- merge(wave_1_merged_final, hosp_merged_final,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2 <- merge(hosp_w1, wave_2_merged_final,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2_w3 <- merge(hosp_w1_w2, wave_3_merged_final,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2_w3_w4 <- merge(hosp_w1_w2_w3, wave_4_merged_final,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2_w3_w4_w5 <- merge(hosp_w1_w2_w3_w4, wave_5_merged_final,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2_w3_w4_w5_w6 <- merge(hosp_w1_w2_w3_w4_w5, wave_6_merged_final,  by = c("mcsid", "cnum", "pnum"), all=T)

### add mcs_longitudinal_family_file
index_mcsid <- which(colnames(mcs_longitudinal_family_file_sub)=="mcsid" | colnames(mcs_longitudinal_family_file_sub)=="MCSID")
colnames(mcs_longitudinal_family_file_sub)[index_mcsid] <- "mcsid" # change from capital letters to small letters
parent_level <- merge(hosp_w1_w2_w3_w4_w5_w6, mcs_longitudinal_family_file_sub,  by=c("mcsid"), all=T)



###merge all IMD files from wave 6 together
mcs_sweep6_imd_e_n <- merge(mcs_sweep6_imd_e_2004_sub, mcs_sweep6_imd_n_2004_sub, by=c("MCSID"), all=T)
mcs_sweep6_imd_e_n_s <- merge(mcs_sweep6_imd_e_n, mcs_sweep6_imd_s_2004_sub, by=c("MCSID"), all=T)
mcs_sweep6_imd <- merge(mcs_sweep6_imd_e_n_s, mcs_sweep6_imd_w_2004_sub, by=c("MCSID"), all=T)
index_mcsid <- which(colnames(mcs_sweep6_imd)=="mcsid" | colnames(mcs_sweep6_imd)=="MCSID")
colnames(mcs_sweep6_imd)[index_mcsid] <- "mcsid" # change from capital letters to small letters


###add wave6 IMD files
final_parent_level <- merge(parent_level, mcs_sweep6_imd, by=c("mcsid"), all=T)



#Merge all datasets together (child level only)----
hosp_w1_w2_w3_w4_w5_child_level <- merge(hosp_w1_w2_w3_w4, wave_5_merged_final_child_level,  by = c("mcsid", "cnum"), all=T)
hosp_w1_w2_w3_w4_w5_w6_child_level <- merge(hosp_w1_w2_w3_w4_w5_child_level, wave_6_merged_final_child_level,  by = c("mcsid", "cnum"), all=T)

### add mcs_longitudinal_family_file
child_level <- merge(hosp_w1_w2_w3_w4_w5_w6_child_level, mcs_longitudinal_family_file_sub,  by=c("mcsid"), all=T)

###add wave6 IMD files
final_child_level <- merge(child_level, mcs_sweep6_imd, by=c("mcsid"), all=T)


#save final datasets----
alldata_parent_level <- final_parent_level 
alldata_child_level <- final_child_level

#save data as csv files
write.csv(alldata_parent_level, file = "data/alldata_parent_level.csv")
write.csv(alldata_child_level, file = "data/alldata_child_level.csv")

