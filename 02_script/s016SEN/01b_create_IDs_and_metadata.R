#------------------------------------------------------------------------------#
# TITLE:      01b_create_IDs_and_metadata.R                                    #
# PURPOSE:    Create a unique household number (uhn) to allow for merging.     #
#             Create a dataframe containing all important metadata of the      #
#             survey rounds.                                                   #
# INPUTS:     One dataframe per survey round, named rXX with XX being the time #
#             in months between baseline and this rounds data collection.      #
# OUTPUTS:    A list <dfs> of dataframes, each containing the data from one    #
#             survey round, now containing a column "uhn".                     #
#             <waves>: a dataframe containing relevant metadata about the      #
#             survey rounds.                                                   #                      
# PUBLISHED:  2021-04-21                                                       # 
# AUTHOR:     Janina Roemer                                                    #
#------------------------------------------------------------------------------#

## Section 1: Create a universal respondent ID: unique household number (uhn) ##
# No universal identifier was found in the raw data, that would allow
# for merging of the baseline data with the CATI rounds. A personal respondent
# identifier could not be created since no matching personal IDs were provided.
# Therefore a universal household identifier (uhn) that allows to merge baseline
# data from the kish data set and CATI data will be created in the following.
# Baseline: The baseline data contain a column "grappe" (DR number = Census 
#   tract number) and  a column "num_menage" (Number of houshold within the DR). 
#   These are now combined to a universal household number, uniquely identifying 
#   the household.
# CATI rounds: All CATI round contain a column "ID du manage" (or "Code 
#   d'identification de l'individu kish" in the case of r15) in which the uhn   
#   created for the baseline is contained. For r08, r10, r12 and r18E the format 
#   of this number is different than in the remaining datasets. Depending on the 
#   round different digits of this number are extracted to construct uhn. 
#   In all cases it is assumed that the first three digits of "ID du menage"
#   correspond to the baseline column "grappe" and that the last two digits of
#   "ID du menage" correspond to the baseline column "num_menage". The first 
#   For all CATI waves that contain a "DR" column the script stops if the 
#   column "DR" does not equal the first 3 digits of the "ID du menage" column 
#   in any CATI round dataset, since this would propose that the first 
#   assumption was not fulfilled, 
# Only if all values of uhn are unique and appear in the baseline, the creation
# of the identifiers is considered successful. 
# CATI rounds for which the uhn creation failed are dropped from the analysis.
# The following issues arise, preventing analysis of r08, r10 and r18E:
# r08: No baseline-compatible uhn could be created: No way to partition 
# the digits of household ID to numbers with ranges that match household number 
# and DR. DR provided, but household number missing.
# r18E (Education): Although the ID_Men in r18E appears to have  similar format 
# to the original household ID columns of the other CATI rounds, the digits that 
# correspond to the household number in other rounds however have 82 outliers 
# that are higher than the maximum household number of 25.
# 

  ## A: Creating uhn for the baseline -----------------------------------------
  ## Baseline contains a column "grappe" (DR number = Census tract number) and 
  ## a column "num_menage" (Number of houshold within the DR). 
  ## These are now combined to a universal household number, uniquely  
  ## identifying the household. 
  
    # The dataset "L2S_Fichier_Kish_v2.sav" is used as baseline since this  
    # should only contain the randomly selected household members that were 
    # later called for the CATI rounds.
    baseline$uhn <- baseline$grappe*100 + as.numeric(baseline$num_menage)       # First 3 digits = Census tract (DR), last 2 digits = household within the DR.
    stopifnot(sum(duplicated(baseline$uhn)) == 0)                               # Control mechanism: Stop if any uhn is not unique.
    baseline <- baseline[ , c(ncol(baseline), 1:(ncol(baseline)-1))]            # Sort columns to have ID first.


  ## B: Create baseline-compatible uhn for CATI rounds ------------------------
  ## All CATI round contain a column "ID du manage" in which the uhn created  
  ## for the baseline is contained. Depending on the round, slightly different
  ## formatting is required. 
    stopifnot(sum(nchar(r21$A7_r)!= 8) == 0)                                    # Control mechanism: Stop if any "ID du menage" deviates from expected number of digits.
    stopifnot(sum(r21$DR != substring(r21$A7_r, 1,3), na.rm = T)== 0)           # Control mechanism: Stop if the first 3 digits of "ID du menage" are not equal to "DR".
    r21$uhn <- as.numeric(substring(r21$A7_r, 1,3))*100 +                       # uhn is created from the first 3 and last 2 digits of "ID du menage"
                                    as.numeric(substring(r21$A7_r, 7,8))
    stopifnot(sum(duplicated(r21$uhn)) == 0)                                    # Control mechanism: Stop if any uhn is not unique
    stopifnot(sum(r21$uhn %in% baseline$uhn) == nrow(r21))                      # Control mechanism: Stop if any uhn from this round does not exist in baseline.
    
  # uhn for r15, r22, r25, r18 and r24 is built identical to previous one,  
  # only adapting for the different column names of "ID du menage".
    stopifnot(sum(nchar(r15$a8_r)!= 8) == 0)   
    r15$uhn <- as.numeric(substring(r15$a8_r, 1,3))*100 + 
                                    as.numeric(substring(r15$a8_r, 7,8))
    stopifnot(sum(duplicated(r15$uhn)) == 0) 
    stopifnot(sum(r15$uhn %in% baseline$uhn) == nrow(r15) )
    
    stopifnot(sum(nchar(r22$A7_r)!= 8) == 0)
    stopifnot(sum(r22$DR != substring(r22$A7_r, 1,3), na.rm = T)== 0)
    r22$uhn <- as.numeric(substring(r22$A7_r, 1,3))*100 +
                                    as.numeric(substring(r22$A7_r, 7,8))
    stopifnot(sum(duplicated(r22$uhn)) == 0)
    stopifnot(sum(r22$uhn %in% baseline$uhn) == nrow(r22))
    
    stopifnot(sum(nchar(r25$A7_r)!= 8) == 0) 
    stopifnot(sum(r25$DR != substring(r25$A7_r, 1,3), na.rm = T)== 0)
    r25$uhn <- as.numeric(substring(r25$A7_r, 1,3))*100 + 
                                    as.numeric(substring(r25$A7_r, 7,8))
    stopifnot(sum(duplicated(r25$uhn)) == 0 )
    stopifnot(sum(r25$uhn %in% baseline$uhn) == nrow(r25))
    
    stopifnot(sum(nchar(r18$ID_Men)!=8) == 0)
    r18$uhn <- as.numeric(substring(r18$ID_Men, 1, 3))*100 + 
                                    as.numeric(substring(r18$ID_Men, 7,8))
    stopifnot(sum(duplicated(r18$uhn))  == 0 )                        
    stopifnot(sum(r18$uhn %in% baseline$uhn) == nrow(r18))
    
    stopifnot(sum(nchar(r24$A7_r)!=8)== 0)
    stopifnot(sum(r24$DR != substring(r24$A7_r, 1,3), na.rm = T)== 0)
    r24$uhn <- as.numeric(substring(r24$A7_r, 1, 3))*100 + 
                                    as.numeric(substring(r24$A7_r, 7,8))
    stopifnot(sum(duplicated(r24$uhn))  == 0)                       
    stopifnot(sum(r24$uhn %in% baseline$uhn) == nrow(r24))
    
  # ID du menage in r12 is already in uhn format, but of type character
    stopifnot(sum(nchar(r12$a7)!=5)==0)                                         # Control mechanism
    stopifnot(sum(r12$DR != substring(r12$a7, 1,3), na.rm = T)== 0)             # Control mechanism
    r12$uhn <- as.numeric(r12$a7)                                               # simply transfer to numeric format to create uhn
    stopifnot(sum(duplicated(r12$uhn)) == 0)                                    # Control mechanism: Stop if any uhn is not unique
    stopifnot(sum(r12$uhn %in% baseline$uhn) == nrow(r12))                      # Control mechanism: Stop if any uhn from this round does not exist in baseline
    
    
 ## The creation of uhn failed for r10 and r18E. It is therefore commented out. 
    # # ID du menage in r10 has inconsistent number of digits => slightly different
    # # determination of uhn.
    # # Commented out, since 131 values of uhn of r10 do not exist in baseline.
    # # sum(nchar(r10$a5a)!= 5)   # [1] 1 !
    # library("stringr")
    # r10$uhn <- as.numeric(str_sub(r10$a5a, 1L,-3))*100 + 
    #                               as.numeric(str_sub(r10$a5a, -2,-1))           # since there is a missing leading 0 in one entry of a5a, this is not identical to the following column!
    # # r10$uhn <- as.numeric(substring(r10$a5a, 1,3))*100 + as.numeric(substring(r10$a5a, 4,5))
    # if (sum(duplicated(r10$uhn)) !=0 ) warning('uhns for r10 are not unique!')
    # if (sum(r10$uhn %in% baseline$uhn) != nrow(r10)) {
    #   warning(paste0(nrow(r10) - sum(r10$uhn %in% baseline$uhn), 
    #                           ' uhns of r10 do not appear in the baseline!'))   # creates a warning since 131 uhns do not exist  in baseline
    #   }
    # 
    # # No unique uhns could be created for r18E. => commented out
    # stopifnot(sum(nchar(r18E$A6_r)!= 9) == 0) 
    # r18E$uhn <- as.numeric(substring(r18E$A6_r, 1,3))*100 + as.numeric(substring(r18E$A6_r, 8,9))
    # if(sum(duplicated(r18E$uhn)) != 0) {
    # warning(paste(sum(duplicated(r18E$uhn)), 'duplicated uhns in r18E', sep = " "))   # creates a warning since 26 uhns are not unique
    # } 
    # if(sum(r18E$uhn %in% baseline$uhn) != nrow(r18E)){
    #   warning(paste0(nrow(r18E) - sum(r18E$uhn %in% baseline$uhn), 
    #                          ' uhns of r18E do not appear in the baseline!'))
    # } 
    


## Section 2: Summing up relevant information about the waves ##################
##            Creates a dataframe "waves" containing the following:           ##

# Short name of each survey round
wave_name <- c("baseline", "r21", "r18E", "r08", "r15", "r22", "r25", 
                                                    "r10", "r12", "r18", "r24")

# Start date of the data collection period of each round
start <- c("2014-10-23", "2016-07-19", "2016-04-18", "2015-07-09", "2016-01-18", 
           "2016-08-22", "2016-11-21", "2015-08-31", "2015-11-02", 
           "2016-04-18", "2016-11-02") 

# End date of the data collection period of each round
end <-   c("2014-12-14", "2016-08-09", "2016-05-07", "2015-08-04", "2016-02-03", 
           "2016-09-09", "2016-12-09", "2015-09-14", "2015-12-05", 
           "2016-05-07", "2016-11-11") 

# Survey topic of each round
topic <- c("Baseline", "Financial inclusion", "Education", "Food and Food Security", 
           "Food and Food Security", "Food and Food Security", "Food and Food Security", 
           "Electricity and Transport", "Health", "Living Conditions", "Tabaski")

# Time difference between each round and the baseline In DAYS (starting days)
time_from_BL <- as.numeric(as.Date(start, "%Y-%m-%d")) - 
               rep(as.numeric(as.Date("2014-10-23", "%Y-%m-%d"), length(start)))

# Time difference between each round and the baseline In MONTHS (starting days)
months_from_BL <- round(time_from_BL/30.5)

waves <- data.frame(wave_name, months_from_BL, topic, start, end, 
                    stringsAsFactors = FALSE)                                   # Combining the vectors to one dataframe

# Adding long names (for printing) to the dataframe
waves$long_name <- c("Baseline\n(Face to Face)", "Round 21", "Round 18 Education", "Round 8", 
                     "Round 15", "Round 22", "Round 25", "Round 10 ", "Round 12", 
                     "Round 18", "Round 24")

# Adding the duration of the data collection period                            
waves$duration <- as.numeric(as.Date(waves$end, "%Y-%m-%d")) - 
                  as.numeric(as.Date(waves$start, "%Y-%m-%d")) + 1

# For the purpose of cross validation a column containing the response rates 
# reported in the documentation is added to "waves"
waves$rr_documentation <- c(1, 0.89, 0.91, 0.93, 0.93, 0.89, 0.85, 0.95, 0.93, 
                            0.8753, 0.88)

# In order to be able to use the same scripts that work for data with unsure 
# data collection times, add a column "wave_number" which in this case will be 
# identical to  the number of months after baseline data collection start. 
waves$wave_number <- waves$months_from_BL

# In order to be able to use scripts that work for data with only one survey 
# date for round, add a column "date" containing the start date of collection. 
waves$date <- waves$start

# Sort the dataframe "waves" in alphabetical order of the wave_name.
waves <- waves[order(waves$wave_name), ]                                     

## Section 3: Creating a list containing the data of all waves.#################
##            A list <dfs> is created, which contains the data of the individual
##            survey rounds as elements. The wave_name of each survey round  
##            names the respective list element. 
dfs <- waves[ , 1] %>% as.list() %>% lapply(get)                                
names(dfs) <- waves[ , 1]                                                       # Naming the list elements with their wave-name


## Clean up ####################################################################
 rm(list = names(dfs))
