#------------------------------------------------------------------------------#
# TITLE:      02a_merge_recode_s016TZA.R                                       #      
# PURPOSE:    Merge the relevant variables of the F2F-Baseline survey with     # 
#             the columns of the CATI waves containing the call status,        # 
#             remove ambiguous observations and convert the demographic        #
#             variables to the universal format used in this analysis.         #
# INPUTS:     <dfs>: named list of dataframes (one per survey round).          #
# OUTPUTS:    <data_wide>: dataframe containing the relevant columns of the    # 
#                          baseline and call status of all waves.              # 
#             <data_wide_clean>: Identical to <data_wide> with the exception   #
#                                that all ambiguous observations have been     #
#                                removed.                                      # 
#             <data_wide_formatted>: Identical to <data_wide_clean> with the   #
#                                    exception that the demographic variables  #
#                                    have been converted to the universal      #
#                                    format used in this analysis and all      #
#                                    reserve household observations have been  #
#                                    removed.                                  #
# PUBLISHED:  2021-04-21                                                       # 
# AUTHOR:     Janina Roemer                                                    #            
#------------------------------------------------------------------------------#

## Section 1: Select relevant variables ######################################## 

    # Create a dataframe data_wide containing only columns of baseline containing 
    # information considered relevant for attrition analysis.
    data_wide <- dplyr::select(dfs[[1]], uhn,     # Unique household number
                                Sttng,            # Setting: 1 = rural, 2 = urban
                                Qn10201,          # gender identity
                                age,              # Age in years
                                Qn10601,          # Main occupation
                                edlevel,          # "Highest Level of schooling (grade) completed by respondent"
                                s1, s2, s3, s4,   # Variables that might help verify if the demographics in baseline correspond to the same person who did (or did not) answere to the later waves
                                s5,               # Interviewer observation: "Is ... the 1st, 2nd, 3rd or 4th randomly selected respondent?
                                Qn1010,           # "Is (Name) the selected respondent?" 
                                reshh,            # reserve household 
                                num_responses)    # Number of responses per observation
    
    
## Section 2: Format the dataframe #############################################
    
    # Create demographic columns with standardized column names for further analysis
    data_wide["de_urban"] <- data_wide$Sttng
    data_wide["de_gender"] <- data_wide$Qn10201                               
    data_wide["de_age"] <- data_wide$age
    data_wide["de_employed"] <- data_wide$Qn10601
    data_wide["de_agriculture"] <- data_wide$Qn10601
    data_wide["de_education"] <- data_wide$edlevel
    

    # Add wave suffix to all columns of <data_wide> other than "uhn"
    colnames(data_wide) <- paste(colnames(data_wide), "_baseline", sep = "")
    colnames(data_wide)[1] <- "uhn" 
  
  
    # Add a suffix identifying the wave to all columns but "uhn" (used for joining)
    for(i in seq_along(dfs)){
      if (i != 1){
        colnames(dfs[[i]])[colnames(dfs[[i]]) != "uhn"] <- 
          paste(colnames(dfs[[i]])[colnames(dfs[[i]]) != "uhn"], 
                                            names(dfs)[[i]], sep = "_")
      }
    }
    
## Section 3: Merging the call status information to data_wide  ################ 
  
  # Merge the call status columns of all survey rounds to data_wide matching by
  # the unique household number
  dfs_short <- dfs
  for (i in names(dfs)[2:length(dfs)]){
    # Select call status columns of each dataframe. 
    # q1 (education level) of r02 is kept for crosschecking. 
    if (i == "r02") {                                                    
      dfs_short[[i]] <- dplyr::select(dfs_short[[i]], uhn, 
                                      (paste0("q1_", i)),
                                      (paste0("num_responses_", i)),
                                      (paste0("respondent_", i)))

    }
   
    # Status column of r08 is kept.
    else if (i == "r08") {                                                    
      dfs_short[[i]] <- dplyr::select(dfs_short[[i]], uhn, 
                                      (paste0("Status_", i)),
                                      (paste0("num_responses_", i)),
                                      (paste0("respondent_", i)))
    }
    
    # Call-status column (type2) of r14 is kept.
    else if (i == "r14") {                                                    
      dfs_short[[i]] <- dplyr::select(dfs_short[[i]], uhn, 
                                      (paste0("type1_", i)),
                                      (paste0("type2_", i)),
                                      (paste0("num_responses_", i)),
                                      (paste0("respondent_", i)))
    }     
    # For all other survey rounds only the number of responses and the 
    # "respondent" dummy is kept.  
    else {
      dfs_short[[i]] <-  dplyr::select(dfs_short[[i]], uhn, 
                                       (paste0("num_responses_", i)),
                                       (paste0("respondent_", i)))
    }
    
    # Merge to data_wide.
    data_wide <- left_join(data_wide, dfs_short[[i]], by = "uhn")    
    

    # Consistency check: Stop if at any i the sum of NAs in the respondent  
    # column is not equal to difference of column numbers between baseline 
    # and the i-th survey round.
    stopifnot(sum(is.na(data_wide[, ncol(data_wide)]))
              == nrow(dfs[[1]]) - nrow(dfs[[i]]))
    
    # Print for crosscheck by user: How many values of "uhn" occur in both datasets
    print(paste(sum(dfs_short[[i]]$uhn %in% data_wide$uhn), 
                " values of uhn appear in baseline and ",  i, ", ", 
                sum(!(data_wide$uhn %in% dfs_short[[i]]$uhn)), 
                " appear in basline only.", sep = ""))
    
    if (sum(!(dfs_short[[i]]$uhn %in% data_wide$uhn)) != 0) {                   # Warn user, if observations are omitted.
        print(paste("Attention!", sum(!(dfs_short[[i]]$uhn %in%             
        data_wide$uhn)), " observations only occuring in ", 
        i, "were omitted!"))
    }
    
  }
    
  rm("dfs_short")
  

  # Set all NAs in "respondent" columns to FALSE
  data_wide[ , grep("respondent", colnames(data_wide))][is.na(data_wide[ , 
                          grep("respondent", colnames(data_wide))])] <- FALSE
    
## Section 4: Consistency checks if the respondent/non-respondent classification 
  # worked properly for the survey rounds that contain a response-variable.
  stopifnot(sum(data_wide$Status_r08 == 1 & 
                  data_wide$respondent_r08 != TRUE, na.rm = T) == 0)
  
  stopifnot(sum(data_wide$Status_r08 != 1 & 
                  data_wide$respondent_r08 == TRUE, na.rm = T) == 0)
  
  stopifnot(sum(data_wide$type2_r14 == 1 & 
                  data_wide$respondent_r14 != TRUE, na.rm = T) == 0)
  
  stopifnot(sum(data_wide$type2_r14 != 1 & 
                  data_wide$respondent_r14 == TRUE, na.rm = T) == 0)

## Section 5: Clearing the data from ambiguous observations. ###################
  # Remove respondents for whom answers were recorded that should have led to 
  # replacement of the respondent according to the questionnaire.
  # Keep iff: Did not answer with "no" to s2 (Tanzanian) AND did not answer 
  #           with "no" to s4 (is there mobile network at the place they are
  #           planning to move to - only asked if planning to relocate).

  data_wide_clean <- data_wide[(data_wide$s2_baseline == 2 | 
                                  data_wide$s4_baseline == 2) == F | 
                                 (is.na((data_wide$s2_baseline == 2 | 
                                           data_wide$s4_baseline == 2))), ]

   
## Section 6: Transfer the data to match the coding used in the analysis: ######
##   New format:
##   de_age: age in years, NA= no answer or not a respondent in this wave 
##           (no change).
##   de_female: 1 = female, 0 = male, NA = no answer.
##   de_urban: 1 = urban, 0 = rural, NA = no answer.
##   de_agriculture: 1 = main occupation in agriculture (including livestock), 
##                   0 = every other main occupation.
##                   User can manually include fishing to agriculture.
##   de_completed_secondary: 1 for Form 4 or higher completed, 1 for < Form 4, 
##                           NA for "Other" and "Has not attained school going 
##                           age" (as all respondents should be 18 or older).   

  adapt.coding  <- function(data, newnames){
    # RETURNS: A new dataframe in which all demographic columns are coded in 
    #          the new format.
    # INPUTS: <data>: A dataframe in wide format containing all relevant columns 
    #                from baseline and CATI rounds and respondent dummies for 
    #                all CATI rounds. 

    
    # Define a new dataframe, originally containing only "uhn" and the 
    # dummies indicating the call status of each CATI round. 
    new <- dplyr::select(data, uhn, contains("respondent_"))
    
    # Set all "no response" to NA, without touching the "uhn" column.
    data[, colnames(data) != "uhn"][data[,  colnames(data) != "uhn"] == 998] <- NA
    
    # Format demographics variables
    new["de_age"] <- data["de_age_baseline"]
    
    # Format gender from "male" = 1, "female" = 2 to de_female dummy 
    # (yes = 1, no = 0)
    stopifnot(sum(!(data$de_gender_baseline %in% 1:2|
                      is.na(data$de_gender_baseline ))) == 0)
    new["de_female"] <- data["de_gender_baseline"] - 1
    
    # Format urban from "rural" = 1, "urban" = 2 to de_urban dummy 
    # (yes = 1, no = 0)
    stopifnot(sum(!(data$de_urban_baseline %in% 1:2| 
                      is.na(data$de_urban_baseline))) == 0)      
    new["de_urban"] <-  data["de_urban_baseline"] - 1
    
    # Format agriculture from "farming/livestock" = 1, "fishing" = 2, other
    # occupations and no occupation > 3 to de_agriculture dummy (yes = 1, no = 0). 
    # Change the first cutoff in the following ifelse statement to 3, if fishing 
    # should be included in de_agriculture.
    stopifnot(sum(!(data$de_agriculture_baseline %in% 1:18 | 
                      is.na(data$de_agriculture_baseline ))) == 0)  
    new["de_agriculture"] <- ifelse((data["de_agriculture_baseline"] >= 2 & 
                                       data["de_agriculture_baseline"] <= 18), 0, 1) 
    
    # Create a dummy for completed secondary education, that is 1 for 
    # Form 4 or higher and 0 otherwise. "other" = 23 will be set to NA. 
    # "Has not attained school going age" (=1) will be set to NA too, 
    # since all survey participants are at least 18 years old. 
    new["de_completed_secondary"] <- data["de_education_baseline"]
    
    new["de_completed_secondary"][data["de_education_baseline"] == 1 | 
                                  data["de_education_baseline"] == 23] <- NA    # Set "Other" and "Has not attained school going age" to NA.
    
    new["de_completed_secondary"][data["de_education_baseline"] < 14 &
                                    data["de_education_baseline"] > 1 ] <- 0    # All values below Form 4 are set to 0.  
    
    new["de_completed_secondary"][data["de_education_baseline"] >= 14 & 
                                    data["de_education_baseline"] < 23] <- 1    # Value 14 (Form 4) and higher is set to 1.

    
    # Some consistency checks. 
    stopifnot(sum(new["de_urban"] == 1, na.rm = T) == 
                sum(data["de_urban_baseline"] == 2, na.rm = T))
    
    stopifnot(sum(new["de_urban"] == 0, na.rm = T) == 
                sum(data["de_urban_baseline"] == 1, na.rm = T))
    
    stopifnot(sum(new["de_female"] == 1, na.rm = T) == 
                sum(data["de_gender_baseline"] == 2, na.rm = T))
    
    stopifnot(sum(new["de_female"] == 0, na.rm = T) == 
                sum(data["de_gender_baseline"] == 1, na.rm = T))

    return(new) 
  }
    
  data_wide_formatted <- 
    data_wide_clean[data_wide_clean$reshh_baseline == 0 , ] %>% adapt.coding()

  ## Section 7: Save the standardized data as .rds and .csv files.
  saveRDS(data_wide_formatted, paste0(data_clean, "/data_formatted_", study_id, ".rds"))
  write_csv(data_wide_formatted, paste0(data_clean, "/data_formatted_", study_id, ".csv"))  