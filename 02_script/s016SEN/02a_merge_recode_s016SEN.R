#------------------------------------------------------------------------------#
# TITLE:      02a_merge_and_recode_s016SEN.R                                   #       
# PURPOSE:    Merge the relevant Variables of the F2F-Baseline survey with     # 
#             the columns of the CATI waves containing the call status and     # 
#             convert the demographic variables to the universal format used   #
#             in this analysis.                                                #
# INPUTS:     <dfs>, <waves>                                                   # 
# OUTPUTS:    <data_wide>: dataframe containing the relevant columns of the    # 
#                            baseline and call status of all waves.            # 
#             <data_wide_formatted>: Identical to <data_wide> with the         #
#                                    exception that the demographic variables  #
#                                    have been converted to the universal      #
#                                    format used in this analysis.             #
# PUBLISHED:  2021-04-21                                                       # 
# AUTHOR:     Janina Roemer                                                    #            
#------------------------------------------------------------------------------#

## Section 1: Formatting the relevant dataframes for merging ###################

# Exclude all datasets for which uhn creation failed
dfs <- dfs[!(names(dfs) %in% c("r08", "r10", "r18E"))] 
waves <- subset(waves, !(waves$wave_name %in% c("r08", "r10", "r18E")))


for(i in seq_along(dfs)){
  
  # Add a call status column ("respondent") filled with 1s to all waves
  dfs[[i]]["respondent"] <- rep(TRUE, nrow(dfs[[i]]))
  
  # Add a suffix identifying the wave to all columns but "uhn" (used for merging)
  if (i != 1){
    colnames(dfs[[i]])[colnames(dfs[[i]]) != "uhn"] <- 
      paste(colnames(dfs[[i]])[colnames(dfs[[i]]) != "uhn"], names(dfs)[[i]], sep = "_")
  }
}


## Section 2: Select relevant variables ######################################## 

    # Create a dataframe "data_wide" containing only columns of baseline containing 
    # information considered relevant for attrition analysis.
    
    data_wide <- dplyr::select(dfs[[1]], uhn,
                                         age_mois_r,  # age in 5 year brackets starting at 18 = 4
                                         b02bis,      # gender identity (b02bis quel es le sexe de [nom])
                                         c04bis,      # has he / she ever been to school?
                                         c06bis,      # education (quelle est la classe la plus ?lev?e que [nom] a achev?e avec succ?s)
                                         strate,      # urbanicity (Milieu de r?sidence: Dakar urbain = 1, Autres urbains = 2, Rural = 3)
                                         b10bis,      # situation de r?sidence actuelle (R?sident pr?sent.1, R?sident absent..2, Visiteur.....3
                                         e1_06bis,    # employed? INVESTIGATION AGENT: AMONG THE ANSWERS TOE01, E02, E03, E04, E05, Y DOES IT HAVE ONE OR MORE "YES"?
                                         e1_07bis,    # main occupation short
                                         e1_08bis     # sector of main occupation
                                         )


## Section 3: Format the dataframe #############################################

    # Create demographic columns with standardized column names for further analysis
    data_wide["de_urban"] <- data_wide$strate
    data_wide["de_gender"] <- data_wide$b02bis                                  
    data_wide["de_age"] <- data_wide$age_mois_r
    data_wide["de_employed"] <- data_wide$e1_06bis
    data_wide["de_occupation"] <- data_wide$e1_08bis
    data_wide["de_education"] <- data_wide$c06bis
  

## Section 4: Creating additional demographic columns ##########################

  # Combine "has been to school" and highest grade finished to one education 
  # variable: In the original survey data respondents who have never been to 
  # school have NA in the column c06bis (highest grade finished). The following
  # lines change these entries to -1 to indicate "no grade finished".
    
    # Check data consistency: Warn user if any respondent has finished a grade 
    # without ever attending school. 
    if(sum(data_wide$c04bis == 2 & (!(is.na(data_wide$de_education)) & 
                              data_wide$de_education != 0), na.rm = T) != 0) {
      warning("Some respondents finished a grade without ever going to school.")
      }                                                                         
    
    # Set education to -1 if the respondent has never been to school. 
    data_wide$de_education[is.na(data_wide$de_education) & data_wide$c04bis == 2] <- -1
    
  # Create a "de_agriculture" column that is 0 for everybody who is not have an 
  # occupation in the past 6 months ("de_emloyed" - originally "e1_06bis") appear 
  # as NA in the e1_08bis (sector of main  activity) column. The following lines 
  # set these entries to 0, since we are interested in the ratio: 
  # (people working in agriculture)/(all people).
  data_wide["de_agriculture"] <- data_wide$de_occupation
  data_wide[data_wide$de_employed == 2 & 
              !(is.na(data_wide$de_employed)) &
              is.na(data_wide$de_agriculture), ]["de_agriculture"] <- 0         
  
  
  # Add a suffix "_baseline" to all baseline columns but "uhn". 
  # This will allow to easily identify baseline columns after merging with data 
  # from the CATI rounds. 
  colnames(data_wide) <- (paste(colnames(data_wide), "_baseline", sep = ""))
  colnames(data_wide)[1] <- "uhn" 


## Section 5: Merging the call status information to data_wide ###################

    # Crate a new list of dataframes dfs_short that contains the the survey data of 
    # all rounds, but for all CATI rounds only the call status columns are kept.  
    # These columns are then merged to the baseline data. 
    dfs_short <- dfs
    for (i in 2:length(dfs)){
      # Create a short version of dfs containing only "uhn" and call status columns 
      # of each dataframe
    
        dfs_short[[i]] <- dplyr::select(dfs_short[[i]], uhn,
                                        (paste0("respondent_", names(dfs)[[i]] )))
      
    
      # Merge to data_wide.
      data_wide <- left_join(data_wide, dfs_short[[i]], by = "uhn",
                           suffix = c( paste("_", names(dfs)[i-1],  sep=""),    
                                       paste("_", names(dfs)[i],  sep="") ) )   # Adds wave as suffix if any column names appeared more than once.
    

      # Consistency check: Stop if at any i the sum of NAs in the respondent  
      # column is not equal to difference of column numbers between baseline 
      # and the i-th survey round.
      stopifnot(sum(is.na(data_wide[, ncol(data_wide)]))
                == nrow(dfs[[1]]) - nrow(dfs[[i]]))
    
      # Print for crosscheck by user: How many values of "uhn" occur in both datasets.
      print(paste(sum(dfs_short[[i]]$uhn %in% data_wide$uhn),
                  " values of uhn appear in baseline and ",  names(dfs)[[i]], ", ",
                  sum(!(data_wide$uhn %in% dfs_short[[i]]$uhn)),
                  " appear in basline only.", sep = ""))
      if (sum(!(dfs_short[[i]]$uhn %in% data_wide$uhn)) != 0) {                 # Warn user, if observations are omitted.
        print(paste("Attention!", sum(!(dfs_short[[i]]$uhn %in%                 
                    data_wide$uhn)), " observations only occuring in ",
                    names(dfs)[[i]], "were omitted!"))
      }
    
    }
  
    rm("dfs_short")
    
    # set all NAs in "respondent" columns to FALSE
    data_wide[ , grep("respondent", colnames(data_wide))][is.na(data_wide[ , 
                            grep("respondent", colnames(data_wide))])] <- FALSE



## Section 6: Transfer the data to match the coding used in the analysis. ######
## New format:
##   de_age: age in years, NA= no answer. Averages of bins will be used.     
##   de_female: 1 = female, 0 = male, NA = no answer.
##   de_urban: 1 = urban, 0 = rural, NA = no answer.
##   de_agriculture: 1 = main occupation in agriculture (including livestock), 
##                 0 = every other main occupation or no occupation.
##                 User can manually include fishing to agriculture.
##   de_completed_secondary: 1 = Tle or higher completed. 0= lower than Tle, 
##                         NA = don't know or NA.
  
  adapt.coding <- function(data){
    # RETURNS: A new dataframe in which all demographic columns are coded 
    #          in the new format. 
    # INPUTS: <data>: A dataframe in wide format containing all relevant columns 
    #                 from baseline and CATI rounds and respondent dummies for 
    #                 all CATI rounds. 

  # Define a new dataframe, originally  containing only "uhn" and the dummies
    # indicating the call status of each CATI round. 
    new <- dplyr::select(data, uhn, contains("respondent_"))
    
    # Formatting the demographics variables:
    # See Attrition_analysis_Codebook (Tab: SEN_Demographics_formatting) for details.
    
    # The age is defined as the average age of the respective age-bin. 
    new["de_age"] <- data["de_age_baseline"] + 17 +                            
                                      (data["de_age_baseline"] -5) * 4            
    new["de_age"] [new["de_age"] == 17] <- 18.5                                 # Adapt the average of the first bin to 18.5 since this bin only contains 18 and 19 year old respondents. 
    
    # Format gender from "Masculin" = 1, "F?minin" = 2 to de_female dummy 
    # (yes = 1, no = 0)
    stopifnot(sum(!(data$de_gender_baseline %in% 1:2)) == 0)                    # Stop if the original format of gender identity is not as expected.
    new["de_female"] <- data["de_gender_baseline"] - 1  
    
    # Format urban from "Dakar urbain" = 1, "Autres urbains" = 2, Rural" = 3
    # to de_urban dummy (yes = 1, no = 0)
    stopifnot(sum(!(data$de_urban_baseline %in% 1:3)) == 0)                     # Stop if the original format of urban is not as expected.
    new["de_urban"] <- ifelse(data["de_urban_baseline"] == 3, 0, 1)

    # Format agriculture from "Agriculture" = 1, "Breeding" = 2, "Fishing" = 3,
    # other occupations > 3 to de_agriculture dummy (yes = 1, no = 0). 
    # Change the ifelse-criterion in the following if fishing should be 
    # included, or if breeding should be excluded from de_agriculture.
    stopifnot(sum(!(data$de_agriculture_baseline %in% 0:16 | 
                      is.na(data$de_agriculture_baseline ))) == 0)              # Stop if the original format is not as expected.
    new["de_agriculture"] <- ifelse((data["de_agriculture_baseline"] >= 3 & 
                                    data["de_agriculture_baseline"] <= 16 | 
                                      data["de_agriculture_baseline"] == 0), 0, 1) 
    
    # Create the variable "completed_secondary_education" and set it 0 for all
    # respondents with education < Tle (including "ne sait pas") and to 1 for
    # respondents with education >= Tle.
    new["de_completed_secondary"] <- data["de_education_baseline"]
    
    new["de_completed_secondary"][data["de_education_baseline"] < 14 &
                                  data["de_education_baseline"] > -2 ] <- 0    # all values below 1?re are set to 0  
    
    new["de_completed_secondary"][data["de_education_baseline"] >= 14 & 
                                  data["de_education_baseline"] < 99] <- 1
    new["de_completed_secondary"][data["de_education_baseline"] == 99 ] <- NA   # Ne sait pas is set to NA 
      
    return(new)
  }
  
  data_wide_formatted <- data_wide %>% adapt.coding()

  ## Section 7: Save the standardized data as .rds and .csv files.
  saveRDS(data_wide_formatted, paste0(data_clean, "/data_formatted_", study_id, ".rds"))
  write_csv(data_wide_formatted, paste0(data_clean, "/data_formatted_", study_id, ".csv"))  
