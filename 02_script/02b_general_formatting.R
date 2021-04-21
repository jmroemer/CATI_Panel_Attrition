#------------------------------------------------------------------------------#
# TITLE:      02b_general_formatting.R                                         #      
# PURPOSE:    Transforming data to formats required for further analysis.      # 
# INPUTS:     <data_wide_formatted>, <dfs>.                                    #
# OUTPUTS:    <formatted_primary>: List of dataframes (one per demographic     #
#                                  variable), each dataframe contains one      #
#                                  demographic variable, with the baseline     #
#                                  values as first column and one additional   #
#                                  column per CATI round, containing the value #
#                                  at baseline for respondent-rows and NA for  #
#                                  non-respondents.                            #
#             <formatted_long>: Equal to <formatted_primary> with each list-   #
#                               element in long format, with columns "number"  #
#                               and "wave_name" identifying the survey round.  #
# PUBLISHED:  2021-04-21                                                       #
# AUTHOR:     Janina Roemer                                                    #            
#------------------------------------------------------------------------------#


## Section 1:  Create a list of dataframes, each containing one demographic ###
##             characteristic for all rounds.                               ###
demographics <- as.list(names(data_wide_formatted)[(grep("^de_", 
                                                names(data_wide_formatted)))])
## USER: adapt if necessary, using the following line. 
# demographics <- list("de_age", "de_female","de_urban", "de_agriculture")


## A: Create a list of dataframes (one per demographic variable), each data- ##
##    frame contains one demographic variable, copied to as many columns as  ##
##    there are survey rounds.                                               ##
##    In each column the demographic of non-respondents is replaced by NA.   ##

demographics.by.wave <- function(data1, data2, variables){
  # RETURNS:  A list of dataframes (one per demographic variable) with people 
  #           surveyed in baseline as observations and one column per survey 
  #           round containing the value of the respective demographics from the 
  #           baseline for each respondent and NA for each non-respondent.
  # INPUTS:   <data1>:     Dataframe containing baseline demographics and dummy 
  #                        variables for call status in each round
  #           <data2>:     Named list of dataframes (one for each survey round)
  #           <variables>: List of variables of interest. 
  
  # Control mechanisms:
  # Stop if any demographic observation contains 0 - necessary because 
  # non-response-values will be set 0 before altered to NA.
  if(sum(data1[ , grep("^de_", colnames(data1))]== 0, na.rm = TRUE) > 0){
    data1[ , grep("^de_", colnames(data1))][data1[ , grep("^de_", colnames(data1))] == 0] <- -999
  } 
  
  stopifnot(sum(data1[ , grep("^de_", colnames(data1))]== 0, na.rm = TRUE) == 0) 
  
  
  waves_list <- list()
  for (k in variables){
    # Assign the baseline identifier and the demographic k to a dataframe named according to the demographic k 
    assign(paste("wave", k, sep = "_"), dplyr::select(data1, 
                                                      uhn,
                                                      all_of(k)),
           envir = parent.frame())
    # Add this dataframe to the list "waves_list"
    waves_list[[k]] <- get(paste("wave", k, sep = "_")) 
  }
  
  ##B: Calculate summary statistics for each demographic and wave  #########
  for (i in seq_along(waves_list)){
    for (j in 2:length(data2)){
      waves_list[[i]] <- left_join(waves_list[[i]], data1[c("uhn",
                                                            paste("respondent", names(data2)[[j]],
                                                                  sep = "_"))], by = "uhn")
      
      # Remove labels to enable numeric processing
      waves_list[[i]] <- zap_labels(waves_list[[i]])
      
      # Multiply demographic with respondent column
      waves_list[[i]][paste("respondent", names(data2)[[j]], sep = "_")] <-
        waves_list[[i]][paste("respondent", names(data2)[[j]], sep = "_")] *
        waves_list[[i]][ , 2]
      
      
      # Change all 0s in respondent-columns to -888 symbolizing non response.
      waves_list[[i]][ , paste("respondent", names(data2)[[j]], sep = "_")][
        waves_list[[i]][ , paste("respondent", names(data2)[[j]], sep = "_")] == 0   
      ] <- -888         # non response is marked by -888
      # => allows to distinguish between NA and non-response
      
      waves_list[[i]][ , paste("respondent", names(data2)[[j]], sep = "_")][
        waves_list[[i]][ , paste("respondent", names(data2)[[j]], sep = "_")] == 998
      ] <- NA
      
      waves_list[[i]][ , 2][waves_list[[i]][ , 2] == 998] <- NA
    }
    waves_list[[i]][waves_list[[i]] == -999] <- 0                               # set all values that had been changed from 0 to -999 back to 0
  }
  
  return(waves_list)
}

formatted_primary <- demographics.by.wave(data_wide_formatted, dfs, demographics)


# Change all -888 (not a respondent) to NA. 
for(i in seq_along(formatted_primary)){
  formatted_primary[[i]][formatted_primary[[i]] == -888] <- NA
}

# Add _baseline suffix to the column names of the demographic variables. 
for (i in 1:length(formatted_primary)){
  names(formatted_primary[[i]])[2] <- 
                      paste0(names(formatted_primary[[i]][2]), "_baseline") 
}



## Section 2: Convert data to long format #####################################  

  prepare.data <- function(data1, bl_column){
    # RETURNS: List of dataframes in long format with a column "number" containing
    #          the wave-number and a column "wave_name" containing the wavename 
    #          (BL, r01, r02...)
    # INPUT:  <data1>: List of dataframes in wide format
    #         <bl_column>: Set TRUE if there is a basleine column that should be 
    #          kept.
    names(data1) <- gsub(pattern = "respondent_", replacement = "", 
                         x = names(data1))                                      # creates wave names (r1, r2,...) from column names (respondent_r1, respondent_r2...))
    if (bl_column == T){
      data2 <-  pivot_longer(data1, cols = !c(uhn, baseline),  
                             names_to = "wave_name", values_to = "value")       # transfer to long format
    }
    else if (bl_column == F){
      data2 <-  pivot_longer(data1, cols = !uhn, names_to = "wave_name", 
                             values_to = "value")                               # transfer to long format
    }
    else {print("Error in prepare.data: <bl_column> needs to be logical.")}
    
    
    # Add a column containing the wave number - for s016SEN defined as the number  
    # of months after baseline, for s016TZA loosely correlated to time after bl.
    data2["number"] <- rep(-99, nrow(data2))                                      
    data2["number"][grepl("baseline", data2$wave_name, fixed = T), ] <- 
      waves[waves$wave_name == "baseline", ]$wave_number                        # Sets the baseline value using the respective entry of the "waves"-dataframe
    data2[!(grepl("baseline", data2$wave_name, fixed = T)), ]["number"] <-      # for all waves following baseline the wave number is extracted from the wave name
      substring(data2$wave_name[!(grepl("baseline", data2$wave_name,
                                        fixed = T))], 2) %>%  as.double()
    
    data2["wave_name"][grepl("baseline", data2$wave_name, fixed = T), ] <- "BL" # wave name "baseline" is shortened to "BL"
    data2["long_name"] <- vector(mode = "character", length =  nrow(data2))
    data2["long_name"][grepl("BL", data2$wave_name, fixed = T), ] <- 
      "Baseline\n(Face to Face)"
    data2$long_name[data2["long_name"] == "" ] <- 
      paste("Round", substring(data2$wave_name[!(grepl("BL", data2$wave_name, 
                                                   fixed = T))], 2), sep = " ")
    return(data2)                                                                 
  }
  
  # Create a long version of the cleaned primary household data containing 
  # columns "wave_name" and "number" (= wave number in numeric format).
  formatted_long <- formatted_primary %>%  lapply(prepare.data, bl_column = F)
