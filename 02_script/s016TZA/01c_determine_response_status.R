#------------------------------------------------------------------------------#
# TITLE:        01c_determine_response_status.R                                #
# PURPOSE:      For each wave, count the number of numerical responses         #
#               recorded for each survey participant and accordingly classify  #
#               each participant either as respondent or non-respondent.       #
# INPUTS:       <dfs>: Named list of dataframes containing the data of the     #
#               individual CATI rounds.                                        #
# OUTPUTS:      <dfs>: now with added response status columns.                 #
# PUBLISHED:    2021-04-21                                                     #  
# AUTHOR:       Janina Roemer                                                  #            
#------------------------------------------------------------------------------#

# Loop over all dataframes in "dfs" adding 3 columns to all dataframes:
#   - num_NAs: number of missing values (NA) for each observation
#   - num_responses: number of numerical responses 
#   - respondent: response status 

for(i in seq_along(dfs)){
    frameColNum <- ncol(dfs[[i]])                                              # Store original number of columns for later. 
    dfs[[i]]["num_NAs"]    <- rowSums(is.na(dfs[[i]][ , 1:frameColNum]))       # create the num_NAs columns.
    dfs[[i]]["num_responses"] <- (rep(frameColNum                              # create the num_responses columns, counting the non-missing values. 
            - sum(lapply(dfs[[i]][ , 1:frameColNum], class) == "character"),
                    nrow(dfs[[i]])) - dfs[[i]]$num_NAs) 
    
    # Create the respondent columns
    # If the minimum number of responses occurs more than twice AND there is a 
    # larger gap between the smallest and second smallest number of responses 
    # than between second and third then: respondent is only set to FALSE for 
    # the observations with the minimum number of responses.
    if (  (plyr::count(dfs[[i]], vars="num_responses"))[1,2] > 2
        & (plyr::count(dfs[[i]], vars="num_responses"))[2,1] -                           
          (plyr::count(dfs[[i]], vars="num_responses"))[1,1] > 
          (plyr::count(dfs[[i]], vars="num_responses"))[3,1] -                           
          (plyr::count(dfs[[i]], vars="num_responses"))[2,1] || i == 3 || i == 5) { # Exception r02 and r04 added: For the 80 (r02) and 170 (r04)
                    # observations with the second smallest number of values the survey was terminated early in accordance with the script. 
        dfs[[i]]["respondent"] <- (dfs[[i]]$num_responses                          
                                   != min(dfs[[i]]$num_responses)) 
    } 
        # No distance between smallest and 2nd smallest number of responses nor 
        # between the 2nd and the 3rd => TRUE is assigned to $respondent for all observations.    
           else if((((plyr::count(dfs[[i]], vars="num_responses"))[2,1]                 
                         - (plyr::count(dfs[[i]], vars="num_responses"))[1,1]) < 2)     
                 & (((plyr::count(dfs[[i]], vars="num_responses"))[3,1] 
                         - (plyr::count(dfs[[i]], vars="num_responses"))[2,1]) < 2)
                   || (plyr::count(dfs[[i]], vars="num_responses"))[1,1] > 50) {
            dfs[[i]]["respondent"] <- TRUE
            
            # Warn user about all dataframes exclusively containing respondents
            print(paste("Dataframe '", names(dfs)[[i]], 
                         "' was found to contain only respondents.", sep = ""))
           } 
        # Else if the two smallest numbers of responses occurring are close to each 
        # other, but separated from the rest, define all observations with more 
        # responses than the 2 smallest numbers of responses as respondent.
            else if((((plyr::count(dfs[[i]], vars="num_responses"))[2,1]                  
                    - (plyr::count(dfs[[i]], vars="num_responses"))[1,1]) < 3 ) 
                  & (((plyr::count(dfs[[i]], vars="num_responses"))[3,1] 
                    - (plyr::count(dfs[[i]], vars="num_responses"))[2,1]) > 10 )) {  # USER: Adapt here to what you consider a reasonable minimum number of answers to count as respondent
              dfs[[i]]["respondent"] <- ((dfs[[i]]$num_responses                    
                  != min(dfs[[i]]$num_responses) )& (dfs[[i]]$num_responses 
                            != (plyr::count(dfs[[i]], vars="num_responses"))[2,1]) )    
            } 
        # Otherwise warn user, that the response status could not be determined. 
            else {
                dfs[[i]]["respondent"] <- NA 
                print(paste("Attention: Respondents for dataframe '", 
                                   names(dfs)[[i]], "' could not be determined!"))      
            }

    
    # Output for user control
    print(paste0("Dataframe '", names(dfs)[[i]] , "': " ,                          
        sum(dfs[[i]]$respondent), " respondents out of ", nrow(dfs[[i]]), 
        " attempts."))

    
    # Histograms of the numbers of responses for user control   
    hist(dfs[[i]]$num_responses, min(100, ncol(dfs[[i]])),  
            main = paste("Numbers of answers per observation in ", names(dfs)[[i]]))
    
    # Consistency check: Stop if the number of NAs, "no responses", character 
    #       columns, and responses does not add up to the number of variables.
    stopifnot((dfs[[i]]$num_NAs + dfs[[i]]$num_responses +     
              rep(sum(lapply(dfs[[i]], class) == "character"), nrow(dfs[[i]])))
              == rep(frameColNum, nrow(dfs[[i]])))
    
   
    # Create a vector containing the number of missing answers of each column.
    char_col         <- which(lapply(dfs[[i]], class) == "character")
    missing_per_col  <- rep(NA, ncol(dfs[[i]]))                          
    for (j in 1:frameColNum) {                                                 
        missing_per_col[j]  <- ((sum(dfs[[i]][ , j] == 998, na.rm = TRUE) 
                                 + sum(is.na(dfs[[i]][, j]))) + 
                                   ifelse(j %in% char_col, sum(dfs[[i]][ , j] == "", na.rm = T), 0) )
        }
    # Create vectors containing the columns that have no missing values.
    assign(paste("complete_cols", names(dfs)[i], sep = "_"), which(missing_per_col == 0))
    
    # Consistency check: Stop if number of complete columns does not equal minimum number of responses
    stopifnot((sum(missing_per_col[1:frameColNum] == 0) == 
                        min(dfs[[i]]$num_responses) || i == 1 || i == 8))      # Tolerated exceptions added manually: baseline and r07 appear to have 
                                                                               # been cleared of non-respondents before the data was published.
    
} 

## Section 2: Clean up ---------------------------------------------------------
rm(list = ls(pattern = "^complete_cols_"))
rm(list = ls(pattern = "^missing_per"))
rm("frameColNum", "char_col")

