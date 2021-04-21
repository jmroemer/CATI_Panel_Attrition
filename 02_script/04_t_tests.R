#------------------------------------------------------------------------------#
# TITLE:      04_t_test.R                                                      #      
# PURPOSE:    Test for each CATI round and demographic variable if the         #
#             variables mean in this round is equal to the baseline mean       #
#             (t-tests). Write mean, mean difference from baseline and p-value #
#             into <t_tests_BL_vs_CATI_study_id.csv>.                          #
# INPUTS:     <formatted_primary>: List of dataframes, each containing the     #
#             baseline values of 1 demographic variable: Full baseline column  #
#             and 1 column per CATI round containing NAs for non-respondents.  #
#             <dfs>: Named list containing the data of all survey rounds.      #
# OUTPUTS:    <t_tests_BL_vs_CATI_study_id.csv>: For each CATI round and       #  
#             demographic variable it contains the mean, differences in means  #
#             between baseline and the respective CATI round and the p-value.  #
#             p-values for each round for different demographic variables      #
# PUBLISHED:  2021-04-21                                                       #
# AUTHOR:     Janina Roemer                                                    #            
#------------------------------------------------------------------------------#

t.test.dem <- function(data1, data2){
  # RETURNS: A table that for each survey round (row) contains mean, mean      
  #          difference from baseline and p-value of t-test of the means 
  #          comparing the round to baseline for different demographic variables 
  #          (columns).
  # INTPUTS: <data1>: List of dataframes, each containing the baseline values 
  #                   of one demographic variable: Full baseline column and one
  #                   column per CATI round containing NAs for non-respondents.
  #          <data2>: Named list of dataframes (one for each survey round).
  
  # Create a table with outcomes of t-tests on different demographics and waves
  columns_t <- list()
  for (i in 1:length(data1)){
    columns_t[[i*3-2]] <- names(data1)[[i]]                  
    columns_t[[i*3-1]] <- paste("Difference", names(data1)[[i]], sep = "_")
    columns_t[[i*3-0]] <-  paste("p_value", names(data1)[[i]], sep = "_")
  }
  
  # Create a dataframe with waves as rows and demographics x 3 as columns 
  # (Mean, difference and p-value).
  test_out <- data.frame(matrix(rep(NA, length(data2)*3*length(data1)),
                                nrow = length(data2), ncol = 3*length(data1), 
                                dimnames = list(names(data2), columns_t)))
  
  # Assign Baseline average, 
  for(j in 1:length(data1)){  
    test_out[1, j*3-2] <- t.test(data1[[j]][2], data1[[j]][2])[[5]][1]          # Adds baseline average to 1st row column 1 (j*3-2).
    
    for(i in 3:ncol(data1[[j]])){
      test_out[i-1, j*3] <- t.test(data1[[j]][2],  data1[[j]][i])[[3]]          # Adds p-values to column 3 (j*3-0).
      
      if((test_out[i-1, j*3] < 0.05)
         & (j <= 6  | j == 14)                                                  
      ) {                                                                       
        print(paste("p-value = ", test_out[i-1, j*3], " for ", names(data1)[[j]], 
                    " in wave ", names(data2)[[i-1]], sep = ""))                # Informs user about all p-values smaller than 0.05.
      }
      
      test_out[i-1, j*3-2] <- t.test(data1[[j]][2], data1[[j]][i])[[5]][2]      # Adds wave averages to column 1 (j*3-2).
      
      test_out[i-1, j*3-1] <- t.test(data1[[j]][2], data1[[j]][i])[[5]][2] - 
                              t.test(data1[[j]][2], data1[[j]][i])[[5]][1]      # Adds difference of wave average and baseline average to column 2 (j*3-1).
    }
  }
  
  return(test_out)
}


# Add survey round date, number, subject and response rate to the t_test results.
  add.number.date.subject.response <- function(data, numbers, dates, 
                                               subjects, response) {
    data$number <- numbers
    data$date <- dates
    data$subject <- subjects
    data$response_rate <- response
    data$response_rate[1] <- 1
    return(data)
  }
  
  test_results_primary <- t.test.dem(formatted_primary, dfs) %>% 
    add.number.date.subject.response(waves$months_from_BL, waves$date, 
                                     waves$topic, waves$response_rate)

#Export T-Test outcomes to csv file.
test_results_primary %>% write.csv(paste0(output_tables, "/t_tests_BL_vs_CATI_", 
                                          study_id, ".csv"), row.names= T)

## Cleanup ------------------------------------------------------------------##
rm(list = ls(pattern = "^wave_de_"))
rm(demographics)
