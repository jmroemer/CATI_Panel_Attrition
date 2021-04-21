#------------------------------------------------------------------------------#
# TITLE:      01b_metadata_overview.R                                          #
# PURPOSE:    Create a dataframe containing all important metadata of the      #
#             survey rounds.                                                   #
# INPUTS:     <wave_name>: a vector containing the short names of all survey   #
#             rounds.                                                          #
# OUTPUTS:    <waves>: a dataframe containing relevant metadata about the      #
#             survey rounds.                                                   #                      
# PUBLISHED:  2021-04-21                                                       # 
# AUTHOR:     Janina Roemer                                                    #
#------------------------------------------------------------------------------#

## Section 1: Summing up relevant information about the waves ##################
##            Creates a dataframe "waves" containing the following:           ##

# Wave name - already in the environment as <wave_name>

# Date of the data collection period of each round
date <- c("2012-11-01", "2013-04-01", "2013-04-01", "2013-05-01", 
           "2013-06-01", "2013-07-01", "2013-08-01", "2013-09-01", "2013-10-01", # Attention! Date of R07 us missing, NA instead of 2013-09 would thus be more accurate
           "2013-10-01", "2014-03-01", "2014-09-01")

# Combining the vectors to one dataframe
waves <- data.frame(wave_name, date, stringsAsFactors = FALSE)                  # wave_name are the names of the elements of dfs. See 01a.

# A wave number that loosely correlates to date of data collection. For the 
# baseline it is set to -4 since the first CATI round started approximately 
# 5 months after the baseline. 
waves$wave_number <- c(-4, 1, 2, 3, 4, 5, 6, 7, 8, 10, 14, 24)

# Survey topic of each round.
waves$topic <- c("Baseline", "Form 4 exam results", "Capitation grants in 
                   education", "Access to information", "Health", "Draft 
                   Constitution", "Water", "Corruption", "Security", "Political 
                   Poll", "Second Draft Constitution", "Political Poll")

# Time difference between each round and the baseline In DAYS (starting days)
waves$time_from_BL <- as.numeric(as.Date(waves$date, "%Y-%m-%d")) - 
  rep(as.numeric(as.Date(waves$date[[1]], "%Y-%m-%d"), length(waves$date)))

# Time difference between each round and the baseline In MONTHS (starting days)
waves$months_from_BL <- round(waves$time_from_BL/30.5)
     

# Adding long names (for printing) to the dataframe
waves["long_name"] <- vector(mode = "character", length =  nrow(waves))
waves["long_name"][grepl("baseline", waves$wave_name, fixed = T), ] <- "Baseline\n(Face to Face)"
waves["long_name"][waves["long_name"] == "" ] <- 
  paste("Round", substring(waves$wave_name[!(grepl("baseline", waves$wave_name, 
                                                   fixed = T))], 2), sep = " ")


# Cleanup ######################################################################
rm("wave_name" )
