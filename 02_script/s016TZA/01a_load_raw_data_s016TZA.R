#------------------------------------------------------------------------------#
# TITLE:        01a_load_raw_data_s016TZA.R                                    #
# PURPOSE:      Load all open household data from .dta files into environment, # 
#               Create a named list <dfs> containing one dataframe for each    #
#               survey round.                                                  #
# INPUTS:       raw-data.                                                      #
# OUTPUTS:      Environment: <dfs>: A list of dataframes, each containing the  #
#               data of one survey round.                                      #
# PUBLISHED:    2021-04-21                                                     # 
# AUTHOR:       Janina Roemer                                                  #
#------------------------------------------------------------------------------#


# Load the data from the .dta files in raw into the environment.
baseline <- read_dta(paste(raw, "SzW_data_hh_baseline_2012.dta", sep = "/"))
r01 <- read_dta(paste(raw, "SzW_data_hh_R1_April_2013.dta", sep = "/"))
r02 <- read_dta(paste(raw, "SzW_data_hh_R2_April_2013.dta", sep = "/"))
r03 <- read_dta(paste(raw, "SzW_data_hh_R3_May_2013.dta", sep = "/"))
r04 <- read_dta(paste(raw, "SzW_data_hh_R4_June_2013.dta", sep = "/"))
r05 <- read_dta(paste(raw, "SzW_data_hh_R5_July_2013.dta", sep = "/"))
r06 <- read_dta(paste(raw, "SzW_data_hh_R6_August_2013.dta", sep = "/"))
r07 <- read_dta(paste(raw, "SzW_round7.dta", sep = "/"))
r08 <- read_dta(paste(raw, "SzW_data_hh_R8_October_2013.dta", sep = "/"))
r10 <- read_dta(paste(raw, "SzW_StataData_HH_R10_October_2013.dta", sep = "/"))
r14 <- read_dta(paste(raw, "SzW_data_hh_R14_March_2014.dta", sep = "/"))
r24 <- read_dta(paste(raw, "SzW_StataData_HH_R24_September_2014.dta", sep = "/"))


# Create a list  "dfs" with the dataframes of all waves and assign the wave 
# name to the list elements. 
wave_name <- c("baseline", "r01", "r02", "r03", "r04", "r05", "r06", "r07", 
                  "r08", "r10", "r14", "r24")
dfs <- lapply(wave_name, get)
names(dfs) <- wave_name  

# Remove the individual dataframes from the environment. 
rm(list = wave_name)
