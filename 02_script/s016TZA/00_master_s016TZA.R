#------------------------------------------------------------------------------#
# TITLE:      00_master_s016TZA.R                                              #
# PURPOSE:    Analyze attrition in CATI panel surveys that build onto a face-  #
#             to-face baseline survey:                                         #
#             Prepare the environment, load required packages and run all      #
#             scripts of this project.                                         #
# ADAPTED TO: "Sauti za Wananchi" survey, Tanzania.                            #
# PUBLISHED:  2021-04-21                                                       #  
# AUTHORS:    Janina Roemer                                                    # 
#                                                                              #
#                                    MASTER                                    #                           
#                                                                              #
#------------------------------------------------------------------------------#


## SECTION 0: Preparing the environment ########################################

## A. Clear environment  -------------------------------------------------------
    rm(list=ls())
    master <- 1 # Show master is run


## B. User options -------------------------------------------------------------
    
    # Optional: Using absolute paths. 
    # USER: Un-comment the following line and adapt the root path to match
    #       the project folder on your device, if you prefer manually setting 
    #       the absolute path.
    # root <- file.path("C:", "Users", "User", "Documents", "R", 
    #                   "Attrition_analysis")
    
    # Adjust study_id
    study_id <- "s016TZA"
    
    # Adjust study_reference
    study_reference <- "Tanzania"


## C. Set folder folder paths --------------------------------------------------
    
    # Set root folder using here if no explicit reference defined
    if(!exists("root")) {
        
        # Install "here" if it is not yet installed, then set root with here()
        if(!("here" %in% installed.packages())) {  
            install.packages("here")
        }
        library(here)
        
        # Set path with root
        oldwd <- getwd()
        setwd(here::here()) 
        if (file.exists(here::here("02_script", study_id, 
                                   paste0("00_master_", study_id, ".R")))){ 
            root <- getwd()
            setwd(oldwd)
        } else {
            stop("File path not found. Please close R and start it by opening   
                 the file 00_master_s016TZA.R or provide the root folder   
                 path in the user options (Section 0 B)")                       # Return an error if here() was not set correctly.
        }

    }


    ## Project folders
    raw         <- file.path(root, "01_raw", study_id)
    script      <- file.path(root, "02_script")
    data_clean  <- file.path(root, "03_data_clean", study_id) 
    output      <- file.path(root, "04_output")
    output_graphs   <- file.path(output, study_id, "graphs")
    output_tables   <- file.path(output, study_id, "tables")


## D. Package Management ------------------------------------------------------
    
    # Create a vector containing the names of all required packages. 
    packages <- c("tidyverse", "directlabels", "haven", "kableExtra", "lmtest",        
                  "plyr", "sandwich", "see")
    
    # Check for new packages.
    new_packages <- packages[!(packages %in% 
                                   installed.packages()[,"Package"])]
    
    # Install all new packages.
    if(length(new_packages)) {
        install.packages(new_packages)
    } 
    
    # Load all required packages.
    library(plyr)                                                                # Loaded first so that count() from plyr is used 
    invisible(sapply(packages, library, character.only = TRUE))                  # Suppress load


## SECTION 1: Data preparation ################################################
        
## A. Load raw data ------------------------------------------------------------
    # PURPOSE:  Create a named list <dfs> containing the open household data, 
    #           one dataframe for each survey round. 
    # INPUTS:   Original open data
    # OTUPUTS:  <dfs>
    source(here::here(script, study_id, "01a_load_raw_data_s016TZA.R"))
    
## B. Load and save raw data ---------------------------------------------------
    # PURPOSE:  Create a dataframe containing relevant metadata of the survey
    #           rounds.
    # INPUTS:   <dfs>
    # OTUPUTS:  <waves>, <study_id>
    source(here::here(script, study_id, "01b_metadata_overview.R"))    
    
    
## C. Create response status variables ---------------------------------------------
    # PURPOSE:  For each CATI round classify the response status of each 
    #           participant
    # INPUTS:   <dfs>         
    # OUTPUT:   <dfs> with response status column added. 
    source(here::here(script, study_id, "01c_determine_response_status.R"))
 
    
## Section 2: Merge and format the data ########################################
    
## A: Merge data and convert variables -----------------------------------------
    # PURPOSE:  - Merge the relevant variables of the F2F-Baseline with the 
    #             response status columns.
    #           - Remove ambiguous observations and reserve households. 
    #           - Transfer to standardized coding of variables of interest used 
    #             by following scripts. 
    # INPUTS:   <dfs>, <waves>        
    # OUTPUT:   <data_wide>, <data_wide_clean>, <data_wide>, <data_wide_formatted>
    source(here::here(script, study_id, "02a_merge_recode_s016TZA.R"))
    
    
## B: Transforming data to formats required for further analysis. --------------
    # PURPOSE:  Transforming data to formats required for further analysis.
    # INPUTS:   <data_wide_formatted>, <dfs>       
    # OUTPUT:   <formatted_primary>, <formatted_long>
    source(here::here(script, "02b_general_formatting.R"))
    
    
## Section 3: Calculate Response Rates #########################################
    # PURPOSE: Calculate and plot the response rates of all survey rounds.
    # INPUTS:   <data_wide_formatted>, <dfs>       
    # OUTPUT:   <metadata_survey_rounds.csv>, <Response_rates.png>
    source(here::here(script, "03_response_rates_simple.R"))
    
    
## Section 4: T-tests ##########################################################   
    # PURPOSE:  Test for each CATI round and variable if the mean in this round 
    #           is equal to the baseline mean (t-tests). 
    #           Save a table with mean, mean difference and p-value.
    # INPUTS:   <formatted_primary>, <dfs>       
    # OUTPUT:   <test_results_primary>, <t_tests_BL_vs_CATI_study_id.csv>
    source(here::here(script, "04_t_tests.R")) 
    
    
## Section 5: Attrition table  #################################################
    # PURPOSE:  For each CATI round, regress response against all analyzed 
    #           variables. Create an attrition table, containing coefficients,  
    #           robust SEs and Wald-test p-values.
    # INPUTS:   <data_wide_formatted>, <study_id>                             
    # OUTPUTS:  <Attrition_table_study_id.html> 
    source(here::here(script, "05_attrition_table.R"))
    
    
## Section 6: Plot outcomes and safe as png ####################################
    # PURPOSE:  Plot the mean and 95% confidence intervals of binary variables 
    #           against time and against survey round. 
    #           Plot the age distribution for each survey round. 
    # INPUTS:   <formatted_long>, <formatted_primary>, <waves>, 
    #           <test_results_primary>                             
    # OUTPUTS:  <Attrition_table_study_id.html> 
    source(here::here(script, "06_plot.R"))
       
    