 #------------------------------------------------------------------------------#
# TITLE:        01a_load_raw_data_s016SEN.R                                     #
# PURPOSE:      Load all open data from .sav files into environment.            #
# INPUTS:       Original open data.                                             #
# OUTPUTS:      Environment: One dataframe per survey round, named following    #
#               the scheme rXX with XX being the time in months between data    #
#               collection for this round and for baseline.                     #
# PUBLISHED:    2021-04-21                                                      #  
# AUTHOR:       Janina Roemer                                                   #
#-------------------------------------------------------------------------------#

## Section 1: Load the data from the .sav files in raw into the environment ####
##            The variable names correspond to the number of months passed    ##
##            between the start of the baseline data collection and the       ##
##            start of the data collection of the respective wave             ##

baseline <- read_sav(paste(raw, "L2S_Fichier_Kish_v2.sav", sep = "/")) 

r21 <- read_sav(paste(raw, "Base Finale Inclusion_financiere aout2016 ano.sav", 
                      sep = "/")) 

r18E <- read_sav(paste(raw, "Education avr_mai_2016_v1.1.sav", sep = "/"))      
                                                                                

r08 <- read_sav(paste(raw, "base alimentation 1.sav", sep = "/"))

r15 <- read_sav(paste(raw, "base alimentation 2.sav", sep = "/"))

r22 <- read_sav(paste(raw, "Base Finale Alimentation 3 septembre2016 ano.sav", 
                      sep = "/"))

r25 <- read_sav(paste(raw, "Base Finale Alimentation 4 decembre2016 ano.sav", 
                      sep = "/"))

# Data for Alimentation 5 still missing. 

r10 <- read_sav(paste(raw, "Electricite_transport aout_Sept_2015_V1.1.sav", 
                      sep = "/")) 

r12 <- read_sav(paste(raw, "L2S_SANTE_2015.sav", sep = "/"))         

r18 <- read_sav(paste(raw, "L2S_CONDITIONS DE VIE 2016.sav", sep = "/"))    

r24 <- read_sav(paste(raw, "Base Finale Tabaski novembre2016_anonymisee VF.sav", 
                      sep = "/")) 