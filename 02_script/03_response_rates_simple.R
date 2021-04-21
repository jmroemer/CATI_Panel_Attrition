#------------------------------------------------------------------------------#
# TITLE:        03_response_rates_simple.R                                     #      
# PURPOSE:      Calculate the response rate for each survey round, add it to   #
#               the metadata table <waves> and save this as csv.               #
#               into "response_rates.csv".                                     #
# INPUTS:       <data_wide_formatted>, <dfs>.                                  #
# OUTPUTS:      <metadata_survey_rounds.csv>: Metadata table now including the #
#                   response rates.                                            #
#               <Response_rates.png>: Plot of response rates against time      #
#                   after baseline.                                       #
# PUBLISHED:    2021-04-21                                                     # 
# VERSION:      Simple version only calculating the response rate of           #
#               <data_wide_formatted> and creating a simple plot.              #
# AUTHOR:       Janina Roemer                                                  #            
#------------------------------------------------------------------------------#

## Section 1: Calculating the response rate for all survey rounds ##############

  response_rates <- 
    data_wide_formatted[, grepl("respondent", 
                                colnames(data_wide_formatted), fixed = T)] %>% 
    sapply(function(x) sum(x)/sum(!is.na(x)))
  
  # Add a first element 1 to response_rates to stand for the baseline. 
  response_rates <- c(1, response_rates)
  
  # Add a column "response_rate" to <waves>. 
  waves$response_rate <- response_rates
  
  # Save the metadata as "/metadata_survey_rounds.csv" - now including 
  # the response rates.
  write.csv(waves, paste0(output_tables, "/metadata_survey_rounds_", study_id, ".csv"), row.names = F)
  

## Section 2: plotting the response rates against survey months ##############
    
  plot_response <- ggplot(waves[waves$months_from_BL!=0, ], mapping = aes(x = as.numeric(months_from_BL), 
                                               y = 100 * response_rate)) +
      geom_point() +
      labs( x= "Time (months)", y = "Response rate (%)") +
      coord_cartesian(xlim = c(0, max(waves$months_from_BL)+1), 
                      ylim = c(0, 100), 
                      expand = F) +
      theme_classic() + 
      theme(legend.position = "none") 
  
  print(plot_response)
  ggsave(paste(output_graphs, "Response_rates.png", sep= "/"), width = 4, 
         height = 3, dpi = 200)
  
## Cleanup ------------------------------------------------------------------##
rm("response_rates")

