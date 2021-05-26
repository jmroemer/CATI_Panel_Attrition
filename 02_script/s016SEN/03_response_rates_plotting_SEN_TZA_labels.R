#------------------------------------------------------------------------------#
# TITLE:        03a_response_rates_plotting_SEN_TZA.R                          #       
# PURPOSE:      Calculate the response rate for each survey round, add it to   #
#               the metadata table <waves> and save this as csv.               #
#               Plot response rate against time after baseline for SEN and TZA.#
# INPUTS:       <data_wide_formatted>, <dfs>.                                  #
# OUTPUTS:      <metadata_survey_rounds.csv>: Metadata table now including the #
#                   response rates.                                            #
#               <Response_rates_SEN_TZA.png>: Plot of response rates against   #
#                   time after baseline.                                       #
# PUBLISHED:    2021-04-21                                                     # 
# MODIFIED:     2021-05-17                                                     #
# VERSION:      Plots the response rates of s016SEN and s016TZA into one graph.#
#               Data-labels showing response-rates for selected points added.  #
# REQUIREMENT:  00_master_s016TZA.R needs to have been run already.            #
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
  write.csv(waves, paste0(output_tables, "/metadata_survey_rounds_", 
                          study_id, ".csv"), row.names = F)
  

## Section 2: Plotting the response rates against survey months for SEN & TZA ##
  
  # Load metadata of the TZA survey and store all metadata together in waves2
  waves_TZA <- read.csv(paste0(root, 
                 "/04_output/s016TZA/tables/metadata_survey_rounds_s016TZA.csv"))
  
  
  # Add a column with study identifiers. Needed to plot in different colors. 
  waves_TZA$survey <- as.factor(rep("Tanzania", nrow(waves_TZA)))
  waves$survey <- as.factor(rep("Senegal", nrow(waves)))
  
  # Combine the metadata-information from SEN and TZA to one dataframe.
  waves2 <- rbind(waves[, colnames(waves) %in% colnames(waves_TZA)], 
                  waves_TZA[,  colnames(waves_TZA) %in% colnames(waves)])
  
  # Jitter datapoints with identical number of months after baseline, adding 
  # a new column "months_jittered" to waves2. 
  waves2["months_jittered"] <- waves2["months_from_BL"]
  for(i in 1:(nrow(waves2)-1)){
    if (waves2$months_jittered[i] == waves2$months_jittered[i+1]){
      waves2$months_jittered[i] <- waves2$months_jittered[i]-0.1
      waves2$months_jittered[i+1] <- waves2$months_jittered[i+1]+0.1
    }
  }
  
  waves2 <- waves2[waves2$months_from_BL != 0, ]                                # Remove baseline, since it has response of 100% by definition. 
  
  # Set the color palette.
  colors <- c("Tanzania" = "#006EB9", "Senegal" = "#81B53C")
  
  plot_response <- ggplot(waves2, mapping = aes(x = as.numeric(months_jittered), 
                                                y = 100 * response_rate, color = survey)) +
    geom_point(size = 1.2) +
    geom_smooth(method = lm,  se = F, size = 0.4) +                            # Add a linear fit as guide to the eye. 
    labs( x= "Time since face-to-face baseline (months)", y = "Response rate (%)") +
    scale_color_manual(values = colors) +                                      # Plot SEN and TZA in different colors.
    geom_dl(aes(label = survey), method = list(cex = 0.7,                         
                                               dl.trans(x = x - 0.25, 
                                                        y = y +0), 
                                               "first.points")) +
    
    # annotate("text", label = "Data: Survey - Listening to Senegal,           # Comment back in if in-graph data references are desired.
    #          Year 2014-2017, National Agency for Statistics and Demography 
    #          of the Republic of Senegal", x = 0.8, y = 10, hjust = 0,
    #          color = "#81B53C", cex = 2.5, lineheight = 0.8) +
    # annotate("text", label = "Twaweza, SzW survey (Round 1-24, October 
    #          2012-February 2015)", x = 2.0, y = 5, hjust = 0,
    #          color = "#006EB9", cex = 2.5, lineheight = 0.8) +
    
   # Add data labels with the response rate for the extrema and one point in between. 
      geom_text(aes(label = ifelse(
        months_jittered == max(waves2[waves2$survey== "Tanzania",]$months_jittered) & 
          survey == "Tanzania" |
          months_jittered == max(waves2[waves2$survey== "Senegal",]$months_jittered) & 
          survey == "Senegal" |
          months_jittered == min(waves2[waves2$survey== "Tanzania",]$months_jittered) & 
          survey == "Tanzania" | 
          months_jittered == min(waves2[waves2$survey== "Senegal",]$months_jittered) & 
          survey == "Senegal" |
          months_from_BL == round((max(waves2[waves2$survey== "Senegal",]$months_from_BL) + 
                                     min(waves2[waves2$survey== "Senegal",]$months_from_BL))/2, 0) & 
          survey == "Senegal" |
          months_from_BL == round((max(waves2[waves2$survey== "Tanzania",]$months_from_BL) + 
                                     min(waves2[waves2$survey== "Tanzania",]$months_from_BL))/2, 0) & 
          survey == "Tanzania" |
          months_jittered == 10.9  & survey == "Tanzania",                      # Added manually since Tanzania data had too irregular intervals to work with this algorithm
        
        paste0(round(100*response_rate, 0), "%"), "")) , 
                angle = 0, hjust = 0, vjust = 0.5, nudge_x = -0.27, nudge_y = 4.5, cex = 2.5) +
    
    coord_cartesian(xlim = c(0, max(waves$months_from_BL)+1),
                    ylim = c(0, 100), 
                    expand = F) +
    theme_classic() + 
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8)) 
  
  
  # Print and safe the plot, suppress messages (`geom_smooth()` using formula 'y ~ x')
  suppressMessages(print(plot_response))
  suppressMessages(ggsave(paste(output, "Response_rates_SEN_TZA.png", sep= "/"), 
                           width = 6, height = 3, dpi = 200))
                            # width = 6, height = 3, dpi = 300))
  

## Cleanup ------------------------------------------------------------------##
rm("response_rates")

