#------------------------------------------------------------------------------#
# TITLE:      06_plot.R                                                        #      
# PURPOSE:    Plot the mean and confidence intervals of binary demographic     #
#             variables against time and against survey round. Means and       #
#             heteroskedasticity robust standard errors are determined using   #
#             linear regression.                                               #
#             Plot the probability density of ages for each survey round.      # 
# INPUTS:     <dfs>, <formatted_long>, <formatted_primary>, <waves>,           #
#             <test_results_primary>.                                          #
# OUTPUTS:    <Sample_composition_of_respondents_against_time_study_id.png>,   #
#             <Sample_composition_of_respondents_against_round_study_id.png>:  #
#                 Plots of the mean of the binary demographic variables and    #
#                 95% confidence intervals against time and survey round,      #
#                 respectively.                                                #
#             <Age_distribution_of_respondents_study_id.png>: Violin plots of  #
#                 the age distribution for all survey rounds.                  #
# PUBLISHED:  2021-04-21                                                       #
# AUTHOR:     Janina Roemer                                                    #            
#------------------------------------------------------------------------------#

## Section 1: Linear regression to determine Means and Standard Errors ########

  # For each demographic variable regress its value against the survey rounds.
  calc.lm <- function(data1){
    # RETURNS: Regression output.
    # INPUT: <data1>: List of dataframes in long format. Each dataframe contains  
    #        the baseline values of one demographic variable for all respondents. 
    #        Non-respondents are omitted (set to NA). Long format with columns 
    #        "number" and "wave_name" identifying the survey round. 
    result <- lm(data1$value ~ as.factor(data1$number))
    return(result)
  }
  
  # create a list with linear regression results for the different demographic 
  # variables.
  lms <- formatted_long %>% lapply(calc.lm)
  
  # Save regression coefficients and standard errors as list elements.
  coefs <- lms %>% lapply(function(x) coeftest(x, vcov = vcovHC(x, "HC1"))) 
  coefs <- coefs %>% lapply("[", 1:nrow(coefs[[1]]),1)
  
  # Add coefficients to the intercept (baseline mean) to retrieve CATI round means.
  for (j in seq_along(coefs)){
    for (i in 2:length(dfs)){
      coefs[[j]][i] <- coefs[[j]][1] + coefs[[j]][i]
    }
  }
  
  # Jitter datapoints with identical number of months after baseline, adding 
  # a new column "months_jittered" to waves. Avoids overlapping axis labels.
  for(i in 1:(nrow(waves)-1)){
    if (waves$months_from_BL[i] == waves$months_from_BL[i+1]){
      waves$months_from_BL[i] <- waves$months_from_BL[i]-0.21
      waves$months_from_BL[i+1] <- waves$months_from_BL[i+1]+0.21
    }
  }
  
  # Create a list containing fit results of each demographic variable  as 
  # elements, with  wave-numbers, -names, -dates, -subject and -response rates
  # added.
  lm_results <- list()
  for(i in seq_along(coefs)){
   lm_results[[i]] <- data.frame(coefs[[i]], waves$months_from_BL, 
                                 names(dfs), waves$long_name, waves$date, 
                                 waves$topic, waves$response_rate)
   
   lm_results[[i]]$waves.months_from_BL <- 
                          as.numeric(lm_results[[i]]$waves.months_from_BL)
   names(lm_results[[i]])[c(1, 3, 7)] <- c("mean", "wave_name", "responseR")
   
   # Calculate the confidence interval based on the robust standard errors:
   lm_results[[i]]$HC1_SE <- coeftest(lms[[i]], vcov. = vcovHC(lms[[i]], type = 'HC1'))[, 2]
   lm_results[[i]]$CI_min_HC1 <- lm_results[[i]]$mean - 1.9599639845* lm_results[[i]]$HC1_SE 
   lm_results[[i]]$CI_max_HC1 <- lm_results[[i]]$mean + 1.9599639845* lm_results[[i]]$HC1_SE
   
   lm_results[[i]]$wave_name[lm_results[[i]]$wave_name == "baseline" ] <- "BL"
  } 
  
  # Name the list elements according to the demographic variable they represent. 
  names(lm_results) <- names(coefs)
  
  # Provide demographic variables of interest in desired order. 
  dem_list <- c("de_agriculture", "de_female", "de_urban", "de_completed_secondary") # USER: Add variables of interest in order you want them to appear.
  
  # Provide labels for these variables in the same order as above. 
  # This will be used for in graph labeling. Here the format "XY% are female", 
  # etc. was chosen. 
  dem_labels = c(paste0(round(lm_results[["de_agriculture"]]$mean[1]*100),        # USER: Add the labels you want them to have (in the same order as above).
                        "% work in agriculture."), 
                 paste0(round(lm_results[["de_female"]]$mean[1]*100), 
                        "% are female."),
                 paste0(round(lm_results[["de_urban"]]$mean[1]*100), 
                        "% live in urban areas."),         
                 paste0(round(lm_results[["de_completed_secondary"]]$mean[1]*100), 
                        "% have completed\nsecondary education."))
  
  # Create a dataframe connecting variable names with the corresponding labels. 
  dem_names <- as.data.frame(cbind(dem_list, dem_labels))
  
  # Define color palette to IPA Secondary palette #2 and name them with the 
  # labels created above.
  colors1 <- c('#006EB9', '#F8971D', '#00AEEF', '#8D288F')
  names(colors1) <- dem_labels
  

  # Add a column with the name of the demographic variable to each list element 
  # of lm_results. 
  for(i in seq_along(lm_results)){                                              
    lm_results[[i]]["demogr"] <- rep(names(lm_results)[[i]], nrow(lm_results[[i]]))
  }
  
  # # Add a column with the long name of the demographic variable to the list
  # # elements that will be plotted. - Can be used for labeling.
  # for(i in seq_along(lm_results)){
  #   if (lm_results[[i]][1, "demogr"] %in% dem_names$dem_list){
  #     lm_results[[i]]["long_demogr"] <- rep(dem_names[lm_results[[i]][1, "demogr"] == dem_names$dem_list,]$dem_labels, nrow(lm_results[[i]]))
  #   }
  #  else  lm_results[[i]]["long_demogr"] <- NA
  # }

  
  # Add a column with the difference between mean of baseline and each CATI 
  # round (will be needed to position labels) and one with the p-values from 
  # the t-test.
  for(i in seq_along(names(lm_results))){
    lm_results[[names(lm_results)[i]]]$difference <- 
               test_results_primary[, paste0("Difference_", names(lm_results)[i])]
    lm_results[[names(lm_results)[i]]]$p_value <- 
      coeftest(lms[[i]], vcov = vcovHC(lms[[i]], "HC1"))[,4]
    lm_results[[names(lm_results)[i]]][is.na(
                lm_results[[names(lm_results)[i]]]$difference), ]$difference <- 0  
  }
  
  
  # Add Significance levels to as and extra column
  add.significance <- function(data1){
    # INPUT:    Dataframe containing a p-value column.
    # RETURNS:  The same dataframe with a character column stating the 
    #           significance level: * for < 0.1, ** for < 0.05, *** for < 0.01.
    data1$significance <- rep(NA, nrow(data1))
    data1$significance <- ifelse(is.na(data1["p_value"]), "", 
                                ifelse(data1["p_value"] < 0.01, "***", 
                                ifelse(data1["p_value"] < 0.05, "**", 
                                ifelse(data1["p_value"] < 0.1, "*", ""))))
    data1$significance[1] <- ""                                                   # Set all baseline significance levels to ""
    return(data1)
  }
  
  # Add sigificance column to all elements of lm_results.
  lm_results <- lapply(lm_results, add.significance)
  
  # Create a list of dataframes expressing binary variable results in percentages
  lm_results_percentage <- lm_results
  percentage.binary <- function(data1){
    if (min(data1$mean) > 0 & max(data1$mean) < 1){
      data1[c("mean", "HC1_SE", "CI_min_HC1", "CI_max_HC1", "difference")] <- 
                     data1[c("mean", "HC1_SE", "CI_min_HC1", "CI_max_HC1", "difference")]*100
    }
    return(data1)
  }
  
  lm_results_percentage <- lapply(lm_results, percentage.binary)
  
  # Combine all list elements to one dataframe in long format. 
  summary_all_percentage <- bind_rows(lm_results_percentage)
  
  
## Section 2: Creating and saving data visualizations ##########################

## 2A. Plotting binary demographic variables against time after baseline -----##
##     Plot means and confidence intervals using heteroskedasticity robust 
##     Standard errors. 
##     Use different shape to mark baseline and add a horizontal reference line
##     at baseline value.
##     Add labels at end of this line, indicating the demographic variable and 
##     its baseline mean. 

  # Labels will be added to the last datapoints but should be identified with 
  # the horizontal lines indicating the baseline values. The labels will 
  # therefore be shifted according to the mean difference between last round 
  # and baseline.
  label.pos <- function(data1){                                               
    # RETURNS: A vector containing the position shifts for the labels. Needs to  
    #          be stored in global environment with variable name "shifts".
    # INPUTS: <data1>:  A long-format dataframe containing a column "demogr" 
    #                   indicating the demographic variable, and a column
    #                   "difference" with the mean difference of this variable 
    #                   between baseline and the respective CATI round. 
    shifts <- (transform(data1[(data1$demogr %in%  dem_list), ],
                         demogr = factor(demogr, levels = dem_list,
                                  labels = dem_labels))$difference[c(length(dfs),
                                  2*length(dfs), 3*length(dfs), 4*length(dfs))])
    shifts <- shifts[c(3,2,1,4)]                                                # Sorting to match order of the demographics in plot
    shifts[4] <- shifts[4]*3*sign(shifts[4])                                  # Last demographic description has two lines, thus shifted further
    return(shifts)
  }
  
    
  plot.dems <- function(data1, ymin, ymax, ytitle){
    # RETURNS: Plot of demographic variable means and confidence intervals
    #          against the number of month after baseline, using the survey
    #          round numbers as x-labels. 
    # INPUTS:  <data1>:   Data in long format, containing means, SE, CI,  
    #                     response-rate,"demogr" column.
    #          <ymin>, <ymax>:  Minimum and maximum value of y-axis scaling. 
    #          <ytitle>:  y-axis label.                         
      plot_dem <- 
      ggplot(transform(data1[(data1$demogr %in%  dem_list), ],
                              demogr = factor(demogr, levels = dem_list, 
                                                     labels = dem_labels)),
             mapping = aes(x = waves.months_from_BL, y = mean, color = demogr)) +
      
      scale_color_manual(values = colors1) +
      labs(color = "Demographic characteristic") +
      
      geom_point(size = 2) +                                                    # USER: comment this line out and the following 3 in if opacity should scale to the response rate. 
      # geom_point(aes(alpha = responseR), size = 2) +
      # labs(alpha = "Response rate") +
      # scale_alpha(range = c(0.5*min(data1["responseR"]), max(data1["responseR"]))) +
      
      geom_dl(aes(label = demogr), method = list(cex = 0.7, 
                  dl.trans(x = x + 0.5, 
                           y = y - shifts/14 + 0.2), "last.points")) +   
      
      geom_errorbar(aes(ymin = CI_min_HC1, ymax = CI_max_HC1), width = 0.5) +
      
      geom_text(aes(label = significance), check_overlap = F, hjust = 0, 
                nudge_x = 0.25, vjust = 0, 
                nudge_y = ymax*ifelse(sign(transform(data1[(data1$demogr %in% dem_list), ],
                          demogr = factor(demogr, levels = dem_list, 
                          labels = dem_labels))$difference) == -1, -0.048, 0.005)) + # Move asterisks up if the difference from BL is positive and down if negative. 
      
      # Different propositions for possible x-Axis labels. 
      xlab("Approximate time") +
      # xlab("Survey round number") +
      # xlab(ifelse(study_id == "s016TZA", "Approximate time", "Time")) +
      ylab(ytitle) +
      
      # coord_cartesian(ylim = c(ymin, ymax), expand = FALSE, 
      #                 xlim = c(-1, max(data1$waves.months_from_BL)+10)) +
      coord_cartesian(ylim = c(ymin, ymax), expand = FALSE, 
                      xlim = c(-1, (max(data1$waves.months_from_BL)+1.3)*1.35)) +
        
      theme_classic() +
      theme(legend.position = "none",  
            axis.text.x     = element_text(angle = 90, hjust = 1),              
            plot.title      = element_text(hjust = 0.5),
            text = element_text(size = rel(3.25))
            ) 
    
    return(plot_dem)
   }
   
  # Create and save the plot of all binary demographics as percentages against 
  #  wave number with different marker for the Baseline.
  dem_labels_sorted <- dem_labels[c(2,3,1,4)]                                   # Required for adding horizontal lines with correct colors.  
  
  plot.dems.specialBL <- function(data1, ymin, ymax, ytitle){
    plot_dem_special <- 
      plot.dems(data1[data1$waves.months_from_BL != 0, ], ymin, ymax, ytitle) + # Creates a plot of the means of all the CATI waves.
      
      geom_point(inherit.aes = F, 
                 data = transform(data1[(data1$demogr %in%  dem_list &
                                         data1$waves.months_from_BL == 0), ],
                                  demogr = factor(demogr, levels = dem_list, 
                                                          labels = dem_labels)),
                 mapping =  aes(x = waves.months_from_BL, y = mean, color =  demogr), 
                 size = 3, shape = 18) +                                        # Adds the Baseline means as different markers.
      
      geom_hline(yintercept = data1[(data1$waves.months_from_BL == 0 &            
                                     data1$demogr %in% dem_list), ]$mean,        
                 color = colors1[dem_labels_sorted]) +                          # Adds the Baseline means as horizontal lines.
      
      scale_x_continuous(breaks = data1$waves.months_from_BL[1:length(dfs)],    # Scale according to the approximate time correlation provided by "months_from_BL".
                         labels = data1$waves.long_name[1:length(dfs)])         # Use survey round name as labels.
    return(plot_dem_special)
  }
  
  # Creating and saving the plot of all binary demographics as percentages
  # against wave number
  shifts <- label.pos(summary_all_percentage)
  plot_prim_dem_100_BL <- summary_all_percentage  %>%  
    plot.dems.specialBL(0, 100, "Percentage")
  
  print(plot_prim_dem_100_BL
       # + ggtitle(paste0("Sample composition of responding panel members in each round - ", study_id))
  )
  ggsave(paste0(output_graphs, "/Sample_composition_of_respondents_against_time_", 
                study_id, ".png"), width = 6, height = 4, dpi = 200)
  

## 2B. Plotting binary demographic variables against survey round ------------##
##     Plot means and confidence intervals equidistantly against the survey
##     round, using heteroskedasticity robust Standard errors. 
##     Use different shape to mark baseline and add a horizontal reference line
##     at baseline value.
##     Add labels at end of this line, indicating the demographic variable and 
##     its baseline mean. 
  
  plot.dems.name <- function(data1, ymin, ymax, ytitle){
    # RETURNS: Plot of demographic variables means and confidence intervals 
    #          equidistantly against survey round name.
    # INPUTS:  <data1>:   Data in long format, containing means, SE, CI,  
    #                     response-rate,"demogr" column.
    #          <ymin>, <ymax>:  Minimum and maximum value of y-axis scaling. 
    #          <ytitle>:  y-axis label.  
    plot_dem <- 
      ggplot(transform(data1[(data1$demogr %in%  dem_list), ],
                       demogr = factor(demogr, levels = dem_list, 
                                               labels = dem_labels)),
             mapping = aes(x = wave_name, y = mean, color = demogr)) +

      scale_color_manual(values = colors1) +
      
      labs(color = "Demographic characteristic") +
      
      geom_point(size = 2) +                                                    # USER: comment this line out and the following 3 in if opacity should scale to the response rate.
      # geom_point(aes(alpha = responseR), size = 2) +
      # scale_alpha(range = c(0.5*min(data1["responseR"]), max(data1["responseR"]))) +
      
      geom_errorbar(aes(ymin = CI_min_HC1, ymax = CI_max_HC1), width = 0.4) +
      
      geom_dl(aes(label = demogr), 
              method = list(cex = 0.7, 
                            dl.trans(x = x + 0.5, 
                                     y = y - shifts/14 + 0.2), "last.points")) +
      
      geom_text(aes(label = significance),  
                hjust = 0, nudge_x = 0.25, vjust = 0, 
                nudge_y = ymax*
                  ifelse(sign(transform(data1[(data1$demogr %in%  dem_list), ], 
                              demogr = factor(demogr,
                                              levels = dem_list, 
                                              labels = dem_labels))$difference) == -1, 
                                                            -0.048, 0.005)) +   # Move asterisks up if the difference from BL is positive and down if negative. 
      
      coord_cartesian(ylim = c(ymin, ymax), expand = FALSE, 
                      xlim = c(0.5, (length(unique(data1$wave_name))+1.3)*1.35)) +

      xlab("Survey round") +
      ylab(ytitle) +
      theme_classic() +
      theme(legend.position = "none", 
            axis.text.x     = element_text(angle= 90, hjust = 1, size = 7),
           plot.title = element_text(hjust = 0.5, size = 12),
           axis.title = element_text(size = 9),
           axis.text.y = element_text(size = 7))
    return(plot_dem)
  }
  
  plot.dems.name.specialBL <- function(data1, ymin, ymax, ytitle){
    plot_dem_special <- plot.dems.name(data1[data1$waves.months_from_BL != 0, ], 
                                                        ymin, ymax, ytitle) +   # Creates a plot of the means of all the CATI waves.
      
      geom_point(inherit.aes = F, 
                 data = transform(data1[(
                   data1$demogr %in%  dem_list &
                     data1$waves.months_from_BL == 0), ],
                   demogr = factor(demogr, 
                                   levels = dem_list, 
                                   labels = dem_labels)),
                 mapping =  aes(x = wave_name, y = mean, color =  demogr), 
                 size = 3, shape = 18) +                                        # Adds the Baseline means as different markers.
      
      geom_hline(yintercept = data1[(data1$waves.months_from_BL  == 0 & 
                                     data1$demogr %in% dem_list), ]$mean, 
                 color = colors1[dem_labels_sorted]) +                          # Adds the Baseline means as horizontal lines.
      
      scale_x_discrete(labels = data1$waves.long_name)                          # Use survey round name as labels.
    return(plot_dem_special)
  }

  # Creating and saving the plot of all binary demographics as percentages
  # against wave name
  shifts <- label.pos(summary_all_percentage)                                              
  plot_prim_dem_100_BL_name <- summary_all_percentage  %>%  
    plot.dems.name.specialBL(0, 100, "Percentage")
  
  print(plot_prim_dem_100_BL_name  
        + ggtitle(study_reference)
        # + ggtitle(paste0("Sample composition of responding panel members in each round - ", study_id))
        )
  
  ggsave(paste0(output_graphs, "/Sample_composition_of_respondents_against_round_", 
                study_id, ".png"), width = 6, height = 4, dpi = 200)


## 2C: Creating violin plots of the age distribution for each survey round. --##

  # Create a dataframe <age_long> containing the age value for all survey 
  # participants and all rounds in long format (columns "uhn" identifying the 
  # participant and "wave_name" identifying the survey round).
  age_long <- formatted_primary[[1]] 
  age_long$baseline <- age_long$de_age_baseline                                 # Add a column containing all baseline values. Helps to plot the baseline distribution as outline for comparison. 
  age_long <- age_long %>% prepare.data(bl_column = T)
  
  # Add a response rate column for labeling.
  age_long$response <- rep(ifelse((is.na(waves$response_rate) | 
                                   waves$wave_name == "baseline"), NA , 
                                   paste0(formatC(round(waves$response_rate*100, 
                                    digits = 1), format='f', digits = 1), "%")),
                                    nrow(formatted_primary[[1]]))               
  
  # # Add quantiles, mean and median columns. Can be added to the plot. 
  # bl75 <- quantile(formatted_primary[[1]]$de_age_baseline, 0.75, na.rm = T)
  # bl25 <- quantile(formatted_primary[[1]]$de_age_baseline, 0.25, na.rm = T)
  # bl_mean <- mean(formatted_primary[[1]]$de_age_baseline, na.rm = T)
  # bl_median <- median(formatted_primary[[1]]$de_age_baseline, na.rm = T)
  
  
  # Create a violin plot of respondent age for every survey round. 
  plot_age <- ggplot(data = age_long, trim= T) +
    
    see::geom_violinhalf(mapping = 
                           aes(x = wave_name, y = baseline, group = number), 
                         #bw = 2, 
                         na.rm = T, scale = "count",  
                         color = "#006EB9", 
                         size  = 0.5,
                         width = 1.75) +                                        # Plot the baseline distribution as blue outline to all survey rounds.
    
    see::geom_violinhalf(mapping = 
                           aes(x = wave_name, y = value, group = number), 
                          # bw = 2, 
                           na.rm = T, scale = "count", 
                           fill  = "#81B53C", 
                           color = "#81B53C", 
                           width = 1.75) +                                      # Plot the distribution of each survey round in green (fill).
    
    see::geom_violinhalf(data = age_long[age_long$number == waves$wave_number[1], ],
                         mapping =
                           aes(x = wave_name, y = baseline, group = number),
                          # bw = 2, 
                           na.rm = T, scale = "count",
                           fill  = "#006EB9",
                           color = "#006EB9",
                           width = 1.75) +                                      # Plot the baseline distribution in blue (fill).
    
    # # Comment this block back in, to include the mean or median and quantiles.##
    ##############################################################################
    # geom_hline(yintercept = bl_mean, 
    #            # yintercept = bl_median, 
    #            size  = 0.4, alpha = 1,
    #            linetype   = "solid", color = "#006EB9") +
    # stat_summary(mapping = aes(x = wave_name, y = value, group = number),
    #              fun = mean,
    #              # fun = median,
    #              geom = "crossbar",
    #              width = 0.2,
    #              na.rm = T,
    #              size = 0.2
    # ) +
    # stat_summary(mapping = aes(x = wave_name, y = value, group = number),
    #              fun.min = function(z) { quantile(z,0.25) },
    #              fun.max = function(z) { quantile(z,0.75) },
    #              geom = "linerange",
    #              na.rm = T,
    #              size = 1
    # ) +
    # annotate("rect", xmin = 0, xmax = 15, ymin = bl25, ymax = bl75,
    #          fill = "#006EB9", alpha = 0.2) +
    ##############################################################################
  
    geom_dl(x =  age_long$wave_name, y = age_long$value, aes(label = response),
            method = list(cex = 0.6, 
                          dl.trans(x = x + 1.5/length(unique(age_long$wave_name)), 
                                   y = max(y)-0.7), "last.points"),
          color = "#81B53C", na.rm = T) +                                       # Add labels with the response rates of each CATI round. 
   
    annotate("text", label = "Response\nrate:", 
             x = 2 + 0.75/length(unique(age_long$wave_name)),
             y = max(age_long$value, na.rm = T) - 3, hjust = 0,  
             color = "#81B53C", cex = 2.3, lineheight = 0.8) +                  # Add a label "Response rate:" above the first response rate. 
    
    xlab("Survey round") +
    ylab("Age") +
    # labs(title = "Age distribution in the different survey rounds") +
    theme_classic() +
    theme(axis.text.x = element_text(angle= 90, hjust = 1),
          plot.title  = element_text(hjust = 0.5)) +
    
    coord_cartesian(expand = FALSE, xlim = c(0.7, 
                                    length(unique(age_long$wave_name)) + 1)) +
    
    theme(legend.position = "none") +
    scale_x_discrete(labels = age_long$long_name) 
  
  # Print and save the age violin plots. 
  print(plot_age)
  ggsave(paste0(output_graphs, "/Age_distribution_of_respondents_", study_id, ".png"), 
         width = 6, height = 3, dpi = 200)
  

## Clean up ####################################################################
rm("i", "j")


