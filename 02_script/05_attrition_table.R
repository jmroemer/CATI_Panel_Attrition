#------------------------------------------------------------------------------#
# TITLE:        05_attrition_table.R                                           #      
# PURPOSE:      Create and save an attrition table:                            #
#               - Runs one linear regression for each CATI round,              #
#                 regressing response against all analyzed variables (age,     #
#                 gender identity, urbanicity, working in agriculture and      #
#                 completed secondary education).                              #
#               - Stores the regression coefficients, robust standard errors   #
#                 and Wald-test p-values in <Attrition_table_study_id.html>.   #
# INPUTS:       <data_wide_formatted>, <study_id>                              #
# OUTPUTS:      <Attrition_table_study_id.html>                                #
# PUBLISHED:    2021_04_21                                                     #  
# AUTHOR:       Janina Roemer                                                  #            
#------------------------------------------------------------------------------#

# Create a vector with all variables that should be part of the  regression.
de_vars <- 
  colnames(data_wide_formatted)[grepl("^de_", colnames(data_wide_formatted))]
# # USER: adapt if necessary, using the following line. 
# de_vars <- c("de_age",  "de_female",  "de_completed_secondary", 
#              "de_agriculture", "de_urban")


# Create a vector with all column names of <data_wide_formatted>, that will be 
# used as dependent variable in the regression:
response <- colnames(data_wide_formatted)[grepl("^respondent_", 
                                                   colnames(data_wide_formatted))]

# Create an empty list that will hold the regression formulas for the indivi-
# dual CATI rounds. 
formulas <- list()

# Create an empty list that will hold the regression output for the indivi-
# dual CATI rounds. 
model <- list()

# Create an empty list that will contain the coeftest() output for each survey
# round.
coef_results <- list()

# Loop through all CATI rounds in <response>, running the regression of 
# response-status against all variables in <de_vars>, storing the regression
# output in <model> and <coef_results>.
for (i in 1:length(response)){
  formulas[[i]] <- as.formula(paste(response[[i]], 
                                paste(de_vars, collapse =  "+ "), sep = " ~ ")) # Adding the formula for the i-th fit to the list "formulas"
  model[[i]] <- lm(formula = formulas[[i]], data = data_wide_formatted)
  
 
  # Name the list of models so that each element shows the CATI round for 
  # which it is regressing response on the variables in <de_vars>. 
  names(model)[[i]] <- paste0("Round ",  
                              gsub("^.*?_r", "", response[[i]]))   
  
  # Create a named list containing coefficients and robust standard errors of 
  # all coefficients. 
  coef_results[[i]] <- coeftest(model[[i]], vcov = vcovHC(model[[i]], "HC1"))
  names(coef_results)[[i]] <- paste0("Round ",  
                                     gsub("^.*?_r", "", response[[i]]))         # Name the list elements according to the CATI rounds
}

# Create a dataframe "errors" that contains the robust (HC1) standard errors for 
# all coefficients and the standard deviations of the respective demographic 
# variables at baseline as first column. 
# One row per demographic variable, one column per round.

  # Create a list containing the robust standard errors: Select the Std. Error
  # column from, <coef_results> and remove the first element (intercept).
  se_list <- lapply(coef_results, "[", 2:nrow(coef_results[[1]]) , 2) 
  
  # Add the standard deviations of the variables at baseline to se_list...
  se_list[["Baseline mean"]] <- apply(data_wide_formatted[, 
          grepl("^de_", colnames(data_wide_formatted))], 2, sd, na.rm = T)
  
  # ... and make it the first element of se_list. 
  se_list <- se_list[c(length(se_list), 1:(length(se_list)-1))]
  
  # Create a dataframe containing the elements of se_list as columns.
  errors <- as.data.frame(se_list)
  
  # Extract the row names, which are destroyed by sapply().
  row_names <- rownames(errors)
  
  # Use sapply to format the errors to 4 decimals and wrapped in ( ).
  errors <- sapply(errors, function(x) 
                            paste0("(", formatC(x, format="f", digits=4), ")")) # Can use signif instead of formatC if desired
  
  # Save errors as a dataframe
  errors <- as.data.frame(errors)
  
# Create a dataframe containing the p_values of a Wald test with HC1 standard 
# errors testing for each round if the coefficients of a lm regressing response 
# status against the variables in <de_vars> are all zero:
  p_values <- lapply(model, function(x) 
    waldtest(x, vcov = vcovHC(x, "HC1"))[[2,4]]) %>%  
    lapply(function(x) formatC(x, format="f", digits=4)) %>% 
    as.data.frame()
  
  p_values <- cbind("row_names" = "p-value of joint test of equivalence", 
                    "Baseline mean" = "", p_values)


# Create a dataframe containing the coefficients of the linear models that  
# regressed response on the selected variables for each survey round. 
# One row per variable, one column per round.
  # Extract the coefficients from each element of the list "model"
  coefficients <- model %>%  
    lapply("[[",1)  %>%          # Takes only the first sub-element of all elements in model, i.e. the coefficients.
    lapply(function(x)  x[-1])   # Removes the first element of the coefficients (intercept)
  
  # Add the baseline means as a first column to coefficients. 
  coefficients[["Baseline mean"]] <- 
    apply(data_wide_formatted[, grepl("^de_", colnames(data_wide_formatted))], 
          2, mean, na.rm = T) 
  # ... and make it the first element of se_list. 
  coefficients <- coefficients[c(length(coefficients), 1:(length(coefficients)-1))]
  
  # Adapt coefficients to output format. 
  coefficients <- sapply(coefficients, function(x) paste0("", formatC(x, format="f", digits=4), ""))

  # Save coefficients as a dataframe
  coefficients <- as.data.frame(coefficients)
  
# Extract the p-values from <coef_results> and safe as list <coef_results_p>
coef_results_p <- coef_results %>%  lapply("[", 2:nrow(coef_results[[1]]) , 4) # Take all but the first row of the 4th column (Pr(>|t|)) of each element of <coefficients>

# Create a dataframe of p_values of the coefficients, that matches the format 
# of  <coefficients> and <errors> (respective variables as rows and the
# CATI rounds as columns). 
p_frame <- as.data.frame(coef_results_p)

# Create a dataframe <significance> that holds the symbols indicating 
# significance at all positions at which a coefficient in <coefficients> is 
# significant and " " where the coefficient is not significant. 

  #First define and empty dataframe of the same size as <p_frame>
  significance <- sapply(p_frame, as.character)
  significance[] <- NA                
  
  # Then fill it with the significance levels:
  significance[p_frame[]<= 0.01] <- "***"
  significance[p_frame[] > 0.01 & p_frame[]<= 0.05] <- "**"
  significance[p_frame[] > 0.05 & p_frame[]<= 0.1] <- "*"
  significance[p_frame[] > 0.1] <- " "

  # Add stars indicating significance to the coefficients. First column of 
  # <coefficients> is not changed, since it contains the baseline averages. 
  for(i in 2:ncol(coefficients)){
    coefficients[, i] <- paste0(coefficients[ ,i], significance[, i-1])
  }

  
# Feed the first row name and an empty "" into the full vector with empty 
# row names for the errors
all_row_names <- c(row_names[1], "")
  
  
  # Create an attrition table from the coefficients and the standard errors, 
  # always alternating one coefficient row with one standard error row. 
  attrition_table <- coefficients[1, ]
  attrition_table[2, ] <- errors[1, ]
  
  for(i in 2:nrow(coefficients)){
    attrition_table[2*i-1, ] <- coefficients[i, ]
    attrition_table[2*i, ] <- errors[i, ]
    # Add the row name and an empty "" to the vector with row names
    all_row_names[2*i-1] <- row_names[i]
    all_row_names[2*i] <- ""
  }

# Add the column with the row names at the beginning.
attrition_table <- cbind(row_names=all_row_names, attrition_table)
attrition_table["p_values", ] <- p_values

# Get the column names out of the attrition table
col_names <- colnames(attrition_table) 
# Remove the name of the first column, which just has the row names
col_names[1] <- ""


# Remove all leading zeros from the round names.
col_names <- gsub(" 0", " ", col_names)


# Change the column "row_names" of attrition_table to contain the row names that 
# should appear in the final table. 
attrition_table$row_names[attrition_table$row_names == "de_age"] <- 
  "Respondent age in years"

attrition_table$row_names[attrition_table$row_names == "de_female"] <- 
  "Dummy = 1; Is female"

attrition_table$row_names[attrition_table$row_names == "de_urban"] <- 
  "Dummy = 1; Lives in urban area"

attrition_table$row_names[attrition_table$row_names == "de_agriculture"] <- 
  "Dummy = 1; Works in agriculture"

attrition_table$row_names[attrition_table$row_names == "de_completed_secondary"] <-
  "Dummy = 1; Compled secondary education"



# Create the Kable HTML table removing the R-provided row names, and renaming 
# the columns as desired.
kbl(attrition_table, row.names=F, col.names = col_names) %>%
  kable_classic() %>% 
  # kable_classic("hover") %>%                                                  # Line under Grouping disappears if hover is chosen
  add_header_above(c(" " = 2,
                      # 1, "Mean" = 1, 
                     "Regression coefficients" = 
                       ncol(attrition_table)-2), bold = T, line = T) %>% 
  row_spec(0,bold=TRUE) %>% 
  column_spec(1, bold = T) %>% 
  column_spec(1, width = "18em") %>% 
  column_spec(2:ncol(attrition_table), width_min = "5em") %>% 
  row_spec(seq(2,nrow(attrition_table), 2), extra_css = "padding-bottom:15px" 
           #, color="gray"                                                      # Comment this back in if you want the standard errors to appear in gray.
           ) %>%  
  row_spec(nrow(attrition_table), extra_css = "border-top:1px solid black; padding:5px")  %>% 
  save_kable(paste0(output_tables, "/Attrition_table_", study_id, ".html"))

