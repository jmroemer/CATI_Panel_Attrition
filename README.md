# Attrition in Mobile Phone Panel Surveys

Using open data of the World Bank’s [Listening to Africa](https://www.worldbank.org/en/programs/listening-to-africa) (L2A) initiative, this project analyzes attrition in two long-term panels that were conducted using computer assisted telephone interviews (CATI) in sub-Saharan Africa: The [Listening to Senegal](https://www.ansd.sn/index.php?option=com_content&view=article&id=344) survey (SEN) performed between 2014 and 2017 and the [Sauti za Wananchi](https://www.twaweza.org/go/sauti-za-wananchi-english) survey (TZA) performed in Tanzania from 2012 to 2014.<br>

In both projects a face-to-face baseline interview was conducted with randomly selected participants. Mobile phones were provided to the participants. 
Over the following two years several rounds of CATI surveys were performed based on the baseline sample. 
This allows us to analyze potential attrition bias by directly comparing selected characteristics between the group of people who responded to a particular survey round and the ones who did not. In the present version the code analyzes the following characteristics: age, gender identity, whether a participant lives in an urban area, has completed secondary education and whether their main occupation is in agriculture. 

## Purpose
The R-code provided in this repository was created to analyze the following aspects of attrition in two long-term panels: For each survey round it tests weather the group of respondents to this round is equal to the group of non-respondents with respect to a set of selected characteristics. It further tests for all survey rounds whether their sample composition is equal to the baseline sample with respect to the selected variables. Graphs showing how response rate, mean of the selected binary variables and age distribution vary over time or survey rounds are created and test-results are saved in csv/html tables. 

This is done by taking survey data of each survey round in wide format as input: Each row representing a survey participant, and each column corresponding to one variable, including a participant identifier (in case of the SEN data this identifier is created by the script 01b_create_IDs_and_metadata.R). The code merges the datasets of each survey round by the identifier column and analyzes attrition with regard to response rate development and potential introduction of biases. 

### Outputs
* A plot of the response rates of the individual survey rounds against approximate time after the face-to-face baseline.
* An attrition table that contains the coefficients of a linear model using OLS, for each CATI round regressing the dummy response variable (1 = the participant responded, 0 = the participant did not respond) against a set of demographic and occupational variables. Huber-White standard errors (HC1) are shown in parentheses. Wald-test p-values are included as last row, for each round testing the null hypothesis that none of the coefficients is significantly different from zero, that is that respondents do not systematically differ from non-respondents in the respective characteristics. The first column of the table contains the means and standard deviations of the respective characteristics in the baseline sample. 
* A table with results of t-tests each testing for one CATI round if this round’s sample composition is different from the baseline composition in a set of demographic and occupational variables. Means, mean differences and p-values are shown for each CATI round and each variable. 
* Plots of the means of the binary variables against survey round and against time after baseline, respectively. 95% confidence intervals based on Huber–White standard errors are included. The baseline values are indicated by horizontal lines as a guide to the eye. 
* A graph containing one violin plot per survey round showing the age distribution of this round’s respondents.    

## Usage

This project uses the renv package for dependency management.
Opening the RStudio project files will create a project library in the project folder and install renv to this library. renv::restore() will add required packages to this library. 

The same can be achieved by running 00_master_[study_id].R and following the prompts in the console.
    
### Reproducing the outputs in the [Results folder](https://github.com/jmroemer/Attrition_in_Mobile_Phone_Panel_Surveys/tree/main/Results)
* Download [Code.zip]( https://github.com/jmroemer/Attrition_in_Mobile_Phone_Panel_Surveys/blob/main/Code.zip) and work with the 01_Attrition_analysis_WB_data folder. 
* Download the raw data of the respective survey (data sources provided in the References section below) and store them in 01_raw. 
* Open 01_Attrition_analysis_WB_data.Rproj an run the respective master file 00_master_[study_id].R. 


### Applying the code to new data
Download [Code.zip]( https://github.com/jmroemer/Attrition_in_Mobile_Phone_Panel_Surveys/blob/main/Code.zip) and work with the folder 02_Attrition_analysis_new_data_example. 

* Adapt 01_load_data_create_metadata_template.R to:
    * Load your data to environment as <data_wide_formatted>. The required format is shown in 02_Attrition_analysis_new_data_example/02_script/02_example_data.csv: One row per respondent and the following columns:
        * Call-status information of all survey rounds 
        * Standardized variables of interest (de_age, de_female, de_urban, de_agricutlure, de_completed_secondary)
    * Create a metadata dataframe <waves> containing the columns: wave_name, date, wave_number, months_from_BL, long_name
    * Adapt the study_id
* Run 00_master_sXXX.R
    
### Applying the code to existing open data    
Proceed as described above. Depending on the format of the open data (e.g. if respondent identifiers are missing, or response status is not clearly indicated) adapting some scripts from the folder [Attrition_analysis/02_scripts](https://github.com/jmroemer/Attrition_in_Mobile_Phone_Panel_Surveys/tree/main/Attrition_analysis/02_script) might save some time. 

### Additional documentation
Tables explaining the individual scripts can be found in [Codebook_Panel_Attrition.xlsx](https://github.com/jmroemer/Attrition_in_Mobile_Phone_Panel_Surveys/blob/main/Codebook_Panel_Attrition.xlsx) in sheets "Scripts_new_data_example", “Scripts_TZA” and “Scripts_SEN”, respectively.<br>
The sheets “TZA_variable_formatting” and “SEN_variable_formatting” illustrate how the standardized analysis variables were obtained from the original open data for the two projects. 

## References
### Data
* Survey – [Listening to Senegal]( https://www.ansd.sn/index.php?option=com_content&view=article&id=344), Year 2014-2017, National Agency for Statistics and Demography (ANSD) of the Republic of Senegal , www.ansd.sn
* Twaweza, [Sauti za Wananchi survey]( https://www.twaweza.org/go/sauti-za-wananchi-english) (Rounds 1-24, October 2012-September 2014), twaweza.org

## Contributions
This project is part of my internship with Innovations for Poverty Action. It evolved in close collaboration of Michael Rosenbaum who provided strong support and guidance. 

## License 
All code in this repository is released under a [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.
