# Attrition in Mobile Phone Panel Surveys

Using open data of the World Bank’s [Listening to Africa](https://www.worldbank.org/en/programs/listening-to-africa) initiative, this project analyzes attrition in two long-term panels that were conducted using computer-assisted telephone interviews (CATI) in sub-Saharan Africa: The [Listening to Senegal](https://www.ansd.sn/index.php?option=com_content&view=article&id=344) survey (L2S) performed between 2014 and 2017 and the [Sauti za Wananchi](https://twaweza.org/amplifying-citizen-voices/sauti-za-wananchi/) survey (SzW) performed in Tanzania from 2012 to 2014.<br>

In both projects a face-to-face baseline interview was conducted, and mobile phones were provided to the participants. 
Over the following two years several rounds of CATI surveys were performed based on the baseline sample. 
This allows us to analyze potential attrition bias by directly comparing selected characteristics between the group of people who responded to a particular survey round and the ones who did not. In the present version the code analyzes the following characteristics: age, gender identity, whether a participant lives in an urban area, has completed secondary education and whether their main occupation is in agriculture. 

## Purpose
The R-code provided in this repository was created to analyze the following aspects of attrition in long-term panels: For each survey round it tests weather the group of respondents to this round is equivalent to the group of non-respondents with respect to a set of selected characteristics.
If further tests for all CATI rounds and all these characteristics individually if the sample mean of the CATI respondents is equivalent to the baseline sample mean.
Graphs showing how response rate, mean of the selected binary variables and age distribution vary over time or survey rounds are created and test-results are saved in csv/html tables. Please refer to the [Methods Appendix](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Methods_Appendix.pdf) for further information.

### Outputs
* A plot of the response rates of the individual survey rounds against approximate time after the face-to-face baseline.
* An attrition table that contains the coefficients of a multivariate linear OLS regression, for each CATI round regressing the dummy response variable (1 = the participant responded, 0 = the participant did not respond) against a set of demographic and occupational variables. Huber-White standard errors (HC1) are shown in parentheses. Wald-test p-values are included as last row, for each round testing the null hypothesis that none of the coefficients is significantly different from zero, that is that respondents do not systematically differ from non-respondents in the respective characteristics. The first column of the table contains the means and standard deviations of the respective characteristics in the baseline sample. 
* A table with results of t-tests each testing for one CATI round if this round’s sample composition is different from the baseline composition in a set of demographic and occupational variables. Means, mean differences and p-values are shown for each CATI round and each variable. 
* Plots of the sample means of the binary variables against survey round and against time after baseline, respectively. 95% confidence intervals based on Huber–White standard errors are included. The baseline values are indicated by horizontal lines as a guide to the eye and significant differences from baseline are indicated with asterisks (* p < 0.10, ** p < 0.05, *** p < 0.01). 
* A graph containing one violin plot per survey round showing the age distribution of this round’s respondents.    

## Usage
The code takes survey data of each survey round in wide format as input: Each row representing a survey participant, and each column corresponding to one variable, including a participant identifier (in case of the L2S data this identifier is created by the script [01b_create_IDs_and_metadata.R](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/02_script/s016SEN/01b_create_IDs_and_metadata.R)). The code merges the datasets of each survey round by the identifier column and analyzes attrition with regard to response rate development and potential introduction of biases. More information about the analysis process and used packages can be found in the [Methods Appendix](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Methods_Appendix.pdf).

This project uses the [renv](https://rstudio.github.io/renv/index.html) package for dependency management.
Opening the RStudio project files will create a project library in the project folder and install renv to this library. Calling renv::restore() will add required packages to this library. 
The same can be achieved by running 00_master_[study_id].R and following the prompts in the console.
    
### Reproducing the outputs in the [Output folder](https://github.com/jmroemer/CATI_Panel_Attrition/tree/main/04_output)
* Download [Code.zip](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Code.zip) and work with the 01_Attrition_analysis_WB_data folder. 
* Download the raw data of the respective survey and store them in 01_raw.<sup>[1](#footnote1)</sup> 
* Open 01_Attrition_analysis_WB_data.Rproj an run the respective master file 00_master_[study_id].R. 


### Applying the code to new data
Download [Code.zip](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Code.zip) and work with the folder 02_Attrition_analysis_new_data_example. 

* Adapt 01_load_data_create_metadata_template.R to:
    * Load your data to environment as "data_wide_formatted". The required format is shown in 02_Attrition_analysis_new_data_example/02_script/02_example_data.csv: One row per respondent and the following columns:
        * Call-status information of all survey rounds 
        * Standardized variables of interest (de_age, de_female, de_urban, de_agricutlure, de_completed_secondary)
    * Create a metadata dataframe "waves" containing the columns: wave_name, date, wave_number, months_from_BL, long_name
    * Adapt the study_id
* Run 00_master_sXXX.R
    
### Applying the code to existing open data    
Proceed as described above. Depending on the format of the open data (e.g. if respondent identifiers are missing, or response status is not clearly indicated) adapting some scripts from the folder [Attrition_analysis/02_scripts](https://github.com/jmroemer/CATI_Panel_Attrition/tree/main/02_script) might save some time. 

### Additional documentation
* Tables explaining the individual scripts can be found in [Codebook_Panel_Attrition.xlsx](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Codebook_Panel_Attrition.xlsx) in sheets "Scripts_new_data_example", “Scripts_TZA” and “Scripts_SEN”, respectively.
The sheets “TZA_variable_formatting” and “SEN_variable_formatting” illustrate how the standardized analysis variables were obtained from the original open data for the two projects. <br>
* A comprehensive description of the analysis, including functions and packages used, is provided in the [Methods Appendix](https://github.com/jmroemer/CATI_Panel_Attrition/blob/main/Methods_Appendix.pdf). 
    
## References
### Data
* Survey – [Listening to Senegal]( https://www.ansd.sn/index.php?option=com_content&view=article&id=344), Year 2014-2017, National Agency for Statistics and Demography (ANSD) of the Republic of Senegal , www.ansd.sn
* Twaweza, [Sauti za Wananchi survey]( https://www.twaweza.org/go/sauti-za-wananchi-english) (Rounds 1-24, October 2012-September 2014), twaweza.org

### Report on analysis and results
The main results of this analysis are presented in the Innovations for Poverty Action Evidence Brief [Attrition in Mobile Phone Panel Surveys](https://www.poverty-action.org/publication/evidence-brief-attrition-mobile-phone-panel-surveys). 
Additional information about the analysis can be found in [Methods_Appendix.pdf](https://github.com/jmroemer/Methods_Appendix.pdf)

## License 
All code in this repository is released under a [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.<br>
<br>
    
    
    
<span style= "font-size:0.9em;"><a name="footnote1"><sup>1</sup></a> As of April 2021 only the data of the [L2S](https://www.ansd.sn/index.php?option=com_content&view=article&id=344) project remain online. </span>