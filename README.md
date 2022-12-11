# Using Machine Learning to Predict Covid-19 Risk in California Communities
### By Lyndsey Umsted

## Project Description

This project is intended to build and deploy a machine learning classification model which categorizes California zip codes into low, moderate, or high risk for Covid-19 outbreaks by the end of the year of 2020. Risk predictions of Covid-19 were made using 21 different predictors including socioeconomic, health, abiotic, and pollution indicators. The intention of this project is to identify the underlying correlations for communities with high rates of Covid-19 cases cumulatively by the end of the year 2020. All models were implemented using a Tidymodels implementation in the R programming language.

This project originates from a personal interest in public health and previous studying done on disease dynamics. The machine learning power of these models have the ability to identify underlying correlations between variables very quickly and build predictions upon observations providing much information about communities most heavily affected by this disease.

## Necessary Installations

This project is run entirely in RStudio so an updated version of R needs to be installed.

## How to Use

All data is stored under the "data" folder where you can find the original data set "pandemic_weekly.csv" with 268 columns and 55,620 rows. The file "tidying_data.r" under the "scripts" folder contains code and instructions explaining how I condensed the original data set into the desired data set called "pandemic_cum" which can be viewed as an .xlsx file under the "data" folder with 26 columns and 1,153 rows.

Under the "models" folder, there are R Markdown files with code chunks and instructions with how to install necessary packages and libraries and also how to build and deploy each of the machine learning models using the tidied data set. Each R Markdown stores results for accuracy, ROC Curves and AUC values which are used for comparison in "comparing_model_performance.Rmd."

Note: some models take longer times to run so be aware of values used when tuning parameters.

## Contributions

The data set used for this research project was provided to me by a UC Santa Barbara faculty member, Dr. Andrew MacDonald, who I had the opportunity to work under this summer during an undergraduate research internship. Dr. MacDonald merged data from the *LA Times* on socioeconomic factors and food access for different Californian zip codes with data reflecting Covid-19 case counts, weather, and pollution levels from the same zip codes during the Covid-19 pandemic in 2020.

The code used for EDA and machine learning modeling was referenced from both my UCSB Machine Learning class (PSTAT 131) taught by Dr. Katie Coburn, as well from a previous machine learning project done on disease dynamics that can be found in my MacDonald-REU-Summer-22 GitHub repository.
