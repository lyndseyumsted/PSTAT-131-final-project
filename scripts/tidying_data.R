# reading in data
pandemic_weekly_og <- read.csv("data/pandemic_weekly.csv")

# changing naming to shorten
pandemic_weekly_og$retail_change <- pandemic_weekly_og$retail_and_recreation_percent_change_from_baseline
pandemic_weekly_og$grocery_change <- pandemic_weekly_og$grocery_and_pharmacy_percent_change_from_baseline
pandemic_weekly_og$transit_change <- pandemic_weekly_og$transit_stations_percent_change_from_baseline
pandemic_weekly_og$confirmed_cases <- pandemic_weekly_og$confirmed_cases_fill
pandemic_weekly_og$new_cases <- pandemic_weekly_og$new_cases_fill


library(dplyr)
library(ggplot2)

# average cumulative case count by week
mean_cases_cum <- pandemic_weekly_og %>%
  group_by(week) %>%
  summarise_at(vars(confirmed_cases), list(name = mean))
mean_cases_cum

ggplot(mean_cases_cum, aes(x = week, y = name)) + 
  geom_point() +
  geom_line()

barplot(mean_cases_cum$name, mean_cases_cum$week, names.arg = c(7:51))

# total cumulative case count by week
total_cases_cum <- pandemic_weekly_og %>%
  group_by(week) %>%
  summarise_at(vars(confirmed_cases), list(name = sum))
total_cases_cum

ggplot(total_cases_cum, aes(x = week, y = name)) + 
  geom_point() +
  geom_line()

barplot(total_cases_cum$name, total_cases_cum$week, names.arg = c(7:51))

# average new case count by week
mean_cases <- pandemic_weekly_og %>%
  group_by(week) %>%
  summarise_at(vars(new_cases), list(name = mean))
mean_cases

ggplot(mean_cases, aes(x = week, y = name)) + 
  geom_point() +
  geom_line()

barplot(mean_cases$name, mean_cases$week, names.arg = c(7:51))


# total new case count by week
total_cases <- pandemic_weekly_og %>%
  group_by(week) %>%
  summarise_at(vars(new_cases), list(name = sum))
total_cases

ggplot(total_cases, aes(x = week, y = name)) + 
  geom_point() +
  geom_line()

barplot(total_cases$name, total_cases$week, names.arg = c(7:51))



# HERE I WILL BE CREATING TWO DATA SETS

# 1. THE FIRST DATA SET WILL BE NAMED pandemic_cum
# AND WILL BE USED TO PREDICT CUMULATIVE 
# CASE NUMBERS AT THE END OF THE YEAR THUS ONLY
# WEEK 51 WILL BE OBSERVED AND WEEK WILL NOT BE 
# USED AS A PREDICTOR

# 2. THE SECOND DATA SET WILL BE NAMED pandemic_weekly
# AND WILL BE USED TO PREDICT WEEKLY CASE COUNTS OVER
# THE YEAR 2020 AND WEEK WILL BE USED AS A PREDICTOR




# 1. pandemic_cum

# selecting only week 51 to look at cumulative cases over 
# the entire year of 2020

# distribution of case counts at 51 weeks excluding any
# case counts of 0
pandemic_weekly_og %>%
  filter(week == "51") %>%
  filter(confirmed_cases > 0) %>%
  ggplot(aes(x = confirmed_cases)) +
  geom_histogram(bins = 100) +
  theme_bw()

# creating data set with only cumulative counts over 
# the year of 2020
pandemic_new_cum <- pandemic_weekly_og %>%
  filter(week == "51") %>%
  filter(confirmed_cases > 0)

# subset with outcome and predictor variables
# that I want
library(dplyr)
pandemic_cum_sub <- pandemic_new_cum %>%
  select("confirmed_cases", "population", "Mean_Tmax", "Urban", "Pop2010", 
         "OHU2010", "LowIncomeTracts", "PovertyRate", "MedianFamilyIncome", "Ozone", "Diesel.PM", "Drinking.Water", "Pesticides", 
         "Traffic", "Groundwater.Threats", "Haz..Waste", "Solid.Waste", "Asthma", "Low.Birth.Weight", "Cardiovascular.Disease", 
         "Education", "Linguistic.Isolation", "Poverty", "Unemployment", "Pop..Char.", "under_10_.", "Age11_to_64_.", "over_65_.", 
         "Hispanic_.", "White_.", "African_Am_.", "Asian_Am_.", "Native_Am_.", "Other_ethnicity_.", "retail_change", "grocery_change", 
         "transit_change")

library(finalfit)

# missing values plot
missing_plot(pandemic_cum_sub)

# removing any missing values
pandemic_cum <- pandemic_cum_sub %>%
  na.omit()

missing_plot(pandemic_cum)

# adjusting some variable types
# pandemic_cum$population <- as.numeric(pandemic_cum$population)

# verifying variable types
str(pandemic_cum)

# correlation plot between all variables besides
# ID_co
#install.packages("corrplot")
library(corrplot)
pandemic_cum %>% 
  select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')

# strongest pos corrs for confirmed cases:
# hispanic, urban, under_10, pop..char, education 

# strongest neg corrs for confirmed cases:
# white 

# positive correlation between Urban and (Mean Temp Max,
# Diesel.PM, Traffic, and Asian_Am_.)
# positive correlation between OHU2010 and Pop2010
# positive correlation between population and 
# confirmed_cases
# negative correlation between median fam income and (low
# income tracts, poverty rate, solid waste, asthma, cardio
# disease, education, lang isolation, poverty, unemployment,
# pop..char)
# pos corr between ozone and drinking water, retail change,
# grocery change, and transit change)
# pos corr between traffic and (mean_tmax, urban, diesel)
# positive corr between diesel.pm and (urban, traffic, 
# asian_am_.)




# summary and histogram of cumulative cases
summary(pandemic_cum$confirmed_cases)
hist(pandemic_cum$confirmed_cases, breaks = 100)








# 2. pandemic_weekly

# keeping all weeks to look at new weekly cases

# excluding any new case counts of 0
pandemic_weekly_og %>%
  filter(new_cases > 0) %>%
  ggplot(aes(x = new_cases)) +
  geom_histogram(bins = 100) +
  theme_bw()

# creating data set with weekly counts over 
# the year of 2020 excluding new case counts of 0
pandemic_new_weekly <- pandemic_weekly_og %>%
  filter(new_cases > 0)

# subset with outcome and predictor variables
# that I want
library(dplyr)
pandemic_weekly_sub <- pandemic_new_weekly %>%
  select("new_cases", "population", "week", "Mean_Tmax", "Urban", "Pop2010", 
         "OHU2010", "LowIncomeTracts", "PovertyRate", "MedianFamilyIncome", "Ozone", "Diesel.PM", "Drinking.Water", "Pesticides", 
         "Traffic", "Groundwater.Threats", "Haz..Waste", "Solid.Waste", "Asthma", "Low.Birth.Weight", "Cardiovascular.Disease", 
         "Education", "Linguistic.Isolation", "Poverty", "Unemployment", "Pop..Char.", "under_10_.", "Age11_to_64_.", "over_65_.", 
         "Hispanic_.", "White_.", "African_Am_.", "Asian_Am_.", "Native_Am_.", "Other_ethnicity_.", "retail_change", "grocery_change", 
         "transit_change")

library(finalfit)

# missing values plot
missing_plot(pandemic_weekly_sub)

# removing any missing values
pandemic_weekly <- pandemic_weekly_sub %>%
  na.omit()

missing_plot(pandemic_weekly)

# adjusting some variable types
# pandemic_weekly$population <- as.numeric(pandemic_weekly$population)

# verifying variable types
str(pandemic_weekly)

# correlation plot between all variables besides
# ID_co
#install.packages("corrplot")
library(corrplot)
pandemic_weekly %>% 
  select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')

# summary and histogram of cumulative cases
summary(pandemic_weekly$new_cases)
hist(pandemic_weekly$new_cases, breaks = 200)


# final data sets:
pandemic_cum
pandemic_weekly



