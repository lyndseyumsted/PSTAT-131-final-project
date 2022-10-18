pandemic_weekly_og <- read.csv("data/pandemic_weekly.csv")

pandemic_weekly_og$retail_change <- pandemic_weekly_og$retail_and_recreation_percent_change_from_baseline
pandemic_weekly_og$grocery_change <- pandemic_weekly_og$grocery_and_pharmacy_percent_change_from_baseline
pandemic_weekly_og$transit_change <- pandemic_weekly_og$transit_stations_percent_change_from_baseline
pandemic_weekly_og$confirmed_cases <- pandemic_weekly_og$confirmed_cases_fill
pandemic_weekly_og$new_cases <- pandemic_weekly_og$new_cases_fill

library(dplyr)
pandemic_weekly_sub <- pandemic_weekly_og %>%
  select("ID_co", "week", "confirmed_cases", "new_cases", "population", "Mean_Tmax", "Urban", "Pop2010", 
         "OHU2010", "LowIncomeTracts", "PovertyRate", "MedianFamilyIncome", "Ozone", "Diesel.PM", "Drinking.Water", "Pesticides", 
         "Traffic", "Groundwater.Threats", "Haz..Waste", "Solid.Waste", "Asthma", "Low.Birth.Weight", "Cardiovascular.Disease", 
         "Education", "Linguistic.Isolation", "Poverty", "Unemployment", "Pop..Char.", "under_10_.", "Age11_to_64_.", "over_65_.", 
         "Hispanic_.", "White_.", "African_Am_.", "Asian_Am_.", "Native_Am_.", "Other_ethnicity_.", "retail_change", "grocery_change", 
         "transit_change")

library(finalfit)
missing_plot(pandemic_weekly_sub)

pandemic_weekly <- pandemic_weekly_sub %>%
  na.omit()

missing_plot(pandemic_weekly)

pandemic_weekly$ID_co <- as.factor(pandemic_weekly$ID_co)
pandemic_weekly$week <- as.factor(pandemic_weekly$week)
pandemic_weekly$population <- as.numeric(pandemic_weekly$population)

str(pandemic_weekly)

{plot.new(); dev.off()}

a <- summary(pandemic_weekly)
View(a)


#install.packages("corrplot")
library(corrplot)
pandemic_cor <- cor(pandemic_weekly[,-c(1,2)])
corrplot(pandemic_cor, method = 'square')
typeof(pandemic_weekly)
