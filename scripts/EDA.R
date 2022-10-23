pandemic_weekly_og <- read.csv("data/pandemic_weekly.csv")

pandemic_weekly_og$retail_change <- pandemic_weekly_og$retail_and_recreation_percent_change_from_baseline
pandemic_weekly_og$grocery_change <- pandemic_weekly_og$grocery_and_pharmacy_percent_change_from_baseline
pandemic_weekly_og$transit_change <- pandemic_weekly_og$transit_stations_percent_change_from_baseline
pandemic_weekly_og$confirmed_cases <- pandemic_weekly_og$confirmed_cases_fill
pandemic_weekly_og$new_cases <- pandemic_weekly_og$new_cases_fill

pandemic_weekly_og %>%
  group_by(ID_co) %>%
  filter(week == "51") %>%
  ggplot(aes(x = confirmed_cases)) +
  geom_histogram(bins = 100) +
  theme_bw()


pandemic_new <- pandemic_weekly_og %>%
  group_by(ID_co) %>%
  filter(week == "51")

library(dplyr)
pandemic_weekly_sub <- pandemic_new %>%
  select("ID_co", "confirmed_cases", "new_cases", "population", "Mean_Tmax", "Urban", "Pop2010", 
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
pandemic_weekly$population <- as.numeric(pandemic_weekly$population)

str(pandemic_weekly)

{plot.new(); dev.off()}

a <- summary(pandemic_weekly)
View(a)


#install.packages("corrplot")
library(corrplot)
pandemic_weekly[,-1] %>% 
  select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')


summary(pandemic_weekly$confirmed_cases)
hist(pandemic_weekly$confirmed_cases, breaks = 100)

summary(pandemic_weekly$new_cases)
hist(pandemic_weekly$new_cases, breaks = 100)

pandemic_data <- subset(pandemic_weekly, 
                        confirmed_cases > 20)

hist(pandemic_data$confirmed_cases, include.lowest = TRUE)

