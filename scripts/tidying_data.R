# reading in data
setwd("C:/Users/18586/Desktop/PSTAT 131/PSTAT-131-final-project")

pandemic_weekly_og <- read.csv("data/pandemic_weekly.csv")

# changing naming to shorten
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

barplot(mean_cases_cum$name, mean_cases_cum$week, names.arg = c(7:51),
        ylab = "cumulative cases", xlab = "week",
        main = "Cumulative Number of Covid-19 Cases per Week in 2020",
        col = "pink")

# total cumulative case count by week
total_cases_cum <- pandemic_weekly_og %>%
  group_by(week) %>%
  summarise_at(vars(confirmed_cases), list(name = sum))
total_cases_cum

total_cases_cum$cumulative_cases <- total_cases_cum$name

gg_cum <- ggplot(total_cases_cum, aes(x = week, y = cumulative_cases)) + 
  geom_point() +
  geom_line()

barplot(total_cases_cum$cumulative_cases, total_cases_cum$week, names.arg = c(7:51),
        ylab = "cumulative cases", xlab = "week",
        main = "Cumulative Number of Covid-19 Cases per Week in 2020",
        col = "pink")

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

total_cases$new_cases <- total_cases$name

gg_new <- ggplot(total_cases, aes(x = week, y = new_cases)) + 
  geom_point() +
  geom_line()

bar_new <- barplot(total_cases$new_cases, total_cases$week, names.arg = c(7:51),
        ylab = "new cases", xlab = "week",
        main = "New Number of Covid-19 Cases per Week in 2020",
        col = "pink")

save(gg_cum, file = "visuals/gg_cum.rda")
save(gg_new, file = "visuals/gg_new.rda")
save(total_cases_cum, file = "visuals/total_cases_cum.rda")
save(total_cases, file = "visuals/total_cases.rda")



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

# dplyr::selecting only week 51 to look at cumulative cases over 
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
  dplyr::select("ID_co", "confirmed_cases", "population", "Mean_Tmax", "Urban", "PovertyRate", "MedianFamilyIncome", 
         "Traffic", "Solid.Waste", "Asthma", "Low.Birth.Weight", "Cardiovascular.Disease", 
         "Education", "Linguistic.Isolation", "Unemployment", "under_10_.", "Age11_to_64_.", "over_65_.", 
         "Hispanic_.", "White_.", "African_Am_.", "Asian_Am_.", "Native_Am_.", "Other_ethnicity_.")

save(pandemic_cum_sub, file = "models/pandemic_cum_sub.rda")

library(finalfit)

# missing values plot
getwd()
og_missing_plot <- missing_plot(pandemic_cum_sub)
save(og_missing_plot, file = "visuals/og_missing_plot.rda")

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
  dplyr::select(is.numeric) %>% 
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
  dplyr::select("ID_co", "new_cases", "population", "week", "Mean_Tmax", "Urban", 
         "OHU2010", "PovertyRate", "MedianFamilyIncome", "Ozone", "Diesel.PM", "Drinking.Water", "Pesticides", 
         "Traffic", "Groundwater.Threats", "Haz..Waste", "Solid.Waste", "Asthma", "Low.Birth.Weight", "Cardiovascular.Disease", 
         "Education", "Linguistic.Isolation", "Unemployment", "under_10_.", "Age11_to_64_.", "over_65_.", 
         "Hispanic_.", "White_.", "African_Am_.", "Asian_Am_.", "Native_Am_.", "Other_ethnicity_.")

library(finalfit)

# missing values plot
missing_plot(pandemic_weekly_sub)

# removing any missing values
pandemic_weekly <- pandemic_weekly_sub %>%
  na.omit()

# adjusting some variable types
# pandemic_weekly$population <- as.numeric(pandemic_weekly$population)

# verifying variable types
str(pandemic_weekly)

# correlation plot between all variables besides
# ID_co
#install.packages("corrplot")
library(corrplot)
pandemic_weekly %>% 
  dplyr::select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')

# summary and histogram of cumulative cases
summary(pandemic_weekly$new_cases)
hist(pandemic_weekly$new_cases, breaks = 200)




pandemic_cum %>% 
  dplyr::select("confirmed_cases", "Education") %>%
  mutate(Education = cut(Education, breaks = 
                            seq(min(Education), max(Education), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Education) %>%
  na.omit(Education) %>%
  ggplot(aes(Education, confirmed_cases), confirmed_cases) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per 3 % Pop Char Intervals"
  )

pandemic_cum %>% 
  dplyr::select("confirmed_cases", "PovertyRate") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                            seq(min(PovertyRate), max(PovertyRate), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  ggplot(aes(PovertyRate, confirmed_cases), confirmed_cases) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per 3 % Pop Char Intervals"
  )


# How to remove unnesseary messages in knitted file:
# at top of code chunk say '''{r. message = FALSE}

# How to save and load models:
# 1. make r markdown file where code is meant to be loaded
# 2. make code chunks of all code needed that takes long to 
# load in r script and run once
# 3. save(object_name, file = "object_name_results.rda")
# 4. In R markdown: put '''{r, eval = FALSE} and then
# load(file = "object_name_results.rda")

# or write_rds(object, "file") and then
# read_rds("file")



# Creating a proportions variable for possible categorical
# problem

#cumulative

pandemic_cum$prop <- 
  pandemic_cum$confirmed_cases/pandemic_cum$population

# getting rid of proportions greater than 1 because that
# has to be a mistake and shouldn't have any 0s but getting
# rid of them anyways
pandemic_cum <- pandemic_cum %>%
  filter(prop < 1) %>%
  filter(prop > 0)

hist(pandemic_cum$prop, breaks = 50, main = "Distribution of Proportion of Covid-19 Cases in a Population",
     xlab = "Proportion of Covid-19 Cases in a Population", col = "light blue")


# based on histogram, I think the best split for high is 7%, and then
# the next split between low and moderate I would choose to be the 
# median

summary(pandemic_cum$prop) # median proportion of city with covid 
# covid by the end of the year is 0.0311 or about 3 %
# mean proportion of covid by the end of the year is 0.0392
# or about 4 % (pulled upward by high outliers)


# splits at 0.02 and 0.06

boxplot(pandemic_cum$prop, horizontal = TRUE, main = "Distribution of Proportion of Covid-19 Cases in a Population",
        xlab = "Proportion of Covid-19 Cases in a Population", col = "light blue")



# creating low, moderate, and high categories based on
# log-transformed data cut 0.02 and 0.05

pandemic_cum$risk <- 
  cut(pandemic_cum$prop, breaks = 
        c(0, 0.02 , 0.05, 1), 
      labels = c("low", "moderate", "high")) # 0.03 and 0.07

# checking
round(prop.table(table(pandemic_cum$risk)), 2)


# weekly
pandemic_weekly$prop <- 
  pandemic_weekly$new_cases/pandemic_weekly$population

# getting rid of proportions greater than 1 because that
# has to be a mistake and shouldn't have any 0s but getting
# rid of them anyways
pandemic_weekly <- pandemic_weekly %>%
  filter(prop < 1) %>%
  filter(prop > 0)

boxplot(pandemic_weekly$prop, horizontal = TRUE)

hist(pandemic_weekly$prop[pandemic_weekly$prop < 0.002], breaks = 100)
summary(pandemic_weekly$prop) # median proportion of city
# with covid each week is 0.0001022 or .01%
# mean proportion of covid each week is 0.0002432
# or about .02 % (pulled upward by high outliers)

# based on the histogram of prop below 0.002 or .2%,
# I would mark a distinct split between low, moderate, 
# and high around 0.00015 and 0.0003


summary(pandemic_cum$prop)

# log transformation
pandemic_weekly$proplog <-log(pandemic_weekly$prop)

hist(pandemic_weekly$proplog, breaks = 50)
boxplot(pandemic_weekly$proplog, horizontal = TRUE)
summary(pandemic_weekly$proplog)


# creating low, moderate, and high categories based on
# log-transformed data cut by the 1st quartile, IQR,
# and 4th quartile

pandemic_weekly$risk <- 
  cut(pandemic_weekly$proplog, breaks = 
        c(-14, log(0.00015) , log(0.0003), 0), 
      labels = c("low", "moderate", "high")) # 0.00015 and 0.0003

# checking
round(prop.table(table(pandemic_weekly$risk)), 2)


# need smaller data set size
set.seed(2002)
pandemic_weekly <- 
  pandemic_weekly[sample(nrow(pandemic_weekly), 5000), ]


# final data sets:
save(pandemic_cum, file = "models/pandemic_cum.rda")
save(pandemic_weekly, file = "models/pandemic_weekly.rda")

#install.packages("writexl")
library(writexl)
write_xlsx(pandemic_cum, "data/pandemic_cum.xlsx")



pandemic_cum %>% 
  dplyr::select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')


pandemic_weekly %>% 
  dplyr::select(is.numeric) %>% 
  cor() %>% 
  corrplot(diag = FALSE, 
           method = 'square')

