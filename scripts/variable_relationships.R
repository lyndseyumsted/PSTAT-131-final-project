
# RECALL:
# strongest pos corrs for confirmed cases:
# hispanic, urban, under_10, pop..char, education 

# strongest neg corrs for confirmed cases:
# white 


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ HISPANIC

# distribution of confirmed cases for different Hispanic Population 
# percentiles:

pandemic_cum %>% 
  dplyr::select("confirmed_cases", "Hispanic_.") %>%
  mutate(Hispanic_. = cut(Hispanic_., breaks = 
                            seq(min(Hispanic_.), max(Hispanic_.), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Hispanic_.) %>%
  na.omit(Hispanic_.) %>%
  ggplot(aes(reorder(Hispanic_., confirmed_cases), confirmed_cases)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per 3 % Hispanic Makeup Intervals"
  )


# mean cumulative cases and hispanic:

small_df_hispanic <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Hispanic_.") %>%
  mutate(Hispanic_. = cut(Hispanic_., breaks = 
                            seq(min(Hispanic_.), max(Hispanic_.), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Hispanic_.) %>%
  na.omit(Hispanic_.) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_hispanic$Hispanic_., 
     small_df_hispanic$confirmed_cases)

ggplot(data = small_df_hispanic, mapping = aes(Hispanic_., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_hispanic$interval <- seq(1, nrow(small_df_hispanic))

fit_hispanic <- glm(small_df_hispanic$confirmed_cases ~ small_df_hispanic$interval)
summary(fit_hispanic)

plot(small_df_hispanic$interval, small_df_hispanic$confirmed_cases)



# total cumulative cases and hispanic:

small_df_hispanic <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Hispanic_.") %>%
  mutate(Hispanic_. = cut(Hispanic_., breaks = 
                            seq(min(Hispanic_.), max(Hispanic_.), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Hispanic_.) %>%
  na.omit(Hispanic_.) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_hispanic$Hispanic_., 
     small_df_hispanic$confirmed_cases)

ggplot(data = small_df_hispanic, mapping = aes(Hispanic_., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_hispanic$interval <- seq(1, nrow(small_df_hispanic))

fit_hispanic <- glm(small_df_hispanic$confirmed_cases ~ small_df_hispanic$interval)
summary(fit_hispanic)

plot(small_df_hispanic$interval, small_df_hispanic$confirmed_cases)


# mean new cases and hispanic:

small_df_hispanic <- pandemic_weekly %>%
  dplyr::select("new_cases", "Hispanic_.") %>%
  mutate(Hispanic_. = cut(Hispanic_., breaks = 
                            seq(min(Hispanic_.), max(Hispanic_.), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Hispanic_.) %>%
  na.omit(Hispanic_.) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_hispanic$Hispanic_., 
     small_df_hispanic$new_cases)

ggplot(data = small_df_hispanic, mapping = aes(Hispanic_., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_hispanic$interval <- seq(1, nrow(small_df_hispanic))

fit_hispanic <- glm(small_df_hispanic$new_cases ~ small_df_hispanic$interval)
summary(fit_hispanic)

plot(small_df_hispanic$interval, small_df_hispanic$new_cases)



# total new cases and hispanic:

small_df_hispanic <- pandemic_weekly %>%
  dplyr::select("new_cases", "Hispanic_.") %>%
  mutate(Hispanic_. = cut(Hispanic_., breaks = 
                            seq(min(Hispanic_.), max(Hispanic_.), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Hispanic_.) %>%
  na.omit(Hispanic_.) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_hispanic$Hispanic_., 
     small_df_hispanic$new_cases)

ggplot(data = small_df_hispanic, mapping = aes(Hispanic_., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_hispanic$interval <- seq(1, nrow(small_df_hispanic))

fit_hispanic <- glm(small_df_hispanic$new_cases ~ small_df_hispanic$interval)
summary(fit_hispanic)

plot(small_df_hispanic$interval, small_df_hispanic$new_cases)
 # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ URBAN

# distribution of confirmed cases per 5% intervals

pandemic_cum %>% 
  dplyr::select("confirmed_cases", "Urban") %>%
  mutate(Urban = cut(Urban, breaks = 
                            seq(min(Urban), max(Urban), by = 0.05), 
                          include.lowest = TRUE)) %>%
  group_by(Urban) %>%
  na.omit(Urban) %>%
  ggplot(aes(reorder(Urban, confirmed_cases), confirmed_cases)) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per Intervals"
  )

# mean cumulative cases and Urban:

small_df_urban <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Urban") %>%
  mutate(Urban = cut(Urban, breaks = 
                            seq(min(Urban), max(Urban), by = 0.001), 
                          include.lowest = TRUE)) %>%
  group_by(Urban) %>%
  na.omit(Urban) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_urban$Urban, 
     small_df_urban$confirmed_cases)

ggplot(data = small_df_urban, mapping = aes(Urban, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_urban$interval <- seq(1, nrow(small_df_urban))

fit_urban <- glm(small_df_urban$confirmed_cases ~ small_df_urban$interval)
summary(fit_urban)

plot(small_df_urban$interval, small_df_urban$confirmed_cases)



# total cumulative cases and urban:

small_df_urban <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Urban") %>%
  mutate(Urban = cut(Urban, breaks = 
                            seq(min(Urban), max(Urban), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Urban) %>%
  na.omit(Urban) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_urban$Urban, 
     small_df_urban$confirmed_cases)


ggplot(data = small_df_urban, mapping = aes(Urban, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_urban$interval <- seq(1, nrow(small_df_urban))

fit_urban <- glm(small_df_urban$confirmed_cases ~ small_df_urban$interval)
summary(fit_urban)

plot(small_df_urban$interval, small_df_urban$confirmed_cases)



# mean new cases and urban:

small_df_urban <- pandemic_weekly %>%
  dplyr::select("new_cases", "Urban") %>%
  mutate(Urban = cut(Urban, breaks = 
                            seq(min(Urban), max(Urban), by = 0.0001), 
                          include.lowest = TRUE)) %>%
  group_by(Urban) %>%
  na.omit(Urban) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_urban$Urban, 
     small_df_urban$new_cases)

ggplot(data = small_df_urban, mapping = aes(Urban, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_urban$interval <- seq(1, nrow(small_df_urban))

fit_urban <- glm(small_df_urban$new_cases ~ small_df_urban$interval)
summary(fit_urban)

plot(small_df_urban$interval, small_df_urban$new_cases)



# total new cases and urban:

small_df_urban <- pandemic_weekly %>%
  dplyr::select("new_cases", "Urban") %>%
  mutate(Urban = cut(Urban, breaks = 
                            seq(min(Urban), max(Urban), by = 3), 
                          include.lowest = TRUE)) %>%
  group_by(Urban) %>%
  na.omit(Urban) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_urban$Urban, 
     small_df_urban$new_cases)

ggplot(data = small_df_urban, mapping = aes(Urban, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_urban$interval <- seq(1, nrow(small_df_urban))

fit_urban <- glm(small_df_urban$new_cases ~ small_df_urban$interval)
summary(fit_urban)

plot(small_df_urban$interval, small_df_urban$new_cases)

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ under_10



# distribution of confirmed cases for each of the three age groups

dist_10 <- pandemic_cum %>% 
  dplyr::select("confirmed_cases", "under_10_.") %>%
  mutate(under_10_. = cut(under_10_., breaks = 
                       seq(min(under_10_.), max(under_10_.), by = 0.5), 
                     include.lowest = TRUE)) %>%
  group_by(under_10_.) %>%
  na.omit(under_10_.) %>%
  ggplot(aes(under_10_., confirmed_cases), confirmed_cases) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per Intervals"
  )

dist_35 <- pandemic_cum %>% 
  dplyr::select("confirmed_cases", "Age11_to_64_.") %>%
  mutate(Age11_to_64_. = cut(Age11_to_64_., breaks = 
                            seq(min(Age11_to_64_.), max(Age11_to_64_.), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Age11_to_64_.) %>%
  na.omit(Age11_to_64_.) %>%
  ggplot(aes(Age11_to_64_., confirmed_cases), confirmed_cases) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per Intervals"
  )

dist_65 <- pandemic_cum %>% 
  dplyr::select("confirmed_cases", "over_65_.") %>%
  mutate(over_65_. = cut(over_65_., breaks = 
                               seq(min(over_65_.), max(over_65_.), by = 1), 
                             include.lowest = TRUE)) %>%
  group_by(over_65_.) %>%
  na.omit(over_65_.) %>%
  ggplot(aes(over_65_., confirmed_cases), confirmed_cases) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Confirmed Cases per Intervals"
  )




# mean cumulative cases and under_10:

small_df_under_10 <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "under_10_.") %>%
  mutate(under_10_. = cut(under_10_., breaks = 
                            seq(min(under_10_.), max(under_10_.), by = 0.35), 
                          include.lowest = TRUE)) %>%
  group_by(under_10_.) %>%
  na.omit(under_10_.) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_under_10$under_10_., 
     small_df_under_10$confirmed_cases)

ggplot(data = small_df_under_10, mapping = aes(under_10_., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_under_10$interval <- seq(1, nrow(small_df_under_10))

fit_under_10 <- glm(small_df_under_10$confirmed_cases ~ small_df_under_10$interval)
summary(fit_under_10)

plot(small_df_under_10$interval, small_df_under_10$confirmed_cases)



# total cumulative cases and under_10:

small_df_under_10 <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "under_10_.") %>%
  mutate(under_10_. = cut(under_10_., breaks = 
                            seq(min(under_10_.), max(under_10_.), by = 0.35), 
                          include.lowest = TRUE)) %>%
  group_by(under_10_.) %>%
  na.omit(under_10_.) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_under_10$under_10_., 
     small_df_under_10$confirmed_cases)

ggplot(data = small_df_under_10, mapping = aes(under_10_., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_under_10$interval <- seq(1, nrow(small_df_under_10))

fit_under_10 <- glm(small_df_under_10$confirmed_cases ~ small_df_under_10$interval)
summary(fit_under_10)

plot(small_df_under_10$interval, small_df_under_10$confirmed_cases)



# mean new cases and under_10:

small_df_under_10 <- pandemic_weekly %>%
  dplyr::select("new_cases", "under_10_.") %>%
  mutate(under_10_. = cut(under_10_., breaks = 
                            seq(min(under_10_.), max(under_10_.), by = 0.25), 
                          include.lowest = TRUE)) %>%
  group_by(under_10_.) %>%
  na.omit(under_10_.) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_under_10$under_10_., 
     small_df_under_10$new_cases)

ggplot(data = small_df_under_10, mapping = aes(under_10_., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_under_10$interval <- seq(1, nrow(small_df_under_10))

fit_under_10 <- glm(small_df_under_10$new_cases ~ small_df_under_10$interval)
summary(fit_under_10)

plot(small_df_under_10$interval, small_df_under_10$new_cases)



# total new cases and under_10:

small_df_under_10 <- pandemic_weekly %>%
  dplyr::select("new_cases", "under_10_.") %>%
  mutate(under_10_. = cut(under_10_., breaks = 
                            seq(min(under_10_.), max(under_10_.), by = 0.25), 
                          include.lowest = TRUE)) %>%
  group_by(under_10_.) %>%
  na.omit(under_10_.) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_under_10$under_10_., 
     small_df_under_10$new_cases)

ggplot(data = small_df_under_10, mapping = aes(under_10_., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_under_10$interval <- seq(1, nrow(small_df_under_10))

fit_under_10 <- glm(small_df_under_10$new_cases ~ small_df_under_10$interval)
summary(fit_under_10)

plot(small_df_under_10$interval, small_df_under_10$new_cases)


# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ Pop..Char.
# mean cumulative cases and Pop..Char:

small_df_Pop..Char <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Pop..Char.") %>%
  mutate(Pop..Char. = cut(Pop..Char., breaks = 
                            seq(min(Pop..Char.), max(Pop..Char.), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Pop..Char.) %>%
  na.omit(Pop..Char.) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_Pop..Char$Pop..Char., 
     small_df_Pop..Char$confirmed_cases)

ggplot(data = small_df_Pop..Char, mapping = aes(Pop..Char., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_Pop..Char$interval <- seq(1, nrow(small_df_Pop..Char))

fit_Pop..Char <- glm(small_df_Pop..Char$confirmed_cases ~ small_df_Pop..Char$interval)
summary(fit_Pop..Char)

plot(small_df_Pop..Char$interval, small_df_Pop..Char$confirmed_cases)



# total cumulative cases and Pop..Char:

small_df_Pop..Char <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Pop..Char.") %>%
  mutate(Pop..Char. = cut(Pop..Char., breaks = 
                            seq(min(Pop..Char.), max(Pop..Char.), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Pop..Char.) %>%
  na.omit(Pop..Char.) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_Pop..Char$Pop..Char., 
     small_df_Pop..Char$confirmed_cases)

ggplot(data = small_df_Pop..Char, mapping = aes(Pop..Char., 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_Pop..Char$interval <- seq(1, nrow(small_df_Pop..Char))

fit_Pop..Char <- glm(small_df_Pop..Char$confirmed_cases ~ small_df_Pop..Char$interval)
summary(fit_Pop..Char)

plot(small_df_Pop..Char$interval, small_df_Pop..Char$confirmed_cases)



# mean new cases and Pop..Char:

small_df_Pop..Char <- pandemic_weekly %>%
  dplyr::select("new_cases", "Pop..Char.") %>%
  mutate(Pop..Char. = cut(Pop..Char., breaks = 
                            seq(min(Pop..Char.), max(Pop..Char.), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Pop..Char.) %>%
  na.omit(Pop..Char.) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_Pop..Char$Pop..Char., 
     small_df_Pop..Char$new_cases)

ggplot(data = small_df_Pop..Char, mapping = aes(Pop..Char., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_Pop..Char$interval <- seq(1, nrow(small_df_Pop..Char))

fit_Pop..Char <- glm(small_df_Pop..Char$new_cases ~ small_df_Pop..Char$interval)
summary(fit_Pop..Char)

plot(small_df_Pop..Char$interval, small_df_Pop..Char$new_cases)



# total new cases and Pop..Char:

small_df_Pop..Char <- pandemic_weekly %>%
  dplyr::select("new_cases", "Pop..Char.") %>%
  mutate(Pop..Char. = cut(Pop..Char., breaks = 
                            seq(min(Pop..Char.), max(Pop..Char.), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Pop..Char.) %>%
  na.omit(Pop..Char.) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_Pop..Char$Pop..Char., 
     small_df_Pop..Char$new_cases)

ggplot(data = small_df_Pop..Char, mapping = aes(Pop..Char., 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_Pop..Char$interval <- seq(1, nrow(small_df_Pop..Char))

fit_Pop..Char <- glm(small_df_Pop..Char$new_cases ~ small_df_Pop..Char$interval)
summary(fit_Pop..Char)

plot(small_df_Pop..Char$interval, small_df_Pop..Char$new_cases)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ Education

# mean cumulative cases and education:

small_df_education <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Education") %>%
  mutate(Education = cut(Education, breaks = 
                            seq(min(Education), max(Education), by = 0.5), 
                          include.lowest = TRUE)) %>%
  group_by(Education) %>%
  na.omit(Education) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_education$Education, 
     small_df_education$confirmed_cases)

ggplot(data = small_df_education, mapping = aes(Education, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_education$interval <- seq(1, nrow(small_df_education))

fit_education <- glm(small_df_education$confirmed_cases ~ small_df_education$interval)
summary(fit_education)

plot(small_df_education$interval, small_df_education$confirmed_cases)



# total cumulative cases and education:

small_df_education <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "Education") %>%
  mutate(Education = cut(Education, breaks = 
                            seq(min(Education), max(Education), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Education) %>%
  na.omit(Education) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_education$Education, 
     small_df_education$confirmed_cases)

ggplot(data = small_df_education, mapping = aes(Education, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_education$interval <- seq(1, nrow(small_df_education))

fit_education <- glm(small_df_education$confirmed_cases ~ small_df_education$interval)
summary(fit_education)

plot(small_df_education$interval, small_df_education$confirmed_cases)



# mean new cases and education:

small_df_education <- pandemic_weekly %>%
  dplyr::select("new_cases", "Education") %>%
  mutate(Education = cut(Education, breaks = 
                            seq(min(Education), max(Education), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Education) %>%
  na.omit(Education) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_education$Education, 
     small_df_education$new_cases)

ggplot(data = small_df_education, mapping = aes(Education, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_education$interval <- seq(1, nrow(small_df_education))

fit_education <- glm(small_df_education$new_cases ~ small_df_education$interval)
summary(fit_education)

plot(small_df_education$interval, small_df_education$new_cases)



# total new cases and education:

small_df_education <- pandemic_weekly %>%
  dplyr::select("new_cases", "Education") %>%
  mutate(Education = cut(Education, breaks = 
                            seq(min(Education), max(Education), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(Education) %>%
  na.omit(Education) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_education$Education, 
     small_df_education$new_cases)

ggplot(data = small_df_education, mapping = aes(Education, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_education$interval <- seq(1, nrow(small_df_education))

fit_education <- glm(small_df_education$new_cases ~ small_df_education$interval)
summary(fit_education)

plot(small_df_education$interval, small_df_education$new_cases)
 # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ Population

# mean cumulative cases and population:

small_df_population <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "population") %>%
  mutate(population = cut(population, breaks = 
                            seq(min(population), max(population), by = 500), 
                          include.lowest = TRUE)) %>%
  group_by(population) %>%
  na.omit(population) %>%
  summarise(confirmed_cases = mean(confirmed_cases))



plot(small_df_population$population, 
     small_df_population$confirmed_cases)

ggplot(data = small_df_population, mapping = aes(population, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_population$interval <- seq(1, nrow(small_df_population))

fit_population <- glm(small_df_population$confirmed_cases ~ small_df_population$interval)
summary(fit_population)

plot(small_df_population$interval, small_df_population$confirmed_cases)



# total cumulative cases and population:

small_df_population <- pandemic_cum %>%
  dplyr::select("confirmed_cases", "population") %>%
  mutate(population = cut(population, breaks = 
                            seq(min(population), max(population), by = ZZ00), 
                          include.lowest = TRUE)) %>%
  group_by(population) %>%
  na.omit(population) %>%
  summarise(confirmed_cases = sum(confirmed_cases))



plot(small_df_population$population, 
     small_df_population$confirmed_cases)

ggplot(data = small_df_population, mapping = aes(population, 
                                               confirmed_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_population$interval <- seq(1, nrow(small_df_population))

fit_population <- glm(small_df_population$confirmed_cases ~ small_df_population$interval)
summary(fit_population)

plot(small_df_population$interval, small_df_population$confirmed_cases)



# mean new cases and population:

small_df_population <- pandemic_weekly %>%
  dplyr::select("new_cases", "population") %>%
  mutate(population = cut(population, breaks = 
                            seq(min(population), max(population), by = 500), 
                          include.lowest = TRUE)) %>%
  group_by(population) %>%
  na.omit(population) %>%
  summarise(new_cases = mean(new_cases))



plot(small_df_population$population, 
     small_df_population$new_cases)

ggplot(data = small_df_population, mapping = aes(population, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_population$interval <- seq(1, nrow(small_df_population))

fit_population <- glm(small_df_population$new_cases ~ small_df_population$interval)
summary(fit_population)

plot(small_df_population$interval, small_df_population$new_cases)



# total new cases and population:

small_df_population <- pandemic_weekly %>%
  dplyr::select("new_cases", "population") %>%
  mutate(population = cut(population, breaks = 
                            seq(min(population), max(population), by = 500), 
                          include.lowest = TRUE)) %>%
  group_by(population) %>%
  na.omit(population) %>%
  summarise(new_cases = sum(new_cases))



plot(small_df_population$population, 
     small_df_population$new_cases)

ggplot(data = small_df_population, mapping = aes(population, 
                                               new_cases)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

small_df_population$interval <- seq(1, nrow(small_df_population))

fit_population <- glm(small_df_population$new_cases ~ small_df_population$interval)
summary(fit_population)

plot(small_df_population$interval, small_df_population$new_cases)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# distribution of asthma for different Poverty Rate
# percentiles:

pov_asthma_boxplot <- pandemic_cum %>% 
  dplyr::select("Asthma", "PovertyRate") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                            seq(min(PovertyRate), max(PovertyRate), by = 2), 
                          include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  ggplot(aes(PovertyRate, Asthma), Asthma) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Asthma per 2% PovertyRate Intervals"
  )
save(pov_asthma_boxplot, file = "visuals/pov_asthma_boxplot.rda")


# Poverty Rate and Asthma:

small_df_poverty <- pandemic_cum %>%
  dplyr::select("PovertyRate", "Asthma") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                            seq(min(PovertyRate), max(PovertyRate), by = 1), 
                          include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  summarise(Asthma = mean(Asthma))



plot(small_df_poverty$PovertyRate, 
     small_df_poverty$Asthma)

pov_asthma_gg <- ggplot(data = small_df_poverty, mapping = aes(PovertyRate, 
                                               Asthma)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

save(pov_asthma_gg, file = "visuals/pov_asthma_gg.rda")

small_df_poverty$interval <- seq(1, nrow(small_df_poverty))

fit_poverty <- glm(small_df_poverty$Asthma ~ small_df_poverty$interval)
summary(fit_poverty)

plot(small_df_poverty$interval, small_df_poverty$Asthma)
abline(fit_poverty)

plot(fit_poverty)


# /////////////////////////////////////////////////////////////////////////////



# distribution of low birth weight for different Poverty Rate
# percentiles:

pov_low_birth_boxplot <- pandemic_cum %>% 
  dplyr::select("Low.Birth.Weight", "PovertyRate") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                             seq(min(PovertyRate), max(PovertyRate), by = 2), 
                           include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  ggplot(aes(PovertyRate, Low.Birth.Weight), Low.Birth.Weight) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Low Birth Weight per 2% Poverty Rate Intervals"
  )
save(pov_low_birth_boxplot, file = "visuals/pov_low_birth_boxplot.rda")


# Poverty Rate and Low.Birth.Weight:

small_df_poverty <- pandemic_cum %>%
  dplyr::select("PovertyRate", "Low.Birth.Weight") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                             seq(min(PovertyRate), max(PovertyRate), by = 1), 
                           include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  summarise(Low.Birth.Weight = mean(Low.Birth.Weight))



plot(small_df_poverty$PovertyRate, 
     small_df_poverty$Low.Birth.Weight)

pov_low_birth_gg <- ggplot(data = small_df_poverty, mapping = aes(PovertyRate, 
                                                               Low.Birth.Weight)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

save(pov_low_birth_gg, file = "visuals/pov_low_birth_gg.rda")

small_df_poverty$interval <- seq(1, nrow(small_df_poverty))

fit_poverty <- glm(small_df_poverty$Low.Birth.Weight ~ small_df_poverty$interval)
summary(fit_poverty)

plot(small_df_poverty$interval, small_df_poverty$Low.Birth.Weight)
abline(fit_poverty)

plot(fit_poverty)


# /////////////////////////////////////////////////////////////////////////////


# distribution of cardiovscular disease for different Poverty Rate
# percentiles:

pov_cardio_boxplot <- pandemic_cum %>% 
  dplyr::select("Cardiovascular.Disease", "PovertyRate") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                             seq(min(PovertyRate), max(PovertyRate), by = 2), 
                           include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  ggplot(aes(PovertyRate, Cardiovascular.Disease), Cardiovascular.Disease) +
  geom_boxplot(varwidth = TRUE) + 
  coord_flip() +
  labs(
    title = "Distribution of Cardovascular Disease per 2% Poverty Rate Intervals"
  )
save(pov_cardio_boxplot, file = "visuals/pov_cardio_boxplot.rda")


# Poverty Rate and Cardiovascular.Disease:

small_df_poverty <- pandemic_cum %>%
  dplyr::select("PovertyRate", "Cardiovascular.Disease") %>%
  mutate(PovertyRate = cut(PovertyRate, breaks = 
                             seq(min(PovertyRate), max(PovertyRate), by = 1), 
                           include.lowest = TRUE)) %>%
  group_by(PovertyRate) %>%
  na.omit(PovertyRate) %>%
  summarise(Cardiovascular.Disease = mean(Cardiovascular.Disease))



plot(small_df_poverty$PovertyRate, 
     small_df_poverty$Cardiovascular.Disease)

pov_cardio_gg <- ggplot(data = small_df_poverty, mapping = aes(PovertyRate, 
                                                                  Cardiovascular.Disease)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

save(pov_cardio_gg, file = "visuals/pov_cardio_gg.rda")

small_df_poverty$interval <- seq(1, nrow(small_df_poverty))

fit_poverty <- glm(small_df_poverty$Cardiovascular.Disease ~ small_df_poverty$interval)
summary(fit_poverty)

plot(small_df_poverty$interval, small_df_poverty$Cardiovascular.Disease)
abline(fit_poverty)

plot(fit_poverty)



# /////////////////////////////////////////////////////////////////////////////








