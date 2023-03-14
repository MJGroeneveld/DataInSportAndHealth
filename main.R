##DATA PREPARATION##

#Setting working directory
#setwd("C:/Users/irisk/Documents/Uni/2022-2023/Data in Sport and Health/Research data")
setwd("/Users/melaniegroeneveld/Documents/Data in Sport and Health")

##Loading R packages
library(dplyr)
library(ggplot2)
library(naniar)
library(corrplot)
library(fastDummies)

source("parameters.R")
source("functions.R")

#Loading data
data_running <- read.csv("data_running.csv")
data_running_raw <- read.csv("data_running_raw.csv")

#Checking data types 
str(data_running)

#Checking missing values
gg_miss_var(data_running, show_pct = TRUE) 
sum(is.na(data_running))

#Feature selection based on description, convert in correct type & delete NA values 
data_running_selected <- data_running %>% 
  dplyr::select("id", "start_date", "start_time", "end_time", "summary_duration", 
                "summary_distance", "summary_speed", "time", "running_speed_max", 
                "running_speed_mean", "running_speed_25p", "running_speed_75p", 
                "running_speed_90p", "running_speed_zeros", "running_HR_max", 
                "running_HR_mean", "running_HR_25p", "running_HR_75p","running_HR_zeros", 
                "training_daypart", "training_session", "training_duration", 
                "training_RPE", "training_sRPE", "daily_wellness_sleep", 
                "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", 
                "daily_wellness_mood", "daily_readiness_train", "daily_HRrest") %>% 
  dplyr::mutate(start_time = setDateTime(dateTimeColumn = start_time,
                                         Table = TRUE), 
                end_time = setDateTime(dateTimeColumn = end_time, 
                                       Table = TRUE),
                start_date = as.POSIXct(start_date)) %>% 
  dplyr::mutate_if(is.integer, as.numeric) %>% 
  na.omit() #deze moet denk hier nog niet. Want denk dat we niet alles willen verwijderen? 

#Dummy encoding 
data_running_dummy <- data_running_selected %>% 
  dplyr::mutate(training_daypart = ifelse(is.na(training_daypart), 'None', training_daypart),
                training_session = ifelse(is.na(training_session), 'None', training_session)) %>% 
  fastDummies::dummy_cols(select_columns = c('training_daypart', 'training_session'), 
                          remove_most_frequent_dummy = T, 
                          remove_selected_columns = T) %>% 
  dplyr::mutate_if(is.integer, as.numeric)

#For our research question it doesn't make sense to look at training_daypart and training_session 
data_running_selected <- data_running_selected %>% 
  dplyr::select(-c('running_HR_zeros', 'training_daypart', 'training_session'))

#Correlation plot 
data_running_corr <- data_running_selected %>% 
  dplyr::select(-c(id, start_date, start_time, end_time)) 

M = cor(data_running_corr)
corrplot(M, method = 'color', order = 'alphabet')

#Duplicates
unique_observations <- unique(data_running_selected$id)
unique_observations_raw <- unique(data_running_raw$id)
length(unique_observations)

# Combine start_date with start_time to get date_time & convert state_date into POSIXct format
data_running_selected <- data_running_selected %>% 
  dplyr::mutate(start_time = setDateTime(dateTimeColumn = start_time,
                                         Table = TRUE), 
                end_time = setDateTime(dateTimeColumn = end_time, 
                                       Table = TRUE),
                start_date = as.POSIXct(start_date), 
                start_datetime = start_date + as.difftime(start_time), 
                end_datetime = start_date + as.difftime(end_time), 
                duration = base::difftime(end_datetime,
                                          start_datetime, units = 'secs'))

#-------------------------------------------------------------------------------

##DATA EXPLORATION##

#Displaying the structure of the data frames
str(data_running)
str(data_running_raw)

#Displaying a summary of the data frames
summary(data_running)
summary(data_running_raw)

#Setting RPE as a factor
data_running_selected <- data_running_selected %>%
  dplyr::mutate(training_RPE = as.factor(training_RPE), 
                wellness = (rowSums(data_running_selected[ ,9:13])/25)*100)

#Creating plots between predictors and RPE
ggplot(data_running_selected, aes(x = training_RPE, y = running_HR_max)) + 
  geom_boxplot() + labs(x = "RPE", y = "HR max")
ggplot(data_running_selected, aes(x = training_RPE, y = wellness)) + 
  geom_boxplot() + labs(x = "RPE", y = "Wellness")
ggplot(data_running_selected, aes(x = training_RPE, y = daily_readiness_train)) + 
  geom_count() + labs(x = "RPE", y = "Readiness to train")


ggplot(data_running_selected, aes(x=summary_duration, y=duration)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Difference Time") +
  theme_bw()

# Points above the diagonal line represent cases where difference_time is greater 
# than summary_duration, and points below the line represent cases where summary_duration 
# is greater than difference_time.

ggplot(data_running_selected, aes(x=summary_duration, y=training_duration*60)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Training duration") +
  theme_bw()

ggplot(data_running_selected, aes(x=duration, y=training_duration*60)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Difference Time", y = "Training duration") +
  theme_bw()

# So we want to delete the ones which are not the same 
data_running_selected <- data_running_selected[data_running_selected$summary_duration==data_running_selected$duration,]

# We hebben dan nog meer 98 obs. & met onderstaande checken we of het nu wel overeenkomt. 
ggplot(data_running_selected, aes(x=summary_duration, y=duration)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Difference Time") +
  theme_bw()

#Calculating statistics

#-------------------------------------------------------------------------------

##FEATURE ENGINEERING##
# TRIMP & SHRZ
data_running_selected <- data_running_selected %>% 
  dplyr::mutate(TRIMP = edwards_TRIMP(summary_duration/60, running_HR_max,running_HR_mean, daily_HRrest), 
                SHRZ  = edwards_SHRZ(summary_duration/60, running_HR_max, running_HR_mean, daily_HRrest))

#-------------------------------------------------------------------------------

##DATA MODELLING##

#We remove ID as there is nothing to learn from this feature (it would just add some noise).
data_running_prepared <- subset(data_running_selected, select = -id)

write.csv(data_running_prepared, file = "/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", row.names = FALSE)


#-------------------------------------------------------------------------------

##DATA VISUALIZATION##