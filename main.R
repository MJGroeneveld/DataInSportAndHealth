##DATA PREPARATION##

#Setting working directory
#setwd("C:/Users/irisk/Documents/Uni/2022-2023/Data in Sport and Health/Research data")
setwd("/Users/melaniegroeneveld/Documents/Data in Sport and Health")

##Loading R packages
library(dplyr)
library(ggplot2)
library(naniar)
library(corrplot)

source("parameters.R")
source("functions.R")

#Loading data
data_running <- read.csv("data_running.csv")
data_running_raw <- read.csv("data_running_raw.csv")

#Checking missing values
gg_miss_var(data_running_selected, show_pct = TRUE) 

data_running <- data_running %>% 
  dplyr::select("id", "start_date", "start_time", "end_time", "summary_duration", "summary_distance", 
                "summary_speed", "summary_calories", "time", "running_HR_max", "running_HR_mean", 
                "running_HR_25p", "running_HR_75p", 
                "training_duration", "training_RPE", "training_sRPE", "daily_wellness_sleep", 
                "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", 
                "daily_wellness_mood", "daily_readiness_train", "daily_HRrest") %>% 
  dplyr::mutate(start_time = setDateTime(dateTimeColumn = start_time,
                                         Table = TRUE), 
                end_time = setDateTime(dateTimeColumn = end_time, 
                                       Table = TRUE),
                start_date = as.POSIXct(start_date),
                id = as.numeric(id), 
                summary_duration = as.numeric(summary_duration),
                time = as.numeric(time), 
                running_HR_25p = as.numeric(running_HR_25p),
                running_HR_75p = as.numeric(running_HR_75p),
                training_duration = as.numeric(training_duration), 
                training_RPE = as.numeric(training_RPE), 
                training_sRPE = as.numeric(training_sRPE), 
                daily_HRrest = as.numeric(daily_HRrest)
                ) %>% 
  na.omit() 
  
data_running_corr <- data_running %>% 
  dplyr::select(-c(id, start_date, start_time, end_time)) 

M = cor(data_running_corr)
corrplot(M, method = 'color', order = 'alphabet')

##Data quality check


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
data_running_selected <- data_running %>% 
  dplyr::mutate(TRIMP = edwards_TRIMP(summary_duration/60, running_HR_max,running_HR_mean, daily_HRrest), 
                SHRZ  = edwards_SHRZ(summary_duration/60, running_HR_max, running_HR_mean, daily_HRrest))

#-------------------------------------------------------------------------------

##DATA MODELLING##

#We remove ID as there is nothing to learn from this feature (it would just add some noise).
data_running_prepared <- subset(data_running, select = -id)

write.csv(data_running_prepared, file = "/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", row.names = FALSE)


#-------------------------------------------------------------------------------

##DATA VISUALIZATION##