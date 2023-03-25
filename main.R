##DATA PREPARATION##

#Setting working directoryEn de
setwd("/Users/melaniegroeneveld/Documents/Data in Sport and Health")

##Loading R packages
library(dplyr)
library(ggplot2)
library(naniar)
library(corrplot)
library(fastDummies)
library(tidyr)
library(reshape2)
source("parameters.R")
source("functions.R")

#Loading data
data_running <- read.csv("data_running.csv")
data_running_raw <- read.csv("data_running_raw.csv")  # We don't use this, 
# because it doesn't give more information

#Checking data types 
str(data_running)
summary(data_running)

#Checking missing values
sum(is.na(data_running))
vis_miss(data_running)

#Feature selection based on description, convert in correct type & delete NA values 
data_running_selected <- data_running %>% 
  dplyr::select("id", "start_date", "start_time", "end_time", "summary_duration", 
                "summary_distance", "summary_speed", "time", "running_HR_25p", "running_speed_max", 
                "running_speed_mean", "running_HR_max", "running_HR_75p", 
                "running_HR_mean", "training_daypart", "training_session", "training_duration", 
                "training_RPE", "training_sRPE", "daily_wellness_sleep", 
                "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", 
                "daily_wellness_mood", "daily_readiness_train", "daily_HRrest") %>% 
  dplyr::mutate(start_time = setDateTime(dateTimeColumn = start_time,
                                         Table = TRUE), 
                end_time = setDateTime(dateTimeColumn = end_time, 
                                       Table = TRUE),
                start_date = as.POSIXct(start_date)) %>% 
  dplyr::mutate_if(is.integer, as.numeric) %>% 
  tidyr::drop_na(training_RPE, running_HR_mean)

vis_miss(data_running_selected)

#Duplicates
unique_observations <- unique(data_running_selected$id)
length(unique_observations)

#Dummy encoding 
data_running_dummy <- data_running_selected %>% 
  dplyr::mutate(#training_daypart = ifelse(is.na(training_daypart), 'None', training_daypart),
    training_session = ifelse(is.na(training_session), 'None', training_session), 
    daily_readiness_train = ifelse(is.na(daily_readiness_train), 'None', daily_readiness_train), 
    readiness = ifelse(daily_readiness_train >= 3, 
                       'ready', 
                       'notready')) %>% 
  fastDummies::dummy_cols(select_columns = c('training_daypart', 'training_session', 'readiness'), 
                          remove_most_frequent_dummy = T, 
                          remove_selected_columns = T) %>% 
  dplyr::mutate_if(is.integer, as.numeric) %>% 
  dplyr::select(-c('daily_readiness_train', 'daily_readiness_train'))

#Correlation plot 
data_running_corr <- data_running_dummy %>% 
  dplyr::select(-c(id, start_date, start_time, end_time)) 

M = cor(data_running_corr)
corrplot(M, method = 'color', order = 'alphabet', tl.cex = 0.5)


# Combine start_date with start_time to get date_time & convert state_date into POSIXct format
data_running_time <- data_running_dummy %>% 
  dplyr::mutate(start_datetime = start_date + as.difftime(start_time), 
                end_datetime = start_date + as.difftime(end_time), 
                duration = as.numeric(base::difftime(end_datetime,
                                                     start_datetime, units = 'secs')))

#-------------------------------------------------------------------------------
##FEATURE ENGINEERING##

# TRIMP & SHRZ
data_running_engineer <- data_running_time %>% 
  dplyr::mutate(TRIMP = TRIMP(summary_duration/60, running_HR_max,running_HR_mean, daily_HRrest), 
                SHRZ  = SHRZ(summary_duration/60, running_HR_max, running_HR_mean, daily_HRrest), 
                training_RPE = as.factor(training_RPE), 
                wellness = rowMeans(data_running_dummy[, c("daily_wellness_sleep", 
                                                            "daily_wellness_fatigue", 
                                                            "daily_wellness_stress", 
                                                            "daily_wellness_soreness", 
                                                            "daily_wellness_mood")]), 
                HRR = running_HR_max - daily_HRrest)

#-------------------------------------------------------------------------------
##DATA EXPLORATION##

#Creating long data for plot
data_long <- data_running_engineer %>%
  select("daily_wellness_sleep", "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", "daily_wellness_mood")
data_long <- reshape2::melt(data_long)

#Creating simple plots
ggplot(data_running_engineer, aes(y = running_HR_mean)) + 
  geom_boxplot() + 
  geom_hline(yintercept = mean(data_running_engineer$running_HR_max), color = "red") + 
  geom_hline(yintercept = mean(data_running_engineer$running_HR_25p), color = "grey", linetype = "dashed") + 
  geom_hline(yintercept = mean(data_running_engineer$running_HR_75p), color = "grey", linetype = "dashed")

ggplot(data_long, aes(x = variable, y = value)) + 
  geom_boxplot() + 
  geom_hline(yintercept = mean(data_running_engineer$wellness), color = "red")

ggplot(data_running_engineer, aes(x = daily_readiness_train)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..)) + 
  geom_density(color = "red")

ggplot(data_running_engineer, aes(x = training_RPE)) + 
  geom_histogram(stat = "count", binwidth = 1) 

#Creating plots between predictors and RPE
ggplot(data_running_engineer, aes(x = running_HR_mean)) + 
  geom_density(aes(fill = training_RPE, alpha = 0.2)) + 
  labs(x = "mean HR") + 
  scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + 
  geom_density(aes(size = 2)) + 
  scale_size_continuous(name = " ", labels = c("Over all")) + 
  scale_alpha_continuous(guide = "none")

ggplot(data_running_engineer, aes(x = wellness)) + 
  geom_density(aes(fill = training_RPE, alpha = 0.2)) + 
  labs(x = "Wellness") + 
  scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + 
  geom_density(aes(size = 2)) + 
  scale_size_continuous(name = " ", labels = c("Over all")) + 
  scale_alpha_continuous(guide = "none")

ggplot(data_running_engineer, aes(x = as.factor(daily_readiness_train))) + 
  geom_histogram(stat = "count", position = "dodge", aes(fill = training_RPE)) + 
  labs(x = "Daily readiness to train") + 
  scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + 
  geom_histogram(stat = "count", aes(alpha = 0.1)) + 
  scale_alpha_continuous(name = " ", labels = "Over all")



# Plots voor het kijken van de time 
ggplot(data_running_engineer, aes(x=summary_duration, y=duration)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Difference Time") +
  theme_bw()

# Points above the diagonal line represent cases where difference_time is greater 
# than summary_duration, and points below the line represent cases where summary_duration 
# is greater than difference_time.

ggplot(data_running_engineer, aes(x=summary_duration, y=training_duration*60)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Training duration") +
  theme_bw()

ggplot(data_running_engineer, aes(x=duration, y=training_duration*60)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Difference Time", y = "Training duration") +
  theme_bw()

# So we want to delete the ones which are not the same 
data_running_engineer <- data_running_engineer[data_running_engineer$summary_duration==data_running_engineer$duration,]

# We hebben dan nog meer 98 obs. & met onderstaande checken we of het nu wel overeenkomt. 
ggplot(data_running_engineer, aes(x=summary_duration, y=duration)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Summary Duration", y = "Difference Time") +
  theme_bw()


############ delete variables who are not relevant to question ############### 
# We also remove id, start_date, start_time, end_time, start_datetime, end_datetime as 
# there is nothing to learn from this feature (it would just add some noise).

data_running_end <- data_running_engineer %>% 
  dplyr::select(-c(id, start_date, start_time, end_time, start_datetime, end_datetime, 
                   summary_duration, summary_distance, summary_speed, time, running_speed_max, 
                   running_speed_mean, training_duration, training_sRPE, duration)) %>% 
  dplyr::mutate_if(is.integer, as.numeric)

# Now we have 30 variables and all of them are numeric values 

#Correlation plot 
data_running_corr <- data_running_end %>% 
  dplyr::select(-c(training_RPE)) 

M = cor(data_running_corr)
corrplot(M, method = 'color', order = 'alphabet', tl.cex = 0.5)


##DATA MODELLING## 
write.csv(data_running_end, file = "/Users/melaniegroeneveld/Documents/Data in Sport and Health/DataInSportAndHealth/df_prepared.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

##DATA VISUALIZATION##