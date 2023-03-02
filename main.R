##DATA PREPARATION##

#Setting working directory
#setwd("C:/Users/irisk/Documents/Uni/2022-2023/Data in Sport and Health/Research data")
setwd("/Users/melaniegroeneveld/Documents/Data in Sport and Health")

##Loading R packages
library("dplyr")
library("ggplot2")
library("naniar")

source("parameters.R")
source("functions.R")

#Loading data
data_running <- read.csv("data_running.csv")
data_running_raw <- read.csv("data_running_raw.csv")

#Filtering important variables
data_running_selected <- data_running %>%
  select("id", "start_date", "start_time", "end_time", "running_HR_max", "running_HR_mean", 
         "running_HR_25p", "running_HR_75p", "summary_duration",
         "running_HR_zeros", "training_RPE", "training_sRPE", "daily_wellness_sleep", 
         "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", 
         "daily_wellness_mood", "daily_readiness_train", "daily_HRrest")

##Data quality check

#Checking missing values
gg_miss_var(data_running_selected, show_pct = TRUE) 

#Deleting missing values
data_running_selected <- na.omit(data_running_selected)

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

# --> do want to delete those points?

#Calculating statistics

#-------------------------------------------------------------------------------

##FEATURE ENGINEERING##
# heart rate reserve (HRR) 
data_running_trimp<- data_running_selected %>% 
  dplyr::mutate(HRR = (running_HR_mean - daily_HRrest)/(running_HR_max - daily_HRrest), 
                # weighting_factor = ifelse(HRR < 0.5, 1, 
                #                           ifelse(HRR < 0.6, 1.1, 
                #                                  ifelse(HRR < 0.7, 1.2, 
                #                                         ifelse(HRR < 0.8, 1.3, 
                #                                                ifelse(HRR < 0.9, 1.4, 1.5))))), 
                TRIMP <- HRR * duration * 0.64)


#-------------------------------------------------------------------------------

##DATA MODELLING##

#-------------------------------------------------------------------------------

##DATA VISUALIZATION##