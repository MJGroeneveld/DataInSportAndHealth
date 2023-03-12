##DATA PREPARATION##

#Setting working directory
setwd("C:/Users/irisk/Documents/Uni/2022-2023/Data in Sport and Health/Research data")

##Loading R packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("reshape2")
install.packages("tidyr")

library("dplyr")
library("ggplot2")
library("naniar")
library("reshape2")
library("tidyr")

#Loading data
data_running <- read.csv("data_running.csv")
data_running_raw <- read.csv("data_running_raw.csv")

#Filtering important variables
data_running_selected <- data_running %>%
  select("id", "running_HR_max", "running_HR_mean", "running_HR_25p", "running_HR_75p", "running_HR_zeros", "training_duration", "training_RPE", "training_sRPE", "daily_wellness_sleep", "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", "daily_wellness_mood", "daily_readiness_train", "daily_HRrest")

##Data quality check

#Checking missing values
missing <- miss_var_summary(data_running_selected[ ,2:15])
ggplot(missing, aes(x = variable, y = pct_miss)) + geom_col() + theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + ylim(0,75) + labs(y = "Percentage missing")
other_missing <- data_running_selected %>% miss_scan_count(search = list("N/A", "n/a", "na", " ", "missing", "E"))
shadow <- as_shadow(data_running_selected)

#Visualizing missing values
vis_miss(data_running_selected)

#Deleting observations without values for RPE and HR
data_running_selected <- data_running_selected %>% drop_na(training_RPE, running_HR_mean)

#Checking missing values
vis_miss(data_running_selected)

#Duplicates
unique_observations <- unique(data_running_selected$id)
length(unique_observations)

#-------------------------------------------------------------------------------

##DATA EXPLORATION##

#Displaying the structure of the data frames
str(data_running)
str(data_running_raw)
str(data_running_selected)

#Displaying a summary of the data frames
summary(data_running)
summary(data_running_raw)
summary(data_running_selected)

#Setting RPE as a factor + creating variable "wellness"
data_running_selected <- data_running_selected %>%
  mutate(training_RPE = as.factor(training_RPE), wellness = (rowMeans(data_running_selected[ ,10:14])))

#Creating long data for plot
data_long <- data_running_selected %>%
  select("daily_wellness_sleep", "daily_wellness_fatigue", "daily_wellness_stress", "daily_wellness_soreness", "daily_wellness_mood")
data_long <- melt(data_long)

#Creating simple plots
ggplot(data_running_selected, aes(y = running_HR_mean)) + geom_boxplot() + geom_hline(yintercept = mean(data_running_selected$running_HR_max), color = "red") + geom_hline(yintercept = mean(data_running_selected$running_HR_25p), color = "grey", linetype = "dashed") + geom_hline(yintercept = mean(data_running_selected$running_HR_75p), color = "grey", linetype = "dashed")
ggplot(data_long, aes(x = variable, y = value)) + geom_boxplot() + geom_hline(yintercept = mean(data_running_selected$wellness), color = "red")
ggplot(data_running_selected, aes(x = daily_readiness_train)) + geom_histogram(binwidth = 0.5, aes(y = ..density..)) + geom_density(color = "red")
ggplot(data_running_selected, aes(x = training_RPE)) + geom_histogram(stat = "count", binwidth = 1) 

#Creating a correlation table between all variables

#Creating plots between predictors and RPE
ggplot(data_running_selected, aes(x = running_HR_mean)) + geom_density(aes(fill = training_RPE, alpha = 0.2)) + labs(x = "mean HR") + scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + geom_density(aes(size = 2)) + scale_size_continuous(name = " ", labels = c("Over all")) + scale_alpha_continuous(guide = "none")
ggplot(data_running_selected, aes(x = wellness)) + geom_density(aes(fill = training_RPE, alpha = 0.2)) + labs(x = "Wellness") + scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + geom_density(aes(size = 2)) + scale_size_continuous(name = " ", labels = c("Over all")) + scale_alpha_continuous(guide = "none")
ggplot(data_running_selected, aes(x = as.factor(daily_readiness_train))) + geom_histogram(stat = "count", position = "dodge", aes(fill = training_RPE)) + labs(x = "Daily readiness to train") + scale_fill_discrete(name = "RPE", labels = c("2", "3", "4", "5", "6", "7", "9")) + geom_histogram(stat = "count", aes(alpha = 0.1)) + scale_alpha_continuous(name = " ", labels = "Over all")

#Calculating statistics

#-------------------------------------------------------------------------------

##FEATURE ENGINEERING##

#-------------------------------------------------------------------------------

##DATA MODELLING##

#-------------------------------------------------------------------------------

##DATA VISUALIZATION##