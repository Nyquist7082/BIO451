# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Loading data

library(dplyr)
library(tidyverse)     # Data manipulation
library(googlesheets4) # Read google sheets data into R 

####################### Trial 1: grazing and traits ############################

# Data ####
trial1_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "trial1")

# Add column: wetweight change in % ####
trial1_data$ww_perc <- (trial1_data$`ww_after`- trial1_data$`ww_before`)/(trial1_data$`ww_before`)*100

# Grazed and baseline separation ####  
trial1_data_ex_c <-trial1_data[trial1_data$treatment == 'grazed', ]   #Gazed, without baseline
trial1_data_c <-trial1_data[trial1_data$treatment == 'not_grazed', ]  #Baseline

# Growth corrected ####
correct_growth <- trial1_data %>% select(sample_ID , ww_perc, treatment)
correct_growth <- correct_growth %>% pivot_wider(names_from = "treatment", values_from = "ww_prec")
correct_growth$growth_corr <- correct_growth$not_grazed- correct_growth$grazed
trial1_data <- left_join(trial1_data, correct_growth, by = "sample_ID")
trial1_data$growth_corr[trial1_data$treatment =="not_grazed"]=NA


###################### Trial 2: preference #####################################

# Data ####
preference_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# 


