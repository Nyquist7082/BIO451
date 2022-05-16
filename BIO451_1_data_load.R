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

# grazed and baseline separation ####  
trial1_data_ex_c <-trial1_data[trial1_data$treatment == 'grazed', ]   #Gazed, without baseline
trial1_data_c <-trial1_data[trial1_data$treatment == 'not_grazed', ]  #Baseline




###################### Trial 2: preference #####################################

# Data ####
preference_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# 


