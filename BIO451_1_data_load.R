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

#Calculations of dif. in PHL####
trial1_data_wider <- trial1_data %>% pivot_wider(names_from = "treatment", values_from = "PHL")
view(trial1_data_wider)

select_1 <- trial1_data_wider %>% select(grazed,ecotype, sample_ID)
select_1_removeNA <- na.omit(try1)
select_2 <- trial1_data_wider %>% select(not_grazed,ecotype,sample_ID)
select_2_removeNA <- na.omit(try2)
joined <- full_join(select_1_removeNA, select_2_removeNA)
joined$PHLdiff <- joined$grazed-joined$not_grazed
#Just run it once!
trial1_data <- left_join(trial1_data, joined)
trial1_data$PHLdiff[trial1_data$treatment =="not_grazed"]=NA



# Growth corrected ####
correct_growth <- trial1_data %>% select(sample_ID , ww_perc, treatment)
correct_growth <- correct_growth %>% pivot_wider(names_from = "treatment", values_from = "ww_perc")
correct_growth$growth_corr <- correct_growth$not_grazed- correct_growth$grazed
trial1_data <- left_join(trial1_data, correct_growth, by = "sample_ID")
trial1_data$growth_corr[trial1_data$treatment =="not_grazed"]=NA


###################### Trial 2: preference #####################################

# Data ####
preference_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# Add column: wetweight change in % ####
preference_data$ww_perc <- (preference_data$`ww_after`- preference_data$`ww_before`)/(preference_data$`ww_before`)*100


###################### Combined data trial 1 and 2 #############################
combined_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "combined")

# Add column: wetweight change in % ####
combined_data$ww_perc <- (combined_data$`ww_after`- combined_data$`ww_before`)/(combined_data$`ww_before`)*100
