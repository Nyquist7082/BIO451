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

# Calculations of dif. in PHL####
trial1_data_wider <- trial1_data %>% pivot_wider(names_from = "treatment", values_from = "PHL")
#view(trial1_data_wider)

select_1 <- trial1_data_wider %>% select(grazed,ecotype, sample_ID)
select_1_removeNA <- na.omit(select_1)
select_2 <- trial1_data_wider %>% select(not_grazed,ecotype,sample_ID)
select_2_removeNA <- na.omit(select_2)
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

# Grazed and baseline separation ####  

trial1_data_ex_c <-trial1_data[trial1_data$treatment == 'grazed', ]   #Gazed, without baseline
trial1_data_c <-trial1_data[trial1_data$treatment == 'not_grazed', ]  #Baseline


###################### Trial 2: preference #####################################

# Data ####
preference_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# Add column: wetweight change in % ####
preference_data$ww_perc <- (preference_data$`ww_after`- preference_data$`ww_before`)/(preference_data$`ww_before`)*100

# Wide format for jars####
preference_exposed_jar<- preference_data %>% filter(ecotype=="exposed") %>% select("jar","TDMC","SAP", "STA", "ww_perc"  )
preference_sheltered_jar <- preference_data %>% filter(ecotype=="sheltered") %>% select("jar","TDMC","SAP", "STA", "ww_perc"  )
preference_jar <- merge(preference_exposed_jar, preference_sheltered_jar, by="jar")

# Absolute difference values ####
preference_jar$ww_diff <- abs(preference_jar$ww_perc.x - preference_jar$ww_perc.y) # Wetweight percent
preference_jar$STA_diff <- abs(preference_jar$STA.x - preference_jar$STA.y)        # STA
preference_jar$SAP_diff <- abs(preference_jar$SAP.x - preference_jar$SAP.y)        # SAP
preference_jar$TDMC_diff <- abs(preference_jar$TDMC.x - preference_jar$TDMC.y)     # TDMC


###################### Combined data trial 1 and 2 #############################
combined_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "combined")

# Add column: wetweight change in % ####
combined_data$ww_perc <- (combined_data$`ww_after`- combined_data$`ww_before`)/(combined_data$`ww_before`)*100

# Filter exposed and sheltered####
combined_data_exposed <- combined_data %>% filter(ecotype=="exposed")
combined_data_sheltered <- combined_data %>% filter(ecotype=="sheltered")
