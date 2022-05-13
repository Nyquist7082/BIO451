# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Trial 2 - Preference experiment

library(vegan)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)        #Publication-ready graphics
library(car)           #Regression
library(corrplot)      #Correlation plots
library(tidyverse)     #Data manipulation
library(rstatix)       #Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) #Read google sheets data into R 

########################## Data ################################################

# Load data sheet####
preference_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# Calculate ww_perc ####
preference_data$ww_perc <- (preference_data$`ww_after`- preference_data$`ww_before`)/(preference_data$`ww_before`)*100


##################### Paired t-test ############################################

# Long to wide data format ####
preference_data_wide <- dcast(preference_data[c("ecotype", "jar", "ww_perc")],
      jar ~ ecotype, value.var = "ww_perc")


t.test(preference_data_wide$exposed, preference_data_wide$sheltered, paired = T)

# Connected boxplot ####

bxp <- ggpaired(preference_data, x = "ecotype", y = "ww_perc",
                order = c("exposed", "sheltered"))
bxp
