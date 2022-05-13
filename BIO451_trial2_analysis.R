# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Trial 2 - Choice experiment

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
trial2_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "preference")

# Calculate ww_perc ####
trial2_data$ww_perc <- (trial2_data$`ww_after`- trial2_data$`ww_before`)/(trial2_data$`ww_before`)*100


##################### Paired t-test ############################################

# Long to wide data format ####
trial2_data_wide <- dcast(trial2_data[c("ecotype", "jar", "ww_perc")],
      jar ~ ecotype, value.var = "ww_perc")


t.test(trial2_data_wide$exposed, trial2_data_wide$sheltered, paired = T)

