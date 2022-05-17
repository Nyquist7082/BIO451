# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Combined grazing

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)        #Publication-ready graphics
library(car)           #Regression
library(corrplot)
library(tidyverse)     #Data manipulation
library(rstatix)       #Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) #Read google sheets data into R 

#Plot wet weight diff exposed and sheltered choice no choice####
p_comb_shel<- ggboxplot(combined_data_sheltered, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()

p_comb_shel+ theme(legend.title = element_blank())

p_comb_expo<- ggboxplot(combined_data_exposed, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()

p_comb_expo+ theme(legend.title = element_blank())
