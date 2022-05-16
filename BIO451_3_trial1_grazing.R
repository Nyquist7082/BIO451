# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Trial 1 grazing

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)        # Publication-ready graphics
library(car)           # Regression
library(corrplot)
library(tidyverse)     # Data manipulation
library(rstatix)       # Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) # Read google sheets data into R 

############################### Boxplots #######################################

ggboxplot(trial1_data_ex_c, x = "ecotype", y = "ww_dif",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Grazed")

ggboxplot(trial1_data_c, x = "ecotype", y = "ww_dif",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Baseline")

ggboxplot(trial1_data, x = "ecotype", y = "ww_dif",
          color = "treatment",
          add = "jitter", shape = "treatment", title = "Both treatments")



