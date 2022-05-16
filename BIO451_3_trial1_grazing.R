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

############################### Boxplots #######################################

# Weight change in grams ####
ggboxplot(trial1_data_ex_c, x = "ecotype", xlab = "Environment", y = "ww_dif", 
          ylab = "Weight change [g]", color = "ecotype", palette =c(
            "#00AFBB", "#E7B800"), add = "jitter", shape = "ecotype", 
          title = "Grazed") + theme(legend.position = "none")

ggboxplot(trial1_data_c, x = "ecotype", y = "ww_dif",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Baseline") + theme(
            legend.position = "none")

ggboxplot(trial1_data, x = "ecotype", y = "ww_dif",
          color = "treatment", add = "jitter", shape = "treatment", 
          title = "Both treatments")

# Weight change in percent, corrected for growth ####
ggboxplot(trial1_data_ex_c, x = "ecotype", xlab = "Environment", y = "growth_corr",
          ylab = "Weight change [%]",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Grazed") + theme(
            legend.position = "none")


##################### Regression traits vs grazing, corrected for growth #######

# Plot STA ####
ggplot(trial1_data_ex_c, aes(STA, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm)

# Plot SAP ####
ggplot(trial1_data_ex_c, aes(SAP, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm)

# Plot TDMC ####
ggplot(trial1_data_ex_c, aes(TDMC, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm)






