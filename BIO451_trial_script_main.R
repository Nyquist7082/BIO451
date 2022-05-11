# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)        #Publication-ready graphics
library(car)           #Regression
library(corrplot)
library(tidyverse)     #Data manipulation
library(rstatix)       #Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) #Read google sheets data into R 

#################################Trial 1########################################

trial1_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "trial1")

# Manova####
res.man <- manova(cbind(STA, SAP, TDMC, PHL, ww_dif) ~ ecotype, data = trial1_data)
summary(res.man)

# PERMANOVA (non parametric)####
trial1.permanova <- adonis2(cbind(trial1_data$STA, trial1_data$SAP, trial1_data$TDMC,
                                  trial1_data$ww_dif) ~ trial1_data$ecotype,
                           permutations = 9999,
                           method="euclidian")
trial1.permanova


########################### Grazed not grazed separate #########################

#Correction for growth####
corr.ww <- trial1_data %>% 
  pivot_wider(names_from = "treatment", values_from = "ww_dif")

corr.select <- corr.ww %>% select(sample_ID, grazed, not_grazed)
corr.grazed <- corr.select %>% select(sample_ID, grazed)
  na.omit(corr.grazed) -> na.grazed

corr.not_grazed <- corr.select %>% select(sample_ID, not_grazed)
  na.omit(corr.not_grazed) -> na.not_grazed
  
join.corr <- left_join(na.grazed, na.not_grazed)
  join.corr$true_dif <- join.corr()
  
#data####
trial1_data_ex_c <-trial1_data[trial1_data$treatment == 'grazed', ]   #Gazed, without baseline
trial1_data_c <-trial1_data[trial1_data$treatment == 'not_grazed', ]  #Baseline

#trial1_weigthchange <- trial1_data$`ww_before`- trial1_data$`ww_after`
#trial1_weigthchange


#Boxplots####
ggboxplot(trial1_data_ex_c, x = "ecotype", y = "ww_dif",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Grazed")

ggboxplot(trial1_data_c, x = "ecotype", y = "ww_dif",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype", title = "Baseline")

ggboxplot(trial1_data, x = "ecotype", y = "ww_dif",
          color = "treatment",
          add = "jitter", shape = "treatment", title = "Both treatments")


#Anova - ecotype####
anova(lm(ww_dif ~ trial1_data_ex_c$ecotype, trial1_data_ex_c))

#Anova - grazing####
table <- aov(ww_dif~trial1_data$treatment, data = trial1_data)
summary(table)

#################### t-test for each trait (grazed) ############################

#ww_dif####
ttest.STA <- trial1_data_ex_c %>% 
  t_test(ww_dif ~ ecotype) %>%
  add_significance()
ttest.STA

#STA####
ttest.STA <- trial1_data_ex_c %>% 
  t_test(STA ~ ecotype) %>%
  add_significance()
ttest.STA

#SAP####
ttest.SAP <- trial1_data_ex_c %>% 
  t_test(SAP ~ ecotype) %>%
  add_significance()
ttest.SAP

#TDMC####
ttest.TDMC <- trial1_data_ex_c %>% 
  t_test(TDMC ~ ecotype) %>%
  add_significance()
ttest.TDMC


