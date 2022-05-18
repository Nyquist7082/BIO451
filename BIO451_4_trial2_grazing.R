# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Preference trial grazing

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)        #Publication-ready graphics
library(car)           #Regression
library(corrplot)
library(tidyverse)     #Data manipulation
library(rstatix)       #Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) #Read google sheets data into R 

# Add column: wetweight change in % ####
preference_data$ww_perc <- (preference_data$`ww_after`- preference_data$`ww_before`)/(preference_data$`ww_before`)*100

#boxplot ww_perc: exposed vs sheltered####
p_ww_perc_sh_v_ex <- ggboxplot(preference_data, x = "ecotype", y = "ww_perc",
                               color = "ecotype",
                               add = "jitter", shape = "ecotype", width= 0.2)+
  theme_classic()+
  xlab("Environment")
p_ww_perc_sh_v_ex + theme(legend.title = element_blank())


#plots ww_perc vs STA ####

p_ww_perc_sta <- ggplot(data = preference_data, 
                        aes(x = STA,
                            y = ww_perc,
                            colour=ecotype)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()
p_ww_perc_sta + theme(legend.title = element_blank())

#plots ww_perc vs SAP ####

p_ww_perc_sap <- ggplot(data = preference_data, 
                        aes(x = SAP,
                            y = ww_perc,
                            colour=ecotype)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()
p_ww_perc_sap + theme(legend.title = element_blank())

#plots ww_perc vs TDMC####

p_ww_perc_tdmc <- ggplot(data = preference_data, 
                         aes(x = TDMC,
                             y = ww_perc,
                             colour=ecotype)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()
p_ww_perc_tdmc + theme(legend.title = element_blank())

# PERMANOVA (non parametric)####

preference.permanova <- adonis2(cbind(scale(preference_data$STA), scale(preference_data$SAP), 
                                      scale(preference_data$TDMC)
) ~ preference_data$ww_perc,
permutations = 9999,
method="euclidian")

preference.permanova

#ww_perc ttest####

ttest.ww_perc <- preference_data %>% 
  t_test(ww_perc ~ ecotype, paired = TRUE) %>%
  add_significance()
ttest.ww_perc

#STA ttest####

ttest.STA <- preference_data %>% 
  t_test(STA ~ ecotype, paired = TRUE) %>%
  add_significance()
ttest.STA

#SAP ttest####

ttest.SAP <- preference_data %>% 
  t_test(SAP ~ ecotype, paired = TRUE) %>%
  add_significance()
ttest.SAP

#TDMC ttest####

ttest.TDMC <- preference_data %>% 
  t_test(TDMC ~ ecotype, paired = TRUE) %>%
  add_significance()
ttest.TDMC

