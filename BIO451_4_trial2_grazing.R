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
  ggtitle("Reggresion wet wight change STA")+
  ylab("Weight change [%]")+
  theme_classic()+ theme(legend.title = element_blank())
p_ww_perc_sta 

ggsave("RegressionSTA.png")
model11 <- lm(preference_data$STA ~preference_data$ww_perc)
summary(model11)
#plots ww_perc vs SAP ####

p_ww_perc_sap <- ggplot(data = preference_data, 
                        aes(x = SAP,
                            y = ww_perc,
                            colour=ecotype)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  ggtitle("Reggresion wet wight change SAP")+
  ylab("Weight change [%]")+
  theme_classic()+ 
  theme(legend.title = element_blank())

p_ww_perc_sap
ggsave("RegressionSAP.png")
model12 <- lm(preference_data$SAP ~preference_data$ww_perc)
summary(model12)

ggsave(filename = "fig_1.png",  
       plot = p_ww_perc_sap, width = 7, height = 7, units = "cm", 
       dpi = 450) 
 

#plots ww_perc vs TDMC####

p_ww_perc_tdmc <- ggplot(data = preference_data, 
                         aes(x = TDMC,
                             y = ww_perc,
                             colour=ecotype)) + 
  geom_smooth(method=lm) + 
  ggtitle("Reggresion wet wight change TDMC")+
  ylab("Weight change [%]")+
  geom_point(size = 2) +
  theme_classic()+
  theme(legend.title = element_blank())

p_ww_perc_tdmc 

ggsave("RgressionTDMC.png")

model <- lm(preference_data$TDMC ~preference_data$ww_perc)
summary(model)

ggsave("TDMCsheltered_exposed.png")
ggsave(filename = "TDMC_wwperc.png",  
       plot = p_ww_perc_tdmc, width = 10, height = 6.6, units = "cm", 
       dpi = 800) 

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

#plots STA_diff vs ww_diff####

ggplot(data = preference_jar, 
                        aes(x = STA_diff,
                            y = ww_diff)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()

#plots SAP_diff vs ww_diff####

p2 <- ggplot(data = preference_jar, 
       aes(x = SAP_diff,
           y = ww_diff)) + 
  geom_smooth(method=lm) + 
  ggtitle("Regression Δ SAP")+
  ylab("Wet weight difference within jar [%]")+
  xlab("SAP difference within jar")+
  geom_point(size = 2) +
  theme_classic()
p2
ggsave("DeltaSAP_deltegrazing.png")

model4 <- lm(preference_jar$SAP_diff ~preference_jar$ww_diff)
summary(model4)

#plots TDMC_diff vs ww_diff####

p1 <- ggplot(data = preference_jar, 
       aes(x = TDMC_diff,
           y = ww_diff)) + 
  geom_smooth(method=lm) +
  ggtitle("Regression Δ TDMC")+
  ylab("Wet weight difference within jar [%]")+
  xlab("TDMC difference within jar")+
  geom_point(size = 2) +
  theme_classic()
p1
ggsave("DeltaTDMC_deltegrazing.png")

model5 <- lm(preference_jar$TDMC_diff ~preference_jar$ww_diff)
summary(model5)
#permutation test####
model1 <- lm(preference_jar$ww_diff ~preference_jar$TDMC_diff)
summary(model1)
ggsave(filename = "TDMC_diffa.png",  
       plot = p1, width = 10, height = 6.6, units = "cm", 
       dpi = 800) 

#STA_diff ttest

ttest.STA <- preference_jar %>% 
  t_test(STA_diff ~ ww_diff, paired = TRUE) %>%
  add_significance()
ttest.STA
#SAP_diff ttest

ttest.SAP <- preference_jar %>% 
  t_test(SAP_diff ~ ww_diff, paired = TRUE) %>%
  add_significance()
ttest.SAP

#TDMC_diff ttest

ttest.TDMC <- preference_jar %>% 
  t_test(TDMC_diff ~ ww_diff, paired = TRUE) %>%
  add_significance()
ttest.TDMC

