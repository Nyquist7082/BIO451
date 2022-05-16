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


###################### ANOVA, corrected for growth #############################

anova_growth <- aov(growth_corr ~ trial1_data_ex_c$ecotype, dat=trial1_data_ex_c)
summary(anova_growth)


###################### PHL #############################
p_PHL <- ggboxplot(trial1_data, x = "ecotype", y = "PHL",
          color = "treatment",
          add = "jitter", shape = "treatment",width= 0.15,
          title = "Phlorotannins levels grazed vs control",
          xlab = "Environment",
          ylab = "Phlorotannins")+
  theme_classic()
  
p_PHL+ theme(legend.title = element_blank())

trial1_data %>% 
  t_test(PHL~ treatment) %>%
  add_significance()


#PHL Before grazing,baseline ####
ggplot(data = trial1_data_c, 
       mapping = aes(x = ecotype, y = PHL)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  labs(title = "PHL Before grazing(controls)")+
  theme_classic()

p_PHL_before <- ggboxplot(trial1_data_c, x = "ecotype", y = "PHL",
                       color = "ecotype",
                       add = "jitter", shape = "ecotype", width= 0.15,
                       title = "Initial Phlorotannins levels",
                       xlab = "Environment",
                       ylab = "Phlorotannins")+
  theme_classic()

p_PHL_before + theme(legend.title = element_blank())


trial1_data_c %>% 
  t_test(PHL~ ecotype) %>%
  add_significance()

#PHL After grazing ,grazed induced maximum ####

p_PHL_after <- ggboxplot(trial1_data_ex_c, x = "ecotype", y = "PHL",
                          color = "ecotype",
                          add = "jitter", shape = "ecotype", width= 0.15,
                         title = "Phlorotannins levels after grazing",
                         xlab = "Environment",
                         ylab = "Phlorotannins")+
  theme_classic()
  

p_PHL_after + theme(legend.title = element_blank())


trial1_data_ex_c %>% 
  t_test(PHL~ ecotype) %>%
  add_significance()

#Difference in PHL start and induction####

p_PHL_diff <- ggboxplot(joined, x = "ecotype", y = "PHLdiff",
                         color = "ecotype",
                         add = "jitter", shape = "ecotype", 
                        width= 0.15, title = "Induction of Phlorotannins",
                        ylab = "Phlorotannins",
                        xlab = "Environment")+
  theme_classic()
  
  

p_PHL_diff + theme(legend.title = element_blank())

joined%>% 
  t_test(PHLdiff~ecotype) %>% 
  add_significance()

joined%>% 
  wilcox_test(PHLdiff~ecotype) %>% 
  add_significance()
