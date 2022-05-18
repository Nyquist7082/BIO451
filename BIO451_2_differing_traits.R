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

# Difference in traits ####

#Trial 1####
#PCA####
prcomp(~ TDMC + SAP + PHL + STA , data = trial1_data,
       scale = TRUE)

pca_data <- prcomp(~ TDMC + SAP  + STA + PHL,  data = trial1_data,
                   scale = TRUE)

summary(pca_data)
eigenval_data <- pca_data$sdev^2
eigenval_data/sum(eigenval_data)

vars_data <- pca_data$sdev^2
barplot(vars_data/
          sum(vars_data), xlab='PC', ylab='Percent Variance', names.arg=1:length(vars_data), las=1, col='gray') 

pca_data$x[, 1:2]
pca_data$x[, c(1, 2)] # same thing written in a different way

pca_data_scores <- as.data.frame(pca_data$x[, c(1, 2)])

pca_data_scores <- 
  pca_data_scores %>%
  mutate(ecotype = trial1_data$ecotype)

pca_data_scores

ggplot(data = pca_data_scores,
       mapping = aes(x = PC1, y = PC2, colour = ecotype)) +
  geom_point(size = 2) +
  theme_classic()

p_pca <-autoplot(pca_data, data = pca_data_scores, colour = 'ecotype',
              loadings = TRUE, loadings.colour = 'blue',
              loadings.label = TRUE, loadings.label.size = 3)
p_pca+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black",),legend.title = element_blank())

#Boxplot and statistical test for traits####

#TDMC####

p_tdmc <- ggboxplot(trial1_data, x = "ecotype", y = "TDMC",
          color = "ecotype",
          add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_tdmc + theme(legend.title = element_blank())

trial1_data %>% 
  t_test( TDMC~ ecotype ) %>%
  add_significance()


#STA####
p_sta <- ggboxplot(trial1_data, x = "ecotype", y = "STA",
                    color = "ecotype",
                    add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sta + theme(legend.title = element_blank())

tes+ theme(legend.title = element_blank())

trial1_data %>% 
  t_test(STA~ ecotype) %>%
  add_significance()


trial1_data %>% 
  wilcox_test(STA~ ecotype) %>%
  add_significance()


#SAP####
p_sap <- ggboxplot(trial1_data, x = "ecotype", y = "SAP",
                    color = "ecotype",
                    add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sap + theme(legend.title = element_blank())
  

trial1_data %>% 
  t_test(SAP~ ecotype) %>%
  add_significance()


trial1_data %>% 
  wilcox_test(SAP~ ecotype) %>%
  add_significance()

# PERMANOVA (non parametric)####
# Testing if there is significant difference in traits depending on ecotype (sheltered vs exposed)
adonis2(cbind(trial1_data$STA, trial1_data$SAP, trial1_data$TDMC, trial1_data$PHL) ~ trial1_data$ecotype,
                           permutations = 9999,
                           method="euclidian")


names(trial1_data)
#Testing if there is significant difference in traits depending on % of wet weight change
adonis2(cbind(trial1_data_c$STA, trial1_data_c$SAP, trial1_data_c$TDMC, trial1_data_c$PHL) ~ trial1_data_c$growth_corr,
                            permutations = 9999,
                            method="euclidian")

#############################################Trial 2#####################################
#PCA####
prcomp(~ TDMC + SAP  + STA , data = preference_data,
       scale = TRUE)

pca_data_pref <- prcomp(~ TDMC + SAP  + STA ,  data = preference_data,
                   scale = TRUE)

summary(pca_data_pref)
eigenval_data_pref <- pca_data_pref$sdev^2
eigenval_data_pref/sum(eigenval_data_pref)

vars_data_pref <- pca_data_pref$sdev^2
barplot(vars_data_pref/
          sum(vars_data_pref), xlab='PC', ylab='Percent Variance', names.arg=1:length(vars_data_pref), las=1, col='gray') 

pca_data_pref$x[, 1:2]
pca_data_pref$x[, c(1, 2)] # same thing written in a different way

pca_data_scores_pref<- as.data.frame(pca_data_pref$x[, c(1, 2)])

pca_data_scores_pref <- 
  pca_data_scores_pref %>%
  mutate(ecotype = preference_data$ecotype)

pca_data_scores

ggplot(data = pca_data_scores_pref,
       mapping = aes(x = PC1, y = PC2, colour = ecotype)) +
  geom_point(size = 2) +
  theme_classic()

p_pca_pref <-autoplot(pca_data_pref, data = pca_data_scores_pref, colour = 'ecotype',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3, main="PCA preference experiment")
p_pca_pref<- p_pca_pref+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black",),legend.title = element_blank())
p_pca_pref

ggsave("PCApref.png")
#Boxplot and statistical test for traits####

#TDMC####

p_tdmc_pref <- ggboxplot(preference_data, x = "ecotype", y = "TDMC",
                    color = "ecotype",
                    add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_tdmc_pref + theme(legend.title = element_blank())

preference_data %>% 
  t_test( TDMC~ ecotype ) %>%
  add_significance()


#STA####
p_sta_pref<- ggboxplot(preference_data, x = "ecotype", y = "STA",
                   color = "ecotype",
                   add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sta_pref + theme(legend.title = element_blank())



preference_data %>% 
  t_test(STA~ ecotype) %>%
  add_significance()




#SAP####
p_sap_pref<- ggboxplot(preference_data, x = "ecotype", y = "SAP",
                   color = "ecotype",
                   add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sap_pref + theme(legend.title = element_blank())


preference_data %>% 
  t_test(SAP~ ecotype) %>%
  add_significance()


preference_data %>% 
  wilcox_test(SAP~ ecotype) %>%
  add_significance()

# PERMANOVA (non parametric)####
# Testing if there is significant difference in traits depending on ecotype (sheltered vs exposed)
adonis2(cbind(preference_data$STA, preference_data$SAP, preference_data$TDMC) ~ preference_data$ecotype,
        permutations = 9999,
        method="euclidian")

#Testing if there is significant difference in traits depending on % of wet weight change
adonis2(cbind(preference_data$STA, preference_data$SAP, preference_data$TDMC) ~ preference_data$ww_perc,
        permutations = 9999,
        method="euclidian")

#Combined####
#TDMC####
p_tdmc_comb <- ggboxplot(combined_data, x = "ecotype", y = "TDMC",
                         color = "ecotype",
                         add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_tdmc_comb + theme(legend.title = element_blank())


combined_data %>% 
  t_test(TDMC~ ecotype) %>%
  add_significance()

#STA####
p_sta_comb<- ggboxplot(combined_data, x = "ecotype", y = "STA",
                       color = "ecotype",
                       add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sta_comb + theme(legend.title = element_blank())



combined_data %>% 
  t_test(STA~ ecotype) %>%
  add_significance()


#SAP####
p_sap_comb<- ggboxplot(combined_data, x = "ecotype", y = "SAP",
                       color = "ecotype",
                       add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()+
  xlab("Environment")
p_sap_comb + theme(legend.title = element_blank())


combined_data %>% 
  t_test(SAP~ ecotype) %>%
  add_significance()


combined_data %>% 
  wilcox_test(SAP~ ecotype) %>%
  add_significance()

# PERMANOVA (non parametric)####
# Testing if there is significant difference in traits depending on ecotype (sheltered vs exposed)
adonis2(cbind(combined_data$STA, combined_data$SAP, combined_data$TDMC) ~ combined_data$ecotype,
        permutations = 9999,
        method="euclidian")

#Testing if there is significant difference in traits depending on % of wet weight change
adonis2(cbind(combined_data$STA, combined_data$SAP, combined_data$TDMC) ~ combined_data$ww_perc,
        permutations = 9999,
        method="euclidian")





