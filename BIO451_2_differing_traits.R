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




#Boxplot and statistical test for traits####

#TDMC####

p_tdmc <- ggboxplot(trial1_data, x = "ecotype", y = "TDMC",
          color = "ecotype",
          add = "jitter", shape = "ecotype", width= 0.15, panel.labs = "enviro")+
  theme_classic()+
  xlab("Environment")
p_tdmc + theme(legend.title = element_blank())

trial1_data %>% 
  t_test( TDMC~ ecotype ) %>%
  add_significance()


#STA####
p_sta <- ggboxplot(trial1_data, x = "ecotype", y = "STA",
                    color = "ecotype",
                    add = "jitter", shape = "ecotype", width= 0.15, panel.labs = "enviro")+
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
p_sap <- ggboxplot(trial1_data, x = "ecotype", y = "TDMC",
                    color = "ecotype",
                    add = "jitter", shape = "ecotype", width= 0.15, panel.labs = "enviro")+
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



#Testing if there is significant difference in traits depending on % of wet weight change
adonis2(cbind(trial1_data$STA, trial1_data$SAP, trial1_data$TDMC, trial1_data$PHL) ~ trial1_data$ww_prec,
                            permutations = 9999,
                            method="euclidian")


