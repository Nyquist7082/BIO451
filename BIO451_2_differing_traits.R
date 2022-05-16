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

# Different in traits ####

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


p1 <-autoplot(pca_data, data = pca_data_scores, colour = 'ecotype',
              loadings = TRUE, loadings.colour = 'blue',
              loadings.label = TRUE, loadings.label.size = 3)
p1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))



#TDMC####
ggplot(data = trial1_data, 
       mapping = aes(x = ecotype, y = TDMC)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  theme_classic()

trial1_data %>% 
  t_test( TDMC~ ecotype ) %>%
  add_significance()


#STA####
ggplot(data = trial1_data, 
       mapping = aes(x = ecotype, y = STA)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  theme_classic()

trial1_data %>% 
  t_test(STA~ ecotype) %>%
  add_significance()


trial1_data %>% 
  wilcox_test(STA~ ecotype) %>%
  add_significance()


#SAP####
ggplot(data = trial1_data, 
       mapping = aes(x = ecotype, y = SAP)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  theme_classic()

trial1_data %>% 
  t_test(SAP~ ecotype) %>%
  add_significance()


trial1_data %>% 
  wilcox_test(SAP~ ecotype) %>%
  add_significance()


