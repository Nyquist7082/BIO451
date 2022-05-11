# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)
library(corrplot)
library(googlesheets4) #Read google sheets data into R 

#################################Trial 1########################################

trial1_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "trial1")

#Manova
res.man <- manova(cbind(STA, SAP, TDMC, PHL, weigthchange) ~ ecotype, data = trial1_data)
summary(res.man)

# PERMANOVA (non parametric)
trial1.permanova <- adonis2(cbind(trial1_data$STA, trial1_data$SAP, trial1_data$TDMC) ~ trial1_data$ecotype,
                           permutations = 9999,
                           method="euclidian")
trial1.permanova


########################### Grazed not grazed separately ####################### 
trial1_data$weigthchange <- trial1_data$`ww_after` - trial1_data$`ww_before`
trial1_data

trial1_data_ex_c <-trial1_data[trial1_data$treatment == 'grazed', ]
trial1_data_c <-trial1_data[trial1_data$treatment == 'not_grazed', ]

trial1_weigthchange <- trial1_data$`ww_before`- trial1_data$`ww_after`
trial1_weigthchange


#Boxplots####
ggboxplot(trial1_data_ex_c, x = "ecotype", y = "weigthchange",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype")

ggboxplot(trial1_data_c, x = "ecotype", y = "weigthchange",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype")

ggboxplot(trial1_data, x = "ecotype", y = "weigthchange",
          color = "treatment",
          add = "jitter", shape = "treatment")


#Anova - ecotype####
anova(lm(weigthchange ~ trial1_data_ex_c$ecotype, trial1_data_ex_c))

#Anova - grazing####
table <- aov(weigthchange~trial1_data$treatment, data = trial1_data)
summary(table)
