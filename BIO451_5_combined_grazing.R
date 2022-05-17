# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

# Combined grazing

library(vegan)
library(ggplot2)
library(dplyr)
library(ggpubr)        #Publication-ready graphics
library(car)           #Regression
library(corrplot)
library(tidyverse)     #Data manipulation
library(rstatix)       #Pipe-friendly R functions for easy statistical analyses
library(googlesheets4) #Read google sheets data into R 

#Plot wet weight diff exposed and sheltered choice no choice####
p_comb_shel<- ggboxplot(combined_data_sheltered, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()

p_comb_shel+ theme(legend.title = element_blank())

p_comb_expo<- ggboxplot(combined_data_exposed, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "ecotype", width= 0.15)+
  theme_classic()

p_comb_expo+ theme(legend.title = element_blank())

# grazing  TDMC choice no_choice####
ggplot(data = combined_data, 
       aes(x = TDMC,
           y = ww_perc,
           colour=experiment)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ggtitle("TDMC vs Grazing for choice/ no choice")


#Area####
ggplot(data = combined_data, 
       aes(x = AreaBefore,
           y = ww_perc,
           colour=experiment))+ 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  

# grazing  SAP choice no_choice####
ggplot(data = combined_data, 
       aes(x = SAP,
           y = ww_perc,
           colour=experiment))+ 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ggtitle("SAP vs Grazing for choice/ no choice")



# grazing  STA choice no_choice####

ggplot(data = combined_data, 
       aes(x = STA,
           y = ww_perc,
           colour=experiment))+ 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ggtitle("STA vs Grazing for choice/ no choice")
  

