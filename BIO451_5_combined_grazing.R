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
library(reshape2)


#Plot wet weight diff sheltered choice no choice####
p_comb_shel<- ggboxplot(combined_data_sheltered, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "experiment", width= 0.15, title="Sheltered choice vs no choice")+
  xlab("Environment")+
  ylab("Wet weight change [%]")+
  theme_classic()+
  geom_signif(
    comparisons = list(c("no_choice", "choice")),
    y_position = 9.3, tip_length = 0, vjust = 0.2
  )

p_comb_shel+ theme(legend.title = element_blank())

wilcox.test(ww_perc ~ experiment, combined_data_sheltered)

combined_data_sheltered %>% 
  wilcox_test(ww_perc~ experiment) %>%
  add_significance()

#Plot wet weight diff exposed choice no choice####

p_comb_expo<- ggboxplot(combined_data_exposed, x = "experiment", y = "ww_perc",
                        color = "experiment",
                        add = "jitter", shape = "experiment", width= 0.15, title="Exposed choice vs no choice")+
  xlab("Environment")+
  ylab("Wet weight change [%]")+
  theme_classic()+
  geom_signif(
    comparisons = list(c("no_choice", "choice")),
    y_position = 9.3, tip_length = 0, vjust = 0.2
  )

p_comb_expo+ theme(legend.title = element_blank())

wilcox.test(ww_perc ~ experiment, combined_data_exposed)

combined_data_exposed %>% 
  wilcox_test(ww_perc~ experiment) %>%
  add_significance()


# grazing  TDMC choice no_choice####
ggplot(data = combined_data, 
       aes(x = TDMC,
           y = ww_perc,
           colour=experiment)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ylab("Wet wight change [%]")+
  ggtitle("TDMC vs Grazing for choice/ no choice")

#Plot combined####

p_choise_comb <- ggboxplot(combined_data, x = "ecotype", y = "ww_perc",
          color = "experiment",
          add = "jitter", shape = "experiment", width= 0.15, title = "Choice/no choice for sheltered and exposed")+
  xlab("Environment")+
  ylab("Wet weight change [%]")+
  theme(legend.title = element_blank())
  
p_choise_comb

p <-
  ggplot(mtcars, aes(mpg, wt)) +
  geom_point()
png("mtcars.png")
print(p)
dev.off()

# grazing  SAP choice no_choice####
ggplot(data = combined_data, 
       aes(x = SAP,
           y = ww_perc,
           colour=experiment))+ 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ylab("Wet wight change [%]")+
  ggtitle("SAP vs Grazing for choice/ no choice")

combined_data %>% 
  t_test(SAP~ experiment) %>%
  add_significance()

# grazing  STA choice no_choice####

ggplot(data = combined_data, 
       aes(x = STA,
           y = ww_perc,
           colour=experiment))+ 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  theme_classic()+
  ylab("Wet wight change [%]")+
  ggtitle("STA vs Grazing for choice/ no choice")




