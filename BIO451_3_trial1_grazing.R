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

# Weight change in percent ####
ggboxplot(trial1_data_ex_c, x = "ecotype", xlab = "Environment", y = "ww_perc", 
          ylab = "Weight change [%]", color = "ecotype", add = "jitter", 
          shape = "ecotype", 
          title = "Grazed") + theme(legend.position = "none") + 
  scale_x_discrete(labels=c("Exposed", "Sheltered"))

ggboxplot(trial1_data_c, x = "ecotype", xlab = "Environment", y = "ww_perc", 
          ylab = "Weight change [%]", color = "ecotype", add = "jitter", 
          shape = "ecotype", 
          title = "Baseline") + theme(
            legend.position = "none") + 
  scale_x_discrete(labels=c("Exposed", "Sheltered"))

p_both<- ggboxplot(trial1_data, x = "ecotype", xlab = "Environment", y = "ww_perc", 
          ylab = "Weight change [%]",
          color = "treatment", add = "jitter", shape = "treatment", width= 0.15,
          palette = c("#CCCC00","#0000FF"),
          title = "Both treatments") + theme(legend.title = element_blank()) +
  scale_x_discrete(labels=c("Exposed", "Sheltered"))
p_both

ggsave("COntrolTretment.png")


# Weight change in percent, corrected for growth ####
ggboxplot(trial1_data_ex_c, x = "ecotype", xlab = "Environment", y = "growth_corr",
          ylab = "Weight change [%]",
          color = "ecotype", add = "jitter", shape = "ecotype", 
          title = "Grazed") + theme(
            legend.position = "none") + 
  scale_x_discrete(labels=c("Exposed", "Sheltered"))


##################### Regression traits vs grazing, corrected for growth #######

# Plot STA ####
ggplot(trial1_data_ex_c, aes(STA, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm) +
  xlab("STA") +
  ylab("Wetweight change [%]")

# Plot SAP ####
ggplot(trial1_data_ex_c, aes(SAP, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm) +
  xlab("SAP") +
  ylab("Wetweight change [%]")

# Plot TDMC ####
ggplot(trial1_data_ex_c, aes(TDMC, growth_corr)) +
  geom_point() +
  stat_smooth(method = lm) +
  xlab("TDMC") +
  ylab("Wetweight change [%]")


###################### ANOVA, corrected for growth #############################

anova_growth <- aov(growth_corr ~ trial1_data_ex_c$ecotype, dat=trial1_data_ex_c)
summary(anova_growth)

anova_perc <- aov(ww_perc ~ trial1_data_ex_c$ecotype, dat=trial1_data_ex_c)
summary(anova_perc)


###################### PHL #############################
p_PHL <- ggboxplot(trial1_data, x = "ecotype", y = "PHL",
          color = "treatment",
          add = "jitter", shape = "treatment",width= 0.2,
          title = "Phlorotannins levels grazed & control",
          xlab = "Environment",
          ylab = "Phlorotannins",
          palette = c("#CCCC00","#0000FF"))+
  theme_classic() + 
  scale_x_discrete(labels=c("Exposed", "Sheltered"))+ 
  theme(legend.title = element_blank())
  
p_PHL
ggsave("PHLAll.png")
trial1_data %>% 
  t_test(PHL~ treatment) %>%
  add_significance()


# PHL Before grazing,baseline ####
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

# PHL After grazing ,grazed induced maximum ####

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

# Difference in PHL start and induction####

p_PHL_diff <- ggboxplot(joined, x = "ecotype", y = "PHLdiff",
                         color = "ecotype",
                         add = "jitter", shape = "ecotype", 
                        width= 0.15, title = "Phlorotannins",
                        ylab = "Phlorotannins",
                        xlab = "Environment")+
  theme_classic()+ 
  theme(legend.title = element_blank())
  
  

p_PHL_diff 
ggsave("")
joined%>% 
  t_test(PHLdiff~ecotype) %>% 
  add_significance()

joined%>% 
  wilcox_test(PHLdiff~ecotype) %>% 
  add_significance()

