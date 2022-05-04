# BIO451 2022 Group project
# Fucus vesiculosus vs Littorina obtusata/fabalis trials

library(vegan)
library(ggplot2)
library(dplyr)


#############################Dummy data#########################################

#Read google sheets data into R ####
library(googlesheets4)
dummy_data <- read_sheet("https://docs.google.com/spreadsheets/d/1B8JgUlGrqr5kcPiw9d-mjmtuelmGU08LlfG2RV5sLBU/edit#gid=1501238862", range = "DummyData")



#Cal####
weigthchange <- dummy_data$`wetweight_before`- dummy_data$`wetweight_after`
weigthchange
dummy_data$weigthchange <- dummy_data$`wetweight_after` - dummy_data$`wetweight_before`
dummy_data

#############################Controls excluded###################################
# Grazed not grZsed separately #### 
data_ex_c <-dummy_data[dummy_data$treatment == 'grazed', ]
data_c <-dummy_data[dummy_data$treatment == 'not_grazed', ]

#Anova####
boxplot(weigthchange ~ dummy_data$ecotype)
anova(lm(weigthchange ~ data_ex_c$ecotype, data_ex_c))

#?anova
#?aov #use this one

table <- aov(weigthchange ~ data_ex_c$ecotype, dat=data_ex_c)
summary(table)

#Boxplots####
ggboxplot(data_ex_c, x = "ecotype", y = "weigthchange",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype")

ggboxplot(data_c, x = "ecotype", y = "weigthchange",
          color = "ecotype", palette =c("#00AFBB", "#E7B800"),
          add = "jitter", shape = "ecotype")

ggboxplot(dummy_data, x = "ecotype", y = "weigthchange",
          color = "treatment",
          add = "jitter", shape = "treatment")

#PCA####
names(dummy_data)

#dummy_data %>%
 # pivot_longer(cols = c("STA", "SAP","TDMC","PHL"),
  #             names_to = "trait",
   #            values_to = "value")

#view(dummy_data)          	 
#dummy_data_long <-
 # dummy_data %>%
  #pivot_longer(cols = c("STA", "SAP","TDMC","PHL"),
   #            names_to = "trait",
    #           values_to = "value")
#view(dummy_data_long)

#ggplot(data = dummy_data_long,
 #      mapping = aes(x = value)) +
  #geom_histogram() +
  #facet_wrap(~trait, scales = "free") +
  #theme_classic()

ggplot(data = dummy_data,
       mapping = aes(x = SAP, y = TDMC)) +
  geom_point() +
  theme_classic()

ggplot(data = dummy_data,
       mapping = aes(x = SAP, y = TDMC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

str(dummy_data)

ggplot(data = dummy_data,
       mapping = aes(x = ecotype, y = TDMC)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  theme_classic()

#PCA starts####
prcomp(~ TDMC + SAP + PHL + STA , data = dummy_data,
       scale = TRUE)

pca_data <- prcomp(~ TDMC + SAP + PHL + STA , data = dummy_data,
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
  mutate(ecotype = dummy_data$ecotype)

pca_data_scores

ggplot(data = pca_data_scores,
       mapping = aes(x = PC1, y = PC2, colour = ecotype)) +
  geom_point(size = 2) +
  theme_classic()

biplot(pca_data)




#########Bens code#########

#Manova
res.man <- manova(cbind(STA, SAP, TDMC, PHL, weigthchange) ~ ecotype, data = data_ex_c)
summary(res.man)

# PERMANOVA (non parametric)
dummy.permanova <- adonis2(cbind(data_ex_c$STA, data_ex_c$SAP, data_ex_c$TDMC, data_ex_c$weigthchange) ~ data_ex_c$ecotype,
                          permutations = 9999,
                          method="euclidian")
dummy.permanova


ggplot(data = data_ex_c,
       mapping = aes(x = SAP, y = TDMC)) +
  geom_point() +
  theme_classic()

ggplot(data = data_ex_c,
       mapping = aes(x = SAP, y = TDMC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

str(data_ex_c)

ggplot(data = data_ex_c,
       mapping = aes(x = ecotype, y = TDMC)) +
  geom_point() +
  geom_boxplot(width = 0.1) +
  theme_classic()

#PCA starts####
prcomp(~ TDMC + SAP + PHL + STA , data = data_ex_c,
       scale = TRUE)

pca_data <- prcomp(~ TDMC + SAP + PHL + STA , data = data_ex_c,
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
  mutate(ecotype = data_ex_c$ecotype)

pca_data_scores

ggplot(data = pca_data_scores,
       mapping = aes(x = PC1, y = PC2, colour = ecotype)) +
  geom_point(size = 2) +
  theme_classic()

biplot(pca_data)

#hi_nils
#################################################################################