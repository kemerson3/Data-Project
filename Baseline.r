library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggbiplot)

baseline <- read_csv("C:\\Users\\kjeme\\OneDrive\\Desktop\\Data Project\\Emerson Microbial Exp1_baseline.csv")
baseline <- select(baseline, -X24) #got rid of this column
for (i in 2:ncol(baseline)) {
  if (str_detect(baseline[1,i],"%")) {
    baseline[,i] <-as.numeric(strsplit(c(baseline[[i]]), "%"))
  }
}
#this loop serves to make sure all data is numerical and not characters

base.c <- baseline %>%
filter(str_detect(ID, "C")) %>%
select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )
##filter looks at rows
##way to seperate out Colonized vs Depleted by finding the C and D in the ID column
##then, you can select specific columns of information in the select column

base.d <- baseline %>%
  filter(str_detect(ID, "D")) %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )


# boxplot(base.c$`Total Distance (mm)`, base.d$`Total Distance (mm)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Av. Speed (mm/s)`, base.d$`Av. Speed (mm/s)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Av. Accel (mm/s^2)`, base.d$`Av. Accel (mm/s^2)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Mobility Rate (%)`, base.d$`Mobility Rate (%)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Exploration Rate (%)`, base.d$`Exploration Rate (%)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Time In Center (m:s)`, base.d$`Time In Center (m:s)`, names = c("Colonized","Depleted"))
# boxplot(base.c$`Tot. Time Frozen (m:s)`, base.d$`Tot. Time Frozen (m:s)`, names = c("Colonized","Depleted"))
# #Box plots for all DVs based on treatment

base.df <- baseline %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )
#gets all of our DVs that we are testing for into one data frame

base.df.ID <- baseline %>%
  select(ID)
#adds another column data frame with the IDs of each individual that corresponds to its numbered row

base.df$`Time In Center (m:s)`<- (as.numeric(base.df$`Time In Center (m:s)`))/3600
base.df$`Tot. Time Frozen (m:s)`<- (as.numeric(base.df$`Tot. Time Frozen (m:s)`))/3600
#now converted into minutes as a real number, was in hms format
#Purpose of making base.df is solely for PCA analysis. Will give you eigenvalues based on where DVs load

pca1 <- prcomp(base.df, center = TRUE, scale. = TRUE)
summary(pca1) 
#gives summary of PCA with StDev and % of variance explained by PCAs
#most variance explained by PCA1, with PCA2 and PCA3 explaining good portion of variance as well

str(pca1)
#pca1 $Rotation gives us our Factor Loading values
#pca1 $x gives us our REGR scores

ggbiplot(pca1)
# ggbiplot(pca1, labels=rownames(base.df.ID$ID)) 

treatment <- c(rep("Colonized", 1),rep("Depleted", 2), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Colonized", 1),
               rep("Depleted", 1))
#creates our values of treatment for each individual 

ggbiplot(pca1, ellipse=TRUE, obs.scale = 1, var.scale = 1, groups = treatment)
# can include ID names instead of just having the dots: labels = (baseline$ID)
# can remove arrows by adding in var.axes=FALSE command
# ggbiplot(pca1, ellipse=TRUE, circle = TRUE, obs.scale = 1, var.scale = 1, var.axes = FALSE, labels, groups = treatment)
         
ggbiplot(pca1, choices = c(2,3), ellipse=TRUE,obs.scale = 1, var.scale = 1, var.axes = FALSE, groups=treatment) +
  scale_colour_manual(name="Origin", values= c("forest green", "dark blue"))+
  ggtitle("Baseline Behavior")+
  theme_minimal()+
  theme(legend.position = "bottom")
#Code to customize our plot
#choices = c(,) is a tool to customize which pca plots you plot against each other

for (i in 1:nrow(baseline)) {
  if(str_detect(baseline$ID[i], "C")) {
    baseline$cat[i] <- "C"
  } else {
      baseline$cat[i] <- "D"
    }
}
baseline$cat <- factor(baseline$cat)

#bartlett.test(baseline$`Total Distance (mm)`~baseline$cat)
#place to run bartlett test

base.anova <- baseline %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, cat )

base.anova$`Time In Center (m:s)`<- (as.numeric(base.anova$`Time In Center (m:s)`))/3600
base.anova$`Tot. Time Frozen (m:s)`<- (as.numeric(base.anova$`Tot. Time Frozen (m:s)`))/3600
#need to get hms into minutes again

pca.value <- pca1$x  
#These appear to be our REGR scores for PCA 1

pca.value1 <- data.frame(pca.value[,1],baseline$cat)
pca.value2 <- data.frame(pca.value[,2],baseline$cat)
pca.value3 <- data.frame(pca.value[,3],baseline$cat)

PCA.aov1 <- aov(pca.value...1.~baseline.cat, data = pca.value1)
summary(PCA.aov1)

PCA.aov2 <- aov(pca.value...2.~baseline.cat, data = pca.value2)
summary(PCA.aov2)

PCA.aov3 <- aov(pca.value...3.~baseline.cat, data = pca.value3)
summary(PCA.aov3)

baselinePCA1_glmm <- glm(pca.value...1.~baseline.cat, data = pca.value1)
Anova(baselinePCA1_glmm)

baselinePCA2_glmm <- glm(pca.value...2.~baseline.cat, data = pca.value2)
Anova(baselinePCA2_glmm)

baselinePCA3_glmm <- glm(pca.value...3.~baseline.cat, data = pca.value3)
Anova(baselinePCA3_glmm)
