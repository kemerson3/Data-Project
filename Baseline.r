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

#base.c <- baseline %>%
#filter(str_detect(ID, "C")) %>%
#select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )
##filter looks at rows
##way to seperate out Colonized vs Depleted by finding the C and D in the ID column
##then, you can select specific columns of information in the select column

# base.c.ID <- baseline %>%
#   filter(str_detect(ID, "C")) %>%
#   select(ID)
#We dont want ID column involved in the analysis, so I removed it from base.c and made it its own vector for labeling later

# base.d <- baseline %>%
#   filter(str_detect(ID, "D")) %>%
#   select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )
# 
# base.d.ID <- baseline %>%
#   filter(str_detect(ID, "D")) %>%
#   select(ID)

#boxplot(base.c$`Total Distance (mm)`, base.d$`Total Distance (mm)`, names = c("Colonized","Depleted"))


base.df <- baseline %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )

base.df.ID <- baseline %>%
  select(ID)

# olfactory <- read_csv("C:\\Users\\kjeme\\OneDrive\\Desktop\\Data Project\\Emerson Microbial Exp1_olfactory.csv")
# for (j in 2:ncol(olfactory)) {
#   if (str_detect(olfactory[1,i],"%")) {
#     olfactory[,i] <-as.numeric(strsplit(c(olfactory[[i]]), "%"))
#   }
# }
# olf.c <- olfactory %>%
#   filter(str_detect(ID, "C")) %>%
#   select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time in Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, `Time near cue (m:s)` )
# 
# olf.c.ID <- olfactory %>%
#   filter(str_detect(ID, "C")) %>%
#   select(ID)
# 
# olf.d <- olfactory %>%
#   filter(str_detect(ID, "D")) %>%
#   select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time in Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, `Time near cue (m:s)` )
# 
# olf.d.ID <- olfactory %>%
#   filter(str_detect(ID, "D")) %>%
#   select(ID)

base.df$`Time In Center (m:s)`<- (as.numeric(base.df$`Time In Center (m:s)`))/3600
base.df$`Tot. Time Frozen (m:s)`<- (as.numeric(base.df$`Tot. Time Frozen (m:s)`))/3600
#now converted into decimal points instead of time

pca1 <- prcomp(base.df, center = TRUE, scale. = TRUE)
summary(pca1)
str(pca1)
ggbiplot(pca1)
ggbiplot(pca1, labels=rownames(base.df.ID$ID)) 

#Need to get the table correct, be able to add the circles and run an ANOVA
#Can distill into PCAs for each test or combine, but need to get in a format where I can test treatment effect

treatment <- c(rep("Colonized", 1),rep("Depleted", 2), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1),
               rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Depleted", 1), rep("Colonized", 1), rep("Colonized", 1),
               rep("Depleted", 1))
ggbiplot(pca1, ellipse=TRUE, circle = TRUE, obs.scale = 1, var.scale = 1, labels = (baseline$ID), groups = treatment)

# can remove arrows by adding in var.axes=FALSE command
# ggbiplot(pca1, ellipse=TRUE, circle = TRUE, obs.scale = 1, var.scale = 1, var.axes = FALSE, labels = rownames(base.df.ID), groups = treatment)
         
ggbiplot(pca1,ellipse=TRUE,obs.scale = 1, var.scale = 1, var.axes = TRUE, groups=treatment) +
  scale_colour_manual(name="Origin", values= c("forest green", "dark blue"))+
  ggtitle("Baseline Behavior")+
  theme_minimal()+
  theme(legend.position = "bottom")
#Code to customize our plot

for (i in 1:nrow(baseline)) {
  if(str_detect(baseline$ID[i], "C")) {
    baseline$cat[i] <- "C"
  } else {
      baseline$cat[i] <- "D"
    }
}
baseline$cat <- factor(baseline$cat)
base.anova <- baseline %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, cat )
  
base.anova$`Time In Center (m:s)`<- (as.numeric(base.anova$`Time In Center (m:s)`))/3600
base.anova$`Tot. Time Frozen (m:s)`<- (as.numeric(base.anova$`Tot. Time Frozen (m:s)`))/3600

pca.value <- pca1$x  
pca.value1 <- data.frame(pca.value[,1],baseline$cat)
pca.value2 <- data.frame(pca.value[,2],baseline$cat)

PCA.aov1 <- aov(pca.value...1.~baseline.cat, data = pca.value1)
summary(PCA.aov1)

PCA.aov2 <- aov(pca.value...2.~baseline.cat, data = pca.value2)
summary(PCA.aov2)
