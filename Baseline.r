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

base.c <- baseline %>%
filter(str_detect(ID, "C")) %>%
select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )
#filter looks at rows
#way to seperate out Colonized vs Depleted by finding the C and D in the ID column
#then, you can select specific columns of information in the select column
base.c.ID <- baseline %>%
  filter(str_detect(ID, "C")) %>%
  select(ID)

base.d <- baseline %>%
  filter(str_detect(ID, "D")) %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time In Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`  )

base.d.ID <- baseline %>%
  filter(str_detect(ID, "D")) %>%
  select(ID)
#boxplot(base.c$`Total Distance (mm)`, base.d$`Total Distance (mm)`, names = c("Colonized","Depleted"))

olfactory <- read_csv("C:\\Users\\kjeme\\OneDrive\\Desktop\\Data Project\\Emerson Microbial Exp1_olfactory.csv")
for (j in 2:ncol(olfactory)) {
  if (str_detect(olfactory[1,i],"%")) {
    olfactory[,i] <-as.numeric(strsplit(c(olfactory[[i]]), "%"))
  }
}
olf.c <- olfactory %>%
  filter(str_detect(ID, "C")) %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time in Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, `Time near cue (m:s)` )

olf.c.ID <- olfactory %>%
  filter(str_detect(ID, "C")) %>%
  select(ID)

olf.d <- olfactory %>%
  filter(str_detect(ID, "D")) %>%
  select(`Av. Speed (mm/s)`, `Total Distance (mm)`, `Av. Accel (mm/s^2)`, `Mobility Rate (%)`, `Exploration Rate (%)`, `Total Distance (mm)`, `Time in Center (m:s)`, `Tot. Time Frozen (m:s)`, `Exploration Rate (%)`, `Time near cue (m:s)` )

olf.d.ID <- olfactory %>%
  filter(str_detect(ID, "D")) %>%
  select(ID)

pca1 <- prcomp(base.c, center = TRUE, scale. = TRUE)
