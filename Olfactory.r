library(readr)
library(tidyr)
library(dplyr)
library(stringr)

baseline <- read_csv("C:\\Users\\kjeme\\OneDrive\\Desktop\\Data Project\\Emerson Microbial Exp1_olfactory.csv")
baseline <- select(baseline, -X24) #got rid of this column
for (i in 2:ncol(baseline)) {
  if (str_detect(baseline[1,i],"%")) {
    baseline[,i] <-as.numeric(strsplit(c(baseline[,i]), "%"))
  }
}

base.c <- baseline %>%
  filter(str_detect(ID, "C")) %>%
  select(ID, `Total Distance (mm)`)

base.d <- baseline %>%
  filter(str_detect(ID, "D")) %>%
  select(ID, `Total Distance (mm)`)

boxplot(base.c$`Total Distance (mm)`, base.d$`Total Distance (mm)`, names = c("Colonized","Depleted"))
