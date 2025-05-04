library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer = d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer = factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
d1$Smokes..years.<-as.double(d1$Smokes..years.)
smoke_years<-d1[which(d1$Smokes..years.!=0&!is.na(d1$Smokes..years.)),]
ggplot(smoke_years, aes(Smokes..years., colour = CervicalCancer)) +
  geom_density() + ggtitle("Density of Smoking Years") + 
  xlab("Smoke Years")

ggsave("figures/smokes_years_density.png")
