library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer = d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer = factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
d1$Smokes..packs.year.<-as.double(d1$Smokes..packs.year.)
pack_year<-d1[which(d1$Smokes..packs.year.!=0&!is.na(d1$Smokes..packs.year.)),]
ggplot(pack_year, aes(Smokes..packs.year., colour = CervicalCancer)) +
  geom_density() + ggtitle("Density of Smoking Packs per Year") + 
  xlab("Packs per Year")
ggsave("figures/smokes_packs_year_density.png")

