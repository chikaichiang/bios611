library(tidyverse)
library(tidyr)
library(ggplot2)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer = d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer = factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
d1$Smokes<-as.integer(d1$Smokes)
d1<-d1[which(!is.na(d1$Smokes)),]
a<-as.data.frame(table(d1$Smokes,d1$CervicalCancer))
a1<-as.integer(table(d1$Smokes)[1])
a2<-as.integer(table(d1$Smokes)[2])
a<-data.frame(Smoke=a$Var1,Cervical=a$Var2,Freq=c(a$Freq[1]/a1,a$Freq[2]/a2,a$Freq[3]/a1,a$Freq[4]/a2,a$Freq[5]/a1,
                              a$Freq[6]/a2,a$Freq[7]/a1,a$Freq[8]/a2,a$Freq[9]/a1,a$Freq[10]/a2))
ggplot(a, aes(fill=Cervical, y=Freq, x=Smoke)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() + 
  labs(x='Smoke', y='Frequency', title='Frequency Distribution of Smoke or Not for Having Counts of Tests') +
  scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink','yellow','orange'))
ggsave("figures/smokes_cervical_density.png")
