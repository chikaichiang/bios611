library(tidyverse)
library(tidyr)
library(ggplot2)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer = d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer = factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
d1$Number.of.sexual.partners<-as.integer(d1$Number.of.sexual.partners)
d1<-d1[which(!is.na(d1$Number.of.sexual.partners)),]
a<-as.data.frame.matrix(table(d1$Number.of.sexual.partners,d1$CervicalCancer))
test_data <-data.frame(diag0=a["0"]/sum(a["0"]),diag1=a["1"]/sum(a["1"]),diag2=a["2"]/sum(a["2"]),
                       diag3=a["3"]/sum(a["3"]),diag4=a["4"]/sum(a["4"]),counts=c(1,2,3,4,5,6,7,8,9,10,15,28))
colnames(test_data)<-c("Diag0","Diag1","Diag2","Diag3","Diag4","Counts")
test_data %>%
  gather(test,density,Diag0,Diag1,Diag2,Diag3,Diag4) %>%
  ggplot(aes(x=Counts, y=density, colour=test)) +
  geom_line()
ggsave("figures/partner_cervical_density.png")
