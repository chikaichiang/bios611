library(tidyverse)
library(tidyr)
library(ggplot2)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
pos_test<-d1[which(d1$Hinselmann!=0|d1$Schiller!=0|d1$Citology!=0|d1$Biopsy!=0),]
pos_test<-pos_test[ , c(33:36)]
pos1<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==0 &  pos_test$Schiller==0 &  pos_test$Biopsy==0),]))
pos2<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==1 &  pos_test$Schiller==1 &  pos_test$Biopsy==1),]))
pos3<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==0 &  pos_test$Schiller==1 &  pos_test$Biopsy==0),]))
pos4<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==0 &  pos_test$Schiller==1 &  pos_test$Biopsy==1),]))
pos5<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==0 &  pos_test$Schiller==1 &  pos_test$Biopsy==1),]))
pos6<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==1 &  pos_test$Schiller==1 &  pos_test$Biopsy==0),]))
pos7<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==0 &  pos_test$Schiller==0 &  pos_test$Biopsy==1),]))
pos8<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==1 &  pos_test$Schiller==1 &  pos_test$Biopsy==1),]))
pos9<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==0 &  pos_test$Schiller==1 &  pos_test$Biopsy==0),]))
pos10<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==1 &  pos_test$Schiller==1 &  pos_test$Biopsy==0),]))
pos11<-as.integer(count(pos_test[which(pos_test$Citology==0 & pos_test$Hinselmann==1 &  pos_test$Schiller==0 &  pos_test$Biopsy==0),]))
pos12<-as.integer(count(pos_test[which(pos_test$Citology==1 & pos_test$Hinselmann==0 &  pos_test$Schiller==0 &  pos_test$Biopsy==1),]))
pos.data <- data.frame(
  pos = c("Citology","Hinselmann,Schiller,Biopsy","Schiller", "Schiller,Biopsy", "Citology,Schiller,Biopsy", "Hinselmann,Schiller", "Biopsy",
         "Hinselmann,Schiller,Citology,Biopsy","Schiller,Citology","Hinselmann,Schiller,Citology","Hinselmann","Citology,Biopsy"), 
  pos_count = c(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9,pos10,pos11,pos12),
  stringsAsFactors = FALSE
)
dev.new()
png(file="figures/hor_bar_pos_test.png",
    width=600, height=350)
ggplot(pos.data,
       aes(x = pos_count,
           y = pos,
           fill=pos
       )
) +
  geom_col() + xlim(0, 20) +
  labs(title = "Positive Counts of Four Types of Cervical Cancer Test", x = "Count", y="Test Type")
dev.off()