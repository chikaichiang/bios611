library(tidyverse)
library(tidyr)
library(ggplot2)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
count_Dx1<-as.integer(count(d1[which(d1$Dx.Cancer==1 & d1$Dx.HPV==1 & d1$Dx==1),]))
count_Dx2<-as.integer(count(d1[which(d1$Dx.CIN==1 & d1$Dx==1),]))
count_Dx3<-as.integer(count(d1[which(d1$Dx.Cancer==1 & d1$Dx.HPV==1 & d1$Dx==0),]))
count_Dx4<-as.integer(count(d1[which(d1$Dx.Cancer==1 & d1$Dx.HPV==0 & d1$Dx==1),]))
count_Dx5<-as.integer(count(d1[which(d1$Dx.Cancer==0 & d1$Dx.HPV==1 & d1$Dx==1),]))
count_Dx6<-as.integer(count(d1[which(d1$Dx.Cancer==0 & d1$Dx.HPV==1 & d1$Dx==0),]))
Dx.data <- data.frame(
  Dx = c("Cancer, HPV, Dx","CIN, Dx", "Cancer, HPV", "Cancer, Dx", "HPV, Dx", "Only HPV"), 
  Dx_count = c(count_Dx1,count_Dx2,count_Dx3,count_Dx4,count_Dx5,count_Dx6),
  stringsAsFactors = FALSE
)
dev.new()
png(file="figures/hor_bar_non_std.png",
    width=600, height=350)
ggplot(Dx.data,
       aes(x = Dx_count,
           y = Dx,
           fill=Dx
       )
) +
  geom_col() + xlim(0, 15) +
  labs(title = "Horizontal Barplot of Non Sexually Tranmitted Diseases", x = "Count", y="Disease Type")
dev.off()