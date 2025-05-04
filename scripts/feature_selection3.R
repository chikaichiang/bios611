library(Boruta)
library(tidyverse)
library(tidyr)
options(warn=-1)
d1<-read.csv("Data/risk_factors.csv")
d1$CervicalCancer = d1$Hinselmann + d1$Schiller + d1$Citology + d1$Biopsy
d1$CervicalCancer = factor(d1$CervicalCancer, levels=c("0","1","2","3","4"))
d1[,c("Hinselmann","Schiller","Citology","Biopsy")] = NULL
set.seed(2023)
boruta_analysis = Boruta(CervicalCancer ~ ., data=d1, maxRuns=200)
dev.new()
png(file="figures/feature_selection3.png",
    width=600, height=350)
plot(boruta_analysis,las=2,main="Variable Importance Diagram 3")
dev.off()
