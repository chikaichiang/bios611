library(Boruta)
library(tidyverse)
library(tidyr)
d2<-read.csv("Data/behavorial_risk.csv")
colnames(d2)<-c("sex","eat","hygine","aggregate","commit","consist","spont","person","fulfill",
                "vulner","severe","strong","will","emotion","appreci","instrument","know","able","desire",
                "cervical_cancer")
set.seed(2023)
boruta_analysis = Boruta(cervical_cancer ~ ., data=d2, maxRuns=200)
dev.new()
png(file="figures/feature_selection_d2.png",
    width=600, height=350)
plot(boruta_analysis,las=2,main="Variable Importance Diagram for Dataset 2")
dev.off()
