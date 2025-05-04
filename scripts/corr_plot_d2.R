library(corrplot)
options(warn=-1)
d2<-read.csv("Data/behavorial_risk.csv")
colnames(d2)<-c("sex","eat","hygine","aggregate","commit","consist","spont","person","fulfill",
                "vulner","severe","strong","will","emotion","appreci","instrument","know","able","desire",
                "cervical_cancer")
dev.new()
png(file="figures/corr_plot.png",
    width=600, height=350)
corrplot(cor(d2),
         method = "circle",       
         order = "hclust",         
         hclust.method = "ward.D",
         addrect = 2,              
         rect.col = 3,             
         rect.lwd = 3)             
dev.off()

