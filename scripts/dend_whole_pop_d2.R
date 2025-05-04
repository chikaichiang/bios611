library(factoextra)
library(dendextend)
options(warn=-1)
d2<-read.csv("Data/behavorial_risk.csv")
dd <- dist(scale(d2), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
dev.new()
png(file="figures/dend_whole_pop.png",
    width=600, height=350)
fviz_dend(hc, k = 9,                 
          cex = 0.5,              
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#00BF76","#E79C20","#FC4C05","#2E9D4F","#2E9FDE"),
          color_labels_by_k = TRUE,  
          ggtheme = theme_gray()     
)
dev.off()
