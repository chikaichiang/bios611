library(FactoMineR)
library(factoextra)
options(warn=-1)
d2<-read.csv("Data/behavorial_risk.csv")
d2$ca_cervix<-as.factor(d2$ca_cervix)
d2.pca <- PCA(d2[,-20], graph = FALSE)
dev.new()
png(file="figures/cluster_by_cervical_pca.png",
    width=600, height=350)
fviz_pca_ind(d2.pca,
             geom.ind = "point", 
             col.ind = d2$ca_cervix, 
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE, 
             legend.title = "Have Cervical Cancer or Not"
)
dev.off()
