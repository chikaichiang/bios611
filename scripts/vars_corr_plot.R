library(FactoMineR)
library(factoextra)
options(warn=-1)
d2<-read.csv("Data/behavorial_risk.csv")
dev.new()
png(file="figures/variables_correlation_plot.png",
    width=600, height=350)
res.pca <- PCA(d2[,-20], graph = FALSE)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
dev.off()