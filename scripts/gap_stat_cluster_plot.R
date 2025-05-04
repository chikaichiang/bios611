library(factoextra)
library(cluster)
options(warn=-1)
d2<-read.csv("Data/behavorial_risk.csv")
dev.new()
png(file="figures/gap_stat_cluster_plot.png",
    width=600, height=350)
gap_stat <- clusGap(d2,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)
dev.off()
