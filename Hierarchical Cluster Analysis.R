### Hierarchical Cluster Analysis
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

d_2019C <- dist(X2019_N_CMentalHealth[,c(6:26)], method = "euclidean")
d_2020C <- dist(X2020_N_CMentalHealth[,c(6:26)], method = "manhattan")

### Agglomerative Clustering ---------------------------------------------------

AG_WA_M_2019C <- agnes(X2019_N_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2019Y <- agnes(X2019_YN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2019M <- agnes(X2019_MN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2019E <- agnes(X2019_EN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")

AG_WA_M_2020C <- agnes(X2020_N_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2020Y <- agnes(X2020_YN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2020M <- agnes(X2020_MN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")
AG_WA_M_2020E <- agnes(X2020_EN_CMentalHealth[c(6:26)], method = "ward", metric = "manhattan")

### Dendrogram creation, cutting the tree, and plotting the clusters -----------

## 2019

# 2019 Complete - Agnes

pltree(AG_WA_M_2019C, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2019 Complete")
C19_C <- cutree(AG_WA_M_2019C, k =2) 
X2019_N_CMentalHealth_AgnesClusters <- X2019_N_CMentalHealth %>%
  mutate(cluster = C19_C)
fviz_cluster(list(data = X2019_N_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2019_N_CMentalHealth_AgnesClusters$cluster), 
                  main = "2019 Complete, Agnes")

# 2019 Young - Agnes

pltree(AG_WA_M_2019Y, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2019 Youth")
Y19_C <- cutree(AG_WA_M_2019Y, k =2) 
X2019_YN_CMentalHealth_AgnesClusters <- X2019_YN_CMentalHealth %>%
  mutate(cluster = Y19_C)
fviz_cluster(list(data = X2019_YN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2019_YN_CMentalHealth_AgnesClusters$cluster), 
                  main = "2019 Complete, Agnes")

# 2019 Medium Aged - Agnes

pltree(AG_WA_M_2019M, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2019 Medium Aged")
M19_C <- cutree(AG_WA_M_2019M, k =2) 
X2019_MN_CMentalHealth_AgnesClusters <- X2019_MN_CMentalHealth %>%
  mutate(cluster = M19_C)
fviz_cluster(list(data = X2019_MN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2019_MN_CMentalHealth_AgnesClusters$cluster), 
                  main = "2019 Middle, Agnes")

# 2019 Elder - Agnes

pltree(AG_WA_M_2019E, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2019 Elder")
E19_C <- cutree(AG_WA_M_2019E, k =2) 
X2019_EN_CMentalHealth_AgnesClusters <- X2019_EN_CMentalHealth %>%
  mutate(cluster = E19_C)
fviz_cluster(list(data = X2019_EN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2019_EN_CMentalHealth_AgnesClusters$cluster), 
                  main = "2019 Elder, Agnes")

## 2020 

# 2020 Complete

pltree(AG_WA_M_2020C, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2020 Complete")
C20_C <- cutree(AG_WA_M_2020C, k =2) 
X2020_N_CMentalHealth_AgnesClusters <- X2020_N_CMentalHealth %>%
  mutate(cluster = C20_C)
fviz_cluster(list(data = X2020_N_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2020_N_CMentalHealth_AgnesClusters$cluster), 
                  main = "2020 Complete, Agnes")

# 2020 Young - Agnes

pltree(AG_WA_M_2020Y, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2020 Youth")
Y20_C <- cutree(AG_WA_M_2020Y, k =2) 
X2020_YN_CMentalHealth_AgnesClusters <- X2020_YN_CMentalHealth %>%
  mutate(cluster = Y20_C)
fviz_cluster(list(data = X2020_YN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2020_YN_CMentalHealth_AgnesClusters$cluster), 
                  main = "2020 Young, Agnes")

# 2020 Medium Aged - Agnes

pltree(AG_WA_M_2020M, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2020 Medium Aged")
M20_C <- cutree(AG_WA_M_2020M, k =2) 
X2020_MN_CMentalHealth_AgnesClusters <- X2020_MN_CMentalHealth %>%
  mutate(cluster = M20_C)
fviz_cluster(list(data = X2020_MN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2020_MN_CMentalHealth_AgnesClusters$cluster),
                  main = "2020 Middle, Agnes")

# 2020 Elder - Agnes

pltree(AG_WA_M_2020E, cex = 0.6, hang = -1, main = "Agnes, Dendrogram of 2020 Elder")
E20_C <- cutree(AG_WA_M_2020E, k =2) 
X2020_EN_CMentalHealth_AgnesClusters <- X2020_EN_CMentalHealth %>%
  mutate(cluster = E20_C)
fviz_cluster(list(data = X2020_EN_CMentalHealth_AgnesClusters[6:26], 
                  cluster = X2020_EN_CMentalHealth_AgnesClusters$cluster),
                  main = "2020 Elder, Agnes")

### Divisive Clustering --------------------------------------------------------

DI_M_2019C <- diana(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2019Y <- diana(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2019M <- diana(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2019E <- diana(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)

DI_M_2020C <- diana(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2020Y <- diana(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2020M <- diana(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)
DI_M_2020E <- diana(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan", keep.diss = TRUE)

## 2019
# 2019 Complete

pltree(DI_M_2019C, cex = 0.6, hang = -1, main = "Diana, Dendrogram of 2019 Complete")
C19_C <- cutree(DI_M_2019C, k =2) 
X2019_N_CMentalHealth_DianaClusters <- X2019_N_CMentalHealth %>%
  mutate(cluster = C19_C)
fviz_cluster(list(data = X2019_N_CMentalHealth_DianaClusters[6:26], 
                  cluster = X2019_N_CMentalHealth_DianaClusters$cluster),
             main = "2019 Complete, Diana")

# 2019 Young

pltree(DI_M_2019C, cex = 0.6, hang = -1, main = "Diana, Dendrogram of 2019 Complete")
Y19_C <- cutree(DI_M_2019C, k =2) 
X2019_N_CMentalHealth_DianaClusters <- X2019_N_CMentalHealth %>%
  mutate(cluster = C19_C)
fviz_cluster(list(data = X2019_N_CMentalHealth_DianaClusters[6:26], 
                  cluster = X2019_N_CMentalHealth_DianaClusters$cluster),
             main = "2019 Complete, Diana")
