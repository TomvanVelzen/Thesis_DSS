### Partitional Clustering 

library(mice)
library(mitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(kernlab)
library(data.table)
library(foreign)
library(factoextra)
library(VIM)
library(corrplot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(NbClust)
library(rgl)
install.packages("a")
library(RColorBrewer)
library(scales)

###
# WARNING: This file can only be run after the 'ThesisDataPrep.R' file is ran.
# Furthermore: Run this code in blocks in order to interpret to different results. 
###

### Kmeans =====================================================================

################################## 2019 ########################################

## 2019 K-means - Complete - Euclidean -----------------------------------------
KM_2019C <- kmeans(X2019_N_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019C$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019C$cluster)

## 2019 K-means - Youth - Euclidean --------------------------------------------
KM_2019Y <- kmeans(X2019_YN_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(8,'Set1'), 0.5))
plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=KM_2019Y$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019Y$cluster)

## 2019 K-means - Middle - Euclidean --------------------------------------------
KM_2019M <- kmeans(X2019_MN_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CM$PC1, compX_2019CM$PC2, compX_2019CM$PC3, col=KM_2019M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Middle Age", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019M$cluster)

## 2019 K-means - Elder - Euclidean --------------------------------------------
KM_2019E <- kmeans(X2019_EN_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CE$PC1, compX_2019CE$PC2, compX_2019CE$PC3, col=KM_2019E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Elder", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019E$cluster)

## 2019 K-means - PCA - Euclidean --------------------------------------------
KM_2019PC2 <- kmeans(compX_2019C[1:7], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019PC2$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Principal Components Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019PC2$cluster)

KM_2019PC3 <- kmeans(compX_2019C[1:7], 3, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019PC3$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "3 Cluster Kmeans", sub ="2019 Principal Components Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019Y$cluster)

################################## 2020 ########################################

## 2020 K-means - Complete - Euclidean -----------------------------------------
KM_2020C <- kmeans(X2020_N_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(7,'Set1'), 213))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=KM_2020C$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020C$cluster)

## 2020 K-means - Youth - Euclidean --------------------------------------------
KM_2020Y <- kmeans(X2020_YN_CMentalHealth[6:26], 2, nstart=25, iter.max=100000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=KM_2020Y$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020Y$cluster)

## 2020 K-means - Middle - Euclidean --------------------------------------------
KM_2020M <- kmeans(X2020_MN_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CM$PC1, compX_2020CM$PC2, compX_2020CM$PC3, col=KM_2020M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020M$cluster)

## 2020 K-means - Elder - Euclidean --------------------------------------------
KM_2020E <- kmeans(X2020_EN_CMentalHealth[6:26], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CE$PC1, compX_2020CE$PC2, compX_2020CE$PC3, col=KM_2020E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020E$cluster)

## 2020 K-means - PCA - Euclidean ----------------------------------------------
KM_2020PC2 <- kmeans(compX_2020C[1:7], 2, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=KM_2020PC2$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020PC2$cluster)

KM_2020PC3 <- kmeans(compX_2020C[1:5], 3, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=KM_2020PC3$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020PC3$cluster)

### PAM Algorithm ==============================================================

################################## 2019 ########################################

## 2019 PAM - Complete - Euclidean ---------------------------------------------
PAM_2019C_E <- pam(X2019_N_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019C_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019C_E$cluster)

## 2019 PAM - Complete - Manhattan ---------------------------------------------
PAM_2019C_M <- pam(X2019_N_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019C_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019C_M$cluster)



## 2019 PAM - Youth - Euclidean ------------------------------------------------
PAM_2019Y_E <- pam(X2019_YN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=PAM_2019Y_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019Y_E$cluster)

## 2019 PAM - Youth - Manhattan ------------------------------------------------
PAM_2019Y_M <- pam(X2019_YN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=PAM_2019Y_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019Y_M$cluster)



## 2019 PAM - Middle - Euclidean -----------------------------------------------
PAM_2019M_E <- pam(X2019_MN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CM$PC1, compX_2019CM$PC2, compX_2019CM$PC3, col=PAM_2019M_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Middle Aged", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019M_E$cluster)

## 2019 PAM - Middle - Manhattan -----------------------------------------------
PAM_2019M_M <- pam(X2019_MN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CM$PC1, compX_2019CM$PC2, compX_2019CM$PC3, col=PAM_2019M_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Middle Aged", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019M_M$cluster)



## 2019 PAM - Elder - Euclidean ------------------------------------------------
PAM_2019E_E <- pam(X2019_EN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CE$PC1, compX_2019CE$PC2, compX_2019CE$PC3, col=PAM_2019E_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Elder", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019E_E$cluster)

## 2019 PAM - Elder - Manhattan ------------------------------------------------
PAM_2019E_M <- pam(X2019_EN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019CE$PC1, compX_2019CE$PC2, compX_2019CE$PC3, col=PAM_2019E_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Elder", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019E_M$cluster)



## 2019 PAM - PCA - Euclidean - K = 2 ------------------------------------------
PAM_2019PC2_E <- pam(compX_2019C[1:5], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019PC2_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019PC2_E$cluster)

## 2019 PAM - PCA - Euclidean - K = 3 ------------------------------------------
PAM_2019PC3_E <- pam(compX_2019C[1:5], 3, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019PC3_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019PC3_E$cluster)

## 2019 PAM - PCA - Manhattan - K = 2 ------------------------------------------
PAM_2019PC2_M <- pam(compX_2019C[1:5], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019PC2_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019PC2_M$cluster)

## 2019 PAM - PCA - Manhattan - K = 3 ------------------------------------------
PAM_2019PC3_M <- pam(compX_2019C[1:5], 3, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019PC3_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019PC3_M$cluster)

################################## 2020 ########################################

## 2020 PAM - Complete - Euclidean ---------------------------------------------
PAM_2020C_E <- pam(X2020_N_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020C_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020C_E$cluster)

## 2020 PAM - Complete - Manhattan ---------------------------------------------
PAM_2020C_M <- pam(X2020_N_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020C_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020C_M$cluster)



## 2020 PAM - Youth - Euclidean ------------------------------------------------
PAM_2020Y_E <- pam(X2020_YN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=PAM_2020Y_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020Y_E$cluster)

## 2020 PAM - Youth - Manhattan ------------------------------------------------
PAM_2020Y_M <- pam(X2020_YN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=PAM_2020Y_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020Y_M$cluster)



## 2020 PAM - Middle - Euclidean -----------------------------------------------
PAM_2020M_E <- pam(X2020_MN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CM$PC1, compX_2020CM$PC2, compX_2020CM$PC3, col=PAM_2020M_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 Middle Aged", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020M_E$cluster)

## 2020 PAM - Middle - Manhattan -----------------------------------------------
PAM_2020M_M <- pam(X2020_MN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CM$PC1, compX_2020CM$PC2, compX_2020CM$PC3, col=PAM_2020M_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 Middle Aged", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020M_M$cluster)



## 2020 PAM - Elder - Euclidean ------------------------------------------------
PAM_2020E_E <- pam(X2020_EN_CMentalHealth[6:26], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CE$PC1, compX_2020CE$PC2, compX_2020CE$PC3, col=PAM_2020E_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 Elder", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020E_E$cluster)

## 2020 PAM - Elder - Manhattan ------------------------------------------------
PAM_2020E_M <- pam(X2020_EN_CMentalHealth[6:26], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020CE$PC1, compX_2020CE$PC2, compX_2020CE$PC3, col=PAM_2020E_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 Elder", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020E_M$cluster)



## 2020 PAM - PCA - Euclidean - K = 2 ------------------------------------------
PAM_2020PC2_E <- pam(compX_2020C[1:5], 2, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020PC2_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 PC's Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020PC2_E$cluster)

## 2020 PAM - PCA - Euclidean - K = 3 ------------------------------------------
PAM_2020PC3_E <- pam(compX_2020C[1:5], 3, nstart=25, metric = "euclidean")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020PC3_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 PC's Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020PC3_E$cluster)

## 2020 PAM - PCA - Manhattan - K = 2 ------------------------------------------
PAM_2020PC2_M <- pam(compX_2020C[1:5], 2, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020PC2_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 PC's Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020PC2_M$cluster)

## 2020 PAM - PCA - Manhattan - K = 3 ------------------------------------------
PAM_2020PC3_M <- pam(compX_2020C[1:5], 3, nstart=25, metric = "manhattan")
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2020C$PC1, compX_2020C$PC2, compX_2020C$PC3, col=PAM_2020PC3_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "3 Cluster PAM, Manhattan", sub ="2020 PC's Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020PC3_M$cluster)



### Rotating 3D Object ---------------------------------------------------------
# https://2-bitbio.com/2017/04/animated-3d-pca-plots-in-r.html

dir.create("animation_merge")  

for (i in 1:360) {
  view3d(userMatrix=rotationMatrix(2*pi * i/360, 0, 1, 0))
  rgl.snapshot(filename=paste("animation_merge/frame-",
                              sprintf("%03d", i), ".png", sep=""))
}

### RQs ========================================================================

###Young people?

# 2019
plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=KM_2019Y$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019Y$cluster)

plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=PAM_2019PC3_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019PC3_E$cluster)

plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=PAM_2019Y_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2019Y_M$cluster)

# 2020
plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=KM_2020Y$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020Y$cluster)

plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=PAM_2020Y_E$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Euclidean", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020Y_E$cluster)

plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=PAM_2020Y_M$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster PAM, Manhattan", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(PAM_2020Y_M$cluster)


KM_2020Y
KM_2019Y