### RQs ========================================================================

install.packages("clValid")
library(clValid)
###
# WARNING: This is the last codeblock of this thesis.
# in order to run this, all other codeblocks have to run first. 
###

###Elder People?
# 2019
dunn_km_2019e <- dunn(clusters = KM_2019E$cluster, Data = X2019_EN_CMentalHealth[6:26])
dunn_km_2019e    # 0.06839575
dunn_pam_2019e_E <- dunn(clusters = PAM_2019E_E$cluster, Data = X2019_EN_CMentalHealth[6:26])
dunn_pam_2019e_E # 0.1239375
dunn_pam_2019e_M <- dunn(clusters = PAM_2019E_M$cluster, Data = X2019_EN_CMentalHealth[6:26])
dunn_pam_2019e_M # 0.1355383

# 2020
dunn_km_2020e <- dunn(clusters = KM_2020E$cluster, Data = X2020_EN_CMentalHealth[6:26])
dunn_km_2020e    # 0.1201688
dunn_pam_2020e_E <- dunn(clusters = PAM_2020E_E$cluster, Data = X2020_EN_CMentalHealth[6:26])
dunn_pam_2020e_E # 0.1150737
dunn_pam_2020e_M <- dunn(clusters = PAM_2020E_M$cluster, Data = X2020_EN_CMentalHealth[6:26])
dunn_pam_2020e_M # 0.09853756

###Young people?

dunn_km_2019y <- dunn(clusters = KM_2019Y$cluster, Data = X2019_YN_CMentalHealth[6:26])
dunn_km_2019y    # 0.1147986
dunn_pam_2019y_E <- dunn(clusters = PAM_2019Y_E$cluster, Data = X2019_YN_CMentalHealth[6:26])
dunn_pam_2019y_E # 0.1136662
dunn_pam_2019y_M <- dunn(clusters = PAM_2019Y_M$cluster, Data = X2019_YN_CMentalHealth[6:26])
dunn_pam_2019y_M # 0.1065275

dunn_km_2020y <- dunn(clusters = KM_2020Y$cluster, Data = X2020_YN_CMentalHealth[6:26])
dunn_km_2020y    # 0.1679893
dunn_pam_2020y_E <- dunn(clusters = PAM_2020Y_E$cluster, Data = X2020_YN_CMentalHealth[6:26])
dunn_pam_2020y_E # 0.1334417
dunn_pam_2020y_M <- dunn(clusters = PAM_2020Y_M$cluster, Data = X2020_YN_CMentalHealth[6:26])
dunn_pam_2020y_M # 0.1281403

DM_Y2019 <- dist(X2019_YN_CMentalHealth[6:26])
DM_Y2020 <- dist(X2020_YN_CMentalHealth[6:26])

db_km_2019y <- index.DB(X2019_YN_CMentalHealth[6:26], KM_2019Y$cluster, DM_Y2019,)

### Complete
DM_C2019 <- dist(X2019_N_CMentalHealth[6:26])
DM_C2020 <- dist(X2020_N_CMentalHealth[6:26])
DM_PCAC2019 <- dist(compX_2019C[1:5])
DM_PCAC2020 <- dist(compX_2020C[1:5])

dunn_km_2019c <- dunn(clusters = KM_2019C$cluster, Data = X2019_N_CMentalHealth[6:26])
dunn_km_2019c    # 0.08196716
dunn_km_2019pca <- dunn(clusters = KM_2019PC2$cluster, Data = compX_2019C[1:7])
dunn_km_2019pca  # 0.02462523
dunn_pam_2019c_E <- dunn(clusters = PAM_2019C_E$cluster, Data = X2019_N_CMentalHealth[6:26])
dunn_pam_2019c_E # 0.05684513
dunn_pam_2019c_M <- dunn(clusters = PAM_2019C_M$cluster, Data = X2019_N_CMentalHealth[6:26])
dunn_pam_2019c_M # 0.07634567

dunn_km_2020c <- dunn(clusters = KM_2020C$cluster, Data = X2020_N_CMentalHealth[6:26])
dunn_km_2020c    # 0.09781944
dunn_km_2020pca <- dunn(clusters = KM_2020PC2$cluster, Data = compX_2020C[1:7])
dunn_km_2020pca  # 0.04125843
dunn_pam_2020c_E <- dunn(clusters = PAM_2020C_E$cluster, Data = X2020_N_CMentalHealth[6:26])
dunn_pam_2020c_E # 0.09168485
dunn_pam_2020c_M <- dunn(clusters = PAM_2020C_M$cluster, Data = X2020_N_CMentalHealth[6:26])
dunn_pam_2020c_M # 0.08706706


clValid(KM_2019C, nClust = 2, clMethods = "kmeans", validation = "internal")
# Based on Dunn Index, Kmeans will be used to evaluate the Youth. 

# 2019
plot3d(compX_2019CY$PC1, compX_2019CY$PC2, compX_2019CY$PC3, col=KM_2019Y$cluster,xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2019 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019Y$cluster)

### VISUALIZATION --------------------------------------------------------------
## Data preparation
# Youth
VIS_Y_2019_KM <- as.data.frame(compX_2019Y[1:2])
VIS_Y_2019_KM$cluster <- factor(KM_2019Y$cluster)
VIS_Y_2019_KM$Medication <- X2019_YN_CMentalHealth$Ch19l178
VIS_Y_2019_KM$Gender <- X2019_YN_CMentalHealth$Gender
VIS_Y_2019_KM_G1 <- filter(VIS_Y_2019_KM, VIS_Y_2019_KM$cluster == 1)
VIS_Y_2019_KM_G2 <- filter(VIS_Y_2019_KM, VIS_Y_2019_KM$cluster == 2)

VIS_Y_2020_KM <- as.data.frame(compX_2020Y[1:2])
VIS_Y_2020_KM$cluster <- factor(KM_2020Y$cluster)
VIS_Y_2020_KM$Medication <- X2020_YN_CMentalHealth$Ch20m178
VIS_Y_2020_KM$Gender <- X2020_YN_CMentalHealth$Gender
VIS_Y_2020_KM_G1 <- filter(VIS_Y_2020_KM, VIS_Y_2020_KM$cluster == 1)
VIS_Y_2020_KM_G2 <- filter(VIS_Y_2020_KM, VIS_Y_2020_KM$cluster == 2)

# Elderly
VIS_E_2019_KM <- as.data.frame(compX_2019E[,1:2])
VIS_E_2019_KM$cluster <- factor(KM_2019E$cluster)
VIS_E_2019_KM$Medication <- X2019_EN_CMentalHealth$Ch19l178
VIS_E_2019_KM$Gender <- X2019_EN_CMentalHealth$Gender
VIS_E_2019_KM_G1 <- filter(VIS_E_2019_KM, VIS_E_2019_KM$cluster == 1)
VIS_E_2019_KM_G2 <- filter(VIS_E_2019_KM, VIS_E_2019_KM$cluster == 2)

VIS_E_2020_KM <- as.data.frame(compX_2020E[1:2])
VIS_E_2020_KM$cluster <- factor(KM_2020E$cluster)
VIS_E_2020_KM$Medication <- X2020_EN_CMentalHealth$Ch20m178
VIS_E_2020_KM$Gender <- X2020_EN_CMentalHealth$Gender
VIS_E_2020_KM_G1 <- filter(VIS_E_2020_KM, cluster == 1)
VIS_E_2020_KM_G2 <- filter(VIS_E_2020_KM, cluster == 2)

# Complete
VIS_C_2019_KM <- as.data.frame(compX_2019C[1:2])
VIS_C_2019_KM$cluster <- factor(KM_2019C$cluster)
VIS_C_2019_KM$Medication <- X2019_N_CMentalHealth$Ch19l178
VIS_C_2019_KM$Gender <- X2019_N_CMentalHealth$Gender
VIS_C_2019_KM$AgeGroup <- X2019_N_CMentalHealth$AgeCat
VIS_C_2019_KM_G1 <- filter(VIS_C_2019_KM, cluster == 1)
VIS_C_2019_KM_G2 <- filter(VIS_C_2019_KM, cluster == 2)

VIS_C_2020_KM <- as.data.frame(compX_2020C[1:2])
VIS_C_2020_KM$cluster <- factor(KM_2020C$cluster)
VIS_C_2020_KM$Medication <- X2020_N_CMentalHealth$Ch20m178
VIS_C_2020_KM$Gender <- X2020_N_CMentalHealth$Gender
VIS_C_2020_KM$AgeGroup <- X2020_N_CMentalHealth$AgeCat
VIS_C_2020_KM_G1 <- filter(VIS_C_2020_KM, cluster == 1)
VIS_C_2020_KM_G2 <- filter(VIS_C_2020_KM, cluster == 2)

summary(VIS_Y_2020_KM_G1)
summary(VIS_Y_2020_KM_G2)
summary(VIS_C_2020_KM_G1)
summary(VIS_C_2020_KM_G2)
summary(VIS_C_2019_KM_G1)
summary(VIS_C_2019_KM_G2)

table(VIS_C_2020_KM$cluster)
table(VIS_Y_2020_KM$cluster)

## Visualization itself
# Youth 
Y19_SCAT <- ggscatter(
  VIS_Y_2019_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlim = c(-10, 7.5),
  ylim = c(-10, 6)) +
  stat_mean(aes(color = cluster), size = 4)
Y19_SCAT

# Youth 
Y20_SCAT <- ggscatter(
  VIS_Y_2020_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "top", ggtheme = theme_bw(), 
  xlim = c(-10, 7.5),
  ylim = c(-10, 6)) +
  stat_mean(aes(color = cluster), size = 4)
Y20_SCAT

annotate_figure(ggarrange(C19_SCAT, C20_SCAT,  
                          nrow=1, ncol=2, align = "hv", common.legend = TRUE, 
                          legend = "bottom"), 
                top = "Complete Clusters visualized in the PC plots. Left = 2019. Right = 2020.")

table(VIS_Y_2019_KM$cluster)
table(VIS_Y_2020_KM$cluster)

# Elder 
E19_SCAT <- ggscatter(
  VIS_E_2019_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlim = c(-7, 12),
  ylim = c(-7.5,7.5)) +
  stat_mean(aes(color = cluster), size = 4)
E19_SCAT

E20_SCAT <- ggscatter(
  VIS_E_2020_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "right", ggtheme = theme_bw(), 
  xlim = c(-7, 12),
  ylim = c(-7.5,7.5)) +
  stat_mean(aes(color = cluster), size = 4)
E20_SCAT

annotate_figure(ggarrange(E19_SCAT, BIPLOT_E_2019, E20_SCAT, BIPLOT_E_2020, 
                          nrow=2, ncol=2, align = "hv", common.legend = TRUE, 
                          legend = "left"), 
                top = "Clusters visualized in the PC plots. Top = 2019. Bottom = 2020")

table(VIS_E_2019_KM$cluster) # 1(B) - 456, 2 - 1138
table(VIS_E_2020_KM$cluster) # 1(B) - 568, 2 - 1123

# Complete 2019
C19_SCAT <- ggscatter(
  VIS_C_2019_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "right", ggtheme = theme_bw(), 
  xlim = c(-10, 15),
  ylim = c(-10, 10)) +
  stat_mean(aes(color = cluster), size = 4)
C19_SCAT

# Complete 2020
C20_SCAT <- ggscatter(
  VIS_C_2020_KM, x = "PC1", y = "PC2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Medication", size = 1.5,  legend = "right", ggtheme = theme_bw(), 
  xlim = c(-15, 10),
  ylim = c(-10, 10)) +
  stat_mean(aes(color = cluster), size = 4)
C20_SCAT

VIS_C_2019_KM_M <- filter(VIS_Y_2019_KM, VIS_Y_2019_KM$Gender == "Male")
VIS_C_2019_KM_F <- filter(VIS_Y_2019_KM, VIS_Y_2019_KM$Gender == "Female")
VIS_C_2020_KM_M <- filter(VIS_Y_2020_KM, VIS_Y_2020_KM$Gender == "Male")
VIS_C_2020_KM_F <- filter(VIS_Y_2020_KM, VIS_Y_2020_KM$Gender == "Female")


annotate_figure(ggarrange(C19_SCAT, BIPLOT2019, C20_SCAT, BIPLOT2020, 
                          nrow=2, ncol=2, align = "hv", common.legend = TRUE, 
                          legend = "left"), 
                top = "Clusters visualized in the PC plots. Top = 2019. Bottom = 2020")

table(VIS_C_2019_KM$cluster) # 1(B) - 1399 (30.5%), 2 - 3188 (69.5%)
table(VIS_C_2020_KM$cluster) # 1(B) - 1567 (30.8%), 2 - 3520 (69.2%)

# 2020
plot3d(compX_2020CY$PC1, compX_2020CY$PC2, compX_2020CY$PC3, col=c(KM_2020Y$cluster, X2019_YN_CMentalHealth$Ch19l178),xlab = "", ylab = "", zlab = "")
decorate3d(main = "2 Cluster Kmeans", sub ="2020 Youth", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2020Y$cluster)




autoplot(KM_2019C, data = X2019_YN_CMentalHealth, col = KM_2019Y$cluster)
