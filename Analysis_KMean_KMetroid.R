summary(X2019_PCA_C)

# Normalization ----------------------------------------------------------------
# Done using scale() function --> https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/scale

# Whole 2019 Dataset
X2019_N_CMentalHealth <- data.frame(scale(X2019_CMentalHealth[,c(6:26)], center = TRUE, scale = TRUE))
X2019_N_CMentalHealth$ID <- X2019_CMentalHealth$ID
X2019_N_CMentalHealth$Gender <- X2019_CMentalHealth$Gender
X2019_N_CMentalHealth$AgeCat <- X2019_CMentalHealth$AgeCat
X2019_N_CMentalHealth$Age <- X2019_CMentalHealth$Age
X2019_N_CMentalHealth$Ch19l178 <- X2019_CMentalHealth$Ch19l178
X2019_N_CMentalHealth <- subset(X2019_N_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_N_CMentalHealth)
summary(X2019_N_CMentalHealth)

# Whole 2020 Dataset
X2020_N_CMentalHealth <- data.frame(scale(X2020_CMentalHealth[,c(6:26)], center = TRUE, scale = TRUE))
X2020_N_CMentalHealth$ID <- X2020_CMentalHealth$ID
X2020_N_CMentalHealth$Gender <- X2020_CMentalHealth$Gender
X2020_N_CMentalHealth$AgeCat <- X2020_CMentalHealth$AgeCat
X2020_N_CMentalHealth$Age <- X2020_CMentalHealth$Age
X2020_N_CMentalHealth$Ch20m178 <- X2020_CMentalHealth$Ch20m178
X2020_N_CMentalHealth <- subset(X2020_N_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2019_N_CMentalHealth)
summary(X2019_N_CMentalHealth)

# 2019 Age Groups
# Yung '19
X2019_YN_CMentalHealth <- data.frame(scale(YoungPeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_YN_CMentalHealth$ID <- YoungPeople_2019$ID
X2019_YN_CMentalHealth$Gender <- YoungPeople_2019$Gender
X2019_YN_CMentalHealth$AgeCat <- YoungPeople_2019$AgeCat
X2019_YN_CMentalHealth$Age <- YoungPeople_2019$Age
X2019_YN_CMentalHealth$Ch19l178 <- YoungPeople_2019$Ch19l178
X2019_YN_CMentalHealth <- subset(X2019_YN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_YN_CMentalHealth)
summary(X2019_YN_CMentalHealth)
# Middle '19
X2019_MN_CMentalHealth <- data.frame(scale(MiddlePeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_MN_CMentalHealth$ID <- MiddlePeople_2019$ID
X2019_MN_CMentalHealth$Gender <- MiddlePeople_2019$Gender
X2019_MN_CMentalHealth$AgeCat <- MiddlePeople_2019$AgeCat
X2019_MN_CMentalHealth$Age <- MiddlePeople_2019$Age
X2019_MN_CMentalHealth$Ch19l178 <- MiddlePeople_2019$Ch19l178
X2019_MN_CMentalHealth <- subset(X2019_MN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_MN_CMentalHealth)
summary(X2019_MN_CMentalHealth)
# Elder '19
X2019_EN_CMentalHealth <- data.frame(scale(OldPeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_EN_CMentalHealth$ID <- OldPeople_2019$ID
X2019_EN_CMentalHealth$Gender <- OldPeople_2019$Gender
X2019_EN_CMentalHealth$AgeCat <- OldPeople_2019$AgeCat
X2019_EN_CMentalHealth$Age <- OldPeople_2019$Age
X2019_EN_CMentalHealth$Ch19l178 <- OldPeople_2019$Ch19l178
X2019_EN_CMentalHealth <- subset(X2019_EN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_EN_CMentalHealth)
summary(X2019_EN_CMentalHealth)

# 2020 Age Groups
# Young '20
X2020_YN_CMentalHealth <- data.frame(scale(YoungPeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_YN_CMentalHealth$ID <- YoungPeople_2020$ID
X2020_YN_CMentalHealth$Gender <- YoungPeople_2020$Gender
X2020_YN_CMentalHealth$AgeCat <- YoungPeople_2020$AgeCat
X2020_YN_CMentalHealth$Age <- YoungPeople_2020$Age
X2020_YN_CMentalHealth$Ch20m178 <- YoungPeople_2020$Ch20m178
X2020_YN_CMentalHealth <- subset(X2020_YN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_YN_CMentalHealth)
summary(X2020_YN_CMentalHealth)
# Middle '19
X2020_MN_CMentalHealth <- data.frame(scale(MiddlePeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_MN_CMentalHealth$ID <- MiddlePeople_2020$ID
X2020_MN_CMentalHealth$Gender <- MiddlePeople_2020$Gender
X2020_MN_CMentalHealth$AgeCat <- MiddlePeople_2020$AgeCat
X2020_MN_CMentalHealth$Age <- MiddlePeople_2020$Age
X2020_MN_CMentalHealth$Ch20m178 <- MiddlePeople_2020$Ch20m178
X2020_MN_CMentalHealth <- subset(X2020_MN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_MN_CMentalHealth)
summary(X2020_MN_CMentalHealth)
# Elder '19
X2020_EN_CMentalHealth <- data.frame(scale(OldPeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_EN_CMentalHealth$ID <- OldPeople_2020$ID
X2020_EN_CMentalHealth$Gender <- OldPeople_2020$Gender
X2020_EN_CMentalHealth$AgeCat <- OldPeople_2020$AgeCat
X2020_EN_CMentalHealth$Age <- OldPeople_2020$Age
X2020_EN_CMentalHealth$Ch20m178 <- OldPeople_2020$Ch20m178
X2020_EN_CMentalHealth <- subset(X2020_EN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_EN_CMentalHealth)
summary(X2020_EN_CMentalHealth)

####################### Principal Component Analysis

# Creating Correlation Matrixes for the different Datasets ---------------------
# Using Corrplot()

# Whole Dataset

Corr_Matr19 <- cor(X2019_N_CMentalHealth[c(6:26)])
Corr_Matr20 <- cor(X2020_N_CMentalHealth[c(6:26)])

corrplot(Corr_Matr19, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019", mar = c(0,0,1,0))
corrplot(Corr_Matr20, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020", mar = c(0,0,1,0))

#Age Groups
Corr_Matr19_Y <- cor(X2019_YN_CMentalHealth[c(6:26)])
Corr_Matr19_M <- cor(X2019_MN_CMentalHealth[c(6:26)])
Corr_Matr19_E <- cor(X2019_EN_CMentalHealth[c(6:26)])
Corr_Matr20_Y <- cor(X2020_YN_CMentalHealth[c(6:26)])
Corr_Matr20_M <- cor(X2020_MN_CMentalHealth[c(6:26)])
Corr_Matr20_E <- cor(X2020_EN_CMentalHealth[c(6:26)])

corrplot(Corr_Matr19_Y, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Young People", mar = c(0,0,1,0))
corrplot(Corr_Matr19_M, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Middle People", mar = c(0,0,1,0))
corrplot(Corr_Matr19_E, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Elder People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_Y, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Young People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_M, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Middle People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_E, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Elder People", mar = c(0,0,1,0))

### Principal Component Analysis -------------------------------------------------
# PCA done using the prcomp() function: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp
# Visualization done with Screeplot() function: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/screeplot

#2019 & 2020 Complete
X2019_PCA_C <- prcomp(X2019_N_CMentalHealth[c(6:26)])
summary(X2019_PCA_C)
X2020_PCA_C <- prcomp(X2020_N_CMentalHealth[c(6:26)])
summary(X2020_PCA_C)

X2019_PCAL_C <- pcomp

#2019 AgeGroups
X2019_PCA_Y <- prcomp(X2019_YN_CMentalHealth[c(6:26)])
X2019_PCA2_Y <- prcomp(X2019_YN_CMentalHealth[c(6:26)]) #Test to see loadings
X2019_PCA_M <- prcomp(X2019_MN_CMentalHealth[c(6:26)])
X2019_PCA_E <- prcomp(X2019_EN_CMentalHealth[c(6:26)])
summary(X2019_PCA_Y)
summary(X2019_PCA2_Y) #Test to see loadings
summary(X2019_PCA_M)
summary(X2019_PCA_E)

#2020 AgeGroups
X2020_PCA_Y <- prcomp(X2020_YN_CMentalHealth[c(6:26)])
X2020_PCA_M <- prcomp(X2020_MN_CMentalHealth[c(6:26)])
X2020_PCA_E <- prcomp(X2020_EN_CMentalHealth[c(6:26)])
summary(X2020_PCA_Y)
summary(X2020_PCA_M)
summary(X2020_PCA_E)

# Screeplots to visualize the PCA Variance Explained
# Source: https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

# 2019 & 2020
screeplot(X2019_PCA_C, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
Cumpro_2019 <- cumsum(X2019_PCA_C$sdev^2 / sum(X2019_PCA_C$sdev^2))
plot(Cumpro_2019[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(h = 0.62, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.6)

screeplot(X2020_PCA_C, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
Cumpro_2020 <- cumsum(X2020_PCA_C$sdev^2 / sum(X2020_PCA_C$sdev^2))
plot(Cumpro_2020[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(v = 3, col= "red", lty=5 )
abline(h = 0.515, col = "red", lty =5)
abline(h = 0.635, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @ PC5", "Cut-off @ PC3"),
       col=c("blue", "red"), lty=5, cex=0.6)


######################## DETERMINING K
#### K MEANS 
## 2019

fviz_nbclust(X2019_N_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(subtitle = "Elbow Method") #2/3

fviz_nbclust(X2019_N_CMentalHealth[6:26], kmeans, method = "silhouette", iter.max = 10)+
  labs(subtitle = "Silhouette method") #2

set.seed(123)
fviz_nbclust(X2019_N_CMentalHealth[6:26], kmeans, nstart = 25,  method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method") #3

## 2020

fviz_nbclust(X2020_N_CMentalHealth[6:26], kmeans, method = "wss", iter.max = 1000) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(subtitle = "Elbow Method") #2/3

fviz_nbclust(X2020_N_CMentalHealth[6:26], kmeans, method = "silhouette", iter.max = 1000)+
  labs(subtitle = "Silhouette method") #2

set.seed(123)
fviz_nbclust(X2020_N_CMentalHealth[6:26], kmeans, nstart = 25,  method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method") #3

#### K METROIDS
## 2019

fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(subtitle = "Elbow Method") #2/3

fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "silhouette", iter.max = 10)+
  labs(subtitle = "Silhouette method") #2

set.seed(123)
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, nstart = 25,  method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method") #3

## 2020

fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(subtitle = "Elbow Method") #2/3

fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "silhouette", iter.max = 1000)+
  labs(subtitle = "Silhouette method") #2

set.seed(123)
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, nstart = 25,  method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method") #3

### Complete


nb2019C_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019C_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019PCA_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019PCA_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019E_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019E_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019M_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019M_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019Y_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2019Y_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")

nb2020C_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020C_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020PCA_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020PCA_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020E_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020E_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020M_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020M_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020Y_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
nb2020Y_W <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")



nb2019C_KM <- NbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")




nbNbClust(X2019_N_CMentalHealth[6:26], method = "kmeans")
NbClust(X2019_EN_CMentalHealth[6:26], method = "kmeans")
NbClust(X2019_MN_CMentalHealth[6:26], method = "kmeans")
NbClust(X2019_YN_CMentalHealth[6:26], method = "kmeans")




NbClust(X2020_N_CMentalHealth[6:26], method = "centroid")

.######################### K MEAN / K METROIDS

#2019 & 2020 Complete
compX_2019C <- data.frame(X2019_PCA_C$x[,1:21],X2019_CMentalHealth$Ch19l178, X2019_CMentalHealth$AgeCat)
plot(compX_2019C, pch=16, col=rgb(0,0,0,0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col =as.integer(compX_2019C$X2019_CMentalHealth.Ch19l178),pch=1)

compX_2020C <- data.frame(X2020_PCA_C$x[,1:21],X2020_CMentalHealth$Ch20m178, X2020_CMentalHealth$AgeCat)

table(compX_2019C$X2019_CMentalHealth.Ch19l178)

#KMeans with 2 Clusters - Whole Dataset
KM_2019C <- kmeans(X2019_N_CMentalHealth[6:26], 6, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019C$cluster, xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019C$cluster)

#Kmetroids with 2 Clusters - Whole Dataset
PAM_2019C <- pam(X2019_N_CMentalHealth[6:26], 2)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col = PAM_2019C$clustering)
table(PAM_2019C$clustering)

#KMeans with 3 Clusters - Whole Dataset
KM_2019C <- kmeans(X2019_N_CMentalHealth[6:26], 3)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019C$cluster, xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")

#Kmetroids with 3 Clusters - Whole Dataset
PAM_2019C <- pam(X2019_N_CMentalHealth[6:26], 3)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col = PAM_2019C$clustering)
table(PAM_2019C$clustering)
PAM_2019C$diss

#KMeans with 2 Clusters - Principal Components - Experimental
KM_2019C <- kmeans(compX_2019C[1:5], 2, nstart=1, iter.max=10000000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019C$cluster, xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(KM_2019C$cluster)

#KMeans with 3 Clusters - Principal Components - Experimental
KM_2019C <- kmeans(compX_2019C[1:21], 3, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col=KM_2019C$cluster, xlab = "", ylab = "", zlab = "")
decorate3d(main = "Principal Components", sub ="2019 Complete", xlab = "PC1", ylab = "PC2", zlab = "PC3")