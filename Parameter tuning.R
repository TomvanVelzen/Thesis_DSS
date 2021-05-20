### Parameter tuning

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

###
# WARNING: This file can only be run after the 'Dataprep.R' file is ran.
# WARNING 2: If complete file is run, it takes an extremely long time to calculate it all
# and R will likely crash. Advised to do it in the blocks that were created within the file. 
###

### TUNING AGNES----------------------------------------------------------------

# 2019 Complete
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac     #0.9194918
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac     #0.8642659
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac       #0.7631732  
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "single")$ac       #0.6607152  
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac      #0.8296242  
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "average")$ac      #0.7508582  
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac         #0.9908988  
agnes(X2019_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "ward")$ac         #0.9858833 


# 2019 Young
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.8777838
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "complete")$ac    #0.8111854  
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.7301693
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "single")$ac      #0.6609158
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.8056348
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "average")$ac     #0.7509556
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9705797
agnes(X2019_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "ward")$ac        #0.9561174

# 2019 Middle
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.9089442
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac    #0.8540419  
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.7209503
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "single")$ac      #0.6351276  
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.8513106
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "average")$ac     #0.7436422  
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9873085
agnes(X2019_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "ward")$ac        #0.9788451  

# 2019 Elder
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.9035157
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac    #0.8451655
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.6716469
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "single")$ac      #0.5627521
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.8640962
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "average")$ac     #0.7903559
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9834843
agnes(X2019_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "ward")$ac        #0.9691585

### 2020 Tuning

# 2020 Complete
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac     #0.9241518
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac     #0.8691485
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac       #0.7464864  
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "single")$ac       #0.6575445  
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac      #0.8158136  
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "average")$ac      #0.7793358  
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac         #0.9914388  
agnes(X2020_N_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "ward")$ac         #0.9860565 


# 2020 Young
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.8719844
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "complete")$ac    #0.8027604  
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.6146916
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "single")$ac      #0.5140538
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.7118159
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "average")$ac     #0.691255
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9720634
agnes(X2020_YN_CMentalHealth[c(6:26)], metric = "euclidean" ,method = "ward")$ac        #0.9571269

# 2020 Middle
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.9174408
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac    #0.8613577  
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.7187673
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "single")$ac      #0.6297433  
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.8320044
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "average")$ac     #0.7645341  
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9871279
agnes(X2020_MN_CMentalHealth[c(6:26)], metric = "euclidean", method = "ward")$ac        #0.9810942  

# 2020 Elder
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "complete")$ac    #0.8966161
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "complete")$ac    #0.8272865
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "single")$ac      #0.7605138
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "single")$ac      #0.6586081
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "average")$ac     #0.8241499
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "average")$ac     #0.7656841
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan", method = "ward")$ac        #0.9832057
agnes(X2020_EN_CMentalHealth[c(6:26)], metric = "euclidean", method = "ward")$ac        #0.9723836

### Tuning Diana ---------------------------------------------------------------

# 2019 Complete

diana(X2019_N_CMentalHealth[c(6:26)], metric = "euclidean")$dc  #0.8464404
diana(X2019_N_CMentalHealth[c(6:26)], metric = "manhattan")$dc  #0.9026179

# 2019 Youth

diana(X2019_YN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.7946439
diana(X2019_YN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.8569344

# 2019 Middle Age

diana(X2019_MN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.8350148
diana(X2019_MN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.8917497

# 2019 Elder

diana(X2019_EN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.8279564
diana(X2019_EN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.8853587

# 2020 Complete

diana(X2020_N_CMentalHealth[c(6:26)], metric = "euclidean")$dc  #0.8515878 
diana(X2020_N_CMentalHealth[c(6:26)], metric = "manhattan")$dc  #0.9081461

# 2020 Youth

diana(X2020_YN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.7820028
diana(X2020_YN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.8536246

# 2020 Middle Age

diana(X2020_MN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.8438901
diana(X2020_MN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.9013388

# 2020 Elder

diana(X2020_EN_CMentalHealth[c(6:26)], metric = "euclidean")$dc #0.8070117
diana(X2020_EN_CMentalHealth[c(6:26)], metric = "manhattan")$dc #0.8777106

###K Tuning --------------------------------------------------------------------
## 2019
# 2019 Complete Ward

Ward2019_C <- NbClust(data = X2019_N_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
Ward2019_CY <- NbClust(data = X2019_YN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
Ward2019_CM <- NbClust(data = X2019_MN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
ward2019_CE <- NbClust(data = X2019_EN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")

Ward2019_C_E <- NbClust(data = X2019_N_CMentalHealth[6:26], distance = "euclidean", method = "ward.D2")
Ward2019_CY_E <- NbClust(data = X2019_YN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")
Ward2019_CM_E <- NbClust(data = X2019_MN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")
ward2019_CE_E <- NbClust(data = X2019_EN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")

# 2020 Complete Ward

Ward2020_C <- NbClust(data = X2020_N_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
Ward2020_CY <- NbClust(data = X2020_YN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
Ward2020_CM <- NbClust(data = X2020_MN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")
Ward2020_CE <- NbClust(data = X2020_EN_CMentalHealth[c(6:26)], distance = "manhattan", method = "ward.D2")

Ward2020_C_E <- NbClust(data = X2020_N_CMentalHealth[6:26], distance = "euclidean", method = "ward.D2")
Ward2020_CY_E <- NbClust(data = X2020_YN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")
Ward2020_CM_E <- NbClust(data = X2020_MN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")
ward2020_CE_E <- NbClust(data = X2020_EN_CMentalHealth[c(6:26)], distance = "euclidean", method = "ward.D2")
                                    
# 2019 Complete - Diana Manhatten

#Diana2019_YT1 <- NbClust(data = NULL, diss = DI_M_2019Y$diss, distance = NULL, method = "single", index = "silhouette")
#Diana2019_YT2 <- NbClust(data = YoungPeople_2019[c(6:26)], distance = "manhattan", method = "single", index = "silhouette")
Diana2019_C <- NbClust(data = X2019_N_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2019_Y <- NbClust(data = X2019_YN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2019_M <- NbClust(data = X2019_MN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2019_E <- NbClust(data = X2019_EN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")

Diana2020_C <- NbClust(data = X2020_N_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2020_Y <- NbClust(data = X2020_YN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2020_M <- NbClust(data = X2020_MN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")
Diana2020_E <- NbClust(data = X2020_EN_CMentalHealth[c(6:26)], distance = "manhattan", method = "single")

Diana2019_C

Diana2019_C1 <- fviz_nbclust(X2019_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_N_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2019_C2 <- fviz_nbclust(X2019_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_N_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2019_C3 <- fviz_nbclust(X2019_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_N_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2019_C1, Diana2019_C2, Diana2019_C3, nrow = 1, top =textGrob("'K' for Diana, 2019 Complete"))

Diana2019_Y1 <- fviz_nbclust(X2019_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_YN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2019_Y2 <- fviz_nbclust(X2019_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_YN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2019_Y3 <- fviz_nbclust(X2019_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_YN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2019_Y1, Diana2019_Y2, Diana2019_Y3, nrow = 1, top =textGrob("'K' for Diana, 2019 Youth"))

Diana2019_M1 <- fviz_nbclust(X2019_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_MN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2019_M2 <- fviz_nbclust(X2019_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_MN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2019_M3 <- fviz_nbclust(X2019_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_MN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2019_M1, Diana2019_M2, Diana2019_M3, nrow = 1, top =textGrob("'K' for Diana, 2019 Middle Aged"))


Diana2019_E1 <- fviz_nbclust(X2019_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_EN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2019_E2 <- fviz_nbclust(X2019_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_EN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2019_E3 <- fviz_nbclust(X2019_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2019_EN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2019_E1, Diana2019_E2, Diana2019_E3, nrow = 1, top =textGrob("'K' for Diana, 2019 Elder"))

# 2020 Complete - Diana Manhatten

Diana2020_C1 <- fviz_nbclust(X2020_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_N_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2020_C2 <- fviz_nbclust(X2020_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_N_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2020_C3 <- fviz_nbclust(X2020_N_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_N_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2020_C1, Diana2020_C2, Diana2020_C3, nrow = 1, top =textGrob("'K' for Diana, 2020 Complete"))

Diana2020_Y1 <- fviz_nbclust(X2020_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_YN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2020_Y2 <- fviz_nbclust(X2020_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_YN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2020_Y3 <- fviz_nbclust(X2020_YN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_YN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2020_Y1, Diana2020_Y2, Diana2020_Y3, nrow = 1, top =textGrob("'K' for Diana, 2020 Youth"))

Diana2020_M1 <- fviz_nbclust(X2020_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_MN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2020_M2 <- fviz_nbclust(X2020_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_MN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2020_M3 <- fviz_nbclust(X2020_MN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_MN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2020_M1, Diana2020_M2, Diana2020_M3, nrow = 1, top =textGrob("'K' for Diana, 2020 Middle"))

Diana2020_E1 <- fviz_nbclust(X2020_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_EN_CMentalHealth,"manhattan"), method = "wss", k.max = 10)
Diana2020_E2 <- fviz_nbclust(X2020_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_EN_CMentalHealth,"manhattan"), method = "silhouette", k.max = 10)
Diana2020_E3 <- fviz_nbclust(X2020_EN_CMentalHealth[c(6:26)], FUN = hcut, diss = dist(X2020_EN_CMentalHealth,"manhattan"), method = "gap_stat", nboot = 10, k.max = 10)

gridExtra::grid.arrange(Diana2020_E1, Diana2020_E2, Diana2020_E3, nrow = 1, top =textGrob("'K' for Diana, 2020 Elder"))

#Diana2019_C <- eclust(x = X2019_N_CMentalHealth[c(6:26)], FUNcluster = "diana", hc_metric = "manhattan", nboot = 10)
#Diana2019_Y <- eclust(x = X2019_YN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)
#Diana2019_M <- eclust(x = X2019_MN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)
#Diana2019_E <- eclust(x = X2019_EN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)

# 2020 Complete - Diana Manhatten

#Diana2020_C <- eclust(x = X2020_N_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)
#Diana2020_Y <- eclust(x = X2020_YN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)
#Diana2020_M <- eclust(x = X2020_MN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)
#Diana2020_E <- eclust(x = X2020_EN_CMentalHealth[c(6:26)], hc_metric = "manhattan", FUNcluster = "diana", nboot = 10)



#Diana2019_C$cluster

#Automatic - Kmeans 2019

# 2019 Complete K-means | Euclidean

Kmeans2019_C_E <- NbClust(data = X2019_N_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")
Kmeans2019_CPCA_E <- NbClust(data = compX_2019C[c(1:5)], distance = "euclidean", method = "kmeans")
Kmeans2019_CY_E <- NbClust(data = X2019_YN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")  #13 proposed 2, 6 proposed 3
Kmeans2019_CM_E <- NbClust(data = X2019_MN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")
Kmeans2019_CE_E <- NbClust(data = X2019_EN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")

Kmeans2019_C_E$Best.nc
# 2020 Complete K-means | Euclidean

Kmeans2020_C_E <- NbClust(data = X2020_N_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")
Kmeans2020_CPCA_E <- NbClust(data = compX_2020C[c(1:5)], distance = "euclidean", method = "kmeans")
Kmeans2020_CY_E <- NbClust(data = X2020_YN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")
Kmeans2020_CM_E <- NbClust(data = X2020_MN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")
Kmeans2020_CE_E <- NbClust(data = X2020_EN_CMentalHealth[c(6:26)], distance = "euclidean", method = "kmeans")

#Manual - Kmeans 2020 | Euclidean ----------------------------------------------

# Complete2020
fviz_nbclust(X2020_N_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete, Euclidean")
# ^ 2
fviz_nbclust(X2020_N_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete, Euclidean")
# ^ 10?

# Youth2020
fviz_nbclust(X2020_YN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Youth, Euclidean")     
# ^ 2
fviz_nbclust(X2020_YN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Youth, Euclidean")
# ^ 8 ?

# Medium2020
fviz_nbclust(X2020_MN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Middle, Euclidean")     
# ^ 2
fviz_nbclust(X2020_MN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 50, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Middle, Euclidean")
# ^ 10

#Elder2020
fviz_nbclust(X2020_EN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Elder, Euclidean")     
# ^ 2
fviz_nbclust(X2020_EN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Elder, Euclidean")
# ^ 10

#PCA2020
fviz_nbclust(compX_2020C[1:5], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete PCA, Euclidean")     
# 2
fviz_nbclust(compX_2020C[1:5], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete PCA, Euclidean")
# ^ 1?


# Manual - Kmeans 2019 | Euclidean ---------------------------------------------

# Complete2019
fviz_nbclust(X2019_N_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete, Euclidean")
# ^ 2
fviz_nbclust(X2019_N_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete, Euclidean")
# ^ 3

# Youth2019
fviz_nbclust(X2019_YN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Youth, Euclidean")     
# ^ 2
fviz_nbclust(X2019_YN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Youth, Euclidean")
# ^ 2

# Medium2019
fviz_nbclust(X2019_MN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Middle, Euclidean")     
# ^ 2
fviz_nbclust(X2019_MN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Middle, Euclidean")
# ^ 10

#Elder2019
fviz_nbclust(X2019_EN_CMentalHealth[6:26], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Elder, Euclidean")     
# ^ 2
fviz_nbclust(X2019_EN_CMentalHealth[6:26], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Elder, Euclidean")
# ^ 9

#PCA2019
fviz_nbclust(compX_2019C[1:5], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete PCA, Euclidean")     
# ^ 2
fviz_nbclust(compX_2019C[1:5], kmeans, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete, Euclidean")
# ^ 3

#Manual - PAM 2020 | Manhattan & Euclidean =====================================

# Complete2020
fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2020_N_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete, Manhattan, PAM")
# ^ 2

fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2020 Complete, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2020_N_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2020 Complete, Manhattan, PAM")
#^ 2

fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete, Euclidean, PAM")
# ^ -
fviz_nbclust(X2020_N_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2020_N_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete, Manhattan, PAM")
# ^ -
# Youth2020
fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Youth, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2020_YN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Youth, Manhattan, PAM")     
# ^ 2

fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2020 Youth, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2020_YN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2020 Youth, Manhattan, PAM")
#^ 2

fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Youth, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2020_YN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Youth, Manhattan, PAM")
# ^

# Medium2020
fviz_nbclust(X2020_MN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Middle, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2020_MN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2020_MN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Middle, Manhattan, PAM")     
# ^ 2

fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2020 Middle, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2020_YN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2020 Middle, Manhattan, PAM")
#^ 2

fviz_nbclust(X2020_MN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Middle, Euclidean, PAM")
# ^
fviz_nbclust(X2020_MN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2020_MN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Middle, Manhattan, PAM")
# ^

#Elder2020
fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Elder, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2020_EN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Elder, Manhattan, PAM")     
# ^ 2

fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2020 Elder, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2020_EN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2020 Elder, Manhattan, PAM")
#^ 2

fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Elder, Euclidean, PAM")
# ^ 10?
fviz_nbclust(X2020_EN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2020_EN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Elder, Manhattan, PAM")
# ^ 10?

#PCA2020
fviz_nbclust(compX_2020C[1:5], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete PCA, Euclidean, PAM")     
# ^ 2
fviz_nbclust(compX_2020C[1:5], cluster::pam, method = "wss", diss = dist(compX_2020C[1:5], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2020 Complete PCA, Manhattan, PAM")     
# ^ 2

fviz_nbclust(compX_2020C[1:5], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2020 Complete PCA, Euclidean, PAM")
# ^ 2
fviz_nbclust(compX_2020C[1:5], cluster::pam, method = "silhouette", diss = dist(compX_2020C[1:5], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2020 Complete PCA, Manhattan, PAM")
#^ 2

fviz_nbclust(compX_2020C[1:5], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete PCA, Euclidean, PAM")
# ^ 1
fviz_nbclust(compX_2020C[1:5], cluster::pam, nstart = 25,  diss = dist(compX_2020C[1:5], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2020 Complete PCA, Manhattan, PAM")
# ^ 1

# Manual PAM 2019 Manhattan & Euclidean --------------------------------------------

# Complete2019
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2019_N_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete, Manhattan")     
# ^ 2

fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2019 Complete, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2019_N_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2019 Complete, Manhattan, PAM")
# ^ 2

fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete, Euclidean, PAM")
# ^ 
fviz_nbclust(X2019_N_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2019_N_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete, Manhattan, PAM")
# ^

# Youth2019
fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Youth, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2019_YN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Youth, Manhattan")     
# ^ 2

fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2019 Youth, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2019_YN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2019 Youth, Manhattan, PAM")
#^ 2

fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Youth, Euclidean, PAM")
# ^ -
fviz_nbclust(X2019_YN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2019_YN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Youth, Manhattan, PAM")
# ^ -

# Medium2019
fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Middle, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2019_MN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Middle, Manhattan, PAM")     
# ^ 2

fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2019 Middle, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2019_MN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2019 Middle, Manhattan, PAM")
# ^ 2

fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Middle, Euclidean, PAM")
# ^
fviz_nbclust(X2019_MN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2019_MN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Middle, Manhattan, PAM")
# ^

#Elder2019
fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Elder, Euclidean, PAM")     
# ^ 2
fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, method = "wss", diss = dist(X2019_EN_CMentalHealth[6:26], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Elder, Manhattan, PAM")     
# ^ 2

fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2019 Elder, Euclidean, PAM")
# ^ 2
fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, method = "silhouette", diss = dist(X2019_EN_CMentalHealth[6:26], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2019 Elder, Manhattan, PAM")
# ^ 2

fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Elder, Euclidean, PAM")
# ^ 
fviz_nbclust(X2019_EN_CMentalHealth[6:26], cluster::pam, nstart = 25,  diss = dist(X2019_EN_CMentalHealth[6:26], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Elder, Manhattan, PAM")
# ^

#PCA2019
fviz_nbclust(compX_2019C[1:5], cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete PCA, Euclidean, PAM")     
# ^ 2
fviz_nbclust(compX_2019C[1:5], cluster::pam, method = "wss", diss = dist(compX_2019C[1:5], method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method, 2019 Complete PCA, Manhattan, PAM")     # 2
# ^ 2
fviz_nbclust(compX_2019C[1:5], cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette Method, 2019 Complete PCA, Euclidean, PAM")
# ^ 2
fviz_nbclust(compX_2019C[1:5], cluster::pam, method = "silhouette", diss = dist(compX_2019C[1:5], method = "manhattan")) +
  labs(subtitle = "Silhouette Method, 2019 Complete PCA, Manhattan, PAM")
#^ 2

fviz_nbclust(compX_2019C[1:5], cluster::pam, nstart = 25, method = "gap_stat", nboot = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete PCA, Euclidean, PAM")

fviz_nbclust(compX_2019C[1:5], cluster::pam, nstart = 25,  diss = dist(compX_2019C[1:5], method = "manhattan"),
             method = "gap_stat", nboot = 20, iter.max = 20)+
  labs(subtitle = "Gap statistic method, 2019 Complete PCA, Manhattan, PAM")


