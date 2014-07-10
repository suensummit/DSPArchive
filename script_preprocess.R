# ���ܤu�@�ؿ�
setwd("D:/SUEN_SUMMIT/DataSci_camp/ProjectData/DSP_cleaned/")

# Ū csv �ɮ�
priceData <- read.csv("f001-big5.csv", header = TRUE)

# (�w��)�I�s�j�jø�ϮM��
# install.packages("ggplot2")
library(ggplot2)

priceData2 <- priceData

# �ܼƭ��s�s�X�G��/�L => 1/0
priceData2$�ت��{�p�槽.�j�� <- gsub("��", 1, priceData2$�ت��{�p�槽.�j��)
priceData2$�ت��{�p�槽.�j�� <- gsub("�L", 0, priceData2$�ت��{�p�槽.�j��)

# �ܼƭ��s�s�X�G��/�L => 1/0
priceData2$���L�޲z��´ <- gsub("��", 1, priceData2$���L�޲z��´)
priceData2$���L�޲z��´ <- gsub("�L", 0, priceData2$���L�޲z��´)

# �������~��2010�~�����ơ]�Ӥ[�H�e����ƷǽT�ʸ��C�^
priceData2 <- subset(priceData2, priceData2$�~�� > 2010)

# ����n�i����s���ܼơ]���^
priceData2 <- priceData2[c("������ɼ�_�g�a", "������ɼ�_�ت�", "������ɼ�_����", "����h��", "�`�Ӽh��", "�ت����A", "�ت��{�p�槽.��", "�ت��{�p�槽.�U", "�ت��{�p�槽.��", "�ت��{�p�槽.�j��", "���L�޲z��´", "����ɫ���", "���.��.�W", "�g�a�����`���n.�W", "�ت������`���n.�W", "����Ъ����", "����Ъ��a����", "Longitude", "Latitude")]


priceData2 <- subset(priceData2, ������ɼ�_�g�a > 0 & ������ɼ�_�ت� > 0)

selectSet <- table(priceData2$����h��)
selectSet <- priceData2$����h�� %in% names(selectSet[selectSet > 50])
priceData2 <- subset(priceData2, selectSet)
priceData2$����h�� <- gsub("�Q�@�h", 11, priceData2$����h��)
priceData2$����h�� <- gsub("�Q�G�h", 12, priceData2$����h��)
priceData2$����h�� <- gsub("�Q�T�h", 13, priceData2$����h��)
priceData2$����h�� <- gsub("�Q�|�h", 14, priceData2$����h��)
priceData2$����h�� <- gsub("�Q���h", 15, priceData2$����h��)
priceData2$����h�� <- gsub("�Q���h", 16, priceData2$����h��)
priceData2$����h�� <- gsub("�@�h�A���x", 1, priceData2$����h��)
priceData2$����h�� <- gsub("�@�h�A�M��", 1, priceData2$����h��)
priceData2$����h�� <- gsub("�G�h�A���x", 2, priceData2$����h��)
priceData2$����h�� <- gsub("�T�h�A���x", 3, priceData2$����h��)
priceData2$����h�� <- gsub("�|�h�A���x", 4, priceData2$����h��)
priceData2$����h�� <- gsub("�a�U�@�h", -1, priceData2$����h��)
priceData2$����h�� <- gsub("�a�U�h", -1, priceData2$����h��)
priceData2$����h�� <- gsub("��", 0, priceData2$����h��)
priceData2$����h�� <- gsub("�@�h", 1, priceData2$����h��)
priceData2$����h�� <- gsub("�G�h", 2, priceData2$����h��)
priceData2$����h�� <- gsub("�T�h", 3, priceData2$����h��)
priceData2$����h�� <- gsub("�|�h", 4, priceData2$����h��)
priceData2$����h�� <- gsub("���h", 5, priceData2$����h��)
priceData2$����h�� <- gsub("���h", 6, priceData2$����h��)
priceData2$����h�� <- gsub("�C�h", 7, priceData2$����h��)
priceData2$����h�� <- gsub("�K�h", 8, priceData2$����h��)
priceData2$����h�� <- gsub("�E�h", 9, priceData2$����h��)
priceData2$����h�� <- gsub("�Q�h", 10, priceData2$����h��)

selectSet <- table(priceData2$�ت����A)
selectSet <- priceData2$�ت����A %in% names(selectSet[selectSet > 300])
priceData2 <- subset(priceData2, selectSet)
priceData2$�ت����A <- gsub("�M��\\(1��1�U1��\\)", 1, priceData2$�ت����A)
priceData2$�ت����A <- gsub("���J\\(5�ӧt�H�U�L�q��\\)", 2, priceData2$�ت����A)
priceData2$�ت����A <- gsub("�طH\\(10�h�t�H�U���q��\\)", 3, priceData2$�ت����A)
priceData2$�ت����A <- gsub("���v�j��\\(11�h�t�H�W���q��\\)", 4, priceData2$�ت����A)
priceData2$�ت����A <- gsub("�z�ѭ�", 5, priceData2$�ت����A)

priceData2[is.na(priceData2)] <- 0

priceTotss <- c()
priceTotwithinss <- c()
priceBetweenss <- c()
for(i in 5:100) {
  priceCluster <- kmeans(priceData2, i)
  priceTotss[i] <- priceCluster$totss
  priceTotwithinss[i] <- priceCluster$tot.withinss
  priceBetweenss[i] <- priceCluster$betweenss
}
qplot(c(1:100), priceTotwithinss)

priceCluster <- kmeans(priceData2, 30)
qplot(x=priceData2$����Ъ����, y=priceData2$����Ъ��a����, col=factor(priceCluster$cluster)) + geom_point(size=2)
priceCenters <- data.frame(priceCluster$centers)
qplot(x=priceCenters$Longitude, y=priceCenters$Latitude, col=factor(1:30)) + geom_point(size=5)
qplot(x=priceCenters$����Ъ����, y=priceCenters$����Ъ��a����, col=factor(1:30)) + geom_point(size=5)

# library(ggmap)
# 
# df <- data.frame(x=priceCenters$Longitude, y=priceCenters$Latitude)
# map <- get_googlemap(center = c(lon = 121.518303, lat = 25.045248), zoom = 13, markers = df, scale = 2)
# ggmap(map, extent = 'normal')

for(i in 1:30){
clus <- subset(priceData2, priceCluster$cluster == i)
qplot(clus$���)
ggsave(filename=paste("priceDist_cluster_",i,".png",sep=""))
}