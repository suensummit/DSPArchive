# 改變工作目錄
setwd("D:/SUEN_SUMMIT/DataSci_camp/ProjectData/DSP_cleaned/")

# 讀 csv 檔案
priceData <- read.csv("f001-big5.csv", header = TRUE)

# (安裝)呼叫強大繪圖套件
# install.packages("ggplot2")
library(ggplot2)

priceData2 <- priceData

# 變數重新編碼：有/無 => 1/0
priceData2$建物現況格局.隔間 <- gsub("有", 1, priceData2$建物現況格局.隔間)
priceData2$建物現況格局.隔間 <- gsub("無", 0, priceData2$建物現況格局.隔間)

# 變數重新編碼：有/無 => 1/0
priceData2$有無管理組織 <- gsub("有", 1, priceData2$有無管理組織)
priceData2$有無管理組織 <- gsub("無", 0, priceData2$有無管理組織)

# 選取交易年分2010年之後資料（太久以前的資料準確性較低）
priceData2 <- subset(priceData2, priceData2$年份 > 2010)

# 選取要進行分群的變數（欄位）
priceData2 <- priceData2[c("交易筆棟數_土地", "交易筆棟數_建物", "交易筆棟數_車位", "移轉層次", "總樓層數", "建物型態", "建物現況格局.房", "建物現況格局.廳", "建物現況格局.衛", "建物現況格局.隔間", "有無管理組織", "成交時屋齡", "單價.元.坪", "土地移轉總面積.坪", "建物移轉總面積.坪", "交易標的橫坐標", "交易標的縱坐標", "Longitude", "Latitude")]


priceData2 <- subset(priceData2, 交易筆棟數_土地 > 0 & 交易筆棟數_建物 > 0)

selectSet <- table(priceData2$移轉層次)
selectSet <- priceData2$移轉層次 %in% names(selectSet[selectSet > 50])
priceData2 <- subset(priceData2, selectSet)
priceData2$移轉層次 <- gsub("十一層", 11, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十二層", 12, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十三層", 13, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十四層", 14, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十五層", 15, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十六層", 16, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("一層，平台", 1, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("一層，騎樓", 1, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("二層，陽台", 2, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("三層，陽台", 3, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("四層，陽台", 4, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("地下一層", -1, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("地下層", -1, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("全", 0, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("一層", 1, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("二層", 2, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("三層", 3, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("四層", 4, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("五層", 5, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("六層", 6, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("七層", 7, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("八層", 8, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("九層", 9, priceData2$移轉層次)
priceData2$移轉層次 <- gsub("十層", 10, priceData2$移轉層次)

selectSet <- table(priceData2$建物型態)
selectSet <- priceData2$建物型態 %in% names(selectSet[selectSet > 300])
priceData2 <- subset(priceData2, selectSet)
priceData2$建物型態 <- gsub("套房\\(1房1廳1衛\\)", 1, priceData2$建物型態)
priceData2$建物型態 <- gsub("公寓\\(5樓含以下無電梯\\)", 2, priceData2$建物型態)
priceData2$建物型態 <- gsub("華廈\\(10層含以下有電梯\\)", 3, priceData2$建物型態)
priceData2$建物型態 <- gsub("住宅大樓\\(11層含以上有電梯\\)", 4, priceData2$建物型態)
priceData2$建物型態 <- gsub("透天厝", 5, priceData2$建物型態)

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
qplot(x=priceData2$交易標的橫坐標, y=priceData2$交易標的縱坐標, col=factor(priceCluster$cluster)) + geom_point(size=2)
priceCenters <- data.frame(priceCluster$centers)
qplot(x=priceCenters$Longitude, y=priceCenters$Latitude, col=factor(1:30)) + geom_point(size=5)
qplot(x=priceCenters$交易標的橫坐標, y=priceCenters$交易標的縱坐標, col=factor(1:30)) + geom_point(size=5)

# library(ggmap)
# 
# df <- data.frame(x=priceCenters$Longitude, y=priceCenters$Latitude)
# map <- get_googlemap(center = c(lon = 121.518303, lat = 25.045248), zoom = 13, markers = df, scale = 2)
# ggmap(map, extent = 'normal')

for(i in 1:30){
clus <- subset(priceData2, priceCluster$cluster == i)
qplot(clus$單價)
ggsave(filename=paste("priceDist_cluster_",i,".png",sep=""))
}