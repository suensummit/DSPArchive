## set path & load house-price data

setwd("D:/SUEN_SUMMIT/DataSci_camp/ProjectData/DSP_cleaned/")
priceData <- read.csv("RealPriceGeocodes.csv", header = TRUE)

taipei <- subset(priceData, 縣市=="臺北市" & 交易類別=="買賣")
yilan <- subset(priceData, 縣市=="宜蘭縣" & 交易類別=="買賣")

test <- as.numeric(gsub(",", "", taipei$總價))
test <- test[test < 100000000]

test2 <- as.numeric(gsub(",", "", yilan$總價))
test2 <- test2[test2 < 100000000]

library("ggplot2")
qplot(test)

qplot(test2, xlab = "Price", binwidth = 1000000)
qplot(test, xlab = "Price", binwidth = 1000000)

qplot(x=test1$交易標的橫坐標, y=test1$交易標的縱坐標, col=factor(testkmeans$cluster))
testkmeans <- kmeans(test1, 12)

qplot(x=test1$交易標的橫坐標, y=test1$交易標的縱坐標, col=factor(testkmeans$cluster))