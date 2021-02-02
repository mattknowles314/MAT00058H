tumourData = read.csv("/home/matthew/Documents/University/PDS/Datasets/tumour.csv")

#Create a correlation matrix, plot heatmaps
corMat = as.matrix(cor(tumourData[,3:32]))
heatmap3(corMat)
heatmap3(corMat,scale = "none")

#PCA Analysis

tumPCA = prcomp(tumourData[,3:32])
biplot(tumPCA)

boxplot(tumourData[,3:32]) #Suggests scaling

tumPCAScaled = prcomp(tumourData[,3:32], scale = T)
biplot(tumPCAScaled, cex = 0.6)

#Stitch together new dataset and leave out outliers
tumDataNew = rbind(tumourData[1:3,],tumourData[5:152,],tumourData[154:212,],tumourData[214:461,],tumourData[463:569,])

tumPCAFinal =prcomp(tumDataNew[,3:32], scale=T)
biplot(tumPCAFinal, cex= 0.6)

corMatPCA <- as.matrix(cor(tumPCAFinal$rotation))
heatmap3(corMatPCA)

#LDA/L-O-O

pcaScores = tumPCAFinal$x[,1:6]
tumLDA = lda(pcaScores, tumDataNew[,2], CV=T)
table(tumDataNew[,2], tumLDA$class)

class=table(tumDataNew[,2],tumLDA$class)
diag(prop.table(class,1))
sum(diag(prop.table(class)))
