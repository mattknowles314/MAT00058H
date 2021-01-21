data1 = read.table("~/Documents/University/PDS/Datasets/trace1.txt",header=T, row.names = 1)
data2 = read.table("~/Documents/University/PDS/Datasets/trace2.txt",header=T)

## Q2: Difference between samples is that samples are numbered data1

#Q6:

data1PCA = prcomp(data1)

#Q7:
summary(data1PCA)
#PC1 accounts for 0.9945 of the variance

#Q8:
data1PCA$rotation
#Can see that log(As) contributes the most to PC1 - 0.999689917

#Q9:
biplot(data1PCA)
#Log(As) appears to dominate.

#Q10:
data1PCA$x
# High scores in PC2 send samples to the top of the biplot, visa versa, High scores in PC1 send to right and visa-versa.

#Q11-13:

P3 <- data1PCA$x[,3]
P4 <- data1PCA$x[,4]

plot(P3,P4, xlab="PC3", ylab="PC4", main=" PCA Plot", pch=15, col="blue")
points(data1PCA$x[1:6,3], data1PCA$x[1:6,4], pch=15, col="red")
points(data1PCA$x[7:12,3], data1PCA$x[7:12,4], pch=15, col="green")

#Q14:
summary(data1)
#Clearly, log(As) has a max value that is much larger than the others, therefore dominating the analysis
boxplot(data1)

#Q15:
pcs = prcomp(data1, scale=TRUE)
biplot(pcs)
summary(pcs)
pcs$rotation
# Log(Sr contributes teh most to PC1)