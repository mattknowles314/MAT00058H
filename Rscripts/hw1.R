#Import data
coffeeData = read.table("~/Documents/University/PDS/Datasets/coffee.txt")

#Check dimensions
dim(coffeeData)

#Should we scale?
boxplot(coffeeData)

#Perform initial PCA
coffeePCA = prcomp(coffeeData)
summary(coffeePCA)
biplot(coffeePCA, cex=0.8)

coffeePCAScaled = prcomp(coffeeData, scale = T)
summary(coffeePCAScaled)
biplot(coffeePCAScaled, cex=0.5)

#The following finds the variable which contributes the most to PC1
sort((coffeePCAScaled$rotation[,1]))

coffeeData <- coffeeData[-52:-53]

#Perform PCA on other components

PC1 <- coffeePCAScaled$x[,1]

summary(PC1)


PC2 <- coffeePCAScaled$x[,2]
PC3 <- coffeePCAScaled$x[,3]
plot(PC2, PC3,xlab="PC2",ylab="PC3", pch=15, col="black")

summary(PC2)
summary(PC3)

PC4 <- coffeePCAScaled$x[,4]
plot(PC3, PC4,xlab="PC3",ylab="PC4", pch=15, col="black")

summary(PC4)

PC5 <- coffeePCAScaled$x[,5]
plot(PC4, PC5,xlab="PC4",ylab="PC5", pch=15, col="black")

summary(PC5)

#Find most significant variables
sort(coffeePCAScaled$rotation)
