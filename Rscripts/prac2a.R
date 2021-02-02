bank = read.table("/home/matthew/Documents/University/PDS/Datasets/bank.txt", head=TRUE)
bankData = bank[2:200,] #Remove NA Column

bankPCA = prcomp(bankData)
biplot(bankPCA) 

#The above suggests that V131 is an outlier. We try scaling to check if this is ture

bankPCAScaled = prcomp(bankData, scale = T)
biplot(bankPCAScaled)

#This shows it is still an outlier. It would also just 167 maybe but I dont think it is far
#enough out to be counted.

n = nrow(bankData)
trainIndex = sample(1:n, size = round(0.7*n), replace=F)
trainSet = bankData[trainIndex,]
testSet =  bankData[-trainIndex,]

group = trainSet[,1]
bankLDA = lda(trainSet[,2:7], group)
bankLDA
plot(bankLDA)

predtest = predict(bankLDA, testSet[,2:7])
table(predtest$class, real=testSet[,1])

