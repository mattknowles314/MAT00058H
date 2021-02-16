tumour <- read.csv("/home/matthew/Documents/University/PDS/Datasets/tumour.csv", header = T)

#Randomly assing data to test and train sets
trainIndex=sample(1:nrow(tumour), size= round(0.7*nrow(tumour)), replace=FALSE)
trainData = tumour[trainIndex,-1]
testData = tumour[-trainIndex,-1]

trainData$diagnosis = as.factor(trainData$diagnosis)

tumour.tree = tree(diagnosis~., data=trainData)
plot(tumour.tree)
text(tumour.tree, cex=0.7)


tree.pred = predict(tumor.tree, trainData[,-1], type = "class")


rf = randomForest(diagnosis~., data= trainData, mtree=6, importance= T)
plot(rf)
