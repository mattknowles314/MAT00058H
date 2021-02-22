edu <- read.csv("/home/matthew/Documents/University/PDS/Datasets/education.csv")

trainIndex = createDataPartition(edu$Class, p=0.75, list=F)
trainData = edu[trainIndex,]
testData = edu[-trainIndex,]

testData$Class <- as.factor(testData$Class)

runModel <- function(){
  nnmodel = nnet(Class~., data=trainData, size = 6)
  nnpred = predict(nnmodel, testData[,-11], type="class")
  table(true=testData$Class, predicted=as.factor(nnpred))
  x <- confusionMatrix(as.factor(nnpred), testData$Class)
  return(x[["overall"]][["Accuracy"]])
}

accVals = c()
for(i in 0:10){
  t = runModel()
  accVals[i] <- t
}

sd(accVals)

numFolds = trainControl(method = "cv", number = 10, savePredictions = T)
grid = expand.grid(size =10, decay=0)

nnmodel2 = train(Class~., data=trainData, method="nnet", preProcess=c('center','scale'), trControl=numFolds, tuneGrid=grid)
nnmodel2$results

numFolds2 = trainControl(method = "cv", number = 6, savePredictions = T)
grid = expand.grid(size =10, decay=0)

nnmodel3 = train(Class~., data=trainData, method="nnet", preProcess=c('center','scale'), trControl=numFolds2, tuneGrid=grid, maxit=200)
nnmodel3$results
