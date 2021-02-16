bankNotes <- read.csv("/home/matthew/Documents/University/PDS/Datasets/banknote.csv", header=T)
bankNotes$X0 <- as.factor(bankNotes$X0)
nb = train(X0~.,method="naive_bayes",data = bankNotes, trControl=trainControl(method="LOOCV"),preProcess=c("center","scale"))
nb$results
