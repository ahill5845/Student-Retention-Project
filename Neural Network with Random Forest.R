setwd("C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project\\Project Pt. 2")
pacman::p_load(tidyverse, DataExplorer, ipred, pROC, rpart, caret, ROCR, magrittr, gbm, xgboost)
df <- readRDS("trainingDataUPDATE.RDS")
df %<>% select(.,-DateFrom) 

set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]
cvindx <- createFolds(trainIndex, k=10, returnTrain = TRUE)
ctrl <- trainControl(method="cv", index=cvindx, summaryFunction = twoClassSummary, classProbs = TRUE)
y <- training$target
x <- select(training, -target)

#### RF Variable Selection -----------------------------------------------
library(randomForest)
rf3 <- readRDS("rforest3.RDS")
varI <- varImp(rf3)
var <- as.data.frame(varI$importance)
var <- cbind(rownames(var), data.frame(var, row.names=NULL))
head(varI$importance)
var <- var[order(-var$Overall), ]
terms.rf<-var$`rownames(var)`[1:25, drop=TRUE]


#### Setting up the NN Model ---------------------------------------------
library(nnet)
tunegrid <- expand.grid(.size=1:10, .decay= c(0, 0.1, 0.5))
maxSize <- max(tunegrid$.size)
cl <- makePSOCKcluster(7)

registerDoParallel(cl)
nnetFit.rf <- train(x=x, y=training$target, method="nnet", metric="ROC", linout=FALSE,
                  preProcess = c("range"), tuneGrid = tunegrid, trace=FALSE,maxit=100,
                  MaxNWts=numWts, trControl=ctrl)
stopCluster(cl) 

p.nnet.rf<-predict(nnetFit.rf, data=training, type="prob")
r<-roc(training$target, p.nnet.rf[,2])
r.nnet.rf.auct<-r$auc
r.nnet.rf.auct
p.nnet.rf<-predict(nnetFit.rf, newdata=valid, type="prob")
r<-roc(valid$target, p.nnet.rf[,2])
r.nnet.rf.auc<-r$auc
r.nnet.rf.auc



