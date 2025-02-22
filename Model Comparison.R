setwd("C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project\\Project Pt. 2")
pacman::p_load(tidyverse, DataExplorer, ipred, pROC, rpart, caret, ROCR, magrittr, gbm, xgboost)
df <- readRDS("trainingDataUPDATE.RDS")
df %<>% select(.,-DateFrom, -starts_with("USStateRegion")) 

set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]
cvindx <- createFolds(trainIndex, k=10, returnTrain = TRUE)
ctrl <- trainControl(method="cv", index=cvindx, summaryFunction = twoClassSummary, classProbs = TRUE)
y <- training$target
x <- select(training, -target)


#### RF Model AUCs ----------------------------------------------------------
rf1 <- readRDS("rforest.RDS")
p.rf.t <- predict(rf1, data=train, type="prob")
rt <- roc(training$target, p.rf.t[,2])
r.rf.auc.t<-rt$auc
p.rf<-predict(rf1, newdata = valid, type="prob")
r<-roc(valid$target,  p.rf[,2])
r.rf.auc<-r$auc

rf2 <- readRDS("rforest2.RDS")
p.rf.t.2 <- predict(rf2, data=training, type="prob")
rt.2 <- roc(training$target, p.rf.t.2[,2])
r.rf.auc.t.2<-rt.2$auc
p.rf.2 <- predict(rf2, newdata = valid, type="prob")
r.2 <- roc(valid$target, p.rf.2[,2])
r.rf.auc.2<-r.2$auc

rf3 <- readRDS("rforest3.RDS")
p.rf.t.3 <- predict(rf3, data=training, type="prob")
rt.3 <- roc(training$target, p.rf.t.3[,2])
r.rf.auc.t.3<-rt.3$auc
p.rf.3 <- predict(rf3, newdata = valid, type="prob")
r.3 <- roc(valid$target, p.rf.3[,2])
r.rf.auc.3<-r.3$auc

rf4 <- readRDS("rforest4.RDS")
p.rf.t.4 <- predict(rf4, data=training, type="prob")
rt.4 <- roc(training$target, p.rf.t.4[,2])
r.rf.auc.t.4<-rt.4$auc
p.rf.4 <- predict(rf4, newdata = valid, type="prob")
r.4 <- roc(valid$target, p.rf.4[,2])
r.rf.auc.4<-r.4$auc


#### GB Model AUCs -------------------------------------------------------
gb1 <- readRDS("gbtree.RDS")
p.gb1.t<-predict(gb1, data=training, type="prob")
rt.gb1<-roc(training$target, p.gb1.t[,2])
r.gb.auc.t1<-rt.gb1$auc
p.gb1<-predict(gb1, newdata=valid, type="prob")
rgb1<-roc(valid$target, p.gb1[,2])
r.gb.auc1<-rgb1$auc

gb2 <- readRDS("gbtree2.RDS")
p.gb2.t<-predict(gb2, data=training, type="prob")
rt.gb2<-roc(training$target, p.gb2.t[,2])
r.gb.auc.t2<-rt.gb2$auc
p.gb2<-predict(gb2, newdata=valid, type="prob")
rgb2<-roc(valid$target, p.gb2[,2])
r.gb.auc2<-rgb2$auc

gb3 <- readRDS("gbtree3.RDS")
p.gb3.t<-predict(gb3, data=training, type="prob")
rt.gb3<-roc(training$target, p.gb3.t[,2])
r.gb.auc.t3<-rt.gb3$auc
p.gb3<-predict(gb3, newdata=valid, type="prob")
rgb3<-roc(valid$target, p.gb3[,2])
r.gb.auc3<-rgb3$auc

#### XgBoost Model AUCs --------------------------------------------------
xg1 <- readRDS("xgboost.RDS")
p.xg1.t<-predict(xg1, newdata=training, type="prob")
rt.xg1<-roc(training$target, p.xg1.t[,2])
r.xg.auc.t1<-rt.xg1$auc
p.xg1<-predict(xg1, newdata=valid, type="prob")
r.xg1<-roc(valid$target, p.xg1[,2])
r.xg.auc1<-r.xg1$auc

xg2 <- readRDS("xgboost2.RDS")
p.xg2.t<-predict(xg2, newdata=training, type="prob")
rt.xg2<-roc(training$target, p.xg2.t[,2])
r.xg.auc.t2<-rt.xg2$auc
p.xg2<-predict(xg2, newdata=valid, type="prob")
r.xg2<-roc(valid$target, p.xg2[,2])
r.xg.auc2<-r.xg2$auc

xg3 <- readRDS("xgboost3.RDS")
p.xg3.t<-predict(xg3, newdata=training, type="prob")
rt.xg3<-roc(training$target, p.xg3.t[,2])
r.xg.auc.t3<-rt.xg3$auc
p.xg3<-predict(xg3, newdata=valid, type="prob")
r.xg3<-roc(valid$target, p.xg3[,2])
r.xg.auc3<-r.xg3$auc


#### Logistic Regression AUCs --------------------------------------------
lrstep <- readRDS("lrstepprojnew.RDS")
pst.t<-predict.train(lrstep, type="prob")
rst.t<-roc(training$target, pst.t[,2])
r.lrstep.auc.t <- rst.t$auc
pst<-predict.train(lrstep, type="prob")
rst<-roc(training$target, pst[,2])
r.lrstep.auc <- rst$auc

custom <- readRDS("customlogprojnew.RDS")
pc1t<-predict.train(custom, type="prob")
rft1<-roc(training$target, pc1t[,2])
r.custom.auc.t <- rft1$auc
pf1<-predict.train(custom, newdata=valid, type="prob")
rf1<-roc(valid$target, pf1[,2] )
r.custom.auc <- rf1$auc

#### Neural Network AUCs -------------------------------------------------
nnetFit1 <- readRDS("nnetFitprojnew.RDS")
p.nnet<-predict(nnetFit1, data=training, type="prob")
r<-roc(training$target,  p.nnet[,2])
r.nnet.auct<-r$auc
p.nnet<-predict(nnetFit1, newdata=valid, type="prob")
r<-roc(valid$target,  p.nnet[,2])
r.nnet.auc<-r$auc

nnetFit2 <- readRDS("nnetFit1projnew.RDS")
p.nnet2<-predict(nnetFit2, data=training, type="prob")
r2<-roc(training$target,  p.nnet2[,2])
r.nnet2.auct<-r2$auc
p.nnet2<-predict(nnetFit2, newdata=valid, type="prob")
r2<-roc(valid$target,  p.nnet2[,2])
r.nnet2.auc<-r2$auc

nnetFit3 <- readRDS("nnetFit2projnew.RDS")
p.nnet3<-predict(nnetFit3, data=training, type="prob")
r3<-roc(training$target,  p.nnet3[,2])
r.nnet3.auct<-r3$auc
p.nnet3<-predict(nnetFit3, newdata=valid, type="prob")
r3<-roc(valid$target,  p.nnet3[,2])
r.nnet3.auc<-r3$auc

nnetFitRF <- readRDS("nnetRF.RDS")
p.nnet.rf<-predict(nnfitRF, data=training, type="prob")
r<-roc(training$target, p.nnet.rf[,2])
r.nnet.rf.auct<-r$auc
p.nnet.rf<-predict(nnfitRF, newdata=valid, type="prob")
r<-roc(valid$target, p.nnet.rf[,2])
r.nnet.rf.auc<-r$auc

#### RF Comparison Table ----------------------------------------------------
library(kableExtra)
rfResults <- data.frame(Model = c("RF Model 1", "RF Model 2", "RF Model 3", "RF Model 4"),
                        AUC.Train = c(r.rf.auc.t, r.rf.auc.t.2, r.rf.auc.t.3, r.rf.auc.t.4),
                        AUC.Valid = c(r.rf.auc, r.rf.auc.2, r.rf.auc.3, r.rf.auc.4),
                        mtry = c("2, 5, 10", "2, 5, 10", "2, 5, 10, 12", "2, 5, 10, 12"),
                        min.node.size = c("150,200,250", "150,200,250", "10, 20", "10, 20"),
                        Num.Trees = c(500, 1000, 500, 1000))
knitr::kable(rfResults) %>% kable_styling(full_width = F)

#### Boosted Comparison Table ----------------------------------------------
bstResults <- data.frame(Model = c("GB Model 1", "GB Model 2", "GB Model 3", "XgBoost Model 1", 
                                   "XgBoost Model 2", "XgBoost Model 3"),
                         AUC.Train = c(r.gb.auc.t1, r.gb.auc.t2, r.gb.auc.t3, r.xg.auc.t1,
                                       r.xg.auc.t2, r.xg.auc.t3),
                         AUC.Valid = c(r.gb.auc1, r.gb.auc2, r.gb.auc3, r.xg.auc1, r.xg.auc2,
                                       r.xg.auc3),
                         Complexity = c("N.Trees=150", "N.Trees=300", "N.Trees=500", "depth=(3, 6)",
                                        "depth=(10, 20)", "depth=(30, 40)"))
knitr::kable(bstResults) %>% kable_styling(full_width = F)


#### LR and NN Model Comparisons -----------------------------------------
Results <- data.frame(Model = c("NN Logistic Model 1", "NN Logistic Model 2", "NN Logistic Model 3",
                                  "NN RF Model", "LR Stepwise Model", "Custom LR Model"),
                        AUC.Train = c(r.nnet.auct, r.nnet2.auct, r.nnet3.auct, r.nnet.rf.auct, 
                                      r.lrstep.auc.t, r.custom.auc.t),
                        AUC.Valid = c(r.nnet.auc, r.nnet2.auc, r.nnet3.auc, r.nnet.rf.auc,
                                      r.lrstep.auc, r.custom.auc),
                        Weights = c(length(nnetFit1$finalModel$wts), length(nnetFit2$finalModel$wts),
                                    length(nnetFit3$finalModel$wts), length(nnetFitRF$finalModel$wts), 
                                    "NA", "NA"))
knitr::kable(Results) %>% kable_styling(full_width = F)














