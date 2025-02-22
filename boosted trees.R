setwd("M:\\ISA 491\\Project Pt. 2")
install.packages("pacman")
pacman::p_load(tidyverse, magrittr, DataExplorer, ipred, pROC, rpart, caret, ROCR)
df <- readRDS("trainingDataUPDATE.RDS")
df %<>% select(.,-DateFrom)
library(doParallel)

#### Training and Validation Sets ----------------------------------------
set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]
cvindx<-createFolds(trainIndex, k=10, returnTrain = TRUE)
ctrl <- trainControl(method="cv", index=cvindx, summaryFunction = twoClassSummary, classProbs = TRUE)

#### Gradient Boosted Model 1 --------------------------------------------
GBMgrid1 <- expand.grid(n.trees = seq(50,150,50), 
                        interaction.depth = c(10, 20), 
                        shrinkage = c(0.1, 0.01), 
                        n.minobsinnode=c(25))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
gb.tree <- train(target~., 
                 data=training, 
                 method = 'gbm', 
                 trControl=ctrl, 
                 tuneGrid=GBMgrid1, 
                 metric='ROC')
stopCluster(cl)

p.gbtree1<-predict(gb.tree, data=training, type="prob")
rt1<-roc(training$target,  p.gbtree1[,2])
r.gbtree.auc.t1<-rt1$auc
r.gbtree.auc.t1 #0.6905
p.gbtree1<-predict(gb.tree, newdata=valid, type="prob")
r1<-roc(valid$target,  p.gbtree1[,2])
r.gbtree.auc1<-r1$auc
r.gbtree.auc1 #0.6221

#### Gradient Boosted Model 2 --------------------------------------------
GBMgrid2 <- expand.grid(n.trees = seq(50,300,50), 
                        interaction.depth = c(10, 20),
                        shrinkage = c(0.1, 0.01), 
                        n.minobsinnode=c(25))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
gb.tree2 <- train(target~., 
                  data=training, 
                  method = 'gbm', 
                  trControl=ctrl, 
                  tuneGrid=GBMgrid2, 
                  metric='ROC')
stopCluster(cl)

p.gbtree2<-predict(gb.tree2, data=training, type="prob")
rt2<-roc(training$target,  p.gbtree2[,2])
r.gbtree.auc.t2<-rt2$auc
r.gbtree.auc.t2 #0.6899
p.gbtree2<-predict(gb.tree2, newdata=valid, type="prob")
r2<-roc(valid$target,  p.gbtree2[,2])
r.gbtree.auc2<-r2$auc
r.gbtree.auc2 #0.624


#### Gradient Boosted Model 3 --------------------------------------------
GBMgrid3 <- expand.grid(n.trees = seq(50,500,50), 
                        interaction.depth = c(10, 20),
                        shrinkage = c(0.1, 0.01), 
                        n.minobsinnode=c(25))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
gb.tree3 <- train(target~., 
                  data=training, 
                  method = 'gbm', 
                  trControl=ctrl, 
                  tuneGrid=GBMgrid3, 
                  metric='ROC')
stopCluster(cl)

p.gbtree3<-predict(gb.tree3, data=training, type="prob")
rt3<-roc(training$target, p.gbtree3[,2])
r.gbtree.auc.t3<-rt3$auc
r.gbtree.auc.t3 #0.7028
p.gbtree3<-predict(gb.tree3, newdata=valid, type="prob")
r3<-roc(valid$target, p.gbtree3[,2])
r.gbtree.auc3<-r3$auc
r.gbtree.auc3 #0.6225


#### XgBoost Model 1 -----------------------------------------------------
XGgrid1 <-  expand.grid(nrounds = 100,
                        max_depth = c(3,6),
                        eta = c(0.001,0.1),
                        gamma = c(0.5, 0.75),
                        colsample_bytree = c(0.3, 1),
                        min_child_weight = c(0,15),
                        subsample = c(0.3,1))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
xgboost <- train(target~., 
                 data = training,
                 method = "xgbTree",
                 trControl=ctrl, 
                 tuneGrid=XGgrid1, 
                 metric='ROC')
stopCluster(cl)

p.xgboost1<-predict(xgboost, newdata=training, type="prob")
rt1<-roc(training$target,  p.xgboost1[,2])
r.xgboost.auc.t1<-rt1$auc
r.xgboost.auc.t1 #0.6839
p.xgboost1<-predict(xgboost, newdata=valid, type="prob")
r1<-roc(valid$target,  p.xgboost1[,2])
r.xgboost.auc1<-r1$auc
r.xgboost.auc1 #0.6333

#### XgBoost Model 2 -----------------------------------------------------
XGgrid2 <-  expand.grid(nrounds = 100,
                        max_depth = c(10,20), ###DIFFERENT MAX DEPTH
                        eta = c(0.001,0.1),
                        gamma = c(0.5, 0.75),
                        colsample_bytree = c(0.3, 1),
                        min_child_weight = c(0,15),
                        subsample = c(0.3,1))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
xgboost2 <- train(target~., 
                  data = training,
                  method = "xgbTree",
                  trControl=ctrl, 
                  tuneGrid=XGgrid2, 
                  metric='ROC')
stopCluster(cl)

p.xgboost2<-predict(xgboost2, newdata=training, type="prob")
rt2<-roc(training$target,  p.xgboost2[,2])
r.xgboost.auc.t2<-rt2$auc
r.xgboost.auc.t2 #0.6719
p.xgboost2<-predict(xgboost2, newdata=valid, type="prob")
r2<-roc(valid$target,  p.xgboost2[,2])
r.xgboost.auc2<-r2$auc
r.xgboost.auc2 #0.6213


#### XgBoost Model 3 -----------------------------------------------------
XGgrid3 <-  expand.grid(nrounds = 100,
                        max_depth = c(30,40), ###DIFFERENT MAX DEPTH
                        eta = c(0.001,0.1),
                        gamma = c(0.5, 0.75),
                        colsample_bytree = c(0.3, 1),
                        min_child_weight = c(0,15),
                        subsample = c(0.3,1))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
xgboost3 <- train(target~., 
                  data = training,
                  method = "xgbTree",
                  trControl=ctrl, 
                  tuneGrid=XGgrid3, 
                  metric='ROC')
stopCluster(cl)

p.xgboost3<-predict(xgboost3, newdata=training, type="prob")
rt3<-roc(training$target,  p.xgboost3[,2])
r.xgboost.auc.t3<-rt3$auc
r.xgboost.auc.t3 #0.7425
p.xgboost3<-predict(xgboost3, newdata=valid, type="prob")
r3<-roc(valid$target,  p.xgboost3[,2])
r.xgboost.auc3<-r3$auc
r.xgboost.auc3 #0.6153


#### Save all models -----------------------------------------------------
saveRDS(gb.tree, "gbtree.RDS")
saveRDS(gb.tree2, "gbtree2.RDS")
saveRDS(gb.tree3, "gbtree3.RDS")

saveRDS(xgboost, "xgboost.RDS")
saveRDS(xgboost2, "xgboost2.RDS")
saveRDS(xgboost3, "xgboost3.RDS")

























