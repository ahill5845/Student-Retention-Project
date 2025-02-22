setwd("C:\\Users\\hilla\\OneDrive\\Documents\\ISA 491 Project\\Project Pt. 2")
pacman::p_load(tidyverse, DataExplorer, ipred, pROC, rpart, caret, ROCR, magrittr)
df <- readRDS("trainingDataUPDATE.RDS")
df %<>% select(.,-DateFrom) 

#### Training and Validation Sets ----------------------------------------
set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]


#### Gradient Boosted Model 1 --------------------------------------------
library(gbm)
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
r.gbtree.auc.t1
p.gbtree1<-predict(gb.tree, newdata=valid, type="prob")
r1<-roc(valid$target,  p.gbtree1[,2])
r.gbtree.auc1<-r1$auc
r.gbtree.auc1


#### Gradient Boosted Model 2 --------------------------------------------
GBMgrid2 <- expand.grid(n.trees = seq(50,150,50), 
                        interaction.depth = c(100, 150), ###DIFFERENT INTERACTION DEPTH
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
r.gbtree.auc.t2
p.gbtree2<-predict(gb.tree2, newdata=valid, type="prob")
r2<-roc(valid$target,  p.gbtree2[,2])
r.gbtree.auc2<-r2$auc
r.gbtree.auc2

#### XgBoost Model 1 -----------------------------------------------------
library(xgboost)
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

p.xgboost1<-predict(xgboost, data=training, type="prob")
rt1<-roc(training$target,  p.xgboost1[,2])
r.xgboost.auc.t1<-rt1$auc
r.xgboost.auc.t1
p.xgboost1<-predict(xgboost, newdata=valid, type="prob")
r1<-roc(valid$target,  p.xgboost1[,2])
r.xgboost.auc1<-r1$auc
r.xgboost.auc1

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

p.xgboost2<-predict(xgboost2, data=training, type="prob")
rt2<-roc(training$target,  p.xgboost2[,2])
r.xgboost.auc.t2<-rt2$auc
r.xgboost.auc.t2
p.xgboost2<-predict(xgboost2, newdata=valid, type="prob")
r2<-roc(valid$target,  p.xgboost2[,2])
r.xgboost.auc2<-r2$auc
r.xgboost.auc2
