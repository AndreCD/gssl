library(shiny)
library(rdrop2)
library(dplyr)
library(readr)
library(mxnet)
library(caret)
## 


control <- caret::trainControl(method = "cv", savePredictions = TRUE, verboseIter = TRUE, number=5)
tune.pls <- caret::train(variable~., data=prepro[[i]], method='pls', tuneLength= 5, trControl = control, verbose=TRUE)
return(tune.pls)
selected.pls.c <- tune.pls$bestTune$ncomp
# calibrate the model
mod <- pls::plsr(variable~., data=prepro[[i]], ncomp= tune.pls$bestTune$ncomp, method="simpls", validation="none") 

#cubist
control <- trainControl(method="optimism_boot", number=10)

fit.cubist <- train(variable~.,data=gssl.imported, 
                    method="cubist",
                    trControl=control)
print(fit.cubist)
beep(2)
results.cubist <- getTrainPerf(fit.cubist)
results.cubist





#xgbTree
control <- trainControl(method="optimism_boot", number=10)

fit.xgbTree <- train(variable~.,data=gssl.imported, 
                   method="xgbTree", 
                   trControl=control)
print(fit.xgbTree)
results.xgbTree<- getTrainPerf(fit.xgbTree)
results.xgbTree

#pls
control <- trainControl(method="optimism_boot", number=10)

fit.pls <- train(variable~.,data=gssl.imported, 
                     method="pls",tuneLength = 20, 
                     trControl=control)
print(fit.pls)
beep(2)
results.pls <- getTrainPerf(fit.pls)
results.pls


###########################################################################
#rs-local + 20 imported
#xgbTree
control <- trainControl(method="optimism_boot", number=5)

fit.xgbTree.2 <- train(variable~.,data=rslocal.imported, 
                     method="xgbTree", 
                     trControl=control)
print(fit.xgbTree.2)
results.xgbTree.2<- getTrainPerf(fit.xgbTree.2)
results.xgbTree.2

#pls
control <- trainControl(method="optimism_boot", number=5)

fit.pls.2 <- train(variable~.,data=rslocal.imported, 
                 method="pls",tuneLength = 20, 
                 trControl=control)
print(fit.pls.2)
results.pls.2 <- getTrainPerf(fit.pls.2)
results.pls.2

#cubist
control <- trainControl(method="optimism_boot", number=5)

fit.cubist.2 <- train(variable~.,data=rslocal.imported, 
                    method="cubist",
                    trControl=control)
print(fit.cubist.2)
beep(2)
results.cubist.2 <- getTrainPerf(fit.cubist.2)
results.cubist.2

###########################################################################
#20 imported
#xgbTree
seed <- 1909983
control <- trainControl(method="cv", number=5)

fit.xgbTree.3 <- train(variable~.,data=imported.data, 
                       method="xgbTree", 
                       trControl=control)
print(fit.xgbTree.3)
results.xgbTree.3<- getTrainPerf(fit.xgbTree.3)
results.xgbTree.3

#pls
control <- trainControl(method="cv", number=5)
set.seed(seed)
fit.pls.3 <- train(variable~.,data=imported.data, 
                   method="pls",tuneLength = 10, 
                   trControl=control)
print(fit.pls.3)
results.pls.3 <- getTrainPerf(fit.pls.3)
results.pls.3

#cubist
control <- trainControl(method="cv", number=5)
set.seed(125)
fit.cubist.3 <- train(variable~.,data=imported.data, 
                      method="cubist",
                      trControl=control)
print(fit.cubist.3)
beep(2)
results.cubist.3 <- getTrainPerf(fit.cubist.3)
results.cubist.3


rbind(results.xgbTree, results.pls, results.cubist)
rbind(results.xgbTree.2, results.pls.2, results.cubist.2)
rbind(results.xgbTree.3, results.pls.3, results.cubist.3)



library(plsr)
y <- as.matrix(log(gssl.imported[,1]))
x <- as.matrix(gssl.imported[-1])
mod <- pls::plsr(y~x, ncomp=20, method="simpls", validation="none") 
sm_Stats(mod$fitted.values[,,20], y)


##












































#rslocal.selected
## preprocvessing
# Reflectance
spectra.ref <- imported.data

# absorbance
abs <- log10(1/imported.data[-1])
spectra.abs <- cbind(imported.data[1], abs)

# SG Derivative
der <- data.frame(prospectr::savitzkyGolay(imported.data[-1], p = 2, w = 9, m = 1))
spectra.der <- cbind(imported.data[1], der)

# SNV
snv <- prospectr::standardNormalVariate(X = imported.data[-1]) 
spectra.snv <- cbind(imported.data[1], snv)


## modelling
mycontrol <- trainControl(method='cv', number=10, 
                          savePredictions=TRUE, 
                          verboseIter=TRUE)

library(caretEnsemble)
model_list <- caretList(variable~.,
                        data=spectra.ref,
                        trControl = mycontrol,
                        methodList = c("pls","svmRadial"),
                        tuneList = NULL,
                        continue_on_fail = FALSE)

ensemble_2 <- caretStack(model_list, 
                         method = "glmnet", 
                         metric = "RMSE", 
                         trControl = mycontrol)
print(ensemble_2)
}
