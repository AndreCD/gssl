library(shiny)
library(rdrop2)
library(dplyr)
library(readr)
library(mxnet)
library(caret)
## modelling RS-Local
imported <- read.csv("C:/Users/280240B/Downloads/test.csv")
imported.data <- cbind(variable=imported$C, imported[ ,c(19:2169)])

filesInfo <- drop_dir("drop_test")
filePaths <- filesInfo$path_lower[1] # select the first file from DropBox folder
gssl2 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
gssl2 <- do.call(rbind, gssl2)
gssl <- cbind(variable=gssl2$Carb, gssl[ ,c(15:length(gssl2))])

SL.x <- data.frame(gssl[,c(2:2152)]) #spectra
SL.y <- as.matrix(gssl[1]) 
m.x  <- data.frame(imported.data[,c(2:2152)]) # spectra
m.y  <- as.matrix(imported.data[1]) 
k <- 100
b <- 40
r <- 0.1
setwd("~/GitHub/gssl-proj/R")
source("rslocal.R")
rslocaldata <- rslocal(SL.x, SL.y, m.x, m.y, k, b, r, method = "pls", pls.tune = FALSE, pls.c = 10, allowParallel = FALSE)
rslocal <- cbind(variable = rslocaldata$K.y, rslocaldata$K.x) 


# Calibration with the gssl only - general calib

# Calib with 20 importedonly - local 
imported.data

# gssl + 20 imported - spiking papers by Guerrero et al.
gssl.imported <- rbind(imported.data, gssl)

# gssl + 20 imported - extra weighting papers by Guerrero et al.


# rs-local + 20 imported
rslocal.imported <- rbind(imported.data, rslocal)



# spectral preprocessing







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

#cubist
control <- trainControl(method="optimism_boot", number=10)

fit.cubist <- train(variable~.,data=gssl.imported, 
                 method="cubist",
                 trControl=control)
print(fit.cubist)
beep(2)
results.cubist <- getTrainPerf(fit.cubist)
results.cubist

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
control <- trainControl(method="cv", number=5)

fit.xgbTree.3 <- train(variable~.,data=imported.data, 
                       method="xgbTree", 
                       trControl=control)
print(fit.xgbTree.3)
results.xgbTree.3<- getTrainPerf(fit.xgbTree.3)
results.xgbTree.3

#pls
control <- trainControl(method="cv", number=5)

fit.pls.3 <- train(variable~.,data=imported.data, 
                   method="pls",tuneLength = 10, 
                   trControl=control)
print(fit.pls.3)
results.pls.3 <- getTrainPerf(fit.pls.3)
results.pls.3

#cubist
control <- trainControl(method="cv", number=5)

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
