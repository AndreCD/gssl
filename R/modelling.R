library(Cubist)
library(plsr)
library(mxnet)
library(caret)


#select the imposted spectra
spectra <- rslocal

# create list with all reflectance
pp <- function(spectra, option){
  
  if (any(is.na(spectra[-1]))) {
    warning("Check for NAs!")
  }
  # reflectance
  if (option == "REFLECTANCE") spectra.pp <- spectra
  # absorbance
  if (option == "ABSORBANCE") spectra.pp <- cbind(spectra[1], log10(1/spectra[-1]))
  # SG Derivative
  if (option == "SGDERIVATIVE") spectra.pp <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 9, m = 1)))
  # SNV
  if (option == "SNV") spectra.pp <- cbind(spectra[1], prospectr::standardNormalVariate(X = spectra[-1])) 
  # absorbance + SNV
  if (option == "ABS-SNV") spectra.pp <- cbind(spectra[1], prospectr::standardNormalVariate(X = abs)) 
  # absorbance + derivative
  if (option == "ABS-SGDERIVATIVE") spectra.pp <- cbind(spectra[1], prospectr::standardNormalVariate(X = der))
  #SG smoothing 5
  if (option == "SGSMOOTH5") spectra.pp <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 5,  m = 0)))
  #SG smoothing 15
  if (option == "SGSMOOTH15") spectra.pp <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 15, m = 0)))
  #SG smoothing 29
  if (option == "SGSMOOTH29") spectra.pp <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 29, m = 0)))
  return(spectra.pp)
}

# option for preprocessing options
prepro <- c("REFLECTANCE", 
            "ABSORBANCE", 
            "SGDERIVATIVE", 
            "SNV", 
            "ABS-SNV", 
            "ABS-SGDERIVATIVE", 
            "SGSMOOTH5", 
            "SGSMOOTH15", 
            "SGSMOOTH29") 

# list of the model options
models <- c("pls") 


# create null output
output <- NULL

# Selecting the best model and preprocessing
set.seed(123)
# option 1  
for (p in prepro) { # 9 preprocessing
  for (m in models) { # 2 models
    print(paste0("Running ", p, " and ", m))
    pp.s <- pp(spectra = spectra, option = prepro)
    control  <- caret::trainControl(method = "cv", savePredictions = TRUE, verboseIter = TRUE, number=2, allowParallel = FALSE)
    tune.pls <- caret::train(y=pp.s[ ,1], x=pp.s[,-1], method= m, tuneLength= 5, trControl = control, verbose=TRUE)
    stat <- sm_Stats(tune.pls$pred$pred, tune.pls$pred$obs, plot = F)
    rownames(stat) <- paste0(p, "-", m)
    print(stat)
    if (is.null(output)){
      output <- stat
    } else { 
      if (output[1] < stat[1])
        output <- stat
      print(output)
    }
  } #close m
}#close p



# option 2
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
alloptions <- expand.grid(pp = prepro, model = models)
model.pp <- foreach(k = 1:nrow(alloptions), .combine = rbind) %dopar% {
  p <- as.character(alloptions[k, ]$pp)
  m <- as.character(alloptions[k, ]$model)
  # check 'invisible'
  pp.s <- pp(spectra = spectra, option = prepro)
  control  <- caret::trainControl(method = "cv", number=10, savePredictions = TRUE, verboseIter = TRUE, allowParallel = T)
  tune.pls <- caret::train(y=pp.s[ ,1], x=pp.s[,-1], method= m, tuneLength= 15, trControl = control, verbose=F)
  predict(tune.pls, newdata = data.frame(pp.s[ ,1]))
  stat <- sm_Stats(tune.pls$pred$pred , tune.pls$pred$obs, plot = F)
  rownames(stat) <- paste0(p, "-", m)
  print(stat)
  
}
model.pp
stopCluster(cl)

subset(model.pp, R2 == max(R2))

temp  <- strsplit(dates, "-")




###########################################################################
preprocessing <- 
modelling <- 
 
# option 1  
for (p in preprocessing) { # 9 preprocessing
  for (m in modelling) { # 2 models
    pp.s <- pp(spectra = spectra, option = preprocessing)
     if (preprocessing=="pls"){
      mod <- pls::plsr(y=pp.s[ ,1], x=pp.s[,-1], data=pp.s, ncomp = selected.pls.c, method="simpls", validation="none") 
    } else { 
        if (preprocessing=="cubist")
       
    }
  } #close m
}#close p




control  <- caret::trainControl(method = "cv", savePredictions = TRUE, verboseIter = TRUE, number=2, allowParallel = FALSE)
tune.pls <- caret::train(y=pp.s[ ,1], x=pp.s[,-1], method= m, tuneLength= 5, trControl = control, verbose=TRUE)






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
