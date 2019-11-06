library(Cubist)
library(plsr)
library(mxnet)
library(caret)

#select the imposted spectra
spec <- na.omit(global)
spec <- local20[c(1:10),]
spec <- rslocal.spectra
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
  if (option == "ABS_SNV") spectra.pp <- cbind(spectra[1], prospectr::standardNormalVariate(X = abs)) 
  # absorbance + derivative
  if (option == "ABS_SGDERIVATIVE") spectra.pp <- cbind(spectra[1], prospectr::standardNormalVariate(X = der))
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
            "ABS_SNV", 
            "ABS_SGDERIVATIVE", 
            "SGSMOOTH5", 
            "SGSMOOTH15", 
            "SGSMOOTH29") 

# list of the model options
models <- c("pls") 


# create null output
output <- NULL


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
  pp.s <- pp(spectra = spec, option = prepro)
  control  <- caret::trainControl(method = "cv", number=4, savePredictions = TRUE, verboseIter = TRUE, allowParallel = T)
  tune.pls <- caret::train(y=pp.s[ ,1], x=pp.s[,-1], method= m, tuneLength= 4, trControl = control, verbose=F)
  #predict(tune.pls, newdata = data.frame(pp.s[ ,1]))
  #source("~/GitHub/gssl/R/sm_Stats.R")
  stat <- sm_Stats(tune.pls$pred$pred , tune.pls$pred$obs, plot = F)
  rownames(stat) <- paste0(p, "-", m)
  print(stat)
  
}
model.pp
stopCluster(cl)

best <- subset(model.pp, R2 == max(R2))
best
temp  <- strsplit(rownames(best), "-")

###########################################################################
preprocessing <- temp[[1]][1]
modelling <- temp[[1]][2]
if(modelling == "pls"){ #transform pls to simpls
  modelling = "simpls"
} 
modelling

set.seed(123)
#  pls
for (p in preprocessing) { # preprocessing
  for (m in modelling) { # models
    print(paste0("Running ", p, " and ", m))
    pp.s <- pp(spectra = spec, option = preprocessing)
    y <- as.matrix(pp.s[ ,1])
    x <- as.matrix(pp.s[,-1])
    mod <- pls::plsr(y~x, ncomp=8 , method=modelling, validation="CV", number=8)
    stat2 <- sm_Stats(mod$fitted.values[,,8], y)
    print(stat2)
   }
}







