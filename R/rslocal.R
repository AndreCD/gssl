#' @title A function for data-mining spectral libraries using the ReSampling-Local (\code{RS-LOCAL}) algorithm
#' @description
#' This function implements the ReSampling-Local (\code{RS-LOCAL}) algorithm for data-mining large 
#' spectral libraries (SL) and returning a subset of the library suitable for 'local' or site-specific 
#' spectroscopic calibrations.
#' @usage
#' rslocal(SL.x, SL.y, m.x, m.y,
#'     k,b,r,
#'     method = "pls",
#'     pls.tune = FALSE, 
#'     pls.c = 10,
#'     allowParallel = FALSE,
#'     ...)
#' @param SL.x a \code{data.frame} of predictor variables of the spectral library (observations in rows and variables in columns)
#' @param SL.y a numeric \code{vector} containing the values of the response variable corresponding to the SL.x data
#' @param m.x a \code{data.frame} of predictor variables of the 'local' or site-specific samples (observations in rows and variables in columns)
#' @param m.y a numeric \code{vector} containing the values of the response variable corresponding to the m.x data
#' @param k the number of SL samples randomly selected in the re-sampling step, and also the target number of SL samples returned by the algorithm
#' @param b the number of times each sample in the SL should be tested, on average, in each iteration of the algorithm
#' @param r the proportion of SL samples removed in each iteration of the algorithm
#' @param method a character string indicating the method to be used for the internal calibrations within the rs-local alrgorithm. Options are: \code{"pls"} or \code{"wapls"}. Note: \code{"wapls"} is not yet implemented.
#' @param pls.tune an optional boolean \code{value} to specify the method for pls factor selection when \code{"pls"} is used. If TRUE a 10-fold cross validation is used to inform selection and pls.c is ignored. if FALSE the number of factors must be specified using pls.c. Setting to TRUE greatly increases processing time. Default is FALSE.
#' @param pls.c the number of pls components to be used in the selected regression method.
#'        When \code{"pls"} is used, this argument must be a single numerical value. When \code{"wapls"} is used, this argument must be
#'        a \code{vector} of length 2 indicating the minimum (first value) and the maximum (second value) number of pls components (\code{"wapls"} not yet implemented)
#' @param allowParallel set to TRUE to parallelise the internal re-sampling and calibration. The parallel backend should be registered by the user.         
#' @param ... Other arguments passed on to methods (Not currently used).
#' @details
#' The \code{RS-LOCAL} algorithm requires a spectral library (SL), a small subset of site-specific samples, \code{m}, 
#' and three parameters, \code{k}, \code{b} and \code{r}, which we describe below. The \code{m} samples should be 
#' measured with the spectrometer and analyzed using the reference laboratory analytical method. 
#' These samples should also be representative of the entire population from the study site. They may be selected, 
#' for example, from all of the study site's spectra using the Kennard-Stone (KS) 
#' method (Kennard & Stone, 1969). \code{RS-LOCAL} uses the \code{m} data and re-sampling to evaluate and then remove irrelevant data 
#' from the SL so that the data that remain are the most appropriate for deriving a site-specific calibration.
#' The SL subset, \code{K}, that remains is typically augmented by the user with the \code{m} data by spiking. Together, \code{K} plus \code{m} 
#'  form an \code{RS-LOCAL} data set, which can be used for deriving site-specific spectroscopic calibrations.
#'
#' When selecting values for the \code{k},\code{b},\code{r} parameters,
#' \itemize{
#'  \item{\code{k:}}{ The size of the sampled dataset used in the internal calibration and validation, and the target number of SL samples ultimately returned by the algorithm. A recommended value for \code{k} is 300 (see Lobsey et al. 2017)}
#'  \item{\code{r:}}{ The number of times each SL sample is tested in each iteration of the algorithm (on average). More consistent results are achieved with high \code{b} values, however this will increase processing time. A recommended value for \code{b} is greater than 40}
#'  \item{\code{r:}}{ The number of SL samples removed in each iteration of the algorithm. More consistent results are achieved with low \code{r} values, however this will increase processing time. A recommended value for \code{r} is less than 0.1}
#'  }
#' @return a \code{list} with the following components:
#' \itemize{
#'  \item{\code{K.x}:}{ a \code{data.frame} of predictor variables of the SL samples selected for 'local' or site specific calibration}
#'  \item{\code{K.y}:}{ a numeric \code{vector} containing the values of the response variable corresponding to the K.x data}
#'  \item{\code{idx}:}{ a numeric \code{vector} containing the index into the original SL (row numbers) for the selected samples (K)}
#'  }
#' @author Craig Lobsey and Raphael Viscarra Rossel
#' @references 
#' Lobsey C., Viscarra Rossel R.A., Pierre R., Hedley C.B. 2017. RS-LOCAL data-mines information from large spectral libraries to improve local calibrations. European Journal of Soil Science.
#' 
#' Kennard, R.W. & Stone, L.A. 1969. Computer aided design of experiments. Technometrics, 11(1), pp.137-148.
#' @examples
#' \dontrun{
#' #' rslocal
#' #' spike (rbind) m.x + K.x and (m.y + K.y) as new training data set
#' #' }
#' @export


library(foreach)
rslocal <- function(SL.x, SL.y, m.x, m.y, k, b, r, pls.tune=FALSE, pls.c=10, method="pls", allowParallel=FALSE) {
  
  # check inputs
  if(nrow(SL.x) != length(SL.y))
    stop("The number of spectra in SL.x must equal the length of SL.y")
  
  if(nrow(m.x) != length(m.y))
    stop("The number of spectra in m.x must equal the length of m.y")
  
  if(ncol(SL.x) != ncol(m.x))
    stop("The number of variables in SL.x must equal the numnber of variables in m.x")
  
  # CHECK DATA.FRAME

  # we can't just rely on core registration as the user may want to parallise only some components of the analysis  
  '%dox%' <- if (allowParallel) get('%dopar%') else get('%do%')
  
  message(paste("The initial size of the SL is", length(SL.y), "samples"))
  
  # This list tracks the current selected SL subset (K) at each iteration
  K.list <- list()
  
  # SL.idx is a vector representing an sample index into the SL
  # note in this implementation we operate with an index into the SL and not copies of the SL spectra 
  SL.idx <- seq(1,nrow(SL.x))

    #
    # Step 1 - Initialise K as a subset of the SL, initially full. 
    #
    K.idx <- SL.idx

  #
  # This loop contains step 2-6 of the algorithm (See step 7!). 
  #
  
  # use this to keep track of the number of iterations
  outer.idx <- 0
  
  while (length(K.idx) > (k * (1+r))) {
    
    #
    # Step 1 - Initialise K as a subset of the SL, initially full. K.idx only contains those SL samples still in K
    #
    # select K.idx as those in the SL not marked as dropped (zero)
    K.idx <- SL.idx[SL.idx>0]
    
    
    outer.idx <- outer.idx + 1
    
    # calculate the quantity of samples removed in this iteration
    cull.quantity <- round(length(K.idx)*r)
    
    # provide some status information to the user
    message(paste("iteration: ", outer.idx, ", current K subset size: ", length(K.idx), ", SL samples to drop: ", cull.quantity, sep=""))
    
    #
    # This loop contains step 2-4 of the algorithm (See step 5!). This is the B iteration. 
    #
    B <- round(length(K.idx) * b / k)
    
    message(paste("Performing", B, "sampling iterations (B)"))
    
    results.list = foreach(idx = seq(1,B)) %dox% {
      
      # 
      # Step 2 - sample a training data set of size k from K without replacement
      #
      selected.idx <- sample(K.idx, size=k, replace=FALSE)
 
      # retrieve the spectra and dependent variable from the SL for the training set selection 
      X <- SL.x[selected.idx,]
      Y <- SL.y[selected.idx]
  
      dataset <- cbind(X,Y)
      
      #
      # Step 3 calibrate a PLS model using the selected SL samples
      #
      
      if (pls.tune) {
        # perform a 10-fold cross validation of the selected SL (k) samples to determine a suitable number of pls factors
        control <- caret::trainControl(method = "cv", savePredictions = TRUE, verboseIter = TRUE, number=10, repeats=1, allowParallel = FALSE)
        tune.pls <- caret::train(x=X, y=Y, method='pls', tuneLength=pls.c, trControl = control, verbose=TRUE)
        selected.pls.c <- tune.pls$bestTune$ncomp
      } else {
        selected.pls.c <- pls.c
      }
      
      # we don't want to parallelise here as well
      pls::pls.options(parallel = 1)
      
      # calibrate the model
      mod <- pls::plsr(Y~.,data=dataset, ncomp=selected.pls.c, method="simpls", validation="none") 
      
      #
      # Step 4 - validate on the 'm' site specific samples
      #
      
      Y.pred <- as.numeric(predict(mod, m.x, ncomp=selected.pls.c))
      
      
      # It is possible we get NA predictions, catch this case and ignore this sample test
      if (any(is.na(Y.pred))) {
        
      } else {
        
        RMSE <- sqrt(sum((Y.pred - m.y)^2)/(length(m.y) - 1))
        
        # combine the validation results and the samples that were selected for this model
        a <- c(RMSE, selected.idx)
        names(a)[1] <- 'RMSE.subset'
      
        return(a)
            
      }

    }
    
    #
    # Step 6 - starts here
    #
    
    # compile a data frame of all results of the sampling iterations
    results.df <- data.frame(do.call('rbind', results.list))
    
    # initialise a vector for the sample rankings (by rmse) that corresponds with the full SL
    U <- rep(0,length(SL.idx))
    names(U) <- SL.idx
    
    # also initialise a vector to track the number of times each sample was tested in the 'B' iteration
    V <- rep(0,length(SL.idx))
    names(V) <- SL.idx
    

    #
    # iterate through all B iteration results and increment the rankings (rmse) and test count for each sample
    #
    
    # note there must be a better way to do this with apply - to do.
    for (i in 1:nrow(results.df)) { 
    
      B.result <- as.vector(t(results.df[i,]))
      
      k.samples <- B.result[2:length(B.result)]
      B.rmse <- B.result[1]
      
      U[k.samples] <-  U[k.samples] + B.rmse  # the index of samples used starts at 4
      V[k.samples] <-  V[k.samples] + 1
      
    }
    
    
    # normalise U using V 
    U <- U / V
    U.df <- data.frame(idx=SL.idx, rmse=U)
    
    # now order / rank by rmse
    U.ordered <- U.df[order(-U.df$rmse),]
    
    # select the poorest performing SL samples, the amount is determing by cull.quantity calculated earlier
    worst.SL.set <- U.ordered[1:cull.quantity,]
    
    # now set the latest dropped samples in SL.idx to zero to remove them from consideration
    SL.idx[worst.SL.set$idx] <- 0
    
    
  }
  
  return(list(K.x = SL.x[K.idx,], K.y=SL.y[K.idx], K.idx=K.idx))

}
