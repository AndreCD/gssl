#' sm_KS
#'
#' Selection of samples using the Kennard-Stone algorithm 
#' @param x dataframe of variables on which to select
#' @param k number of points to sample, if greater than half of the sample size will be set to half sample size
#' @param group a vector of identifiers with nrow matching x - if specified entire groups will be selected and removed when one member is selected
#' @param updateProgress an optional function to provide progress bar updates in Shiny, with arguments 'value', 'max.value' and 'detail' 
#' @return a named list with index's for the calibration and validation sets
#' @keywords specmod
#' @references Kennard RW and Stone LA 1969 Computer Aided design of experiments. Technometrics 11, 137--148.
#' @export
#' @examples
#' R.A. Viscarra Rossel 2013

sm_KS <- function(x, k, group = NULL, updateProgress = NULL) {
  
  X <- as.matrix(x)
  m <- nrow(X)
  x <- cbind(1:m,X)
  n <- ncol(x)
  if (k >= m) k <- m-1
  
  # Fist two most distant points to model set
  d <- fast.dist(x[ ,2:n], x[ ,2:n])
  id <- arrayIndex(which.max(d), dim = dim(d))
  
  if (!is.null(group)) {
    # also select the group these samples belong to
    g_id <- which(group %in% group[id])
  } else {
    g_id <- id
  }
  
  model <- as.vector(x[g_id, 1])
  x <- x[-g_id,]
  group <- group[-g_id]
  
  # If we were passed a progress update function (for Shiny), call this
  if (is.function(updateProgress)) {
    text <- "KS sampling"
    updateProgress(value = 0, value.max = k, detail = text)
  } else {
    pb <- R.utils::ProgressBar(max = 50, stepLength = 50 / k, newlineWhenDone = TRUE)
    R.utils::reset(pb)
  }
  
  for (i in 3:k){
    
    d <- fast.dist(x[ ,2:n], X[model,]) 
    id <- which.max(apply(d, 2, min))

    if (!is.null(group)) {
      # also select the group these samples belong to
      g_id <- which(group %in% group[id])
    } else {
      g_id <- id
    }
    
    model <- c(model, x[g_id, 1])
    x <- x[-g_id,]
    group <- group[-g_id]
    
    if (is.function(updateProgress)) {
      text <- paste("KS sampling ", length(model))
      updateProgress(value = length(model), max.value = k, detail = text)
    } else {
      R.utils::increase(pb)   
    }    
    
  }  
  return(list(calibration = model, validation = (1:nrow(X))[-model]))
}
  
