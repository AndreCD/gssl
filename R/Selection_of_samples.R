# Selection of samples by finding the farthest points in a matrix
#' @param spectra data.frame of spectral data
#' @param p number of samples to select

subset <- function(p, spectra){
     n <- p
   mat <- as.data.frame(spectra)
  dmat <- as.matrix(stats::dist(mat))
  ind  <- integer(n)
  ind[1:2] <- as.vector(arrayInd(which.max(dmat), .dim = dim(dmat)))
  for (i in 1:n) {
        mm <- dmat[ind, -ind, drop = FALSE]
         k <- which.max(mm[(1:ncol(mm) - 1) * nrow(mm) + max.col(t(-mm))])
    ind[i] <- as.numeric(dimnames(mm)[[2]][k])
    }
  return(ind)
  }
########################################################################
### how to run the funciton ###
spectra <-          # your spectral data.frame here
p <-                # your number of samples to be selected here
### now run this function
subset(p,spectra) 
### in the result are the numbers of samples selectect
