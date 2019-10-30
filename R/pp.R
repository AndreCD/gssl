
# create list with all reflectance
pp <- function(spectra){
  if (any(is.na(spectra[-1]))) {
    warning("Check for NAs!")
  }
  # Reflectance
  spectra <- spectra
  # absorbance
  abs <- cbind(spectra[1], log10(1/spectra[-1]))
  # SG Derivative
  der <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 9, m = 1)))
  # SNV
  snv <- cbind(spectra[1], prospectr::standardNormalVariate(X = spectra[-1])) 
  # absorbance + SNV
  abs.snv <- cbind(spectra[1], prospectr::standardNormalVariate(X = abs)) 
  # absorbance + derivative
  abs.der <- cbind(spectra[1], prospectr::standardNormalVariate(X = der))
  #SG smoothing 5
  sgs5  <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 5,  m = 0)))
  #SG smoothing 15
  sgs15 <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 15, m = 0)))
  #SG smoothing 29
  sgs29 <- cbind(spectra[1], data.frame(prospectr::savitzkyGolay(spectra[-1], p = 2, w = 29, m = 0)))
  
  list.pp <- list(spectra, sgs5, sgs15, sgs29, abs, abs.snv, abs.der, der, snv) 
  return(list.pp)
  }
  
prepro <- pp(imported)
