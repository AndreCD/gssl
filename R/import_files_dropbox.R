
library(rdrop2)
library(dplyr)
##############################################################################
#################### import data from dropbox 
filesInfo <- drop_dir("drop_test")
# select the first file from DropBox folder
filePaths <- filesInfo$path_lower[] 

##############################################################################
#################### Calib with 20 importedonly - local 
imported.path <- lapply(filePaths[2], drop_read_csv, stringsAsFactors = FALSE)
imported.path <- do.call(rbind, imported.path)
imported      <- cbind(variable=imported.path$C, imported.path[ ,c(26:2176)])
local20  <- read.csv("C:/Users/280240B/Downloads/spectra/20local.csv")
local20  <- data.frame(cbind(variable=local20$Carbono, local20[,c(66:2216)]))

##############################################################################
#################### Calibration with the gssl only - general calib
gssl.path <- lapply(filePaths[3], drop_read_csv, stringsAsFactors = FALSE)
gssl.path <- do.call(rbind, gssl.path)
gssl2     <- cbind(variable=gssl.path$C, gssl.path[ ,c(26:2176)])
gssl      <- data.frame(cbind(variable=gssl.test$C, gssl.test[,c(26:length(gssl.test))])) #spectra
info      <- read.csv("C:/Users/280240B/Downloads/spectra/BESB-info.csv")
spectra   <- read.csv("C:/Users/280240B/Downloads/gssl-test.csv")
global    <- data.frame(cbind(variable=spectra$C, spectra[,c(26:length(spectra))]))
##############################################################################
#################### gssl + 20 imported - spiking papers by Guerrero et al.
imported.gssl <- rbind(imported, gssl)

##############################################################################
SL.x <- data.frame(gssl.test[,c(26:length(gssl.test))]) #spectra
SL.y <- as.matrix(gssl.test$C) 
m.x  <- data.frame(local20[,c(66:2216)]) # spectra
m.y  <- as.matrix(local20$Carbono) 
k <- 100
b <- 10
r <- 0.1
setwd("~/GitHub/gssl/R")
source("rslocal.R")
rslocaldata <- rslocal(SL.x, SL.y, m.x, m.y, k, b, r, method = "pls", pls.tune = FALSE, pls.c = 5, allowParallel = TRUE)
rslocal <- cbind(variable = rslocaldata$K.y, rslocaldata$K.x) 
beepr::beep(2)
rslocal.local <- rbind( local20, rslocal)


#################### upload in dropbox
#write.csv(rslocal, filePath, row.names = FALSE, quote = TRUE)
#drop_upload(filePath, path = "drop_test")

#################### import sr-local
rslocal.path <- lapply(filePaths[1], drop_read_csv, stringsAsFactors = FALSE)
rslocal.path <- do.call(rbind, rslocal.path)
rslocal      <- cbind(variable=rslocal.path$variable, rslocal.path[ ,c(2:2152)])
rslocal.data      <- read.csv("C:/Users/280240B/Downloads/spectra/rslocal.csv")
rslocal.spectra   <- cbind(variable=rslocal.data$C, rslocal.data[ ,c(28:2178)])
##############################################################################

#BESB.info <- read.csv("C:/Users/280240B/Downloads/spectra/BESB-info.csv")
#BESB.spectral <- read.csv("C:/Users/280240B/Downloads/spectra/BESB-spectral.csv")
#gssl  <- data.frame(cbind(BESB.info, BESB.spectral[ ,5:length(BESB.spectral)]))
