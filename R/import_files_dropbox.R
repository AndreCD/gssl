
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

##############################################################################
#################### Calibration with the gssl only - general calib
gssl.path <- lapply(filePaths[3], drop_read_csv, stringsAsFactors = FALSE)
gssl.path <- do.call(rbind, gssl.path)
gssl      <- cbind(variable=gssl.path$C, gssl.path[ ,c(26:2176)])

##############################################################################
#################### gssl + 20 imported - spiking papers by Guerrero et al.
imported.gssl <- rbind(imported, gssl)

##############################################################################
#SL.x <- data.frame(gssl[,c(2:2152)]) #spectra
#SL.y <- as.matrix(gssl[1]) 
#m.x  <- data.frame(imported[,c(2:2152)]) # spectra
#m.y  <- as.matrix(imported[1]) 
#k <- 100
#b <- 10
#r <- 0.1
#setwd("~/GitHub/gssl-proj/R")
#source("rslocal.R")
#rslocaldata <- rslocal(SL.x, SL.y, m.x, m.y, k, b, r, method = "pls", pls.tune = FALSE, pls.c = 5, allowParallel = TRUE)
#rslocal <- cbind(variable = rslocaldata$K.y, rslocaldata$K.x) 
#imported.rslocal <- rbind(imported, rslocal)
#################### upload in dropbox
#write.csv(rslocal, filePath, row.names = FALSE, quote = TRUE)
#drop_upload(filePath, path = "drop_test")

#################### import sr-local
rslocal.path <- lapply(filePaths[1], drop_read_csv, stringsAsFactors = FALSE)
rslocal.path <- do.call(rbind, rslocal.path)
rslocal      <- cbind(variable=rslocal.path$variable, rslocal.path[ ,c(2:2152)])

##############################################################################
