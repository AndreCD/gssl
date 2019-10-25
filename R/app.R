

#function
plot1 <- function(inputdata) {
inputdata <- randomVals() # create object for InputFile
inputdata.spectra <- inputdata[-1] #remove soil variable
# generate PCA using InputFile 
inputdata.pca <- prcomp(inputdata.spectra, scale = F)
# extract the PC1 and PC2 to plot
inputdata.pca.gg <- as.data.frame(inputdata.pca$x[,c(1:2)])
# inport GSSL from DropBox
filesInfo <- drop_dir("drop_test")
filePaths <- filesInfo$path_lower[1] # select the first file from DropBox folder
gssl2 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
gssl <- do.call(rbind, gssl2)
# generate PCA using GSSL dataset
gssl.pca <- prcomp(gssl[,c(15,length(gssl))], scale = T)
# extract the PC1 and PC2 to plot
gssl.pca.gg <- as.data.frame(gssl.pca$x[,c(1:2)])
# generate PCA graphic with GGPLOT2
g <- ggplot() + 
  geom_point(data=gssl.pca.gg, aes(x=PC1, y=PC2), color='black') + 
  geom_point(data=inputdata.pca.gg, aes(x=PC1, y=PC2), color='red', cex=2, pch = 19) +
  geom_text(data=inputdata.pca.gg, aes(x=PC1, y=PC2, label = rownames(inputdata.pca.gg), 
                                       colour = "red", hjust = .5, vjust = -.5)) +
  theme(legend.position = "none")
g
return(g)
}

  
  source("./R/rslocal.R")

  test.outl <- test[ , c(4,19:length(test))]
  outliers <- boxplot(test.outl$Clay, plot=FALSE)$out
  
  print(outliers)
  test.outl2 <-test.outl[-which(test.outl$Clay %in% outliers),]
  boxplot(test.outl$Clay)
  identify(rep(1, length(test.outl$Clay)), test.outl$Clay, labels = seq_along(test.outl$Clay))
  
  model <- pls::plsr(variable~.,data=rslocal.selected, ncomp=5, method="simpls", validation="none")

  