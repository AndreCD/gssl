
library(ggplot2)
# plot PCA 
pca.plot <- function(local, rslocal, global){
  if (any(is.na(local)) | any(is.na(rslocal)) | any(is.na(global))){
    warning("Check for NAs!")
  }
# generate PCA
local.pca    <- prcomp(local, scale = T)
rslocal.pca  <- prcomp(rslocal, scale = T)
global.pca   <- prcomp(global, scale = T)
# extract the PC1 and PC2 to plot
local.pca.gg   <- as.data.frame(local.pca$x[,c(1:2)])
rslocal.pca.gg <- as.data.frame(rslocal.pca$x[,c(1:2)])
global.pca.gg  <- as.data.frame(global.pca$x[,c(1:2)])

# generate PCA graphic with GGPLOT2
pca <- ggplot() + 
  geom_point(data=global.pca.gg, aes(x=PC1, y=PC2), color='gray')+ 
  geom_point(data=rslocal.pca.gg, aes(x=PC1, y=PC2), color='blue')+ 
  geom_point(data=local.pca.gg, aes(x=PC1, y=PC2), color='red', cex=2, pch = 19) +
  geom_text(data=local.pca.gg, aes(x=PC1, y=PC2, label = rownames(local.pca.gg), 
                                       colour = "red", hjust = .5, vjust = -.5)) +
  theme(legend.position = "none")
return(pca)
}

pca.plot(imported[-1], rslocal[-1], gssl[-1])

