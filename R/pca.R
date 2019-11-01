

library(ggplot2)
# plot PCA 
pca.plot <- function(local, rslocal, global){
  if (any(is.na(local)) | any(is.na(rslocal)) | any(is.na(global))){
    warning("Check for NAs!")
  }
  # generate PCA
  global.pca   <- prcomp(global, scale = T)
  rslocal.pca  <- predict(global.pca, rslocal)
  local.pca    <- predict(global.pca, local)
  # extract the PC1 and PC2 to plot
  local.pca.gg   <- as.data.frame(cbind(PC1=local.pca[,1],  PC2=local.pca[,2]))
  rslocal.pca.gg <- as.data.frame(cbind(PC1=rslocal.pca[,1],PC2=rslocal.pca[,2]))
  global.pca.gg  <- as.data.frame(global.pca$x[,c(1:2)])
  
  # generate PCA graphic with GGPLOT2
  pca <- ggplot() + 
    geom_point(data=global.pca.gg, aes(x=PC1, y=PC2), pch = 21, cex=2, col="gray60", bg="gray90",show.legend = TRUE)+ 
    geom_point(data=rslocal.pca.gg, aes(x=PC1, y=PC2),pch = 21, cex=2, col="black", bg="blue",show.legend = TRUE)+ 
    geom_point(data=local.pca.gg, aes(x=PC1, y=PC2),  pch = 21, cex=3, col="black", bg="red",show.legend = TRUE) +
    #ggrepel::geom_text_repel(data=local.pca.gg, aes(x=PC1, y=PC2, label = rownames(local.pca.gg)), size=4,  fontface=1, box.padding = unit(0.2, "lines"),
    #                         point.padding = unit(0.2, "lines"), colour = "black", hjust = .5, vjust = -.5) +
    theme(legend.position = "none") + theme_bw() 
  return(pca)
}
pca.plot(local20[-1], rslocal[-1], gssl[-1])

