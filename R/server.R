
function(input, output){ 
  randomVals <- eventReactive(input$act,{ #returns a reactive expression object 
                                req(input$Inputfile) #Ensure that values are available before proceeding with a calculation or action. 
                                df <- read.csv(input$Inputfile$datapath, header = input$header, sep = input$sep) # read InputFile
                                         }
                              )
  # Renders a reactive plot that is suitable for assigning to an output slot.  
  output$plot <- renderPlot({
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
  })
}



