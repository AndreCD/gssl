library(shiny)
library(rdrop2)
library(dplyr)
library(ggplot2)
library(factoextra)

#if(input$disp == "head") {
#  return(head(df))
#}
#else {
#  return(df)
#}

#write.csv(spectra100, 'spectra.csv')
#drop_create('drop_test') #create folder
#drop_upload('spectra.csv', path = "drop_test") #upload
#drop_delete('drop_test/spectra.csv') 




####################################
ui = fluidPage(
  titlePanel("GSSL"),
   sidebarLayout(
     sidebarPanel(
          fileInput("Inputfile", "Choose CSV File",accept = c("text/csv", "text/comma-separated-values",".csv")),
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep","Separator", choices=c(Comma=",", semicolon=";",Tab="\t"), selected = ","),
          actionButton("act", label = "Input Data")
                 ),
        mainPanel(h3("Results"),
                  plotOutput("plot"),
                 )
                )
   )

  server <- function(input, output) {
    randomVals <- eventReactive(input$act, {
      req(input$Inputfile)
      
      df <- read.csv(input$Inputfile$datapath,
                     header = input$header,
                     sep = input$sep)
    })
    
    output$plot <- renderPlot({
                    inputdata <- randomVals()
            inputdata.spectra <- inputdata[-1]
                    #pca input
                inputdata.pca <- prcomp(inputdata.spectra, scale = F)
             inputdata.pca.gg <- as.data.frame(inputdata.pca$x[,c(1:2)])
                    # pca GSSL
                    filesInfo <- drop_dir("drop_test")
                    filePaths <- filesInfo$path_lower[1]
                        gssl2 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
                         gssl <- do.call(rbind, gssl2)
                     gssl.pca <- prcomp(gssl[,c(15,length(gssl))], scale = T)
                  gssl.pca.gg <- as.data.frame(gssl.pca$x[,c(1:2)])
              
                            g <- ggplot() + 
                                  geom_point(data=gssl.pca.gg, aes(x=PC1, y=PC2), color='black') + 
                                  geom_point(data=inputdata.pca.gg, aes(x=PC1, y=PC2), color='red', cex=2, pch = 19) +
                                  geom_text(data=inputdata.pca.gg, aes(x=PC1, y=PC2, label = rownames(inputdata.pca.gg), 
                                                                       colour = "red", hjust = .5, vjust = -.5)) +
                                  theme(legend.position = "none")
                            g
    })
  }
# Run the app ----
shinyApp(ui = ui, server = server)
  
##########################################################################


