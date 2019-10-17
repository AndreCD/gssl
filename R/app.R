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
        mainPanel( 
          plotOutput("plot")
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
      input.pca <- prcomp(inputdata, scale = TRUE)
            pca <- fviz_pca_ind(res.pca, label="all", title = "")
            pca
    })
  }
  # Run the app ----
shinyApp(ui = ui, server = server)

  input.pca <- prcomp(test, scale = TRUE)
  p <- fviz_pca_ind(input.pca, label="all", title = "", repel = F,  col.ind = "blue")

  gssl.pca <- prcomp(spec[,c(15,length(spec))], scale = TRUE)
         fviz_pca_ind(input.pca, label="all", title = "", repel = F,  col.ind = "blue") 
    g <- fviz_pca_ind(gssl.pca, label="", title = "", repel = F,  col.ind = "black") 
  g$data$x
  plot(x = g$data$x, y = g$data$y, type="p",col="black", pch = 20)
  par(new=TRUE)
  plot(x = p$data$x, y = p$data$y, type="p",col="blue", pch = 20)

  filesInfo <- drop_dir("drop_test")
  filePaths <- filesInfo$path_lower[1]
  data      <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  spec      <- do.call(rbind, data)
  res.pca <- prcomp(spec[,c(15,length(spec))], scale = TRUE)
  groups  <- as.factor(spec$WRB)
  pca <-fviz_pca_ind(res.pca, label="none", title = "")
  pca
  