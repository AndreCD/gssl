library(shiny)
library(rdrop2)
library(dplyr)
library(ggplot2)
library(factoextra)

#write.csv(spectra100, 'spectra.csv')
#drop_create('drop_test') #create folder
#drop_upload('spectra.csv', path = "drop_test") #upload
#drop_delete('drop_test/spectra.csv') 

ui = fluidPage(titlePanel("submitButton example"),
               fileInput("table", "Choose CSV File",accept = c("text/csv", "text/comma-separated-values",".csv")),
               checkboxInput("header", "Header", TRUE),
               radioButtons("sep","Separator", choices=c(Comma=",", semicolon=";",Tab="\t"), selected = ";"),
               actionButton("Submit", label = "Go!"),
               plotOutput("distPlot", width = 400, height = 300), verbatimTextOutput("text")
               )
                 
server <- function(input, output) {
  eventReactive(input$Submit,{
    if(is.null(input$Submit)){
      return()
      }
    })
  df <- eventReactive(input$Submit, {
    plot(input$table)
  })
  
  output$distPlot <- renderPlot({
   df
    })
}

shinyApp(ui = ui, server = server)








server = function(input, output) {
  filesInfo <- drop_dir("drop_test")
  filePaths <- filesInfo$path_lower[1]
  data      <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  spec    <- do.call(rbind, data)
  res.pca <- prcomp(spec[,c(15,length(spec))], scale = TRUE)
  groups  <- as.factor(spec$WRB)
  pca <-fviz_pca_ind(res.pca, label="none", habillage= groups, title = "")
  
  output$plot1 <- renderPlot({
    pca
  })
  

}

shinyApp(ui = ui, server = server)


library(shiny)

ui <- fluidPage(
  actionButton("go", "Go"),
  fileInput("n", "Choose CSV File",accept = c("text/csv", "text/comma-separated-values",".csv")),
  plotOutput("plot")
)

server <- function(input, output) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$go, {
    hist(input$n)
    
  })
  
  output$plot <- renderPlot({
    ntext()
  })
}

shinyApp(ui, server)
