library(shiny)
library(rdrop2)
library(dplyr)
library(ggplot2)
library(prospectr)
library(caret)
library(parallel)
library(reshape2)
library(pls)
library(Cubist)
library(kernlab)
library(nnet)
library(e1071)

fluidPage(  #Elements to include within the page
  titlePanel("GSSL"), #Title of the page
  sidebarLayout(   #containing the %sidebarPanel and %mainPanel
    sidebarPanel(    #containing input controls
      fileInput("Inputfile", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values",".csv")), 
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep","Separator", choices=c(Comma=",", semicolon=";",Tab="\t"), 
                   selected = ","),
      actionButton("act", label = "Input Data")#,
      #numericInput("num", h3("variable Column"), value = "")
                ),
    mainPanel(
      h3("Results"), #containing outputs
      splitLayout(   #split positions 
        plotOutput("plot"),
        tableOutput('table')
      )
    )
  )
)