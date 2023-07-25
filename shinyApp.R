
# Author : Kennedy Gachigi : An R-shiny project to look at meta analysis results from 
           # SPORT178: Outcomes after Subacromial Balloon Spacer Implantation for Massive and Irreparable Rotator Cuff Tears - A Systematic Review and Meta-Analysis


library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyverse)
library(DT)
library(readxl)
library(openxlsx)
library(meta)
library(forestplot)
library(shinythemes)
library(httr) # the GET function is in this library


github_link <- "https://github.com/KennedyGachigi/RShinyApp/raw/main/Dataset.xlsx"


# These lines access the githug link, save the file with an xlsx extension in a local temp folder in the  computer then sace the link in the path object.
temp_file <- tempfile(fileext = ".xlsx")
req <- GET(github_link,  write_disk(path = temp_file))
path <- temp_file 


# getting data from sheets
sheets <- getSheetNames(path)
PROs <- lapply(sheets, read.xlsx, xlsxFile=path)

# assigning names to data frame
names(PROs) <- sheets


# this function puts every item in the list into a data table 
# wrap this function with invisible to remove message from output

invisible(list2env(PROs ,envir=.GlobalEnv))


meta_analysis <-function(outcome) {
  
  meta_data <- metacont(postopN, postopMean, postopSD, preopN, preopMean, preopSD, 
                        studlab = StudyID, random = T, common = F, data = outcome, sm = "MD", 
                        title = "Score", label.e = "Postoperative", label.c = "Preoperative")
  
  forest(meta_data, lefcols = c("StudyID"), layout = "RevMan5") }

# User interface

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    # Set up the dropdown menu
     selectInput(inputId = "table", 
                 label = "Select a table:", 
                 choices = names(PROs), 
                 selected = NULL),

# Set up the table output
tableOutput(outputId = "table_output"),

# set up forest plot
plotOutput(outputId = "forest_output")


)

# server

server <- function(input, output) {
  
  # Load the data into a reactive object
  data <- reactive(PROs)
  
  # Set up the table output
  output$table_output <- renderTable({
    # Get the selected table
    table_selected <- input$table
    
    # Get the table from the data object
    table_data <- data()[[table_selected]]
    
    # Return the table data
    table_data
  })
    
  # Set up the forest plot output
    output$forest_output <- renderPlot({
      # Get the selected table
      table_selected <- input$table
      
      # Get the table from the data object
      table_data <- data()[[table_selected]]
      
      # Return the table data
      meta_analysis(table_data)
    
  })
}


## start R shiny

shinyApp(ui = ui, server = server)


