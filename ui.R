# UI

#Shiny
library(shiny)
options(shiny.maxRequestSize = 50 * 1024^2) # Allow for bigger files

#Synthpop
library(synthpop)

#Anytime
library(anytime)
anytime::addFormats("%d/%m/%Y") # Ensure this format is recognised

#dplyr
library(dplyr)

#tidyr
library(tidyr)

#ggplot2
library(ggplot2)

#gridExtra
library(gridExtra)

#corrplot
library(corrplot)

#vcd
library(vcd)
#-------------------------------------------------------------------------------

# Define UI

fluidPage(
  # Title
  titlePanel("Synthetic Data Generator for Medicine and Healthcare Data"),
  
  # Side column display
  sidebarLayout(
    sidebarPanel(
      
      # Conditional panel that shows instructions if the checkbox is not checked
      conditionalPanel(
        condition = "!$('#hideInstructions').is(':checked')",
        p(strong("Instructions:")),
        p("Upload your file in CSV format."),
        p("Ensure the file contains a Patient ID column and a Date of Encounter column, selecting their positions in the dataset using the below sliders."),
        p("Adjust the number of patients and the maximum number of encounters per patient to generate."),
        p("Click the button below to generate."),
        p("Click the download button to save the synthetic data.")
      ),
      
      # Checkbox to hide or show instructions
      checkboxInput("hideInstructions", "Hide Instructions", value = FALSE),
      
      # Horizontal line
      tags$hr(),
      
      # 'Patient ID' Column Number
      numericInput("patientIDColumn", 
                   "Patient ID Column No.", 1, min = 1, max = 100),
      
      # 'Encounter Number' Column Number
      numericInput("encounterNoColumn", 
                   "Date of Encounter Column No.", 9, min = 1, max = 100),
      
      # Checkbox for variable names in header
      checkboxInput("header", "Tick box if header of file contains variable names", TRUE),
      
      # Horizontal line
      tags$hr(),
      
      # Input file (as CSV)
      fileInput("chosenFile", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      # Column Selection 
      uiOutput("columnSelect", style = "width: 100%;"),
      # Select All (for the above column select)
      checkboxInput("selectAll", "Select all columns for synthesis (warning: long wait times)"),
      
      # Horizontal line
      tags$hr(),
      
      # Maximum number of patient encounters to synthesise - 3rd Quartile in usual data set is 15
      numericInput("maxEncounterSyn", 
                   "Maximum Number of Encounters (per patient) to Synthesise", 20, min = 1, max = 100),
      
      # Number of instances of synthetic data generated  
      numericInput("numGenerated", 
                   "Number of Patients Generated:", 500, min = 1, max = 1000),
      
      # Horizontal line
      tags$hr(),
      
      # Go Button
      actionButton("goButton", strong("Click to Generate Synthetic Data")),
      
      # Horizontal line
      tags$hr(),
      
      # Button to download synthetic dataset
      downloadButton("downloadData", "Download"),
      
      # Width of Sidebar
      width = 3
    ),
    
    # Main column display
    mainPanel(
      # Synthetic Data Display Tab
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Synthetic Data",
          DT::dataTableOutput("synOutput")
        ),
        
        # Individual Synthetic Patient Display
        tabPanel(
          "Individual Synthetic Patients",
          numericInput("patientChosen", 
                       "Choose a patient number:", 1, min = 1, max = 200),
          DT::dataTableOutput("synPatients")
        ),
        
        # Summary Statistics Comparison Tab
        tabPanel(
          "Summary Statistics Comparison",
          # Column Selection 
          uiOutput("summaryColumnSelect", style = "width: 100%;"),
          fluidPage(
            br(),
            fluidRow(
              column(6,
                     h2("Original Data"),
                     verbatimTextOutput("originalSummary")
              ),
              column(6,
                     h2("Synthetic Data"),
                     verbatimTextOutput("synSummary")
              )
            )
          )
        ),
        
        # Individual Comparisons Tab
        tabPanel(
          "Column Comparisons",
          # Column Selection 
          uiOutput("compColumnSelect", style = "width: 100%;"),
          plotOutput("plotComp")
          
        ),
        
        # Correlation Comparisons Tab
        tabPanel(
          "Correlation Comparisons",
          # Column Selection 
          uiOutput("corrColumnSelect", style = "width: 100%;"),
          textOutput("ifPlotCannotBePrinted"),
          plotOutput("plotCorr")
        )
      ),
      
      # Width of Main Panel
      width = 9
    )
  )
)
