# Server

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

# Define Server Logic

function(input, output, session) {
  
  # Read in the dataset
  readInData <- reactive({
    
    # Ensure input is available
    req(input$chosenFile)
    
    inFile <- input$chosenFile
    if (is.null(inFile)) return(NULL) # For column select
    
    # Read the file
    data <- read.csv(inFile$datapath, header = input$header)
    
    # Original column numbers
    patientNoCol = input$patientIDColumn
    dateOfEncounterCol = input$encounterNoColumn
    
    # *** Moving the columns to the correct positions ****
    
    # Ensure patientNoCol and dateOfEncounterCol are within the number of columns in data
    if(patientNoCol > ncol(data) | dateOfEncounterCol > ncol(data) | patientNoCol < 1 | dateOfEncounterCol < 1) {
      stop("patientNoCol or dateOfEncounterCol is out of bounds.")
    }
    
    # Get the sequence of all column indices
    all_indices <- seq_along(data)
    
    # Exclude patientNoCol and dateOfEncounterCol from the sequence
    remaining_indices <- all_indices[!all_indices %in% c(patientNoCol, dateOfEncounterCol)]
    
    # Combine patientNoCol, dateOfEncounterCol and the remaining indices
    new_order <- c(patientNoCol, dateOfEncounterCol, remaining_indices)
    
    # Reorder the dataframe columns
    data <- data[, new_order]
    
    # New column numbers
    patientNoCol = 1
    dateOfEncounterCol = 2
    
    # Change the dates to date object
    data[,dateOfEncounterCol] <- anytime(data[,dateOfEncounterCol]) #Change to date objects
    
    # Clean the dataset of any fully NA values and any columns that are all the same value
    #data <- data %>% select(where(~ any(!is.na(.))))
    nonUniqueColumns <- sapply(data, function(x) length(unique(x)) == 1)
    data <- data[, !nonUniqueColumns]
    
    # Clean the dataset of any columns with over 70% NA or blank values
    # Function to count NA and empty string as missing values
    count_missing <- function(x) {
      sum(is.na(x) | x == "")
    }
    # Calculate the proportion of missing values in each column
    missing_proportion <- apply(data, 2, count_missing) / nrow(data)
    # Identify columns where the proportion of missing values is greater than 70%
    columns_to_remove <- missing_proportion > 0.7
    # Remove those columns from the dataframe
    data <- data[, !columns_to_remove]
    
    # Ensure the columns are in the correct order
    data <- data[order(data[,patientNoCol], data[,dateOfEncounterCol]), ]
    
  })
  
  ################################################################################
  
  # Observer to dynamically update the max value of 'patientChosen'
  observe({
    updateNumericInput(session, "patientChosen", 
                       max = input$numGenerated)
  })
  
  # Column Select Code
  output$columnSelect <- renderUI({
    data <- readInData()
    if (is.null(data)) return(NULL)
    selectInput("selectedColumns", "Select Columns to Synthesize",
                choices = names(data)[-c(1:2)], multiple = TRUE, width = '100%')
  })
  
  ################################################################################
  
  # Summary Statistics Column Select Code
  output$summaryColumnSelect <- renderUI({
    data <- syntheticData()
    #Store variable names
    variableNames <- c(colnames(data)[2:ncol(data)])
    # Create select column
    if (is.null(data)) return(NULL)
    selectInput("summarySelectedColumn", "Select Column for Summary Statistics",
                choices = variableNames, multiple = TRUE, width = '100%')
  })
  
  ################################################################################
  
  # Comp Column Select Code
  output$compColumnSelect <- renderUI({
    data <- syntheticData()
    #Store variable names
    variableNames <- c(colnames(data)[2:ncol(data)])
    # Create select column
    if (is.null(data)) return(NULL)
    selectInput("compSelectedColumn", "Select Column to Compare",
                choices = variableNames, multiple = FALSE, width = '100%')
  })
  
  ################################################################################
  
  # Correlation Column Select Code
  output$corrColumnSelect <- renderUI({
    data <- syntheticData()
    # Store variable names
    variableNames <- colnames(data)[2:ncol(data)]
    #variableNames <- colnames(data)[(daysSinceFirstEncounterCols[1]+1):(daysSinceFirstEncounterCols[2]-1)]
    
    # Select the columns for correlation analysis
    if (is.null(data)) return(NULL)
    selectInput("corrSelectedColumn", "Select Columns to Compare",
                choices = variableNames, multiple = TRUE, width = '100%')
  })
  
  ################################################################################
  
  # Create the dataset to synthesise
  newData <- reactive({
    
    
    data <- readInData() # Read in dataset
    
    # Initialise again
    patientNoCol = 1
    dateOfEncounterCol = 2
    
    # Ensure only valid columns are selected
    columnsSynthesised <- intersect(input$selectedColumns, names(data))
    # Override if the all columns checkbox is selected
    if (input$selectAll)
    {
      columnsSynthesised <- names(data)[-c(1:2)]
    }
    
    req(length(columnsSynthesised) > 0)  # Ensure there's at least one valid column
    
    # Change to Time Intervals
    #------------------------------------------------------------------------------
    
    TimeInterval <- rep(0, nrow(data)) # New Vector
    if (nrow(data) > 1) # Check to ensure there are at least 2 rows
    {  
      firstDate <- data[1,dateOfEncounterCol]
      for (i in 2:nrow(data)) 
      {
        if (data[i,patientNoCol]!=data[i-1,patientNoCol])
        {
          firstDate <- data[i,dateOfEncounterCol]
        }
        TimeInterval[[i]]<-difftime(data[i,dateOfEncounterCol],firstDate, units = "days")
      }
    }
    
    # Select the columns to be used in synthesis
    PatientID <- data[, 1] # First subset using column numbers
    subset2 <- data[, columnsSynthesised] # Second subset from column names
    # Make a variable for new column names
    new_colnames <- "PatientID" 
    new_colnames <- c(new_colnames, colnames(data[columnsSynthesised]))
    
    data <- data.frame(PatientID = as.numeric(PatientID), subset2) # Combine the two subsets
    colnames(data) <- new_colnames # Rename
    
    
    #print(data)
    # Move the fixed values to the beginning
    #------------------------------------------------------------------------------
    
    fixedColCount <- 0 # Create a count of the number of columns with fixed values
    data[, patientNoCol] <- as.numeric(as.character(data[, patientNoCol])) # Ensure patientID column is numeric
    patientIndices <- which(diff(data[,patientNoCol]) != 0) + 1 # Segment the dataset into patients
    for (i in (patientNoCol+1):ncol(data)) # Check for each column
    {
      isFixed <- 1 # Boolean checking if the column values are fixed or variable
      # First patient segment starts at row 1
      uniqueValues <- length(unique(data[1:(patientIndices[1]-1), i]))==1 # Is there only 1 value
      uniqueWithNA <- (length(unique(data[1:(patientIndices[1]-1), i]))==2)&&
        (any(is.na(unique(data[1:(patientIndices[1]-1), i])))) # Are there 2 values and 1 is NA
      if ((!uniqueValues)&&(!uniqueWithNA))
      {
        isFixed <- 0
      }
      if (isFixed == 1)
      {
        for (j in 2:length(patientIndices)) # Check for each patient
        {
          # Check if the value is fixed 
          uniqueValues <- length(unique(data[patientIndices[j-1]:(patientIndices[j]-1), i]))==1 # Is there only 1 value
          uniqueWithNA <- (length(unique(data[patientIndices[j-1]:(patientIndices[j]-1), i]))==2)&&
            (any(is.na(unique(data[patientIndices[j-1]:(patientIndices[j]-1), i])))) # Are there 2 values and 1 is NA
          if ((!uniqueValues)&&(!uniqueWithNA))
          {
            isFixed <- 0
            break
          }
        }
        # Final patient segment ends at row nrow(data)
        uniqueValues <- length(unique(data[patientIndices[length(patientIndices)]:nrow(data), i]))==1 # Is there only 1 value
        uniqueWithNA <- (length(unique(data[patientIndices[length(patientIndices)]:nrow(data), i]))==2)&&
          (any(is.na(unique(data[patientIndices[length(patientIndices)]:nrow(data), i])))) # Are there 2 values and 1 is NA
        if ((!uniqueValues)&&(!uniqueWithNA))
        {
          isFixed <- 0
        }
        # Move to the front if the column is fixed
        if (isFixed == 1)
        {
          fixedColCount = fixedColCount+1 # Add to the fixed column count
          # Create a vector of all column indices
          allCols <- seq_len(ncol(data))
          # Remove 'patientNoCol' and 'i' from the list of all column indices
          remainingCols <- setdiff(allCols, c(patientNoCol, i))
          # Concatenate 'patientNoCol', 'i', and the remaining columns to form the new order
          newColOrder <- c(patientNoCol, i, remainingCols)
          # Reorder the columns in 'data' according to 'newColOrder'
          data <- data[, newColOrder]
          
        }
      }
    }
    
    
    
    # Append the variables of each encounter to the end of the first encounter
    #------------------------------------------------------------------------------
    # Create a new data frame with each patient in their own row
    
    firstEncounterVector <- c(1,patientIndices) # Vector containing the row number of each patient's first encounter
    newData <- data[firstEncounterVector,] # New data frame, other rows will be appended
    
    # Find the largest number of encounters had by a patient
    maxNumberOfEncounters <- max( diff(firstEncounterVector))-1
    # Find where the variable columns start
    variableColStart <- patientNoCol + fixedColCount + 1
    # Find the number of variable columns (including daysSinceFirstEncounter and )
    variableColCount <- ncol(data) - (variableColStart) + 2
    
    maxEncountersSynthesised <- input$maxEncounterSyn # Maximum number of encounters for synthesis
    
    # Add first time interval column - all 0s - if there are variable columns
    if (variableColStart<=ncol(data))
    {
      first_colnames <- colnames(newData)[1:(variableColStart-1)]
      last_colnames <- colnames(newData)[variableColStart:ncol(newData)]
      newData <- data.frame(newData[,1:(variableColStart-1)], DaysSinceFirstEncounter = TimeInterval[firstEncounterVector], 
                            newData[,((variableColStart):ncol(newData))])
      colnames(newData)[1:(variableColStart-1)] <- first_colnames # Rename
      colnames(newData)[(variableColStart+1):ncol(newData)] <- last_colnames # Rename
    }else # No variable columns
    {
      newData <- data.frame(newData, DaysSinceFirstEncounter = TimeInterval[firstEncounterVector])
    }
    
    # Calculate amount of columns to increase newData by
    capacityIncrease <- min(variableColCount*(maxNumberOfEncounters-1),variableColCount*maxEncountersSynthesised)
    # Increase the capacity of newData
    newData <- data.frame(newData, matrix(NA, nrow = nrow(newData), ncol = capacityIncrease))
    
    
    # Loop through the encounters for each patient, starting with encounter 2, then 3,4...
    noMoreEncounters <- 0 # Boolean to signify when loop ends
    index <- 1 # Index - the current encounter number
    currEncounterVector <- firstEncounterVector # The encounter vector that will change
    
    while (noMoreEncounters == 0) 
    {
      # Check which patients have no more encounters and remove them
      currEncounterVector <- currEncounterVector+1 # Increase each by one
      
      # Logical vector approach, indicating where currEncounterVector equals the next element in firstEncounterVector
      lastEncounter <- c(currEncounterVector[-length(currEncounterVector)] == firstEncounterVector[-1], FALSE)
      
      # Set those positions in currEncounterVector to NA
      currEncounterVector[lastEncounter] <- NA
      
      if ((!is.na(currEncounterVector[length(currEncounterVector)])) && (currEncounterVector[length(currEncounterVector)]>=nrow(data)))
      {
        currEncounterVector[length(currEncounterVector)] = NA # Change to NA so it will not be used
      }
      
      # Check if all values are NA
      if (all(is.na(currEncounterVector))||index>=maxEncountersSynthesised)
      {
        noMoreEncounters <- 1
      } else
      {
        # x = start column of the new set of encounters
        # y = end column of the new set of encounters
        x <- variableColStart + variableColCount + (index - 1) * variableColCount 
        y <- x + variableColCount - 1 # Variable col count includes the time interval column
        # Place the time interval values into the new data frame
        newData[, x] <- TimeInterval[currEncounterVector]
        # Change time interval column name
        colnames(newData)[x] <- paste0("DaysSinceFirstEncounter", index+1)
        if (variableColStart<=ncol(data)) # If there are variable columns...
        {
          # Place the variable column values into the new data frame
          newData[, (x+1):y]<- data[currEncounterVector, variableColStart:ncol(data)] 
          # Change rest of column names
          colnames(newData)[(x+1):y] <- paste0(colnames(data)[variableColStart:
                                                                ncol(data)], index+1) # Change the names to append the index to the end
        }
      }
      index = index + 1 # Increment the index
      #print(index)
    }
    
    rownames(newData) <- 1:nrow(newData) # Re-number the rows
    
    # Remove any columns with just NA and blank values
    # Change all blank string values to NA
    replace_blank_with_na <- function(x) {
      # Check if the column is of type character or factor
      if(is.character(x) | is.factor(x)) {
        x[x == ""] <- NA  # Replace blank strings with NA
        return(x)
      } else {
        return(x)  # Return the column unchanged if it's not character or factor
      }
    }
    
    # Apply the function to each column of the dataframe
    newData <- data.frame(lapply(newData, replace_blank_with_na))
    
    # Remove any columns with just NA values
    newData <- newData %>% select(where(~ any(!is.na(.))))
  })
  
  ################################################################################
  
  # Synthesise the data
  syntheticData <- eventReactive(input$goButton, {
    showModal(modalDialog("Loading...", footer = NULL))
    newData <- newData()
    
    # Create Synthetic Data
    mysyn <- syn(newData, k = input$numGenerated, maxfaclevels = 5000)
    
    # Synthetic Data Table
    synTable <- mysyn$syn
    
    # If there is a blank daysSinceFirstEncounter variable, ensure there is only NA values after
    # Find the indices of the daysSinceFirstEncounter columns
    daysSinceFirstEncounterCols <- grep("^DaysSinceFirstEncounter", names(synTable), value = FALSE)
    
    # Loop through all of these columns (if there are variable columns)
    if (!all(diff(daysSinceFirstEncounterCols) == 1)) # Check if there are no variable columns
    {
      for (i in daysSinceFirstEncounterCols)
      {
        na_row_indices <- which(is.na(synTable[,i])) # Select the rows 
        synTable[na_row_indices,(i+1):ncol(synTable)] <- rep(NA, length(synTable[na_row_indices,(i+1):ncol(synTable)]))
      }
    }
    synTable
  })
  
  ################################################################################
  
  # Revert to original format for download/display
  formatData <- reactive({
    # Ensure input is available
    req(input$chosenFile)
    data <- syntheticData() # Read in
    # Rearrange to same format as inputted file
    # Find the daysSinceFirstEncounter columns
    daysSinceFirstEncounterCols <- grep("^DaysSinceFirstEncounter", names(data), value = FALSE)
    # Calculate the number of encounters using DaysSinceFirstEncounter
    noCols <- daysSinceFirstEncounterCols[2] # Including space for new encounterNo column
    noVariableCols <- daysSinceFirstEncounterCols[2]-daysSinceFirstEncounterCols[1]
    noFixedCols <- daysSinceFirstEncounterCols[1]-1
    noEncounters <- length(daysSinceFirstEncounterCols)
    # Create data frame to hold new data format
    # Duplicate the fixed values 
    formattedData <- data.frame(matrix(NA, nrow = nrow(data)*noEncounters, ncol = noCols))
    colnames(formattedData) <- c(colnames(data)[1],"EncounterNo",colnames(data)[2:(noCols-1)]) # Column names
    
    for (i in 1:nrow(data)) # Loop through each patient and fill in their details
    {
      # Select the patient's data
      patient <- data[i,]
      # Start row in formattedData for this patient
      startRow <- (i-1)*noEncounters + 1
      # End row in formattedData for this patient
      endRow <- (i)*noEncounters
      
      # Format the data
      # PatientID row
      formattedData[startRow:endRow,1] <- patient[rep(1, noEncounters), 1]
      # EncounterNo row
      formattedData[startRow:endRow,2] <- 1:noEncounters
      # Fixed rows
      formattedData[startRow:endRow, 3:(noFixedCols+1)] <- patient[rep(1, noEncounters), 2:noFixedCols]
      # Variable rows
      # Loop through all encounters and add into formattedData
      index = 1 # Index of encounter number
      while (index<noEncounters)
      {
        # Use the daysSinceFirstEncounter columns to add variable data - account for encounterNo column
        formattedData[startRow+index-1 , (noFixedCols+2):ncol(formattedData)] <- 
          patient[1, (daysSinceFirstEncounterCols[index]):(daysSinceFirstEncounterCols[index+1]-1)]
        # Increment index
        index = index+1
      }
      # Specific for last encounter - ends at final column
      formattedData[startRow+index-1 , (noFixedCols+2):ncol(formattedData)] <- 
        patient[1, (daysSinceFirstEncounterCols[index]):ncol(patient)]
      
    }
    
    # Remove all rows where daysSinceFirstEncounter is NA
    formattedData <- formattedData[!is.na(formattedData$DaysSinceFirstEncounter), ]
    # Re-number the rows
    rownames(formattedData) <- 1:nrow(formattedData)
    
    removeModal() # Remove Loading Screen
    
    # Print table
    formattedData
  })
  
  
  ################################################################################
  
  # Display synthetic table
  output$synOutput <- DT::renderDataTable({
    
    # Display Synthetic Data
    DT::datatable(formatData())
  })
  
  ################################################################################
  
  individualPatient <- reactive({
    # Choose patient row number
    chosenPatientNo <- input$patientChosen
    # Input synthetic data
    individualData <- syntheticData()
    # Select the patient
    patient <- individualData[chosenPatientNo,]
    # Select the daysSinceFirstEncounter columns
    daysSinceFirstEncounterCols <- grep("^DaysSinceFirstEncounter", names(patient), value = FALSE)
    # Calculate the number of encounters using DaysSinceFirstEncounter
    noVariableCols <- daysSinceFirstEncounterCols[2]-daysSinceFirstEncounterCols[1]
    noFixedCols <- daysSinceFirstEncounterCols[1]-1
    noEncounters <- length(daysSinceFirstEncounterCols)
    # Duplicate the fixed values 
    individualNewData <- patient[rep(1, noEncounters), 1:noFixedCols]
    # Add in the variable values
    individualNewData <- data.frame(individualNewData, matrix(NA, nrow = noEncounters, ncol = noVariableCols))
    colnames(individualNewData)[(noFixedCols+1):ncol(individualNewData)] <-
      colnames(individualData)[(noFixedCols+1):(daysSinceFirstEncounterCols[2]-1)]
    index = 1 # Index of encounter number
    while (index<noEncounters)
    {
      # Use the daysSinceFirstEncounter columns to add variable data
      individualNewData[index , (noFixedCols+1):ncol(individualNewData)] <- patient[1, (daysSinceFirstEncounterCols[index]):(daysSinceFirstEncounterCols[index+1]-1)]
      index = index+1
    }
    # Specific for last encounter - ends at final column
    individualNewData[index , (noFixedCols+1):ncol(individualNewData)] <- patient[1, (daysSinceFirstEncounterCols[index]):ncol(patient)]
    
    rownames(individualNewData) <- 1:nrow(individualNewData) # Re-number the rows
    
    # Remove all rows where daysSinceFirstEncounter is NA
    individualNewData <- individualNewData[!is.na(individualNewData$DaysSinceFirstEncounter), ]
    
    individualNewData
  })
  
  #################################################################################
  
  # Display each synthetic patient
  output$synPatients <- DT::renderDataTable({
    individualNewData <- individualPatient() 
    # Display this data table
    DT::datatable(individualNewData)
  })
  
  ################################################################################
  
  # Download Synthetic csv
  output$downloadData <- downloadHandler(
    
    # Name the file
    filename = function() {
      paste("SyntheticDataset", ".csv", sep = "")
    },
    
    # Download the file
    content = function(file) {
      synTable <- formatData()
      write.csv(synTable, file = file, row.names = FALSE)
    }
  )
  
  ################################################################################
  
  # Display Original Table Summary Statistics
  output$originalSummary <- renderPrint({
    # Read the file
    data <- newData()
    # Choose the variable (select using column names) - add ^ to match start of string
    colsSelected <- intersect(input$summarySelectedColumn, names(data))
    req(length(colsSelected) > 0)  # Ensure there's at least one valid column
    # Print the summary
    summary(data[colsSelected])
  })
  
  ################################################################################
  
  # Display Original Table Summary Statistics
  output$synSummary <- renderPrint({
    synTable <- syntheticData()
    # Choose the variable (select using column names) - add ^ to match start of string
    colsSelected <- intersect(input$summarySelectedColumn, names(synTable))
    req(length(colsSelected) > 0)  # Ensure there's at least one valid column
    # Print the summary
    summary(synTable[colsSelected])
  })
  
  ################################################################################
  
  # Create Comparative Charts - compare the progression of each variable at each encounter
  output$plotComp <- renderPlot({
    compData <- syntheticData()
    compOriginal <- newData()
    # Choose the variable (select using column names) - add ^ to match start of string
    colSelected <- paste0("^",input$compSelectedColumn)
    req(length(colSelected) > 0)  # Ensure there's at least one valid column
    
    # Create the data frame of just that variable by selecting the columns and choosing from DF
    colSelectedAll <- grep(colSelected, names(compData), value = FALSE)
    compData <- compData[,colSelectedAll, drop = FALSE] # New synthetic DF with only the selected variable
    compOriginal <- compOriginal[,colSelectedAll, drop = FALSE] # New original DF
    # Create a bar chart of this variable over time, comparing the real and fake data
    # Check if numerical or categorical (using column 1)
    if (is.numeric(compData[[1]]))
    {
      
      # Grouped box plots - Original vs Synthetic
      # Pivot to facilitate creation of grouped histogram 
      originalLong <- pivot_longer(compOriginal, cols = everything(), names_to = "Column", values_to = "Value")
      synLong <- pivot_longer(compData, cols = everything(), names_to = "Column", values_to = "Value")
      
      # Add new data source column for combining plots
      originalLong$Source <- 'Original'
      synLong$Source <- 'Synthetic'
      # Combine the dataframes
      combinedData <- rbind(originalLong, synLong)
      
      # Ensure correct order of horizontal values
      combinedData$Column <- factor(combinedData$Column, levels = names(compData))
      
      # Create grouped box plots
      ggplot(combinedData, aes(x = interaction(Source, Column, sep = " - "), y = Value, fill = Source)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Box Plots - Original vs. Synthetic Data", x = "Column", y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    }else
    {
      # Produce similar plot for categorical data
      # Create a matrix of encounter number vs percentage of frequency of categorical value
      
      # Create a data frame, containing all of the possible categorical values, using unique() 
      compDataPossibleValues <- unique(unlist(compData, use.names = FALSE))
      originalDataPossibleValues <- unique(unlist(compOriginal, use.names = FALSE))
      allPossibleValues <- unique(c(compDataPossibleValues,originalDataPossibleValues))
      # Remove NA to be added to end
      allMinusNA <- as.vector(na.omit(allPossibleValues))
      allPlusNA <- c(allMinusNA, NA) # For final dataframe
      # Create data frame with allPlusNA as row names and names(compData) as column names
      synCompTable <- data.frame(matrix(ncol = length(names(compData)), nrow = length(allPlusNA)))
      names(synCompTable) <- names(compData) # Set col names
      rownames(synCompTable) <- c(allPlusNA[-length(allPlusNA)], "NA") # Set row names
      synOriginalTable <- data.frame(matrix(ncol = length(names(compData)), nrow = length(allPlusNA)))
      names(synOriginalTable) <- names(compData) # Set col names
      rownames(synOriginalTable) <- c(allPlusNA[-length(allPlusNA)], "NA") # Set col names
      
      # Loop through each row for each column to make vectors of percentages
      for (i in names(compData))
      {
        # Factor
        factorCompData <- addNA(factor(compData[[i]], levels = allMinusNA))
        # Vector (in the order of allMinusNA with NA at end)
        vectorCompData <- as.vector(table(factorCompData))
        # Convert to percentages
        percentage <- (vectorCompData / sum(vectorCompData)) * 100
        # Add to table
        synCompTable[[i]] <- percentage
      }
      
      # For compOriginal
      for (i in names(compOriginal))
      {
        # Factor
        factorCompData <- addNA(factor(compOriginal[[i]], levels = allMinusNA))
        # Vector (in the order of allMinusNA with NA at end)
        vectorCompData <- as.vector(table(factorCompData))
        # Convert to percentages
        percentage <- (vectorCompData / sum(vectorCompData)) * 100
        # Add to table
        synOriginalTable[[i]] <- percentage
      }
      # Pivot to a long table
      synOriginalTable$Group <- rownames(synOriginalTable) # Convert rownames to a column
      synCompTable$Group <- rownames(synCompTable) # Convert rownames to a column
      
      # Convert from wide to long format
      synOriginalTableLong <- pivot_longer(synOriginalTable, cols = -Group, names_to = "Category", values_to = "Value")
      synCompTableLong <- pivot_longer(synCompTable, cols = -Group, names_to = "Category", values_to = "Value")
      
      # Add new data source column for combining plots
      synOriginalTableLong$Source <- 'Original'
      synCompTableLong$Source <- 'Synthetic'
      # Combine the dataframes
      combinedData <- rbind(synOriginalTableLong, synCompTableLong)
      
      # Ensure correct order of horizontal values
      combinedData$Category <- factor(combinedData$Category, levels = names(compData))
      
      # Create grouped bar charts
      ggplot(combinedData, aes(x = interaction(Source, Category, sep = " - "), y = Value, fill = Source, group = interaction(Source, Group))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = Group), position = position_dodge(width = 0.9), hjust = 0.5,
                  angle = 90, size = 3) + 
        theme_minimal() +
        labs(title = "Percentage Comparison - Original vs. Synthetic Data", x = "Category", y = "Percentage", fill = "Data Source") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    }
  })
  
  ################################################################################
  
  # Create Correlation Comparisons - Scatter Plots etc
  output$plotCorr <- renderPlot({
    corrSyn <- syntheticData()
    corrOriginal <- newData()
    # Choose the variable (select using column names)
    colsSelected <- intersect(input$corrSelectedColumn, names(corrSyn))
    req(length(colsSelected) > 1)  # Ensure there are two valid columns
    
    # Create the data frame of the correlation
    corrSyn <- corrSyn[,colsSelected, drop = FALSE] # New synthetic DF with only the selected variables
    corrOriginal <- corrOriginal[,colsSelected, drop = FALSE] # New original DF
    
    if (all(sapply(corrSyn, is.numeric))) # All numeric
    {
      if (length(colsSelected)>2)
      {
        # Compute the correlation matrices
        corMatrixSyn <- cor(corrSyn, use = "complete.obs")
        corMatrixOriginal <- cor(corrOriginal, use = "complete.obs")
        # Create the space to plot horizontally
        par(mfrow = c(1, 2))
        # Plot the correlogram
        corrplot(corMatrixOriginal, method = "number", tl.col = "black", col = "black")
        corrplot(corMatrixSyn, method = "number", tl.col = "black", col = "black")
        
      }else
      {
        # Compute the correlation coefficients
        coefficientOriginal <- cor(corrOriginal[,1], corrOriginal[,2], use = "complete.obs")
        coefficientSyn <- cor(corrSyn[,1], corrSyn[,2], use = "complete.obs")
        
        # Add data sources for combining
        corrOriginal$Source <- 'Original'
        corrSyn$Source <- 'Synthetic'
        combinedData <- rbind(corrOriginal, corrSyn)
        
        # Create grouped scatter plot and annotate with correlation coefficients
        ggplot(combinedData, aes(x = combinedData[,1], y = combinedData[,2], color = Source)) +  # Assuming the columns you're interested in are the first two
          geom_point() +
          geom_text(data = data.frame(x = Inf, y = Inf, label = c(paste("Correlation (Original):", round(coefficientOriginal, 2),
                                                                        " // Correlation (Synthetic):", round(coefficientSyn, 2))),
                                      Source = c("Original", "Synthetic")),
                    aes(x = x, y = y, label = label), hjust = 1.1, vjust = 2, inherit.aes = FALSE) +
          scale_color_manual(values = c("Original" = "blue", "Synthetic" = "red")) +
          theme_minimal() +
          labs(title = "Scatter Plot with Correlation Coefficients",
               x = colnames(combinedData)[1], y = colnames(combinedData)[2],
               color = "Source")
        
      }
      
    }else if(!any(sapply(corrSyn, is.numeric))) # All categorical
    {
      # Create contingency tables
      contingencyTableOriginal <- table(corrOriginal)
      contingencyTableSyn <- table(corrSyn)
      if (length(colsSelected)>2)
      {
        # Print a message saying to select only maximum of 2 categorical variables
      }else
      {
        # Convert the table to a dataframe for ggplot2
        tableDFOriginal <- as.data.frame(as.table(contingencyTableOriginal))
        tableDFSyn <- as.data.frame(as.table(contingencyTableSyn))
        
        plot1 <- ggplot(tableDFOriginal, aes(x = tableDFOriginal[,1], y = tableDFOriginal[,2], fill = Freq)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          theme_minimal() +
          labs(title = "Heat Map (for Contingency Table) - Original Data", x = names(tableDFOriginal)[1], y = names(tableDFOriginal)[2], fill = "Frequency")
        plot2 <- ggplot(tableDFSyn, aes(x = tableDFSyn[,1], y = tableDFSyn[,2], fill = Freq)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          theme_minimal() +
          labs(title = "Heat Map (for Contingency Table) - Synthetic Data", x = names(tableDFSyn)[1], y = names(tableDFSyn)[2], fill = "Frequency")
        
        # Arrange horizontally for comparison
        grid.arrange(plot1, plot2, ncol = 2)
      }
    }else # Both categorical and numeric
    {
      if (length(colsSelected)>2)
      {
        # Nothing displayed - Print a message saying to select only maximum of 2 categorical variables
        
      }else
      {
        orderedCorrSyn <- corrSyn
        orderedCorrOriginal <- corrOriginal
        # Ensure the categorical variable is first
        if (is.numeric(orderedCorrSyn[,1])) # If numeric variable is first...
        {
          # Rearrange to make categorical variable first
          orderedCorrSyn[,1] <- corrSyn[,2]
          orderedCorrSyn[,2] <- corrSyn[,1]
          names(orderedCorrSyn) <- c(names(corrSyn)[2],names(corrSyn)[1]) # Rename
          orderedCorrOriginal[,1] <- corrOriginal[,2]
          orderedCorrOriginal[,2] <- corrOriginal[,1]
          names(orderedCorrOriginal) <- c(names(corrOriginal)[2],names(corrOriginal)[1]) # Rename
        }
        
        # Add new data source column for combining plots
        orderedCorrOriginal$Source <- 'Original'
        orderedCorrSyn$Source <- 'Synthetic'
        # Combine the dataframes
        combinedData <- rbind(orderedCorrOriginal, orderedCorrSyn)
        
        # Ensure correct order of horizontal values
        #combinedData$Category <- factor(combinedData$Category, levels = names(compData))
        
        # Create combined boxplots
        ggplot(combinedData, aes(x = interaction(Source, combinedData[,1], sep = " - "), y = combinedData[,2], fill = Source)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = "Conditional Box Plot - Original vs. Synthetic Data", x = names(combinedData)[1], y = names(combinedData)[2]) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }
  })
  
  output$ifPlotCannotBePrinted <- renderText({
    corrSyn <- syntheticData()
    corrOriginal <- newData()
    # Choose the variable (select using column names)
    colsSelected <- intersect(input$corrSelectedColumn, names(corrSyn))
    req(length(colsSelected) > 1)  # Ensure there are two valid columns
    
    # Create the data frame of the correlation
    corrSyn <- corrSyn[,colsSelected, drop = FALSE] # New synthetic DF with only the selected variables
    corrOriginal <- corrOriginal[,colsSelected, drop = FALSE] # New original DF
    
    if (!all(sapply(corrSyn, is.numeric))) # Only print text if there is a non-numeric column
    {
      if (length(colsSelected)>2) # Only print text if there are over 2 columns selected
      {
        paste("** If there is a non-numeric column selected, only select two columns **")
      }
    }else
    {
      if (length(colsSelected)>2)
      {
        paste("       Plot of Correlation Coefficients - Original vs. Synthetic Data")
      }
    }
  })
}