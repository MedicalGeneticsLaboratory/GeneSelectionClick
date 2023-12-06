# Define the UI function for importing data
importDataUI <- function(id) {
  ns <- NS(id)
  tagList(

    # Box for importing SRR data
    box(width=FALSE, title="Import the SRR data (SRA RUN files: FASTQ files) ",status = "primary", solidHeader = TRUE,
        textInputIcon(ns("folderName"), label="Enter the directory path that contains fastq input file",placeholder = "Enter the directory path",icon = icon("folder-open")),
        (tags$span(style="color:blue", "Make sure that the directory path contains the specific FASTQ file only!")),br(),
        br(),
        actionBttn(ns("btn_upload"),label = "Import Data", icon = icon("download"),color = "danger",size = "sm",no_outline = TRUE),br(),br(),
        tags$b(span(style="color:gray", "Brief description of the imported data")),
        tableOutput(ns("file_description")),
    ),

    # Box for displaying Per-File Top Reads
    box(width=FALSE, title="Per-File Top Reads",status = "primary", solidHeader = TRUE,
        tags$span(style="font-size:17px"," Top Sequencing Reads per File: Most represented sequencing reads and their counts."),br(),br(),
        hr(),
        DT::dataTableOutput(ns("perFileTopReads"))
    )
  )
}

# Define the server function for importing data
importDataServer <- function(input, output, session) {
  ns <- session$ns

  # Reactive expression for reading and processing data
  read_data <- reactive({
    # Original path
    originalPath <- input$folderName

    # Replace backslashes with forward slashes
    genericPath <- gsub("\\\\", "/", originalPath)

    qcResult <- NULL
    if (!is.null(input$btn_upload) && input$btn_upload > 0) {
      tryCatch({
        folder <- genericPath
        files <- list.files(full.names = TRUE, path = folder)
        qcResult <- rqcQA(files)
      }, error = function(e) {
        # Handle errors here if needed
        cat("Error occurred while processing files: ", e$message, "\n")
      })

      # Close any open file connections
      if (!is.null(qcResult)) {
        invisible(lapply(qcResult$files, close))
      }
    }

    # Return a list containing the reactive expressions
    return(qcResult)
  })

  # Event reactive for getting data after upload button is clicked
  getData <- eventReactive(input$btn_upload, {
    read_data()
  })

  # Render the file description table
  output$file_description <- renderTable({
    if (is.null(input$folderName) || input$folderName == "") {
      return(data.frame(Message = "Import your FASTQ files"))
    }
    req(input$folderName)
    if (!is.null(getData())) {
      perFileInformation(getData())
    }
  })

  # Render the Per-File Top Reads DataTable
  output$perFileTopReads <- DT::renderDataTable({
    if (is.null(input$folderName) || input$folderName == "") {
      return(data.frame(Message = "Import your FASTQ files"))
    }
    req(input$folderName)
    if (!is.null(getData())) {
      DT::datatable(as.data.frame(perFileTopReads(getData())), options = list(scrollX = TRUE))
    }
  })

  # Display a message if folderName is empty
  observe({
    if (is.null(input$folderName) || input$folderName == "") {
      showNotification("Please enter a directory path.", type = "warning")
    }
  })

  # Return the reactive expression
  return(read_data)
}
