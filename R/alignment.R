alignmentUI <- function(id) {
  ns <- NS(id)
  tagList(
    box( width= FALSE, title = "Mapping your reads to a reference genome", status= "primary", solidHeader = TRUE,
         box( width = FALSE,
           fileInput(ns("reference_file"),"Download a reference file:fasta file"),
           tags$b(span(style="color:gray", "Bref description of the reference file")),br(),
           tableOutput(ns("Dwn_finish")),
         ),

         box( width = FALSE,
           textInputIcon(ns("filtered_data_folder"),"Enter the directory path that contains your filtered data from the last step of trimming and filtering",placeholder = "Enter the directory path",icon = icon("folder-open")),
           tags$span(style="color:blue", "Make sure that the directory path contains the specific FASTQ file(s) only!"),br(),
           br(),
           br(),
           actionBttn(ns('btn_lunch_alignment'),"Launch alignment",icon=icon("indent"), size="sm", color="danger"),
           br(),br(),
         ),

         box( width = FALSE,
           tags$b(span(style="color:gray", "The result of the alignment")),br(),
           #Show the result of alignment:
           DT::dataTableOutput(ns("alignment_results")))

    )

  )
}

alignmentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # Reactive function to run the alignment
      run_alignment <- reactive({
        if (!is.null(input$reference_file$datapath)) {
          ref <- input$reference_file$datapath
          buildindex(basename = "reference_index", reference = ref)

          # Original path
          path <- input$filtered_data_folder

          # Replace backslashes with forward slashes
          folder <- gsub("\\\\", "/", path)

          print(folder)

          output_format = ".BAM"
          filteredFiles <- list.files(full.names = TRUE, path = folder)
          files_names <- stringr::str_remove(basename(filteredFiles), "\\.fastq\\.gz$")


          # Create output directory if it doesn't exist
          outputDir <- file.path(folder, "subreadOutput")
          if (!dir.exists(outputDir)) {
            dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
          }


          align.stat <- align(
            index = "reference_index",
            readfile1 = filteredFiles,
            input_format = "gzFASTQ",
            output_format = "BAM",
            output_file = paste(outputDir, "/", files_names, "-subread", output_format, sep = ""),
            phredOffset = 33
          )
          return(align.stat)
        } else {
          return(NULL)  # Return NULL when reference file is not selected
        }
      })

      # Render table to display a message or the reference file information
      output$Dwn_finish <- renderTable({
        if (is.null(input$reference_file$datapath)) {
          return(data.frame(Message = "Import the reference file with the FASTA format"))
        } else {
          return(input$reference_file)
        }
      })

      # Event reactive to handle the alignment button click
      lunch_alignment <- eventReactive(input$btn_lunch_alignment, {
        if (is.null(run_alignment())) {
          # If reference file is not selected, show a message
          return(data.frame(Message = "Import the reference file before running the alignment"))
        } else {
          # Run the alignment and return the results as a data frame
          res_alignment <- run_alignment()
          return(as.data.frame(res_alignment))
        }
      })

      # Render the alignment results DataTable
      output$alignment_results <- DT::renderDataTable({
        lunch_alignment()
      })


    }
  )
}
