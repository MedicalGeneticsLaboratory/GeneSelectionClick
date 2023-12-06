qualityControlUI <- function(id) {
  ns <- NS(id)
  tagList(
    box( width = FALSE, title="Quality Assessment", status = "primary", solidHeader = TRUE, height = "100%",
         tags$i(class = "fa-solid fa-thumbtack"),
         tags$span(style="font-size:17px","The sequencing technologies usually produce basecalls with varying quality.In addition, there could be sample-specific issues in your sequencing run, such as adapter contamination. It is standard procedure to check the quality of the reads and identify problems before doing further analysis. Checking the quality and making some decisions for the downstream analysis can influence the outcome of your project."),
         hr(),br(),
         sidebarLayout(
           # Sidebar with a slider input
           sidebarPanel(

             #Select a FASTQ File :
             uiOutput(ns("select_fastq_file")),
             hr(),

             #Setect a Plot :
             selectInput(ns("select_plot"), "Available Graphics",choices = (
               c("Per Read Mean Quality Distribution of Files",
                 "Average Quality",
                 "Cycle-specific Average Quality",
                 "Read Frequency",
                 "Read Length Distribution",
                 "Cycle-specific GC Content",
                 "Cycle-specific Quality Distribution",
                 "Cycle-specific Quality Distribution - Boxplot",
                 "Cycle-specific Base Call Proportion"))),
             hr(),
             tags$span(style="font-size:17px","Generate an HTML report file."),
             br(),br(),
             uiOutput(ns("quality_report")),
             br(), br(),
             textOutput(ns("qualityReport")),
             br()

           ),
           # Show a plot of the generated distribution :
           mainPanel(
             fluidPage( tabsetPanel(
               tabPanel("Data graphic", plotOutput(ns("quality_plot"))),
               tabPanel("Data", DT::dataTableOutput(ns("quality_data"))),
             ))
           )))
  )
}

qualityControlServer <- function(id,importedData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      fileChoices <- reactive({
        choices <- c("None")
        for (i in 1:length(importedData())) {
          filename <- as.character(perFileInformation(subsetByPair(importedData(), i))$filename)
          choices <- c(choices, filename)
        }
        return(choices)
      })

      #Select one of the FASTQ files:
      output$select_fastq_file <- renderUI({
        if (is.null(importedData())) {
          return(tags$span(style="color:blue", "Import your FASTQ files!"))
        }
        radioButtons(ns("selected_file"), "Select a fastq file", choices = fileChoices())
      })

      output$quality_plot <- renderPlot({
        # Check if selected_file is not NULL or empty
        if (is.null(input$selected_file) || input$selected_file == "None") {
          return(plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 5), ylim = c(0, 5)))
        }

        selectedPlot <- input$select_plot
        selectedFile <- input$selected_file

        plot_functions <- list(
          "Per Read Mean Quality Distribution of Files"= rqcReadQualityBoxPlot,
          "Average Quality"= rqcReadQualityPlot,
          "Cycle-specific Average Quality"= rqcCycleAverageQualityPlot,
          "Read Frequency"= rqcReadFrequencyPlot ,
          "Read Length Distribution"= rqcReadWidthPlot,
          "Cycle-specific GC Content"= rqcCycleGCPlot,
          "Cycle-specific Quality Distribution"= rqcCycleQualityPlot,
          "Cycle-specific Quality Distribution - Boxplot"=rqcCycleQualityBoxPlot,
          "Cycle-specific Base Call Proportion"=rqcCycleBaseCallsLinePlot
        )

        # Check if the selectedPlot is a valid key in plot_functions
        if (!(selectedPlot %in% names(plot_functions))) {
          return(plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 5), ylim = c(0, 5)))
        }

        result <- plot_functions[[selectedPlot]](importedData()[selectedFile])
        result
      })

      output$quality_data <- DT::renderDataTable({
        # Check if selected_file is not NULL or empty
        if (is.null(input$selected_file) || input$selected_file == "None") {
          return(as.matrix("Must have at least one file selected."))
        }

        selectedPlot <- input$select_plot
        selectedFile <- input$selected_file
        data_functions <- list(
          "Per Read Mean Quality Distribution of Files"=  rqcReadQualityBoxCalc,
          "Average Quality"= rqcReadQualityCalc,
          "Cycle-specific Average Quality"= rqcCycleAverageQualityCalc,
          "Read Frequency"= rqcReadFrequencyCalc,
          "Read Length Distribution"= rqcReadWidthCalc,
          "Cycle-specific GC Content"= rqcCycleGCCalc,
          "Cycle-specific Quality Distribution"= rqcCycleQualityCalc,
          "Cycle-specific Quality Distribution - Boxplot"=rqcCycleQualityBoxCalc,
          "Cycle-specific Base Call Proportion"= rqcCycleBaseCallsCalc
        )

        result <- data_functions[[selectedPlot]](importedData()[selectedFile])
        result
      })

      output$quality_report <- renderUI({
        if (is.null(importedData())) {
          return(tags$span(style="color:blue", "Import your FASTQ files!"))
        }
        actionBttn(ns("btn_QR"),"Quality Report",icon=icon("play"), size = "sm",color = "danger")

      })

      generate.report<-eventReactive(input$btn_QR,{
        sys.name= Sys.info()[['user']]
        outdir = paste("C:/Users/",sys.name,"/Documents/",sep = "")
        rqcReport(read.data(),outdir = outdir ,file = paste( "Quality-report-",Sys.Date()))
      })
      output$qualityReport<-renderText({
        path<-as.character(generate.report())
        paste("Check the quality report in this path : \n ",path)
      })

    }
  )
}


