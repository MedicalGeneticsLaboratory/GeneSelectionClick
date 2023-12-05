# Run Gene Selection
#' @name runGeneSelection
#' @title RNA-seq Differential Expression Analysis Shiny App
#'
#' @description
#' The Shiny application provides a user-friendly platform for performing detailed differential gene expression analyses for RNA-seq Data.
#' It provides a comprehensive set of tools for processing raw FASTQ files,
#' quality assessment, filtering, trimming, alignment to reference genomes,
#' read counting, and identify the differentiated expressed genes.
#'
#'
#' @return User Interface for RNA-seq Differential Expression Analysis from scratch
#' @export
#'
#' @examples
#' ## You can simply be run using and input files into the shiny app
#' runGeneSelection()
#' @rdname runGeneSelection
#'
# Load required libraries

  library(shiny)
  library(Rqc)
  library(QuasR)
  library(Rsubread)
  library(DT)
  library(DESeq2)
  library(shinydashboard)
  library(shinyWidgets)
  library(rtracklayer)
  library(ggplot2)
  library(tidyverse)


# Load custom modules

  source("./R/import_data.R")
  source("./R/quality_control.R")
  source("./R/quantification.R")
  source("./R/filter_trim.R")
  source("./R/alignment.R")
  source("./R/dif_gene_ex_analysis.R")


# Set Shiny options

  options(shiny.maxRequestSize = 1000000 * 1024^2)


runGeneSelection <- function() {

  # Define the UI (User Interface)
  ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "GSCApp", titleWidth = 300),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Import FASTQ files", tabName = "collected_data", icon = icon("download", lib = "font-awesome")),
        menuItem("Data pre-processing",
                 menuSubItem("Quality Assessment", tabName = "quality", icon = icon("vial-circle-check")),
                 menuSubItem("Filtering and Trimming", tabName = "filtering", icon = icon("filter"))
        ),
        menuItem("Alignment to Reference Genome", tabName = "alignment", icon = icon("indent")),
        menuItem("Read Counting", tabName = "quantification", icon = icon("table-list")),
        menuItem("Differential Expression Analysis", tabName = "DESeq2", icon = icon("dna"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "collected_data", importDataUI("importData")),
        tabItem(tabName = "quality", qualityControlUI("qualityControl")),
        tabItem(tabName = "filtering", Filter_TrimUI("filter_trim")),
        tabItem(tabName = "alignment", alignmentUI("alignment")),
        tabItem(tabName = "quantification", quantificationUI("quantification")),
        tabItem(tabName = "DESeq2", dif_gene_ex_analysisUI("DGEA"))
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {
    # Import Data:
    importedData <- callModule(importDataServer, "importData")

    # Quality Control:
    qualityControlServer("qualityControl", importedData)

    # Filter and Trimming
    Filter_TrimServer("filter_trim", importedData)

    # Alignment
    alignmentServer("alignment")

    # Quantification
    quantificationServer("quantification")

    # Differential expression analysis
    dif_gene_ex_analysisServer("DGEA")
  }

  # Run the Shiny app
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}

