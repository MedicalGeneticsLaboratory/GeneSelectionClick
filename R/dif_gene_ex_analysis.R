dif_gene_ex_analysisUI <- function(id) {
  ns <- NS(id)
  tagList(

    # FOR METADATA OF THE STUDY :
    box(
      width = FALSE,
      title = "Read in the metadata of the study",
      status = "primary",
      solidHeader = TRUE,
      fileInput(ns("phenotype_data"), "Import the metadata of the study as a .csv file"),
      box( width = FALSE,
           tags$h4(tags$b(tags$tspan("Description of the metadata"))),
             DT::dataTableOutput(ns("desc_phenotype_data")),
      ),
      uiOutput(ns("desc_columns"))
    ),

     #FOR THE GENE EXPRESSION DATA :
     box(
        width = FALSE,
        title = "Read in counts data",
        status = "primary",
        solidHeader = TRUE,
        fileInput(ns("gene_expression_data"), "Read in counts data as .csv file"),
        radioButtons(ns("exp_gene_expression_data"), "Explore the counts data", choices = c("Data Table", "Dimension of data", "Visualize gene expression data")),
        box( width = FALSE,
          tags$h4(tags$b(tags$tspan("Description of the counts data"))),
          uiOutput(ns("desc_gene_expression_data")),
          br()
        )
      ),

    box(
      width = FALSE,
      title = "Run Differential Gene Expression Analysis",
      status = "primary",
      solidHeader = TRUE,

          tags$h4(tags$b(tags$tspan("pre-filtering counts data: removing rows with low gene counts"))),
          textInput(ns("filtering_factor"),"Enter the minimum count threshold"),
          textInput(ns("input_alpha"),"Enter the The statistical significance parameter alpha"),

          # keeping rows that have at least 10 reads total
          actionBttn(ns("btn_run_DFEXR"), "Run Analysis", icon = icon("play"), size = "sm", color = "danger"),
          br(),
          br(),
          box( width = FALSE,
               tags$h4(tags$b(tags$tspan("Display the results of the analysis"))),
               DT::dataTableOutput(ns("results")),
               br(),
               downloadBttn(ns("btn_dwn_Degs"), "Export the results", icon = icon("download"), size = "sm", color = "danger"),
               br()
          )

      )
  )
}





dif_gene_ex_analysisServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Function to read phenotype data
      get_metadata <- reactive({
        req(input$phenotype_data$datapath)  # Use req() to check if the file path is provided
        pheno_data <- read.csv(input$phenotype_data$datapath, header = TRUE)
        pheno_data <- cbind(samples = rownames(pheno_data), pheno_data)
        rownames(pheno_data) <- NULL
        return(pheno_data)
      })

      # Render phenotype data table
      output$desc_phenotype_data <- DT::renderDataTable({
        DT::datatable(get_metadata(), options = list(scrollX = TRUE))
      })

      # Reactive expression for metadata columns
      metadata_columns <- reactive({
        req(get_metadata())  # Ensure that metadata is available
        samples = row.names(get_metadata())
        choices <- c("None",colnames(get_metadata()))
        return(choices)
      })

      # Modified metadata based on selected columns
      modified_metadata <- reactive({
        req(input$description_column, input$tissue_column, get_metadata())
        metadata <- get_metadata() %>%
          select(input$description_column, input$tissue_column) %>%
          rename(tissue = colnames(.[1]), description = colnames(.[2]))
        return(metadata)
      })

      # Render UI for selecting metadata columns
      output$desc_columns <- renderUI({
        req(input$phenotype_data$datapath)  # Check if phenotype data is selected
        box(
          width = FALSE,
          tags$h4(tags$b(tags$tspan("Select the description column"))),
          selectInput(ns("description_column"), "Choose the description column", choices = metadata_columns()),
          selectInput(ns("tissue_column"), "Choose the tissue column", choices = metadata_columns())
        )
      })

      # Function to read gene expression data
      get_gene_data <- reactive({
        req(input$gene_expression_data$datapath)  # Check if gene expression data is selected
        gene_file <- input$gene_expression_data$datapath
        gene_data <- read.csv(gene_file, header = TRUE,row.names=NULL)
        return(gene_data)
      })

      # Join gene expression data with modified metadata
      join_data <- reactive({
        req(get_gene_data(), modified_metadata())  # Ensure data availability
        gene_data <- get_gene_data() %>%
          rename(gene=colnames(.[1])) %>%
          gather(key = 'samples', value = 'FPKM', -gene) %>%
          left_join(.,modified_metadata(), by = c("samples" = "tissue"))
        print(gene_data)
        return(gene_data)
      })

      # Render gene expression data table or other visualizations
      output$desc_gene_expression_data <- renderUI({
        req(input$gene_expression_data$datapath)  # Check if gene expression data is selected
        selected_option <- input$exp_gene_expression_data
        switch(
          selected_option,
          "Data Table" = DT::dataTableOutput(ns("desc_expression_data")),
          "Dimension of data" = tableOutput(ns("dim")),
          "Visualize gene expression data" = uiOutput(ns("visualization"))
        )
      })

      # Render data dimensions table
      output$dim <- renderTable({
        dim_data <- cbind(t(dim(get_gene_data())))
        colnames(dim_data) <- c("Number of rows(genes)", "Number of columns(samples)")
        dim_data
      })

      # Render gene expression data table
      output$desc_expression_data <- DT::renderDataTable({
        DT::datatable(join_data(), options = list(scrollX = TRUE))
      })

      # Render UI for gene expression data visualization
      output$visualization <- renderUI({
        req(input$gene_expression_data$datapath)  # Check if gene expression data is selected
        box(
          width = FALSE,
          textInput(ns("geneName"), "Enter the name of gene"),
          radioButtons(ns("choose_plot_type"), "Choose the type of visualization plot", choices = c("Bar plot", "Density")),
          plotOutput(ns("visual_data"))
        )
      })

      # Render selected gene expression data visualization
      output$visual_data <- renderPlot({
        req(input$choose_plot_type, input$geneName, join_data())  # Check required inputs

        plot_type <- input$choose_plot_type
        data <- join_data()

        switch(
          plot_type,
          "Bar plot" = data %>%
            filter(gene == input$geneName) %>%
            ggplot(., aes(x = samples, y = FPKM, fill = description)) + geom_col(),
          "Density" = data %>%
            filter(gene == input$geneName) %>%
            ggplot(., aes(x = FPKM, fill = description)) + geom_density(alpha = 0.3) # Add density plot code here if needed
        )
      })



      #-----------------------------------------------------------------------------------------------------------
      #********************************** Run DESq2: Differential expression analysis *************************** |
      #-----------------------------------------------------------------------------------------------------------


      run_DGEA<-reactive({

        #  construct a DESeqDataSet object ----------
        refColumn <- input$tissue_column
        dds <- DESeqDataSetFromMatrix(countData = data,
                                      colData = metadata,
                                      design = as.formula(paste("~"," ", refColumn)))

        # pre-filtering: removing rows with low gene counts
        # keeping rows that have at least 10 reads total
        keep <- rowSums(counts(dds)) >= as.numeric(input$filtering_factor)
        dds <- dds[keep,]

        # Step 3: Run DESeq ----------------------
        dds <- DESeq(dds)

        res <- results(dds, alpha = as.numeric(input$input_alpha))
        # remove nulls
        res<-as.data.frame(res[complete.cases(res), ])

        # Assuming 'res' is your DESeq2 results object
        numeric_cols <- sapply(res, is.numeric)

        res[, numeric_cols] <- lapply(res[, numeric_cols], round, 4)

        # Convert numeric columns to character
        res[, numeric_cols] <- apply(res[, numeric_cols], 2, as.character)


        # Explore Results ----------------

       res
      })


      DGEA_results <-eventReactive(input$btn_run_DFEXR,{
        run_DGEA()
      })

      output$results <- DT::renderDataTable({
        validate(
          need(!is.null(DGEA_results()), "Run the analysis first.")
        )

        data_results <- DGEA_results()
        print(head(data_results))

        DT::datatable(data_results, options = list(scrollX = TRUE))
      })

      output$btn_dwn_Degs<- downloadHandler(
        filename = function() {
          paste("DEGs-Analysis-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(as.matrix(run_DGEA()),file, quote=F,row.names=T)
        }
      )

    }
  )
}

