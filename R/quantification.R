quantificationUI <- function(id) {
  ns <- NS(id)
  tagList(

      box( width= FALSE, title = "Quatification : A program for assigning sequence reads to genomic features", status= "primary", solidHeader = TRUE,

        box( width = FALSE,
             fileInput(ns("dw_GTF"),"Enter the absolute path to the .gtf ( gtf.gz ) annotation file"),
            #textInputIcon(ns("dw_GTF"), label="Enter the absolute path to the .gtf ( gtf.gz ) annotation file",placeholder = "Enter the path of the gtf or gtf.gz  file",icon = icon("folder-open")),
                  tags$b(span(style="color:gray", "Description of the annotation file uploaded")),br(),br(),
                  DT::dataTableOutput(ns("AnnotationFile")),
             )),

        box( width= FALSE, title = "Flatten Features in GTF  Annotation File", status= "primary", solidHeader = TRUE,
         tags$h3(tags$b(tags$tspan("Looking for 'exon' features... (grouped by 'gene_id')"))),
         DT::dataTableOutput(ns("flatten_features")),
         hr()
          ),

    box( width= FALSE, title = "Feature Count", status= "primary", solidHeader = TRUE,
         box( width = FALSE,
         textInputIcon(ns("BAMFiles"), label="The function takes as input a set of SAM or BAM files containing read mapping results.",placeholder = "Enter the path of the folder",icon = icon("folder-open")),
         tags$span(style="color:blue", "Attention! make sure that the folder contains .BAM files only"),
         br(),
         br(),
         actionBttn(ns("btn_count"),label = "Run Count", icon = icon("play"),color = "danger",size = "sm", no_outline = TRUE),
         br(),
         br(),
         tags$b(span(style="color:gray", "The result of the featureCounts")),br(),br(),
         DT::dataTableOutput(ns("featureCount")),
    ),
         uiOutput(ns("dwn_UI")), br(), br()

    )

  )
}

quantificationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$AnnotationFile <- DT::renderDataTable({
        if (is.null(input$dw_GTF$datapath) || input$dw_GTF$datapath == "") {
          return(as.matrix("Import an annotation file .GTF"))
        }
        GTF_File <- input$dw_GTF$datapath
        Annotation <- rtracklayer::import(GTF_File, format = "gtf")
        dataGTF <- as.data.frame(Annotation)
        DT::datatable(dataGTF, options = list(scrollX = TRUE))
      })


      output$flatten_features<-DT::renderDataTable({
        if(is.null(input$dw_GTF$datapath) || input$dw_GTF$datapath == ""){
          return(as.matrix("Import an annotation file .GTF"))
        }
        file <-input$dw_GTF$datapath
        annotatedData<-flattenGTF(GTFfile =  file)
        DT::datatable(as.data.frame(annotatedData),options =list(scrollX = TRUE) )

      })

      runCount<-eventReactive(input$btn_count,{
        BAM_folder<-input$BAMFiles

        BAM_files<-list.files(full.names=TRUE, path=BAM_folder)
        AnnotationFile<-input$dw_GTF$datapath

        fc_SE <- featureCounts(BAM_files, isGTFAnnotationFile = TRUE, annot.ext=AnnotationFile)
        countMatrix<-as.data.frame(fc_SE$counts)
        colnames(countMatrix) <- stringr::str_remove(colnames(countMatrix), "-subread.BAM")
        #colnames(countMatrix)<-t(import.metadata()['Run'])
        return(countMatrix)

      })

      output$featureCount<- DT::renderDataTable({
        if(input$BAMFiles==""){
          return(as.matrix("Import the BAM file(s)"))
        }
        #if(is.null(input$dw_pheno_file)){
         # return(as.matrix("Import the metadata of the study"))
        #}
        DT::datatable(runCount(), options = list(scrollX = TRUE))
      })


      #-----------------------------------------------------------------------------------------------------
      ################################## Download the count matrix : #######################################
      #-----------------------------------------------------------------------------------------------------
      output$dwn_UI<-renderUI({
        downloadBttn(ns("dwn_count_matrix"),label = "Download the count matrix",icon = icon("download"),size = "sm",color = "danger")
      })

      output$dwn_count_matrix<- downloadHandler(
        filename = function() {
          paste("count-Matrix-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(runCount(), file)
        }
      )

    }
  )
}
