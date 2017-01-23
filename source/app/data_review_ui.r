# sourced by 'ui.r'
# save as 'data_review_ui.r'
# ui elements for data review tab



tabItem(tabName = "data_review", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## CSS
        shiny::tags$head(
          shiny::tags$style(HTML('#reset_data{background-color:#dddddd; color:#000000}'))
        ),
        
        
        ## Welcome message
        fluidRow(style="width:80%",
                 
                 
                 HTML("<div class='section'>
      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Data Review&nbsp;
                      <small>&nbsp;Review your Data and Download Rawdata Files</small>
                      </h1>
                      <hr>
                      <p class='lead'>Review your uploaded data and download generated read count files as well as a quality report of your FASTQ files.
                      </p>
                      </div>
                      </div>
                      </div>
                      </div>")
              
                 ), # End of fluidrow
        
        # HELP as including of data_review_help.r
        source(file.path(config$appDir, "data_review_help.r"))[1],
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        fluidRow(style="width:85%",
                 
                 # Valueboxes
                 # number of files
                 valueBoxOutput("review_numberfiles"),
                 # Filesize total
                 
                 # number of sgRNAs (number of lines divided by two)
                 
                 valueBoxOutput("review_numbersgrnas"),
                 valueBoxOutput("review_filesize"),
                 shiny::tags$br(),
                 shiny::tags$hr()
          
        ),
        
        fluidRow(style = "padding:4%;",
      
               column(width=12,
                      shiny::tags$h3(class="text-success text-left", HTML("<i class='fa fa-angle-double-right  fa-fw'></i>") , "Download Read Count files and FASTQ Quality Report"),
                      shiny::tags$p(class="lead",
                                   "For your convenience, just get the generated read count files and the optional FASTQ quality report (only available if fastq.gz files have been uploaded)." ),
                      helpText("Read Count files can be used the next time - which is much faster than using FASTQ data."),
                      shiny::tags$table(class="text-center",
                      #shiny::tags$tbody(shiny::tags$tr(shiny::tags$td("Get your read count files"), shiny::tags$td("Uploaded FASTQ files? Get your quality report")) ),
                      shiny::tags$tbody(
                        shiny::tags$tr(
                          shiny::tags$td(downloadButton('download_readcounts', 'Download Readcount files'), style = "margin:10px;display:block;"), 
                          shiny::tags$td( downloadButton('download_fastq_report', 'Download Quality Report for FASTQ files'),style = "margin:10px;display:block;")))
                      ))
        ),
       
        
        shiny::tags$hr(width="50%"),
        
        fluidRow(style="width:85%",
                 shiny::tags$h3(class="text-success text-left",HTML("<i class='fa fa-angle-double-right  fa-fw'></i>") ,"Overview of the Uploaded Data"),
                  uiOutput("fastq_progressBar2"),
                  uiOutput("removeLow_warning"),
                 
                 
                  uiOutput("overview_files"),
                  
                  shiny::tags$hr(width="50%")
      # shiny::tags$h4("FASTQ Read Quality"),
      # shiny::tags$br(),
      # plotOutput("rqcReadQualityBoxPlot"),
      # shiny::tags$br()
                  
                 
        ),
      # load footer  
      source(file.path(config$appDir, "footer.r"))[1]
        
          ) # close tab



