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
                      <p class='lead'>Review your uploaded data and download generated read count files or the quality report of your FASTQ files.
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
                 shiny::tags$br()

          
        ),
        
        fluidRow(style = "padding:4%;",
      
               column(width=10, offset=1,
                      box(title = "Download Read Count Files",solidHeader = TRUE, collapsible = TRUE,width = 6,status = "primary",
                          
                          shiny::tags$p(class="text text-center",
                                        "For your convenience, just get the generated read count files." ),
                          helpText("Read count files can be used the next time - which is much faster than using FASTQ data."),
                        shiny::tags$br(),
                        downloadButton('download_readcounts', 'Download Readcount files')
                      ),
                      
                      box(id="dlfastqqc", title = "Download FASTQ QC Report",solidHeader = TRUE, collapsible = TRUE, width = 6,status = "primary", 
                          
                          shiny::tags$p(class="text text-center",
                                        "Download the optional FASTQ QC report."),
                          helpText("This report is only available in case you uploaded NGS raw data."),
                          shiny::tags$br(),
                          downloadButton('download_fastq_report', 'Download Quality Report for FASTQ files')
                      )
                      
                     )
        ),
        # Overview of Data
        fluidRow(
                 column(width=10, offset=1,
                        box(title = "Overview of Uploaded Data and Samples", width=12, solidHeader=TRUE, status="primary", collapsible = TRUE,
                            uiOutput("fastq_progressBar2"),
                            uiOutput("removeLow_warning"),
                            uiOutput("overview_files")
                        )
                        )
        ),
        # after analysis, people can download .tsv data files
        fluidRow(
          column(width=10, offset=1,
                 box(id = "downloadanalysisdata", title = "Download Analysis Data", width=12, solidHeader=TRUE, status="primary", collapsible = TRUE,
                     
                 # Download ALL as XLSX or TSV
                
                 column(width = 6,
                        shiny::tags$h4("Download All Analysis Data"),
                        shiny::helpText("You can download all raw data form the individual hit calling methods either as a tab-separated .TSV file or as a fully formatted .XLSX Excel file."),
                        downloadButton('downloadHC_TSV', 'Download .TSV'),
                        downloadButton('downloadHC_XLSX', 'Download as Excel .XLSX'),
                        
                        shiny::tags$hr(),
                        
                        shiny::tags$h4("Download All Intermediate Analysis Data"),
                        downloadButton('downloadHC_rawdata', 'Download .ZIP'),
                        
                        shiny::tags$hr(),
                        
                        shiny::tags$h4("Download All Generated Raw Data"),
                        shiny::helpText("This will download everything that has been generated and is only for advanced users. The filesize can be larger than 1 GB."),
                        downloadButton('download_alldata', 'Download .ZIP')
                        ),
                 column(width = 6,
                        
                        shiny::tags$h4("Download the sgRNA Re-Evaluation File"),
                        shiny::helpText("You can download the sgRNA Re-Evaluation file that includes genomic locations and scores for each individual sgRNA of your library."),
                        downloadButton('downloadSGRNA', 'Download .TSV')#,
                        #shiny::tags$br(),
                        #shiny::tags$h4("Download all data as RDS Objects for R"),
                        #downloadButton('downloadALL', 'Download All Raw Data')
                        )
                     
                 )
          )
        ),
      # load footer  
      source(file.path(config$appDir, "footer.r"))[1]
        
          ) # close tab



