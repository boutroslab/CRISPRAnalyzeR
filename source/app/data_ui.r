# sourced by 'ui.r'
# save as 'data_ui.r'
# ui elements for data tab




tabItem(tabName = "data", align = "center",

  shinyjs::useShinyjs(),
  
  ## CSS
  shiny::tags$head(
    shiny::tags$style(HTML('#reset_data{background-color:#dddddd; color:#000000}'))
  ),

  
  ## Welcome message
  fluidRow(style="width:80%",
  
  #   h2("Data Upload"),
  #   div(
  #     
  #     "In this tab you can upload your sequencing files and your sgRNA library - both are required for the data analysis.",
  #     "Sequencing files can either be <strong>readcount files (.txt) or zipped fastQ files</strong>.", br(),
  #     tags$h4("FASTQ Files"),
  #     "In case you upload fastQ files, we run the program bowtie2 to map the fastQ reads onto
  #     the provided sgRNA library. More advanced users can adjust bowtie2 parameters in the box on the right.", br(),
  #     "Since fastQ files can be quite large, we would ask you only upload compressed files",
  #     "This will make the whole process a little bit faster.", br(),
  #     "Your sequencer probably already did this. In this case, the files should end with .gz",
  #     "Otherwise, please compress them using gzip first"
  #   )
  # 
  # ), 
    
    HTML("<div class='section'>
      <div class='container'>
         <div class='row'>
         <div class='col-md-12'>
         <h1 class='text-success text-left'>
         <i class='fa fa-angle-double-right  fa-fw'></i>Data Upload
         <small>&nbsp;Upload your FASTQ Screening Files and sgRNA Library</small>
         </h1>
         <hr>
         <p class='lead'>As a first step, you need to upload your sequencing files and your sgRNA library file
         - both are required for the data analysis.
         <br>Sequencing files can either be
         <strong>readcount files (
         <i>.txt</i> - please check the format) or zipped FASTQ files (
         <i>.fastq.gz</i>)</strong>.&nbsp;</p>
         </div>
         </div>
         </div>
         </div>
         ")
  ), # End of fluidrow
  
  
  # HELP as including of data_ui_help.r
  source(file.path(config$appDir, "data_ui_help.r"))[1],
  
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  fluidRow(
    column(width=8, offset=2,
           column(width=12, class="alert alert-warning", style="margin-top:40px;",
                  shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                  shiny::tags$span(
                    shiny::tags$h4("Looking for sample data?"),
                    shiny::tags$br(),
                    shiny::tags$p("Just click on the the help above this box and you will find different sample data sets.")
                  )
           )
  )
  ),

  fluidRow(
           column(10,offset=1,
                  # Now we add the box
                  ## sgRNA library upload Box  
                  box(title = "Step 1: Upload Your sgRNA Library File", collapsible = TRUE, 
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                      column(width=6,
                             column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
                                    shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                                    shiny::tags$span(
                                      shiny::tags$p("You can download pre-made sgRNA library FASTA files from the help section.")
                                    )
                             ),
                             column(width=12,
                                   helpText("Please upload your sgRNA library file that contains all sgRNAs used in the screen. It must be in FASTA format."),
                                  
                                   shiny::tags$br(),
                                   
                                   fileInput("libFile_upload", NULL, multiple = FALSE),
                                   
                                   
                                   shiny::tags$br(),
                                   shiny::tags$hr(),
                                   #textInput("libFile_regex", "Regex for gene extraction", value = "^(.+?)(_.*)$"),
                                   selectizeInput( 'libFile_regex', 'Please select a regular expression which matches your sgRNA library', 
                                                   choices = config[["sgrna_regex"]], options = list(create = TRUE)),
                                   
                                   helpText("The regular expression depends on how you designed the sgRNA identifiers within your sgRNA library file.",
                                            shiny::tags$br(),
                                            "For more information please see the help where you can also download pre-made FASTA library files."), br(),
                                   shiny::tags$p("Some Examples:"),
                                   shiny::tags$table(
                                     shiny::tags$dl(class="dl-horizontal",
                                                    shiny::tags$dt("sgRNA Identifier"),
                                                    shiny::tags$dd("ENSG00000166535_30_12678.2422"),
                                                    shiny::tags$dt("Regular Expression"),
                                                    shiny::tags$dd("^(.+?)(_.*)$")
                                                    ) 
                                     
                                   )
                             )
                      ),
                      column(width=6, div(id = "libFile_upload_exampleUI",
                              shiny::tags$h3("An example from your sgRNA library"),
                              shiny::tags$p(class = "lead" ,"This is how CRISPRAnalyzeR detects the gene from your sgRNA identifier."),
                              helpText("Please select the regular expression from the left dropdown menu or type in your own."),
                              helpText("You selected the correct regular expression in the case your gene is highlighted in red colour."),
                              uiOutput("libFile_upload_example"),
                              shiny::tags$br(),
                              shiny::tags$hr()
                              
                            ),
                            shiny::tags$h4("How to separate the gene-specific and sgRNA-specific identifier?"),
                            
                            shiny::tags$img(src = "./images/regex2.png", class="img-responsive", width="90%")
                            
                            )
                  ))),
  
  fluidRow(
    
    column(width=10, offset=1,
    ## sequencing Files upload Box
    box( title = "Step 2: Upload Your Sequencing Files", collapsible = TRUE,
      solidHeader = TRUE,
      width = 12,
      status = "primary",
      
      column(width=6,
                    column(width=10,offset=1, class="alert alert-info", style="margin-top:40px;",
                           shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                           shiny::tags$span(
                             shiny::tags$p(HTML("<strong>You can upload the following files:</strong>")),
                             shiny::tags$dl(
                               shiny::tags$dt("Read Count Files (.txt)"),
                               shiny::tags$dd("Already mapped read count files in .txt tab-separated format"),
                               shiny::tags$dt("NGS Sequencing Files (.fastq.gz)"),
                               shiny::tags$dd("Compressed FASTQ files as they are provided by the sequencer.")
                             )
                           )
                    
                    ),
             column(width=12,
                      #helpText("You can upload already mapped readcount files 
                      #         (as .txt for example, please check the format) or compressed NGS FASTQ files as they come out of the sequencing machine.
                      #         CRISPRAnalyzeR will then extract the files and map it to your sgRNA library file."),
                      shiny::tags$br(),
                      
                      fileInput("seqFiles_upload", NULL, multiple = TRUE),
                      helpText("Please note: You can upload multiple files at once."),
                      shiny::tags$hr(),
                      shiny::tags$br(),
                      strong("Rename Files"),
                      shiny::tags$div("In order to provide a good user experience, please rename your files to include only letters and numbers."),
                      uiOutput("seqFiles_rename")
             )
      ),
      # other half
      column(width=6,
             column(width=12,
                    # Example of Readcount data
                    shiny::tags$h4("Example of a read count file (.txt)"),
                    shiny::tags$p("The sgRNA identifier needs to be TAB-separated from the counts."),
                    shiny::tags$pre(class="text-left",
"sgRNA	Count
ENSG00000053900_GAAAGCAATGAGATCCCGCT	28
ENSG00000053900_GAAGCAATGAGATCCCGCTT	62
ENSG00000053900_GAAGCGGGATCTCATTGCTT	92
"
                    ),
                    # Example of FASTQ data
                    shiny::tags$hr(),
                    shiny::tags$h4("Example of a FASTQ file (not compressed)"),
                    shiny::tags$p("The file needs be GZIP-compressed."),
                    shiny::tags$pre(class="text-left",
"@M01100:47:000000000-AH40C:1:1101:12289:2057 1:N:0:4
AACACCGTCAGTGTGCTTGCCCCACTGTTTTAGAGCTAGAAATAGCAAGTT
+
GGGGGGGGGGDGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
@M01100:47:000000000-AH40C:1:1101:8758:2058 1:N:0:4
AAACACCGGTTTTGAAACTGCGGACACGTTTAGAGCTAGAAATAGCAAGTTA
+
GGGGGGGEGGGGGGGGGDGGFGGGEEGGFFGGFFCFFF=AFF<CEFFF@EFE
"
                    )
                    
                    )
             
             )
    )
    )
  ),
  
  fluidRow(
    column(width=10, offset=1,
    
    ## FASTQ extraction Box
    box( title = "Step 3: Set FASTQ Options (if FASTQ files uploaded)", collapsible = TRUE, collapsed = FALSE,
      solidHeader = TRUE,
      width = 12,
      status = "primary",
      
      column(width=6,
      helpText("This is relevant if you upload compressed FASTQ files.", 
               "These defaults should work fine in most cases."),
      # MODAL HELP
      #actionLink("fastqextractionhelp", label="", icon = icon("question-sign", lib = "glyphicon"), width=100 , style="font-size: 1.3em", class="primary"),
      # HELP MODAL
      # bsModal(id = "modalfastqextraction", title = "FASTQ Extraction", trigger = "fastqextractionhelp", size = "large",
      #         fluidRow(
      #           style="width:95%;",
      #           htmltools::renderDocument(shiny::htmlTemplate(filename = "www/help/fastqextraction_help.html", document_ = TRUE))
      #         )
      # ),
      
      shiny::tags$br(),
      #textInput("seqFiles_regexTarget", "Regex for target sequence in fastQ files", value = "ACC(.{20,21})GTT"),
      selectizeInput('seqFiles_regexTarget', 'Regular Expression for sgRNA target sequence extraction from FASTQ files)',
          choices = config[["fastq_regex"]], options = list(create=TRUE, maxItems = 1)),
      checkboxInput("seqFiles_rev", "Is data in FASTQ in reverse complement?", value = FALSE),
      selectInput("seqFiles_bt2Sens", "Bowtie2 sensitivity", choices = list("very-sensitive-local", "local", "very-sensitive")),
      selectInput("seqFiles_bt2quali", "Bowtie2 quality", choices = list("perfect", "high", "seed")),
      helpText("If you have a really low overall readcount, you can try to go down with these parameters
               and accept mapping mismatches.")
    )
    )
    # other half
    )
  ),
  
  
  
  fluidRow(style="width:85%",
    ## Submit and Reset Buttons for File Upload and Extraction
    shiny::tags$br(),
    shiny::tags$br(),
    div(style="display:inline-block", actionButton("submit_seqFiles", "Check Files", class="btn-lg")),
    div(style="display:inline-block", actionButton("reset_data", "Use other Files", icon = icon("refresh"), class="btn-lg")),
    shiny::tags$br(),
    uiOutput("fastq_progressBar"),
    shiny::tags$br(),
    column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
           shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
           shiny::tags$span(
             uiOutput("extractiontime")
           )
    )
    # column(width=10, offset=1,
    #                    column(width=3,
    #                           HTML('<i class="fa fa-info text-danger danger fa-5x" aria-hidden="true" style="vertical-align:middle;"></i>')),
    #                    column(width=9,
    #                           shiny::tags$p(class="text-danger text-justify","CRISPRAnalyzeR will check your files, which can take a couple of minutes.", shiny::tags$br(), 
    #                                         "Once your files pass the initial checks, your files will be extracted and mapped to your sgRNA library file if necessary, which also takes some time.", br(),
    #                                         "You can always check the progress in the notification menu in the upper right corner.",
    #                                         "In the meantime, head to the Set Groups and Identifiers/Analysis Parameters section where you can already set the parameters for the later analysis.")
    #                           )
    #                    )
    
  ),
  
  
  
  
  
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
) # close tab



