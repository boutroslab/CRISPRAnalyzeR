# sourced by 'ui.r'
# save as 'welcome_ui.r'
# ui elements for welcome




tabItem(tabName = "welcome", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## Welcome message
        fluidRow( style="width:80%;",
          HTML(" <div class='section'>
      <div class='container text-center'>
               <div class='row'>
               <div class='col-md-12'>
               <img src='./images/CRISPRAnalyzR_logo5.png' class='center-block img-responsive'
               style='height: 200px;'>
               </div>
               </div>
               </div>
               </div>
               </br>
               </br>
               <div class='section'>
               <div class='container'>
               <div class='row'>
               <div class='col-md-12'>
               <p class='lead'>CRISPR-AnalyzeR.org offers you web-based analysis plattform for your pooled
               CRISPR Screens.</br>Analyzing your pooled CRISPR screening data has never been easier
               and just requires a few steps.&nbsp;</p>
               </div>
               </div>
               </div>
               </div>")
        ),
          
          # Beta Version
          fluidRow(
                   column(width=6, offset=3,
                          shiny::tags$div(class="alert alert-info",
                                          HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">&times;</a>
                                            <strong>CRISPRAnalyzeR is being released soon - please stay tuned!</strong>')
                                          )
                          )
                   ),
          
          # CRISPRAnalyzeR version
         
          
          fluidRow(
                   column(width=4, offset=4,
                          shiny::tags$h4("Last Update: 2017-01-17 ", shiny::tags$span(class="label label-default", "Version 0.99 RC"))
                          
                          )),
        shiny::tags$br(),
          # CRISPRAnalyzeR Description with CRISPRAnalyzeR Workflow
          # left half: Workflow, right half: description
        shiny::tags$hr(width="50%"),
          fluidRow(
                   column(width=10, offset=1,
                          shiny::tags$h1("CRISPRAnalyzeR is a user-friendly analysis suite for pooled CRISPR/Cas9 screens"),
                          
                          # Short video here
                          
                          column(width=6,
                                 ## CRISPRAnalyzeR Description
                                 
                                 shiny::tags$p(class="lead", "Explore your Data, Explore your Analysis."),
                                 shiny::tags$p(class="text-justify",
                                               "CRISPRAnalyzeR is a web-based analysis platform for pooled CRISPR screens.",
                                               "CRISPRAnalyzeR is easy-to-use and provides you with an extensive data analysis that includes everything you need to know - without the need to install any software.",
                                               "And once you are finished, you can download all the data as well as your analysis as an interactive HTML file."),
                                 shiny::tags$br(),
                                 
                                 shiny::tags$p(class="lead","It's not just easy to use - it also comes packed with loads of features")
                                 
                                 
                                 ),
                          column(width=6,
                                 shiny::tags$ul(class="cloud", style="width:80%;",
                                                shiny::tags$li("Interactive"),
                                                shiny::tags$li("FASTQ/NGS Quality"),
                                                shiny::tags$li("Easy-to-use"),
                                                shiny::tags$li("Screening Quality"),
                                                shiny::tags$li("6 different Hit-Calling methods"),
                                                shiny::tags$li("Gene Annotation"),
                                                shiny::tags$li("In-depth information about Genes and sgRNAs"),
                                                shiny::tags$li("Essential Genes"),
                                                shiny::tags$li("Gene Ontology"),
                                                shiny::tags$li("KEGG Analysis"),
                                                shiny::tags$li("Protein Interactions"),
                                                shiny::tags$li("Gene Set Analysis"),
                                                shiny::tags$li("Fully-interactive offline Report")
                                 )
                                 ),
                          column(width=12,
                                ## CRISPRAnalyzeR Workflow
                                shiny::tags$hr(width="50%"),
                                shiny::tags$img(class="img-responsive", src="./images/CRISPRAnalyzeR_workflow3.png")
                                )
                          
                          ),
                   
                   column(width=10, offset=1,
                          shiny::tags$br(),
                          shiny::tags$hr(),
                          
                          shiny::tags$h2("Download CRISPRAnalyzeR for offline use!"),
                          
                          # Short video here
                          
                          column(width=12,
                                 ## How to download
                                 
                                 shiny::tags$p(class="text-justify", "You can use our online web service, but also download CRISPRAnalyzeR to install it on your local computer or within your lab/institute.",
                                               "CRISPRAnalyzeR is open-source and free for non-commercial use, please check out the download pages below."),
                                 shiny::tags$p(class="lead", "CRISPRAnalyzeR can be downloaded so you can install it on your computer"),
                                 
                                 shiny::tags$br(),
                                 shiny::tags$h4("For further information please check our Github page:"),
                                 shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/", target="_blank" ,shiny::tags$button(type="button", class="btn btn-success btn-lg", shiny::icon("github", class = NULL, lib = "font-awesome"), "Visit the Github page")),
                                 shiny::tags$br(),
                                 shiny::tags$h4("Or download CRISPRAnalyzeR directly:"),
                                 shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/releases", target="_blank" ,shiny::tags$button(type="button", class="btn btn-primary btn-lg", shiny::icon("download", class = NULL, lib = "font-awesome"), "Download Source Code")),
                                 shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/wiki/Installation:-How-to-download-and-install-CRISPRAnalyzeR", target="_blank" ,shiny::tags$button(type="button", class="btn btn-primary btn-lg", shiny::icon("download", class = NULL, lib = "font-awesome"), "Download Ready-to-use Docker Container"))
                                 
                                 
                                 
                          )
                          
                   )
                   ),
          
        # load footer  
        source(file.path(config$appDir, "footer.r"), echo = FALSE, verbose = FALSE)[1]
          
          
          
          
 ) # End of tabItem
