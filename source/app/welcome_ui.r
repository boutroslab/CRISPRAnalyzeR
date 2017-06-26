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
               <p class='lead'>CRISPR-AnalyzeR.org offers you a web-based analysis plattform for your pooled
               CRISPR Screens.</p>
               </div>
               </div>
               </div>
               </div>")
        ),
          
          
          # CRISPRAnalyzeR version
         
          
          fluidRow(
                   column(width=4, offset=4,
                          shiny::tags$h4("Last Update: 2017-06-22 ", shiny::tags$span(class="label label-default", "Version 1.17"))
                          
                          )),
        
        shiny::tags$br(),
          # CRISPRAnalyzeR Description with CRISPRAnalyzeR Workflow
          # left half: Workflow, right half: description
        shiny::tags$hr(width="50%"),
          fluidRow(
                   column(width=10, offset=1,
                          shiny::tags$h1("CRISPRAnalyzeR is a user-friendly analysis suite for pooled CRISPR/Cas9 screens"),
                          
                          # Short video here
                          column(width=8, offset=2,
                          shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                          shiny::tags$iframe(class="embed-responsive-item",
                                                             src="https://www.youtube.com/embed/QTQ8cTwZqug"
                                                             )
                                          )
                          ),
                          shiny::tags$br(),
                          column(width=6,
                                 ## CRISPRAnalyzeR Description
                                 
                                 shiny::tags$p(class="lead", "Explore your Data, Explore your Analysis."),
                                 shiny::tags$p(class="text-justify",
                                               "CRISPRAnalyzeR is a web-based analysis platform for pooled CRISPR screens.",
                                               "CRISPRAnalyzeR was developed with user experience in mind and provides you with a one-in-all data analysis workflow.",
                                               "And once you are finished, you can download all the data as well as your analysis as an interactive HTML report."),
                                 shiny::tags$br(),
                                 
                                 shiny::tags$p(class="lead","CRISPRAnalyzeR offers the following features")
                                 
                                 
                                 ),
                          column(width=6,
                                 shiny::tags$ul(class="cloud", style="width:80%;",
                                                shiny::tags$li("Interactive"),
                                                shiny::tags$li("FASTQ/NGS Quality"),
                                                shiny::tags$li("Easy-to-use"),
                                                shiny::tags$li("Screening Quality"),
                                                shiny::tags$li("8 different Hit-Calling methods"),
                                                shiny::tags$li("Gene Annotation"),
                                                shiny::tags$li("In-depth information about Genes and sgRNAs from 26 external data ressources"),
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
                          
                          shiny::tags$h2("Download CRISPRAnalyzeR for offline use"),
                          
                          # Short video here
                          
                          column(width=12,
                                 ## How to download
                                 shiny::tags$br(),
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
