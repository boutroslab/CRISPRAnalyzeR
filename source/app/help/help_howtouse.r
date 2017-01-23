# save as help_howtouse.r
shiny::tags$div(class="panel-body",
                # here comes the content
                shiny::tags$p(class="lead", "CRISPRAnalyzeR is used as any other website - check out our tutorials"),
                shiny::tags$p(class="text-justify",
                              "Analysing pooled CRISPR screens using CRISPRAnalyzeR is not difficult. All you need is a modern browser (we recommend Google Chrome), your data and some time.",
                              "Since all analysis is done by the server that runs CRISPRAnalyzeR, your laptop/PC/Tablet does not need to be powerful nor fast.",
                              "We made some tutorial videos that will help you to use CRISPRAnalyzeR and explore all its functionality. In case you need help, just check out the help panel at the top of each page.
                              Just click on it - it will expand and shows you all necessary information.
                              If this doesn't answer your questions, feel free to join the google group and state your question there."),
                shiny::tags$br(),
                # CRISPRAnalyzeR Youtube Video with general usage
                shiny::tags$br(),
                shiny::tags$p(class="lead",
                              "CRISPRAnalyzeR is used from top to bottom"
                ),
                shiny::tags$p(class="text-justify",
                              "First you need to Setup Your Screen which includes uploading all your data, defining the analysis groups and the analysis settings.
                              Once your data is uploaded, you can always go back to the Analysis Settings page, adjust the parameters and re-run the analysis.",
                              "You can also review all uploaded data on the Data Review page, on which can also download ready-to-use readcount files as well as a FASTQ data quality report.",
                              "After the Data Analysis has been performed, you can check your screen quality in the Screen Quality Control Section before heading to the Hit Calling section.",
                              "Please also note the Gene In-Depth Analysis that offers you more detailed information for every gene of interest - including sgRNA performance and Gene Annotation Enrichment."),
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
                               shiny::tags$li("Fully-interactive offline Report")
                )
                
                )