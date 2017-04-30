# sourced by 'ui.r'
# save as 'enrichment_ui.r'
# ui elements for data tab




tabItem(tabName = "enrichment", align = "center",
        
        shinyjs::useShinyjs(),
      
        
        
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
                      <i class='fa fa-angle-double-right  fa-fw'></i>Hit Confirmation
                      <small>&nbsp;Interaction Networks and Gene Set Enrichment</small>
                      </h1>
                      <hr>
                      <p class='lead'>CRISPRAnalyzeR offers you the enrichment of gene sets with interaction networks from the String Database as well as additional information from EnrichR and various other databases.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 ), # End of fluidrow
        
        
        # HELP as including of enrichmenthelp.r
        source(file.path(config$appDir, "enrichment_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        # Page
        fluidRow(
          column(width = 10, offset=1,
                 shinydashboard::box(title = "Select Genes", width = 12, solidHeader = TRUE, status = "primary",
                                     shiny::tags$h4(class="text text-success", "Select individual genes or gene lists"),
                                     
                                     textInput("enrichmentFDR", label = "FDR cutoff", value = 0.05, placeholder = "FDR cutoff for analysis", width="30%"),
                                     
                                     column(width=6,
                                            shiny::tags$h4("Select individual genes for Gene Set Analysis"),
                                            uiOutput("enrichmentGene")
                                            ),
                                     column(width=6,
                                            shiny::tags$h4("Select gene lists for Gene Set Analysis"),
                                            checkboxInput(inputId = "GSE_selectList",label = "Do you want to use pre-defined gene lists?",value = FALSE),
                                            shiny::tags$br(),
                                            # user can select TOP or significant ones of eahc method,
                                            uiOutput(outputId = "GSE_methods"),
                                            uiOutput(outputId = "GSE_methods_genes"),
                                            uiOutput(outputId = "GSE_top")
                                            ),
                                     column(width=12,
                                            helpText("You can add up to 200 genes for the analysis."),
                                            div(style="display:inline-block;", actionButton("submit_enrichment", "Perform Gene Set Analysis")),
                                            div(style="display:inline-block;", actionButton("addReport_enr", "Add to Report")),
                                            shiny::tags$br()
                                            )
                                     
                                     
                                     
                 )
                 
                 )
        ),
        
        fluidRow(style="margin-top:40px;",
          
          column(width=12,
                 tabBox(
                   title = NULL,
                   width = 12,
                   
                   tabPanel("Pathways",
                            
                            
                            # WikiPathways_2016
                            # KEGG_2016
                            # Biocarta_2016
                            # Reactome_2016
                            # NCI_Nature_2016
                            # Panther_2016
                            column(width=6, offset=3
                                   #helpText("Depending on the selected Threshold, the interaction network can be very large, so please start with the default threshold of 999.")
                                   
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Wiki Pathways", collapsed = TRUE,
                                       shiny::tags$h3("Wiki Pathways"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("wikipathways")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("wikipathways_DT")
                                   )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "KEGG", collapsed = TRUE,
                                       shiny::tags$h3("KEGG"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("kegg")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("kegg_DT")
                                   )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Biocarta", collapsed = TRUE,
                                       shiny::tags$h3("Biocarta"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("biocarta")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("biocarta_DT")
                                   )
                            ),column(width=12, style="margin-top:40px;" ,
                                    shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Reactome", collapsed = TRUE,
                                        shiny::tags$h3("Reactome"),
                                        shiny::tags$div(width="100%",
                                                        highchartOutput("reactome")
                                        ),
                                        shiny::tags$hr(),
                                        dataTableOutput("reactome_DT")
                                    )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "NCI Nature 2016", collapsed = TRUE,
                                       shiny::tags$h3("NCI Nature 2016"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("ncinature")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("ncinature_DT")
                                   )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Panther 2016", collapsed = TRUE,
                                       shiny::tags$h3("Panther 2016"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("panther")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("panther_DT")
                                   )
                            )
 
                   ),
                   tabPanel("Transcription",
                            
                            
                            # ChEA_2015
                            # TRANSFAC_and_JASPAR_PWMs
                            # ENCODE and ChEA Consensus TFs from ChIP-X
                            # TargetScan microRNA
                            # Transcription Factor PPIs
                            
                            column(width=6, offset=3
                                   
                                   #helpText("Depending on the selected Threshold, the interaction network can be very large, so please start with the default threshold of 999.")
                                   
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                    shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "ChEA 2015 Data", collapsed = TRUE,
                                        shiny::tags$h3("ChEA 2015"),
                                        shiny::tags$div(width="100%",
                                                        highchartOutput("ChEA_2015")
                                                        ),
                                        shiny::tags$hr(),
                                        dataTableOutput("ChEA_2015_DT")
                                    )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "TRANSFAC and JASPAR PWMs", collapsed = TRUE,
                                       shiny::tags$h3("TRANSFAC and JASPAR PWMs"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("TRANSFAC")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("TRANSFAC_DT")
                                   )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "ENCODE and ChEA Consensus TFs from ChIP-X", collapsed = TRUE,
                                       shiny::tags$h3("ENCODE and ChEA Consensus TFs from ChIP-X"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("ENCODE")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("ENCODE_DT")
                                   )
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "TargetScan microRNA", collapsed = TRUE,
                                       shiny::tags$h3("TargetScan microRNA"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("targetscan")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("targetscan_DT")
                                   )
                            ),
                            shiny::tags$hr(),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Transcription Factor PPIs", collapsed = TRUE,
                                       shiny::tags$h3("Transcription Factor PPIs"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("ppi")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("ppi_DT")
                                   )
                                   
                            )
                            
                   ),
                   tabPanel("Ontologies",
                            
                            # GO Biological Process 2015
                            # GO Cellular Component 2015
                            # GO Molecular Function 2015
                            column(width=6, offset=3
                                   #helpText("Depending on the selected Threshold, the interaction network can be very large, so please start with the default threshold of 999.")
                                   
                            ),
                            column(width=12, style="margin-top:40px;" ,
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "GO Biological Process",collapsed = TRUE,
                                       shiny::tags$h3("GO Biological Process"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("go_biologicalprocess")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("go_biologicalprocess_DT")
                                   ),
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "GO Cellular Component",collapsed = TRUE,
                                       shiny::tags$h3("GO Cellular Component"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("go_cellular")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("go_cellular_DT")
                                   ),
                                   shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "GO Molecular Function",collapsed = TRUE,
                                       shiny::tags$h3("GO Molecular Function"),
                                       shiny::tags$div(width="100%",
                                                       highchartOutput("go_molfunction")
                                       ),
                                       shiny::tags$hr(),
                                       dataTableOutput("go_molfunction_DT")
                                   )
                            )
                            
                   ),
                   tabPanel("Diseases",
                           
                           # OMIM Disease
                           # 
                           column(width=6, offset=3
                                  #helpText("Depending on the selected Threshold, the interaction network can be very large, so please start with the default threshold of 999.")
                                  
                           ),
                           column(width=12,
                                  shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "OMIM Disease",collapsed = TRUE,
                                      shiny::tags$h3("Online Mendelian Inheritance in Man®"),
                                      shiny::tags$div(width="100%",
                                                      highchartOutput("omimdisease")
                                      ),
                                      shiny::tags$hr(),
                                      dataTableOutput("omimdisease_DT")
                                  )
                                  
                           )
                           
                   
                 ),
                 tabPanel("Cell Types",
                          
                          # Cancer Cell Line Encyclopedia
                          # NCI-60 Cancer Cell Lines
                          # 
                          column(width=6, offset=3
                                 #helpText("Depending on the selected Threshold, the interaction network can be very large, so please start with the default threshold of 999.")
                                 
                          ),
                          column(width=12,
                                 shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "Cancer Cell Line Encyclopedia", collapsed = TRUE,
                                     shiny::tags$h3("Cancer Cell Line Encyclopedia"),
                                     shiny::tags$div(width="100%",
                                                     highchartOutput("ccle")
                                     ),
                                     shiny::tags$hr(),
                                     dataTableOutput("ccle_DT")
                                 )
                                 
                          ),
                          column(width=12,
                                 shinydashboard::box(width=12, solidHeader = TRUE, collapsible = TRUE,status = "primary", title = "NCI-60 Cancer Cell Line Panel", collapsed = TRUE,
                                     shiny::tags$h3("NCI-60 Cancer Cell Line Panel"),
                                     shiny::tags$div(width="100%",
                                                     highchartOutput("nci60")
                                     ),
                                     shiny::tags$hr(),
                                     dataTableOutput("nci60_DT")
                                 )
                                 
                          )
                          
                          
                 ),
                 tabPanel("Protein Interactions",
                             column(width=6, offset=3,
                                    helpText("The StringDB allows a maximum of 400 proteins."),
                                    sliderInput("stringDBthreshold", "Select the maximum number of proteins to be shown",
                                                 min = 10, max = 400, value = 100
                                     ),
                                    shiny::tags$br(),
                                    helpText("Please note that the plot requires several minutes of computation time depending on the number of interactions.")
                                    #actionButton(inputId = "startstringdb",label = "Calculate protein interactions")
                             ),
                             column(width=12,
                                    shiny::tags$h3("Protein Interactions"),
                                    plotOutput("stringDBnetwork",width = "100%",height = "1000px")
                             )
                 )
                 
              )
        )
        ),
        
        
        shiny::tags$br(),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
)