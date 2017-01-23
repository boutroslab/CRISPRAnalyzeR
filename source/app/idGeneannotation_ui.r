# sourced by 'ui.r'
# save as 'idSgrna_ui.r'
# ui elements for "In-Depth Analysis" tab "sgRNAs" sub tab




tabItem(tabName = "id_geneannotation", align = "center",
        
        shinyjs::useShinyjs(),
        
        
        ## Welcome message
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Hit Confirmation
                      <font color='#777777'>&nbsp;
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Gene Annotation</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>The more information, especially about annotated fateures, you get about possible hit candidates - the better.
                      <br>You can get annotation for multiple genes and compare these.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 ),    
        
        # HELP as including of idGeneannotation_help.r
        source(file.path(config$appDir, "idGeneannotation_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        fluidRow(
              column(width=8, offset=2,
                 box(width=12, solidHeader=FALSE, color="grey",
                     shiny::tags$div(
                     column(width=6,
                            # submit box
                             shiny::tags$h3("Which Genes do you wish to annotate?"),
                                uiOutput("hit_select_annotation"),
                                helpText("Select (multiple) genes of interest.")
                                
                            
                     ),
                     column(width=6,
                            shiny::tags$h3("Available Annotations"),
                                 helpText("Please select up to 20 different annotation features.", shiny::tags$br(),
                                          "The input supports instant search, so just type and find interesting annotations."),
                                 uiOutput("anno2"),
                                 uiOutput("hit_select_anno")
                            )
                  ),
                     shiny::tags$div(
                       shiny::tags$br(),
                       uiOutput("geneAnnotation_error"),
                       uiOutput("geneAnnotation_progressBar"),
                       div(style="display:inline-block;", actionButton("hit_submit_annotation", "Annotate Genes")),
                       div(style="display:inline-block;", actionButton("addReport_anno", "Add to Report")),
                       helpText("This may take a few seconds.")
                     )
                     
                     )
                 
                
              )
        ), # FluidRow  
        
        
        fluidRow(
          column(width=10,offset=1,
          # tabsetbox with plots
          tabBox(
            title = NULL,
            width = 12,
            
            tabPanel("Gene Annotations",
                     shiny::tags$div(width="100%",
                     helpText("Selected annotations"),
                     
                     h3("Gene Annotation Table"),
                     DT::dataTableOutput("idanno_annotation")
                     
                     )
            )
            
          )
          )
          
          
        ),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        

        
) # close tab










