# save as 'idGview_ui.r'
# sourced by 'ui.r'
# creates ui for Genomic View sub tab in In-Depth Analysis tab




tabItem(tabName = "id_gView", align = "center",

  shinyjs::useShinyjs(),
  
  ## CSS
  #tags$head(
  #  tags$style(HTML('#hit_submit{background-color:#4682B4; font-weight:bold; color:white}'))
  #),

  
  ## Welcome message
  fluidRow(style="width:80%;",
           HTML("<div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success'>
                <i class='fa fa-angle-double-right  fa-fw'></i>In-Depth Analysis
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Genomic Visualizations</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>Genomic Visalizations will provide in-depth information about your favourite
                genes and the targeting sgRNAs on a genomic level. Your can even enrich
                the visualization with a variety of additional information.</p>
                </div>
                </div>
                </div>
                </div>
                <div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <div class='panel panel-default'>
                <div class='panel-heading'>
                <h3 class='panel-title'>
                <a data-toggle='collapse' href='#collapseindepthgenomic'><i class='fa fa-arrow-circle-down fa-fw'></i>Additional Information</a>&nbsp;
                <small>Click to extend</small>
                </h3>
                </div>
                <div class='panel-collapse collapse' id='collapseindepthgenomic'>
                <div class='panel-body'>
                <div class='col-md-12'>
                <div class='col-md-12'>
                <h4>
                <span class='label label-primary'>Please note: So far caR only allows the usage of the latest Human Genome
                Ensembl Release (GRCh38.p5).</span>
                </h4>
                <h3 class='text-success'>Select your gene of interest</h3>
                <p>caR offers you the visualization of your favourite gene on the genomic
                level.
                <br>Select your favourite gene from the list of all genes that were target
                by any sgRNA in your sgRNA library.
                <br>For a faster access to your favourite gene, you can start typing the gene
                identifier and caR will show you all matching genes.
                <br>Please note that the converted gene identifier will be listet in this
                case, which is by default the human nomenclature gene symbol.</p>
                <h3 class='text-success'>Select Annotations</h3>
                <p>You can enrich your visualization by selecting a variety of additional
                annotation features, such as the log2 Foldchange or Regulator elements
                as listed below.</p>
                <h4 class='text-primary'>Annotation Tracks</h4>
                <dl class='dl-horizontal' style='text-align: left;'>
                <dt>Regulation</dt>
                <dd>Regulatory features like Promotor Binding Sites as listed by Ensembl.</dd>
                <dt>Log2 Foldchange</dt>
                <dd>Shows the log2 Foldchange of each single sgRNA (comparison as set at the
                settings page).</dd>
                <dt>Log2 Foldchange
                <br>per Exon</dt>
                <dd>Shows a box-and-whiskers plot (log2 Foldchanges) of all sgRNAs targeting
                the same exon.</dd>
                <dt>Normalized Readcount</dt>
                <dd>Plots the normalized Readcount fo each sgRNA.</dd>
                <dt>Missing sgRNAs</dt>
                <dd>Shows you which sgRNAs were missing in the datasets.</dd>
                </dl>
                <h4 class='text-primary'>Score Tracks</h4>
                <dl class='dl-horizontal' style='text-align: left;'>
                <dt>Doench Efficiency
                <br>Score</dt>
                <dd>Plots the Doench Efficiency Score for each sgRNA.</dd>
                <dt>CDS</dt>
                <dd>Plots the CDS Score as retrieved by E-CRISP.</dd>
                <dt>CU Efficiency Score</dt>
                <dd>Plots the XU Efficiency score for each sgRNA.</dd>
                </dl>
                </div>
                </div>
                </div>
                </div>
                </div>
                </div>
                </div>
                </div>
                </div>")
           ),     
  
  # submit box
  fluidRow(
    box(title = NULL, width = 12, solidHeader = FALSE, color = "gray",
        div(strong(style="margin-right: 10px", "Select a gene of interest")),
        div(style="display:inline-block;", uiOutput("hit_select_gView")),
        tags$hr(),
        uiOutput("info_error_gView"),
        uiOutput("hit_error"),
        uiOutput("hitResults_error"),
        uiOutput("hit_progressBar"),
        br(), #input$hit_options.scores
        checkboxGroupInput("hit_options.anno", "Add Annotation Track", inline = TRUE,
          choices = list("Regulatory Features" = "regulation", "sgRNA Foldchange" = "foldchange", "Foldchange per Exon" = "fcexon", "Missing sgRNAs" = "unmapped", "Normalized Readcount" = "normreadcount"),
          selected = c("regulation", "foldchange")
        ),
        checkboxGroupInput("hit_options.scores", "Add Score Track", inline = TRUE,
                           choices = list("Doench Efficiency Score" = "doench", 
                                          "CDS" = "CDS", "Seed GC Content" = "seed_GC" ,
                                          "XU Efficiency Score" = "XU"),
                           selected = c("regulation", "foldchange")
        ),
        helpText("These are the tracks you can add to the Genomic View plot."),
        div(style="display:inline-block;", actionButton("hit_submit", "Create"))
  )),
    
  
  # tabsetbox with plots
  fluidRow(
    
    tabBox(
      title = NULL,
      width = 12,
      height = 2000,
      tabPanel("Genomic View with Annotations",
              width = 12, height = 2000, status = "danger",
              plotOutput("idGview_plotall")        
      ),
      tabPanel("Genomic View with Scores",
              width = 12, height = 2000, status = "danger",
              plotOutput("idGview_plotscores")        
      )
    )
    
    
  ),
  
  
  br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
) # close tab
