# sourced by 'ui.r'
# save as 'idSgrna_ui.r'
# ui elements for "In-Depth Analysis" tab "sgRNAs" sub tab




tabItem(tabName = "id_sgRNAs", align = "center",

  shinyjs::useShinyjs(),

  
  ## Welcome message
  fluidRow(style="width:80%;",
           HTML("  <div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>Hit Confirmation
                <font color='#777777'>&nbsp;
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>sgRNA Performance</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>It is nice to know about possible hit candidates, but how about the sgRNAs that were used to target these genes?
                <br>On this page you can further investigate the performance and properties of sgRNAs targeting the select gene.</p>
                </div>
                </div>
                </div>
                </div>
                ")
           ),    
  # HELP as including of idSgrna_help.r
  source(file.path(config$appDir, "idSgrna_help.r"))[1],

  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  fluidRow(style="align:center",
    column(4, offset=4,
    # submit box
    box(title = "Investigate Gene",width = 12,solidHeader = FALSE,color = "gray",
        uiOutput("info_error"),
        uiOutput("hit_select"),
        helpText("Select gene of interest."), br(),
        actionButton("addReport_sgRNA", "Add to Report")
    )
    )
  ), # FluidRow  
  
  fluidRow(
    column(width=10, offset=1,
    # tabsetbox with plots
    tabBox(
        title = NULL,
        width = 12,
        
        tabPanel("Readcount",
          helpText("Readcount of each sgRNA targeting the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 5, color = "gray", fluidRow(
            column(6, align = "right", checkboxInput("idReadcount_norm", "normalized", value = TRUE)),
            column(6, align = "left", checkboxInput("idReadcount_pol", "polar plot"))
            #))
          ),
          shiny::tags$h3("sgRNA readcounts"),
          highchartOutput("idReadcount_plot")
        ),
        
        tabPanel("Log2 Foldchange",
          helpText("Foldchange of sgRNAs for the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 5, color = "gray", fluidRow(
            column(6, align = "right", checkboxInput("idEffect_sort", "Sort according to Foldchange?", value = TRUE))#,
            #column(6, align = "left", radioButtons("idEffect_radio", NULL, 
            #    choices = list("fold change" = "fc", "z ratio" = "zr")))
            #))
          ),
          h3("Phenotypic effect of sgRNAs"),
          highchartOutput("idEffect_plot")
        ),
        
        tabPanel("Genomic Binding Sites",
          helpText("These are predicted binding sites of each sgRNA for the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 5, color = "gray", fluidRow(
            column(6, align = "right", checkboxInput("idOfftarget_sort", "sorted", value = TRUE)),
            column(6, align = "left", checkboxInput("idOfftarget_pol", "polar plot"))
            #))
          ),
          h3("Predicted genomic binding sites of sgRNAs (based on E-CRISP.org)"),
          helpText("Conditions: 5' unspecific leading bases = 1, allowed mismatches within target sequence = 2"),
          highchartOutput("idOfftarget_plot")
        ),
        
        tabPanel("Z-Score",
          helpText("Some scores for each sgRNA for the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 3, color = "gray",
            checkboxInput("idZscore_sort", "sort treated group", value = TRUE)
            #)
          ),
          h3("Z-Score of sgRNAs"),
          highchartOutput("idZscore_plot")  
        ),
        
        tabPanel("Efficiency Scores",
          helpText("Some scores for each sgRNA for the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 3, color = "gray",
            checkboxInput("idEscores_pol", "polar")
            #)
          ),
          h3("Efficiency scores of sgRNAs"),
          highchartOutput("idEscores_plot")   
        ),
        
        tabPanel("E-CRISP Scores",
          helpText("Some scores for each sgRNA for the gene of interest."),
          fluidRow(
            #box( title = NULL,  width = 3, color = "gray",
            checkboxInput("idCscores_pol", "polar")
            #)
          ),
          h3("E-CRISP Scores of sgRNAs"),
          highchartOutput("idCscores_plot")  
        ),
        # Sequence TAB
        tabPanel("sgRNA Sequence",
                 helpText("sgRNA target sequences"),
                 
                 h3("sgRNA target sequences"),
                 DT::dataTableOutput("idSGRNAsequence")
        ),
        # Predicted Binding sites TAB
        tabPanel("Predicted sgRNA binding sites",
                 helpText("Preidcted sgRNA binding sites based on E-CRISP.org re-evaluation"),
                 
                 h3("Predicted sgRNA binding sites"),
                 DT::dataTableOutput("idSGRNAtargets")
        )
  
    )
    )
    
  ),
  
  
  br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
) # close tab










