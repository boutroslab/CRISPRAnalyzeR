# sourced by 'ui.r'
# save as 'settings_ui.r'
# ui elements for 'Settings' tab




tabItem(tabName = "settings", align = "center",

  shinyjs::useShinyjs(),
    
  ## CSS
  shiny::tags$head(
    shiny::tags$style(HTML('#resetAnalysis{background-color:#dddddd; color:#000000}'))
  ),
  
  
  ## Welcome message
  fluidRow(style="width:80%;",
          HTML(" <div class='section'>
      <div class='container'>
               <div class='row'>
               <div class='col-md-12'>
               <h1 class='text-success text-left'>
               <i class='fa fa-angle-double-right  fa-fw'></i>Settings
               <small>&nbsp;Analysis Settings</small>
               </h1>
               <hr>
               <p class='lead'>In the settings tab all important parameters for the Hit Analysis are
               set.
               <br>Moreover you can define control genes and select which of the groups you
               defined are compared.
               <br>All information from the Data Upload and Setup page remain unaffected.</p>
               </div>
               </div>
               </div>
               </div>
               ")
    
  ),
  
  # HELP as including of settings_help.r
  source(file.path(config$appDir, "settings_help.r"))[1],
  
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  fluidRow(
    column(width = 8, offset = 2,
           box(title = "Step 1: Set Controls (optional)", collapsible = TRUE, 
               width = 12,
               solidHeader = TRUE,
               status = "primary",
        helpText("Controls are important to judge the screening results."),
        helpText("Please enter the Gene Identiiers of your controls, separate multiple genes by comma (',')."),
        uiOutput("compare_error"),
        textInput("geneID_pos", "Gene Identifier for positive control(s)"),
        textInput("geneID_neg", "Gene Identifier for non-targeting control(s)")
      )
    )
  ),
   fluidRow(
    ## Middle Column
    column(width=8,offset=2,
        
      ## Hit Calling Box
      box( title = "Step 2: Adjust Hit Calling Thresholds",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        width = 12,
        status = "primary",
        
        uiOutput("analysis_error"),
        shiny::tags$h3(class="text-success", "Differential Hit Analysis"),
        helpText("CRISPRAnalyzeR supports 6 different analysis methods which will be used to analyse your screen.",
                 "Please define the p-value cutoffs for each individual analysis.",
                 "From our experience, the p-value threshold for DESeq2 should be set very low."),
        shiny::tags$h4("Wilcox"),
        div(style="display:inline-block", numericInput("wilcox_pval", "P-Value Threshold", 
          value = 0.05, min = 0, max = 1, step = 0.005, width = "150px")),
        div(style="display:inline-block", numericInput("wilcox_rand", "Number of randomly picked sgRNAs", 
          value = 300, min = 1, step = 1, width = "150px")),
        helpText("If you did not specify non-targeting controls Wilcox will use randomly picked sgRNAs."),
        fluidRow(
          column(width = 6, h4("DESeq2"),
            numericInput("deseq2_pval", "P-Value Threshold", value = 0.001, min = 0, max = 1, step = 0.005, width = "150px")
          ), column(width = 6, h4("MAGeCK"),
            numericInput("mageck_pval", "P-Value Threshold", value = 0.05, min = 0, max = 1, step = 0.005, width = "150px")
        )),
        fluidRow(
          column(width = 6, h4("sgRSEA"),
            numericInput("sgrsea_pval", "P-Value Threshold", value = 0.05, min = 0, max = 1, step = 0.005, width = "150px")     
          ), column(width = 6, h4("EdgeR"),
            numericInput("edger_pval", "P-Value Threshold", value = 0.05, min = 0, max = 1, step = 0.005, width = "150px")  
          )
        ),
        # BAGEL and ScreenBEAM
        fluidRow(
          
          column(width = 6, h4("BAGEL"),
                 shiny::tags$h4(class="text-success", "Essential Genes Analysis"),
                 
                 helpText("You can define the range in which you expect the BAGEL cutoff."),
                 numericInput("bagel_lower", "Lowest BAGEL cutoff", value = -50, min = -100, max = 80, step = 1, width = "150px"),
                 numericInput("bagel_higher", "Highest BAGEL cutoff", value = 100, min = 0, max = 200, step = 1, width = "150px")
          ), column(width = 6, h4("ScreenBEAM"),
                    shiny::tags$p("Do you want to run ScreenBEAM data analysis? This will increase the analysis calculation time by at least three times."),
                    checkboxInput("screenbeam_run", value=FALSE, label = HTML("<strong>YES</strong>, activate ScreenBEAM analysis.")),
                    numericInput("screenbeam_pval", "P-Value Threshold", 
                                 value = 0.05, min = 0, max = 1, step = 0.005, width = "150px"),
                    helpText("You can change the default calculation parameters of ScreenBEAM."),
                    helpText("Changing these parameters will lead to increased/decreased calculation times!"),
                    numericInput("screenbeam_iterations", "Number of iterations", value = 1500, min = 200, max = 15000, step = 1, width = "150px"),
                    numericInput("screenbeam_burnin", "Burnin", value = 500, min = 50, max = 5000, step = 1, width = "150px")
                    
          ))
          
        
      )
    )
   ),
  fluidRow(
     
    ## Right Column
    column(width = 8, offset=2,
      
      ## Compare Groups      
      box( title = "Step 3: Select which Groups to Compare",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        status = "primary",
        helpText("CRISPRAnalyzeR will compare the following two groups. Please note that you can only compare two groups at once.
                 If you have more treatment groups, you can always come back to this page and run pairwise analyses."),
        
        uiOutput("groups_compare")
      )
    )
  ),
  
  fluidRow(
    
    ## Right Column
    column(width = 8, offset=2,
           
           ## Compare Groups      
           box( title = "Step 4: Removing low/high Read Counts from Analysis",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                status = "primary",
                
                helpText("CRISPRAnalyzeR allows you to remove sgRNAs with low/high read counts from the entire analysis."),
                shiny::tags$br(),
                column(width=6, offset=3, class="alert alert-info", style="margin-top:10px;",
                       shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                       shiny::tags$span(
                         shiny::tags$p(HTML("<strong>For a more robust analysis, we recommend to remove sgRNAs with a read count of less than 20.</strong>"))
                       )
                ),
                column(width=12,
                       column(width=12,
                              column(width=8, checkboxInput("remove_low", value=FALSE, label = HTML("<strong>YES</strong>, remove sgRNAs with a read count <strong>LOWER or equal</strong> to"))),
                              column(width=4, textInput("remove_threshold_low", label = NULL, value="0", placeholder = "Please enter the read count threshold"))
                       ),
                       column(width=12,
                              column(width=8, checkboxInput("remove_high", value=FALSE, label = HTML("<strong>YES</strong>, remove sgRNAs with a read count <strong>HIGHER or equal</strong> to"))),
                              column(width=4, textInput("remove_threshold_high", label = NULL, value="0", placeholder = "Please enter the read count threshold"))
                       ),
                       column(width=12,
                              shiny::tags$br(),
                              uiOutput("remove_in_group")
                       )
                )
                
                )
    )
  ),
  
  
  ## Start Analysis
  fluidRow(style="width:85%",
    uiOutput("final_error"),
    uiOutput("results_error"),
    div(style="display:inline-block", actionButton("startAnalysis", "Start Analysis")),
    div(style="display:inline-block", actionButton("resetAnalysis", "Change Settings", icon = icon("refresh"))),
    uiOutput("analysis_progressBar"),
    shiny::tags$br(),
    uiOutput("info_progressBar"),
    
    column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
                    shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                    shiny::tags$span(
                      uiOutput("analysisduration")
                      
                    )
                    )
    # helpText("Usually, the analysis should take about 1-5 minutes depending on the server load. The progress is always visible in the notification menu in the upper right corner.", br(),
    #      "Please note that all upcoming pages require a successfully finished analysis.")
  ),
  
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
   
) # close tab
