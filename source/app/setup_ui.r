# sourced by 'ui.r'
# save as 'setup_ui.r'
# ui elements for setup tab




tabItem(tabName = "setup", align = "center",

  shinyjs::useShinyjs(),
  
  ## CSS
  shiny::tags$head(
    shiny::tags$style(HTML('#reset_data2{background-color:#dddddd; color:#000000}'))
  ),
  

  ## Welcome message
  
  fluidRow(style="width:80%;",
           HTML("<div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>Setup&nbsp;
                <small>Define treatment groups and gene identifier</small>
                </h1>
                <hr>
                <p class='lead'> Tell CRISPRAnalyzeR which treatment groups you have, assign samples to it and adjust the gene annotation parameters.</p>
                </div>
                </div>
                <div class='row'>
                <div class='col-md-12'>
                <p>In order to assign samples to a group, the upload of your samples needs to be completed.
                <br>You did a mistake? Don't worry, you can easily change the setup by
                clicking on the Change Setup button.</p>
                </div>
                </div>
                </div>
                </div>
                ")),
  
  # HELP as including of setup_help_help.r
  source(file.path(config$appDir, "setup_help.r"))[1],
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  fluidRow(
           ## Group Names Box
           column(width=8, offset=2,
                  box( title = "Step 1: Which treatment groups do you have?", collapsible = TRUE, collapsed = FALSE,
                       solidHeader = TRUE,
                       width = 12,
                       status = "primary",
                       helpText("Please set the number of treatment groups within your screen and adjust their names.",
                                "You can select the treatments groups you like to compare in the Set Analysis Parameters section later."),
                       uiOutput("groupNames_error"),
                       numericInput("groups_n", "Number of Groups", value = 2, min = 2),
                       uiOutput("groups_names", width = "300px")
                       
                       ))
  ),
  fluidRow(
           ## Arrange Groups Box
           column(width=8,offset=2,
                  box(title = "Step 2: Assign your files to the groups", collapsible = TRUE, collapsed = FALSE,
                      solidHeader = TRUE,
                      width = 12,
                      status = "primary",
                    
                      helpText("Please assign which of the uploaded samples belongs to which treatment group.", br(),
                               "One sample can only belong to one treatment group, you are not allowed to assign two different groups to the same sample.", br(),
                               "We strongly advise you to have replicates, however you can still run the analysis even with a single sample per group.", br(),
                               "Please note: Analysing data without at least two replicates per sample is not advised since most analysis methods do not work properly without replicates."),
                      shiny::tags$p(class="lead", "You can select mutliple samples by pressing ", HTML("<kbd>CMD</kbd>")," or ", HTML("<kbd>CTRL</kbd>")),
                      
                      uiOutput("groups_arrange")
                  ))
           ),
    fluidRow(
             ## biomart options Box
             column(width=8,offset=2,
                    box(title = "Step 3: Adjust the gene annotation parameters", collapsible = TRUE, collapsed = FALSE,
                    solidHeader = TRUE,
                    width = 12,
                    status = "primary",
                    helpText("CRISPRAnalyzeR converts the gene identifier for easier usage, so please select the type of gene identifier within your sgRNA library file.", br(),
                             "If you want to stay with your gene identifier, just select the same identifier twice.",
                             "Please note: Gene ID conversion is done using the Ensembl BiomaRt service."),
                    uiOutput("annos_error"),
                    selectizeInput("biomart_dataset", "Please select the screening organism",
                                   choices = config[["organism"]], options = list(create=FALSE, maxItems = 1), selected = config[["organism"]]["Human"]),
                    #selectInput("biomart_ID", "gene ID in read count file", choices = c("ensembl_gene_id")),
                    selectizeInput(
                      'biomart_ID', 'Please select the gene identifier used in your sgRNA library file',
                      choices = config[["biomart.geneid"]], options = list(create=FALSE, maxItems = 1), selected = config[["biomart.geneid"]]["Ensembl Gene ID"]
                    ),
                    #selectInput("biomart_IDnew", "convert gene ID to", choices = c("hgnc_symbol"))
                    selectizeInput(
                      'biomart_IDnew', 'Please select the gene identifier you want CRISPRAnalyzeR to converted it to',
                      choices = config[["biomart.geneid"]], options = list(create=FALSE, maxItems = 1), selected = config[["biomart.geneid"]]["HGNC symbol"]
                    )
                    )
             )
             ),
  
  fluidRow(
      ## Submit and Reset Button for Group creation
      div(style="display:inline-block", actionButton("submit_groups2", "Set Groups")),
      div(style="display:inline-block", actionButton("reset_data2", "Change Setup", icon = icon("refresh"))),
      helpText("Please don't forget to click. You can always change the settings by clicking on the Change Setup button.")
  ),
  
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
) # close tab



