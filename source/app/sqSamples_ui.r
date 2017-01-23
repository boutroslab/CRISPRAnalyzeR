# sourced by 'ui.r'
# save as 'sqSamples_ui.r'
# ui elements of 'Samples' sub tab in 'Screen Quality' tab






tabItem(tabName = "sq_samples", align = "center",
  
  ## Welcome message
  fluidRow( style="width:80%;",
    HTML("<div class='section'><div class='container'><div class='row'><div class='col-md-12'>
         <h1 class='text-success text-left'><i class='fa fa-angle-double-right fa-fw'>
         </i>Screen Quality<font color='#777777'><span style='font-size:23.3999996185303px;line-height:23.3999996185303px;'>
         Sample comparison</span></font></h1><hr>
         <p class='lead'>For sure your screen was done in replicates.&nbsp;But how well are your replicates?<br>
         This page will give you the answer as an Overview, or more detailed incase you are patient.</p></div></div></div></div>")
  ),
  
  # Help Page
  source(file.path(config$appDir, "sqSamples_help.r"))[1],
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  # Tabset Panel
  fluidRow(
    column(width = 10, offset = 1,
    tabBox(width = 12,
                                                  
    ## Overview Boxplots
    tabPanel("Overview",
             
      shiny::tags$p(class="lead","Overview of log2 Foldchanges and Z-Ratio for all Sample Combinations."),
      shiny::tags$h3("Sample Comparison Overview"),
      shiny::tags$br(),
             
      fluidRow(style="width:100%",
        box(title = "Log2 Foldchange on Gene Effects",
            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewLOG")
                   )
            ),
        
        box(title = "Z-Ratio on Gene Effects", 
            solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = FALSE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewZRATIO")
                   )
            )
      ),
      
      shiny::tags$hr(),
      fluidRow(style="width:100%",
        box(title = "Log2 Foldchange on sgRNA Effects", 
            solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = TRUE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewLOGsgrna")
                   )
            )
      )
    ),
             
    
    ## Pairs
    tabPanel("Pairwise Comparisons",
     
      column(4, offset=4,
        box(title = "Compare Two Screening Samples",
            width = 12,solidHeader = FALSE,color = "gray",
            uiOutput("hcCompareSample_select"),
            helpText("Please select the comparison of interest.")
        )
      ),
       
      fluidRow(style="width:100%",
        box(title = "Log2 Foldchange on Gene Effects", 
            solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = FALSE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewLOG_sample")
                   )
            
        ),
        box(title = "Z-Ratio on Gene Effects", 
            solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = FALSE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewZRATIO_sample")
                   )
        )
      ),
      
      shiny::tags$hr(),
       
      fluidRow(style="width:100%",
        box(title = "Log2 Foldchange on sgRNA Effects", 
            solidHeader = TRUE, width = 12,collapsible = TRUE, collapsed = TRUE, status = "primary",
            column(width=12,
                   highchartOutput("hcCompareSamplesOverviewLOGsgrna_sample")
                   )
        )
      ),
       
      fluidRow(style="width:100%",
          box(title = "Gene Data", 
              solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = TRUE, status = "primary",
              column(width=12,
                     dataTableOutput("hcCompareSamplesTableGene")
                     )
          )
      ),
      fluidRow(style="width:100%",
        box(title = "sgRNA Data", 
            solidHeader = TRUE,width = 12,collapsible = TRUE, collapsed = TRUE, status = "primary",
            column(width=12,
                   dataTableOutput("hcCompareSamplesTableSgrna")
                   )
        )
      )
    ),
        
                                     
    ## SPLOMs
    tabPanel("Scatterplot Matrices",
        
      shiny::tags$p(class = "lead",
                    "This scatter plot matrix gives you an overview of all dataset comparisons at once."),
      shiny::tags$p("Please have a look the options below.", shiny::tags$br(),
                    "You can download the image with a right click -> Save As and also add the actual plot to your CRISPRAnalyzeR Report by clicking on the Add Report button."), 
      shiny::tags$br(),

      fluidRow(column(width=12,
        box( title = "Options", width = 12, solidHeader = FALSE, color = "gray",
             # column(width=6,
             #        radioButtons("sqReplicates_overview_ctrl", "Controls", choices = c("positive" = "pos", "non-targeting" = "neg")),
             #        helpText("Positive controls are highlighted in red, non-targeting in blue.")
             #        ),
          column(width=6, offset = 3,
            column(6, checkboxInput("sqReplicates_overview_log", "Show the plot log2 transformed", value = FALSE)),
            column(6, checkboxInput("sqReplicates_overview_aggro", "Show the plot with gene level read count", value = TRUE))
          ),
          column(width=6, offset = 3,
                 actionButton("addReport_SPLOM", "Add to Report")
                 )
          
        )
      )),
      
      fluidRow(column(12, 
        shiny::tags$h3("Overall Pairwise Correlations"),
        plotOutput("sqReplicates_overview_plot", height = "800px", width = "800px")
      ))
      
    ),
      
    
    ## each dataset
    tabPanel("Scatterplots",
        
      column(width=8, offset=2, shiny::tags$p(class="lead","Here, you can take a closer look at each dataset pair.")),
        
      column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
        shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
        shiny::tags$span(
          shiny::tags$strong(HTML("Please note:</br>Depending on the size of your screen the scatter plot may take some time to show up on your browser - please be patient."))
        )
      ),
           
      shiny::tags$br(),
        
      fluidRow(column(width=12, 
                      box(title = "Options", width=12, solidHeader = FALSE, color = "gray",
                          column(width=4,
                                 uiOutput("sqReplicates_dataset_select")
                                 ),
                          column(width=4,
                                 # column(width=12,
                                 # radioButtons("sqReplicates_dataset_ctrl", "control", choices = c("positive" = "pos", "non-targeting" = "neg")),
                                 # helpText("Positive controls are highlighted in red, non-targeting in blue.")
                                 # ),
                                 div(checkboxInput("sqReplicates_dataset_log", "Show the plot log2 transformed", value = FALSE),
                                     checkboxInput("sqReplicates_dataset_aggro", "Show the plot with gene level read count", value = TRUE), 
                                  class = "inline")
                                 # column(6, checkboxInput("sqReplicates_dataset_log", "Show the plot log2 transformed", value = FALSE)),
                                 # column(6, checkboxInput("sqReplicates_dataset_aggro", "Show the plot with gene level read count", value = TRUE))
                                 ),
                          column(width=4,
                                 uiOutput("sqReplicates_labelgene_select"),
                                 helpText("Highlighted genes will be shown in orange color.")
                                 ),
                          column(width=6, offset=3,
                                 shiny::tags$br(),
                                 actionButton("addReport_replicates", "Add to Report")
                                 )
                          ), 
        shiny::tags$br()
        
      )),
      
      fluidRow(column(12, 
        h3("Comparison"),
        highchartOutput2("sqReplicates_dataset_plot", height = "800px", width = "800px")
      ))
    
    )
      
  ))),
  
  shiny::tags$br(),
  
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
   
) # close tab
