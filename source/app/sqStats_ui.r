# sourced by 'ui.r'
# save as 'sqStats_ui.r'
# ui elements 'General Statistics' sub tab in 'Screen Quality' tab




tabItem(tabName = "sq_stats", align = "center",

  shinyjs::useShinyjs(),
    
  
  ## Welcome message
  fluidRow(style="width:80%",
           HTML("<div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>Screen Quality
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Basic Dataset Stats</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>In case you like numbers, this is the tab for you.
                <br>Here you can have a look at raw numbers of your screen and get a feeling
                for the quality of the screen.
                <br>In all Screening Quality plots these numbers are used for generating the
                plots.</p>
                </div>
                </div>
                </div>
                </div>")
  ),

  # HELP as including of sqStats_help.r
  source(file.path(config$appDir, "sqStats_help.r"))[1],
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  
  # Tabset Panel
  fluidRow(column(width=10,offset=1,
           
    uiOutput("removeLow_warning2"),
           
    tabBox(width = 12, 
        
      ## Overview
      tabPanel("Overview",

        shiny::tags$p(class="lead","You can find a brief summary of read count statistics both per sgRNA and summed up for all reads belonging to a gene"),
        shiny::tags$br(),
       
        fluidRow(
          column(width = 8, offset=2, 
              shiny::tags$h3("sgRNAs"),
              DT::dataTableOutput("sqStats_basic_sgRNA")
          ),
          column(width=8, offset=2, 
              shiny::tags$h3("Genes"),
              DT::dataTableOutput("sqStats_basic_genes")
          )
        )
        
      ),
      
      
      ## Read Distribution
      tabPanel("Distribution",
        shiny::tags$p(class="lead","Below you find the log2-transformed, normalized read count distribution for each sample.", br(),
                      "Depending on the treatment stringency, e.g. in resistance or dropout screens, the data can show asymmetry.", br()
        ),
        br(),
        
        fluidRow(
          column(width=12, 
              shiny::tags$h3("Pseudodensity"),
              shiny::tags$div(width="100%", highchartOutput("sqCoverage_distro_plot")),
              helpText("Click on legend to exclude/include data. Drag rectangle to zoom.")
          )
        ),
        
        br(),
        
        fluidRow(
          column(width=12,
                 shiny::tags$h3("Essential Genes"),
                 column(width=6,
                        shiny::tags$h4("Untreated Group", class="text text-success"),
                        shiny::tags$div(width="100%", highchartOutput("sqCoverage_essential_plot1")),
                        helpText("Click on legend to exclude/include data. Drag rectangle to zoom.")
                        ),
                 column(width=6,
                        shiny::tags$h4("Treated Group", class="text text-success"),
                        shiny::tags$div(width="100%", highchartOutput("sqCoverage_essential_plot2")),
                        helpText("Click on legend to exclude/include data. Drag rectangle to zoom.")
                 )
                 
          )
        ),
        
        "Normalized Overview",
        shiny::tags$p(class="lead","Below you find the normalized, log2-transformed read count distribution for each sample in a boxplot representation.", br()
        ),
        shiny::tags$br(),
        
        fluidRow(
          column(12, 
              shiny::tags$h3("Boxplot"),
              shiny::tags$div(width="100%",highchartOutput("sqCoverage_distroBoxNorm_plot")),
              helpText("Click on legend to exclude/include data. Drag a rectangle to zoom.")
          )
        )
        
      ),
      
      ## Cumulative Frequency Distribution
      tabPanel("Cumulative Frequency",
               shiny::tags$p(class="lead","Below you find the log2-transformed, normalized cumulative frequency for each sample.", shiny::tags$br(),
                             "", shiny::tags$br()
               ),
               shiny::tags$br(),
               
               fluidRow(
                 box(width=6, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE, title = "Cumulative Frequency for Gene Read Counts",
                        #shiny::tags$h3("Cumulative Frequency for Gene Read Counts"),
                        shiny::tags$div(width="80%", highchartOutput("sqCoverage_CDF_gene")),
                        helpText("Click on legend to exclude/include data. Drag rectangle to zoom.")
                 ),
                 box(width=6, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE, title = "Cumulative Frequency for sgRNA Read Counts",
                        #shiny::tags$h3("Cumulative Frequency for sgRNA Read Counts"),
                        shiny::tags$div(width="80%",highchartOutput("sqCoverage_CDF_sgrna")),
                        helpText("Click on legend to exclude/include data. Drag a rectangle to zoom.")
                 )
               )
      ),
      

      # controls
      tabPanel(
        "Controls",
        #shiny::tags$div(style="margin: 20px 10px 20px 10px;",
        #  shiny::tags$span(class="label label-warning", "Nothing shown?", style="padding-right:10px"),
        #    shiny::tags$span("If you have not set any controls in the <strong>Settings Tab</strong>, nothing will be shown here.")
        #) ,
        shiny::tags$p(class="lead" , "If you specified any controls, this is how the read count statistics for those look like."), br(),
        
        
        fluidRow(
          column(width=8,offset=2, 
            #box( title = "positive controls", width = NULL, solidHeader = FALSE, status = "primary",
              h3("Positive Controls"),
              DT::dataTableOutput("sqStats_ctrl_pos")
            #)
          ),
          column(width=8,offset=2, 
            #box(title = "non-targeting controls", width = NULL, solidHeader = FALSE, status = "primary",
              h3("Non-targeting Controls"),
              DT::dataTableOutput("sqStats_ctrl_neg")
            #)
          )
        )
      ),
      
      
      
      
      ## Read Depth
      tabPanel(
        "Read Depth",
        shiny::tags$p(class="lead","This plot shows the read count per gene normalized to the number of sgRNAs for this gene."),
        shiny::tags$p(shiny::tags$br(),
            "Spikes indicate a higher readcount per sgRNA for this particular gene. One would expect no 
            outstanding spikes within the untreated datasets", shiny::tags$br(), "However, spikes within the treated datasets 
            indicate a read count enrichment per sgRNA for this particular gene.", shiny::tags$br()),
        shiny::tags$br(),
        
        fluidRow(
          column(width=6, offset = 3, 
                 box(title = "Available Datasets", width = NULL, solidHeader = FALSE, color = "gray",
                     uiOutput("sqCoverage_readDepth_select")
                 )
          )
        ),
        fluidRow(
          column(width=12, 
                 #box(title = "read depth", width = NULL, solidHeader = FALSE, status = "primary",
                 shiny::tags$h3("Read Depth"),
                 shiny::tags$div(width="100%", highchartOutput("sqCoverage_readDepth_plot")),
                 
                 helpText("Controls are highlighted in red and blue. Drag a rectangle to zoom.")
                 #)
          )
        )
        ),
      
      
      
      tabPanel("Readcounts",
          
        shiny::tags$p(class="lead" ,"Here you can browse all read count information for each sample either on sgRNA or gene level. Don't forget to download the read count files from the Data Review section."), br(),
        br(),
        
        fluidRow(
          column(4, offset=4,
              box( title = "Please select the type of read counts", width = NULL, solidHeader = FALSE, color = "gray",
                  radioButtons("sqStats_details_radio", "show", inline = TRUE, 
                               choices = list("Read count per gene" = "gene", "Read count per sgRNA" = "sgRNA")),
                  shiny::tags$br(),   
                  uiOutput("sqStats_details_select")
              )
          )
        ),
        
        fluidRow(
          column(12, 
              shiny::tags$h3("Readcount of your Data"),
              DT::dataTableOutput("sqStats_details_out")
          )
        )
        
      )
      
      
      
  ))),
  
  br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
   
) # close tab
