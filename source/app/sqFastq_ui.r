## sqFastq_ui.r

# sourced by 'ui.r'

# ui elements for 'screening tab'Coverage' sub tab in 'Screen Quality' tab




tabItem(tabName = "sq_fastq", align = "center",
        
        shinyjs::useShinyjs(),
        
        
        ## Welcome message
        fluidRow(style="width:80%;",
                 HTML("    <div class='section'>
      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Screen Quality
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>FASTQ Sequencing Quality</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>In case you provided NGS Fastq files, you will find visualizations of
                      the sequencing quality on this page.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 
                 ),
        # HELP as including of sqFastq_help.r
        source(file.path(config$appDir, "sq_Fastq_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        fluidRow(style="width:85%",
          column(width=8, offset=2, class="alert alert-info", style="margin-top:20px;",
                 shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                 shiny::tags$span(
                   HTML("<strong>You can download a FASTQ quality report from the Data Review section</strong>")
                 )
          )
        ),
        
        # Tabset Panel
        fluidRow(
          column(width=10,offset=1,
          tabBox(width = 12, #title = tagList(shiny::icon("signal", lib="glyphicon"), "Coverage"),
                
                
                 ## GC Content
                 tabPanel(
                   "Cycle-specific GC Content",
                   shiny::tags$h3("Cycle-specific GC Content"
                       ),
                   br(),
                   
                   plotOutput("rqcGCcontent")
                 ),
                 
                 
                 ## QC per cycle
                 tabPanel(
                   "Cycle-specific Quality Distribution",
                   shiny::tags$h3("Cycle-specific Quality Distribution"),
                   br(),
                   plotOutput("rqccycleqcmap")
                   ),
                 
                 
                 ## QC Per cycle
                 tabPanel(
                   "Cycle-specific Average Quality",
                   shiny::tags$h3("Cycle-specific Average Quality"),
                   br(),
                   
                   plotOutput("rqcQCcycle"),
                   br(),
                   plotOutput("rqcAverageQualityPlot"),
                   br(),
                   
                   plotOutput("rqcReadQualityPlot")
                   
                  ),
                 
                 ## Bases called
                 tabPanel(
                   "Cycle-specific Base Call Proportion",
                   shiny::tags$h3("Cycle-specific Base Call Proportion"),
                   br(),
                   
                   plotOutput("rqcCycleBasecall"),
                   
                   br(),
                   
                   plotOutput("rqcCycleBasecallLine")
                 ),
                 
                 
                 ## Boxplot Read Quality
                 # tabPanel(
                 #   "Cycle-specific Quality Distribution - Boxplot",
                 #   shiny::tags$h3("Cycle-specific Quality Distribution - Boxplot"),
                 #   br(),
                 #   
                 #   
                 # ),
                 
                 ## Average Read Quality
                 # tabPanel(
                 #   "Average Read Quality",
                 #   shiny::tags$h3("Average Read Quality"),
                 #   br(),
                 #   
                 #   
                 # ),
                 
                 ## Read Frequency
                 tabPanel(
                   "Read Frequency",
                   shiny::tags$h3("Frequency of Reads"),
                   br(),
                   
                   plotOutput("rqcReadFrequency")
                 ),
                 
                 ## Read Width
                 tabPanel(
                   "Read Width",
                   shiny::tags$h3("Width of sequenced Reads in the Samples"),
                   br(),
                   
                   plotOutput("rqcWidth")
                 ),
        
        br()
        )
        )
        ),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
) # close tab
