# sourced by 'ui.r'
# save as 'essentials_ui.r'
# ui elements for data tab




tabItem(tabName = "hc_essentials", align = "center",
        
        shinyjs::useShinyjs(),
        
        
        
        ## Welcome message
        fluidRow(style="width:80%",
                 
                 
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Hit Calling
                      <small>&nbsp;Essential Genes</small>
                      </h1>
                      <hr>
                      <p class='lead'>Compare your genes against the DAISY Core Essentials and known essential genes of various cell lines according to GenomeCRISPR.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 ), # End of fluidrow
        
        
        # HELP as including of enrichmenthelp.r
        #source(file.path(config$appDir, "enrichment_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        fluidRow(style="margin-top:40px;",
                 
                 column(width=6,
                        shiny::tags$h3(class="text-success", "DAISY Core Essentials"),
                        shiny::tags$p(class="text", "The DIASY core essential genes are derived from the publication by ", HTML("<a href='http://www.cell.com/action/showImagesData?pii=S0092-8674%2815%2901495-6' target='_blank'>Traver Hart et. al</a>"),shiny::tags$br(), "Hart,T. et al. (2015) Systematic discovery and classification of human cell line essential genes Cold Spring Harbor Labs Journals."),
                        
                        # DAISY
                        highchartOutput(outputId = "essentials_daisy_distribution"),
                        
                        shiny::tags$br(),
                        shiny::tags$hr(),
                        DT::dataTableOutput(outputId = "essentials_DT_DAISY")
                        
                 ),
                 column(width=6,
                        column(width=12,
                               # Selected Cell Lines
                               shiny::tags$h3(class="text-success", "GenomeCRISPR Cell Line Essentials"),
                               shinydashboard::box(status = "primary", solidHeader = FALSE, width = 12,
                                                   uiOutput(outputId = "essentials_cellline")
                               )
                        ),
                        column(width=12,

                               highchartOutput(outputId = "essentials_gcrispr_distribution"),
                               shiny::tags$br(),
                               shiny::tags$hr(),
                               DT::dataTableOutput(outputId = "essentials_DT_GCRISPR")
                               
                          )
                        
                        )
        ),
        
        
        shiny::tags$br(),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
                 )