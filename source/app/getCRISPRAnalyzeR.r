# sourced by 'ui.r'
# save as 'getCRISPRAnalyzeR.r'
# ui elements for downloading CRISPRAnalyzeR




tabItem(tabName = "getCRISPRAnalyzeR", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## Welcome message
        
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Download CRISPRAnalyzeR&nbsp;
                      <small>Download the source code or a Docker container</small>
                      </h1>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")),
        shiny::tags$hr(width="85%"),
        
        column(width=10, offset=1,
               
               shiny::tags$img(src="./images/CRISPRAnalyzR_logo5.png", class="img-responsive", width="70%"),
               
               #shiny::tags$hr(width="50%", style="padding-bottom:60px"),
               shiny::tags$br(),
               shiny::tags$div(width=12,
                                  
                 
                # # Version
                # shiny::tags$h4("Installed Version"),
                shinydashboard::infoBoxOutput("version"),
                 
                 # ## Database
                 # 
                 # # COSMIC
                 # shiny::tags$h4("COSMIC Mutation Database"),
                 shinydashboard::infoBoxOutput("cosmic_active"),
                 
                 # # Enrichr
                 # shiny::tags$h4("Enrichr Web Service"),
                 shinydashboard::infoBoxOutput("enrichr_active"),
                 
                 # # re-evaluation databases
                 shinydashboard::infoBoxOutput("reevaluation_active"),
                 # shiny::tags$h4("sgRNA Re-Evaluation"),
                 
                 # ## Internet Access
                 # shiny::tags$h4("Internet Access"),
                 shinydashboard::infoBoxOutput("internet_access"),
                 
                 ## Proxy
                 shinydashboard::infoBoxOutput("proxy")
                 # 
                 
               ),
               
               shiny::tags$hr(width="50%", style="padding-bottom:60px"),
               
               column(width=6,
                      shiny::tags$h3(class="text-success", "Download CRISPRAnalyzeR Source Code"),
                      shiny::tags$img(src="./images/GitHub_Logo.png", class="img-responsive"),
                      shiny::tags$br(),
                      shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/", target="_blank" ,shiny::tags$button(type="button", class="btn btn-success btn-lg", shiny::icon("github", class = NULL, lib = "font-awesome"), "Visit the Github page")),
                      shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/releases", target="_blank" ,shiny::tags$button(type="button", class="btn btn-primary btn-lg", shiny::icon("download", class = NULL, lib = "font-awesome"), "Download Source Code"))
                      ),
               column(width=6,
                      shiny::tags$h3(class="text-success", "Download CRISPRAnalyzeR as ready-to-use Docker Container"),
                      shiny::tags$img(src="./images/large_h-trans.png", class="img-responsive"),
                      shiny::tags$a(href="https://github.com/boutroslab/crispr-analyzer/wiki/Installation:-How-to-download-and-install-CRISPRAnalyzeR", target="_blank" ,shiny::tags$button(type="button", class="btn btn-primary btn-lg", shiny::icon("download", class = NULL, lib = "font-awesome"), "Download Ready-to-use Docker Container"))
                      )
               
               ),
        column(width=10,offset=1,
               shiny::tags$br()
               ),
        
        
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
) # close tab



