# save as help_analyzer.r

tabItem(tabName = "help_analyzer", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## Welcome message
        fluidRow( style="width:80%;",
                  HTML("<div class='section'>
                       <div class='container'>
                       <div class='row'>
                       <div class='col-md-12'>
                       <h1 class='text-success text-left'>
                       <i class='fa fa-angle-double-right  fa-fw'></i>Help
                       </h1>
                       </div>
                       </div>
                       </div>
                       </div>
                       ")
                  ),
        
        # What is CRISPRAnalyzeR
        fluidRow(
          column(width = 10, offset=1,
                 # collapsable panel
                 shiny::tags$div(class="panel panel-default",
                                 shiny::tags$div(class="panel-heading",
                                                 shiny::tags$h3(class="panel-title", 
                                                                HTML('<a data-toggle="collapse" href="#about1"><i class="fa fa-plus fa-fw"></i> What is CRISPRAnalyzeR?</a>')
                                                 )
                                 ),
                                 shiny::tags$div(id="about1", class="panel-collapse collapse",
                                                 # source help
                                                 source(file.path(config$appDir,"help", "help_whatiscrispranalyzer.r"), echo = FALSE, verbose = FALSE)[1]
                 )
                 ),
                 
                 
                 shiny::tags$div(class="panel panel-default",
                                 shiny::tags$div(class="panel-heading",
                                                 shiny::tags$h3(class="panel-title", 
                                                                HTML('<a data-toggle="collapse" href="#about2"><i class="fa fa-plus fa-fw"></i> How to use CRISPRAnalyzeR?</a>')
                                                 )
                                 ),
                                 
                                 shiny::tags$div(id="about2", class="panel-collapse collapse",
                                                 # Source help How to use
                                                 source(file.path(config$appDir,"help", "help_howtouse.r"), echo = FALSE, verbose = FALSE)[1]
                                                 
                                                 )
                 )
                 ) # end columns
                  ), # end fluidrow
        
        
        
        # Can I use CRISPRAnalyzeR in a local installation?
        
        # What data does CRISPRAnalyzeR store?
        
        
        # load footer  
        source(file.path(config$appDir, "footer.r"), echo = FALSE, verbose = FALSE)[1]
        
        )
