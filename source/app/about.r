# Save as about.r


tabItem(tabName = "aboutcar", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## Welcome message
        fluidRow( style="width:80%;",
                  HTML("<div class='section'>
      <div class='container'>
                       <div class='row'>
                       <div class='col-md-12'>
                       <h1 class='text-success text-left'>
                       <i class='fa fa-angle-double-right  fa-fw'></i>About CRISPRAnalyzeR
                       </h1>
                       </div>
                       </div>
                       </div>
                       </div>
                       ")
                  ),
        

                   
        
        
        # Can I use CRISPRAnalyzeR in a local installation?
        
        # What data does CRISPRAnalyzeR store?
        
        
        # load footer  
        source(file.path(config$appDir, "footer.r"), echo = FALSE, verbose = FALSE)[1]
        
)
