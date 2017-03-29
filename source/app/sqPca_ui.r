# saved as sqPca_ui.r
# uses sqPca_server.r for calculations

tabItem(tabName = "sq_pca", align = "center",
        
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
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Principal Component Analysis</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>A principal component analysis can give you first idea how similar or differential your samples are and which of the samples you provided nicely group together.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 
                 ),
        
        # HELP as including of sqPCA_help.r
        source(file.path(config$appDir, "sqPCA_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        # Tabset Panel
        fluidRow(
          column(width=6, offset=3,
          class="alert alert-info",
                     shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                     shiny::tags$span(
                       shiny::tags$strong("Please note:", HTML("</br>"),
                                     "You can download the plot using the upper right menu. Moreover, you can zoom into the plot for a more detailed view.")
                     )
              ),
          column(width=10, offset=1,
                 box(width=12, title = "Principal Component Analysis", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "primary",
              shiny::tags$br(),
              highcharter::highchartOutput("pcaplotgene", height = "700px", width = "700px")
                 
          )),
          shiny::tags$br(),
          shiny::tags$br()
        ),
        
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
        ) # close tab
