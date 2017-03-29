# sourced by 'ui.r'
# save as 'tutorials_ui.r'
# ui elements for help tab




tabItem(tabName = "help_tutorials", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## CSS
        #tags$head(
        #  tags$style(HTML('#help_submit{background-color:#4682B4; font-weight:bold; color:white}'))
        #),
        
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Help&nbsp;
                      <small>&nbsp;Available Tutorials</small>
                      </h1>
                      <hr>
                      <p class='lead'>Below you find tutorials on how to use CRISPRAnalyzeR or for specific settings you can adjust.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
                 ),
        
        fluidRow(
          column(width=10, offset=1,
                 
                 shiny::tags$h2("Available Video Tutorials"),
                 shiny::tags$h4("All tutorials can also be found on ", shiny::tags$a(href="https://www.youtube.com/channel/UCiR7LwZ-8l_-qdZRWsuOmNA", target="_blank", "Youtube ", icon("play-circle"))),
                 
                 # Add tutorial emebeddings from youtube here
                 column(width=12,
                 shiny::tags$h2(class="text-success", "CRISPRAnalyzeR Usage"),
                 
                 shiny::tags$br(),
                 # Short video here
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to upload FASTQ.gz data"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/Km5nA2YPDxY"
                                        )
                        )
                 ),
                 shiny::tags$br(),
                 
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to find the right regular expression for FASTQ data"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/C4N0o83ryvs"
                                        )
                        )
                 ),
                 shiny::tags$br(),
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to upload read count data"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/LoanJsm8Rk"
                                        )
                        )
                 ),
                 shiny::tags$br(),
                 
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to set the analysis parameters"),
                        shiny::helpText("Not available yet.")
                        ),
                 shiny::tags$br(),
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to assess the sequencing and screening quality"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/J2WJFAo2OTY"
                                        )
                        )
                 ),
                 shiny::tags$br(),
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to upload read count data and set the analysis parameters"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/zIC8OZBX_5U"
                                        )
                        )
                 ),
                 shiny::tags$br(),
                 column(width=6, offset=3,
                 shiny::tags$h4(class="text-sucess", "How to find candidate genes in the Hit Calling section"),
                 shiny::helpText("Not available yet.")
                 ),
                 shiny::tags$br(),
                 column(width=6, offset=3, 
                        shiny::tags$h4(class="text-sucess", "How to annotate genes in the Hit Confirmation section"),
                        shiny::helpText("Not available yet.")
                        ),
                 shiny::tags$br(),
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to compare effects among multiple genes"),
                        shiny::helpText("Not available yet.")
                        ),
                 shiny::tags$br(),
                 column(width=6, offset=3, 
                        shiny::tags$h4(class="text-sucess", "How to set up and generate the interactive report"),
                        shiny::helpText("Not available yet.")
                        ),
                 shiny::tags$br()
                 ),
                 
                 column(width=12,
                 shiny::tags$br(),
                 shiny::tags$h2(class="text-success", "Installation"),
                 
                 shiny::tags$br(),
                 column(width=6, offset=3,
                        shiny::tags$h4(class="text-sucess", "How to setup and install CRISPRAnalyzeR on macOS"),
                        shiny::tags$div(class="embed-responsive embed-responsive-16by9",
                                        shiny::tags$iframe(class="embed-responsive-item",
                                                           src="https://www.youtube.com/embed/IFPojCjW0ns"
                                        )
                        )
                 ),
                 shiny::tags$br()
                 
                 )
                 )
        ),
        
        
        shiny::tags$br(),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
                 )# close tab

