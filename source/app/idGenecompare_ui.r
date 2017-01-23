# sourced by 'ui.r'
# save as 'idSgrna_ui.r'
# ui elements for "In-Depth Analysis" tab "sgRNAs" sub tab




tabItem(tabName = "id_genecompare", align = "center",
        
        shinyjs::useShinyjs(),
        
        
        ## Welcome message
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Hit Confirmation
                      <font color='#777777'>&nbsp;
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Gene Comparison</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>It is nice to know about possible hit candidates, but how about the sgRNAs?
                      <br>This page provides you with a more in-depth view on the sgRNAs which you
                      used to target your favourite genes.</p>
                      </div>
                      </div>
                      </div>
                      </div>
                      ")
                 ),    
        # HELP as including of idGenecompare_help.r
        source(file.path(config$appDir, "idGenecompare_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        fluidRow(style="align:center",
                 column(width=6, offset=3,
                        # submit box
                        box(title = "Compare Genes",width = 12,solidHeader = FALSE,color = "grey",
                            uiOutput("info_error2"),
                            uiOutput("hit_select_violine"),
                            helpText("Select the genes you wish to compare."), shiny::tags$br(),
                            actionButton("addReport_compare", "Add to Report")
                        )
                 )
                 
        ), # FluidRow  
        
        fluidRow(
          column(width=10,offset=1,
          # tabsetbox with plots
          tabBox(
            title = NULL,
            width = 12,
            tabPanel(paste("Readcount","Untreated Group", sep=" "),
                     shiny::tags$div(width="100%",
                     shiny::tags$p(class="lead","Compare the sgRNA read counts of multiple genes in your untreated group"),
                     
                     h3(paste("Readcount","Untreated Group", sep=" ")),
                     plotOutput("idvioline_readcountuntreated", width="100%", height="800px")
                     )
            ),
            tabPanel(paste("Readcount","Treated Group", sep=" "),
                     shiny::tags$div(width="100%",
                     shiny::tags$p(class="lead","Compare the sgRNA read counts of multiple genes in your treated group"),
                     
                     h3(paste("Readcount","Treated Group", sep=" ")),
                     plotOutput("idvioline_readcounttreated", width="100%", height="800px")
                     )
            ),
            
            tabPanel("Log2 Foldchange",
                     shiny::tags$div(width="100%",
                     shiny::tags$p(class="lead","Compare the log2-foldchange of sgRNAs of multiple genes"),
                     
                     h3("Log2 Foldchange"),
                     
                     plotOutput("idvioline_foldchange", width="100%", height="800px")
                     )
            ),
            tabPanel("Z-Score",
                     shiny::tags$div(width="100%",
                     shiny::tags$p(class="lead","Compare the Z-Scores of sgRNAs of multiple genes"),
                     
                     h3("Z-Score"),
                     plotOutput("idvioline_zscorefoldchange", width="100%", height="800px")
                     )
            ),
            tabPanel("sgRNA binding sites",
                     shiny::tags$div(width="100%",
                     shiny::tags$p(class="lead","Compare the number of predicted binding sites of individual sgRNAs among multiple genes"),
                     
                     h3("Predicted binding sites"),
                     plotOutput("idvioline_bindingsites", width="100%", height="800px")
                     )
            )
            
            

            
          )
          )
          
          
        ),
        
        
        shiny::tags$br(),
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
        
) # close tab










