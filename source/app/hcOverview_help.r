# help file
# save as hcOverview_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#hcOverview"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="hcOverview", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              shiny::tags$h3("How can I find overlapping hit candidates?", class="text-success"),
                                                                              shiny::tags$p(class="lead", "Select the analysis methods to see overlapping hit candidates"),
                                                                              shiny::tags$p(class="text-justify", "In case you select an analysis method using the check boxes below, CRISPRAnalyzeR includes its results to show overlapping hit candidates.",
                                                                                            "Genes are only considered as overlapping candidates between the methods, if they are below the p-value threshold of each method that you have set in the analysis settings.",
                                                                                            "If there are no overlapping genes, reduce the number of included analysis methods and check the individial hit calling candidates of each analysis method."),
                                                                              
                                                                              column(width=6,
                                                                                     shiny::tags$h4("Gene List Table"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "You can select which analysis algorithms you would like to compare - and whether you would like to have a look at enriched or depleted hit candidates. 
                                                                                                   Remember that enriched means the gene showed a positive fold change between your treated and untreated group, and depleted indicates the exact opposite (a negative fold change). 
                                                                                                   You can select and de-select methods and by this, get the common hit candidates among them.")
                                                                                     
                                                                                     
                                                                              ),
                                                                              column(width=6,
                                                                                     shiny::tags$h4("Venn Diagram"),
                                                                                     
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "A Venn diagram provides you the information, how many genes overlap in the different analysis algorithms.", 
                                                                                                   "The number within a shared area indicates the number of overlapping candidates between these methods.",
                                                                                                   "A detailed explanation on Venn diagrams can be found on",shiny::tags$a(href="https://en.wikipedia.org/wiki/Venn_diagram", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i><strong> Wikipedia</strong>.')) ),
                                                                                     shiny::tags$img(src="images/Venn1.png", class="img-responsive")
                                                                                     
                                                                                     )
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                       
                                                                )
                                )
                                
                                
                                ###############
                                ### END OF HELP PAGE INSERT
                                )
                                
                                
                                
                )
                
                
)



)




)
