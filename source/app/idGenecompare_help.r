# help file
# save as idGenecompare_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#idGenecompare"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="idGenecompare", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              
                                                                              shiny::tags$p(class="lead", "Compare the sgRNA populations of different genes using violin plots."),
                                                                              shiny::tags$p(class="text-center", "Find out more about violin plots at" ,shiny::tags$a(href="https://en.wikipedia.org/wiki/Violin_plot", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Wikipedia</strong>.'))),
                                                                              
                                                                              
                                                                              # image what is shown
                                                                              shiny::tags$img(src="images/CRISPRAnalyzeR_Violineplot.png", class="img-responsive", width="60%"),
                                                                              column(width=6,
                                                                                     
                                                                                     
                                                                                     shiny::tags$h3("Readcount Untreated Group", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "This violin plots shows the normalized read counts of each sgRNA for the selected genes within the samples marked as the untreated group. The width of the violine gives you an impression of the sgRNA population density."
                                                                                     ),
                                                                                     shiny::tags$h3("Readcount Treated Group", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "This violin plot shows the normalized read counts of all sgRNAs for the selected genes within the samples that have been set as part of the treated group."
                                                                                     ),
                                                                                     shiny::tags$h3("Log2 Foldchange", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "Shows the log2-transformed fold change of all sgRNAs for the selected genes between your treated and untreated group."
                                                                                     )
                                                                                    
                                                                                     
                                                                              ),
                                                                              column(width=6,
                                                                                     shiny::tags$h3("Z-Score", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "Shows the z-score of sgRNAs for the selected genes."
                                                                                     ),
                                                                                     
                                                                                     shiny::tags$h3("sgRNA Binding Sites", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "Displays the number of predicted genomic binding sites of all sgRNAs for the selected genes. Genomic binding sites are predicted using E-CRISP.org with ignoring the first nucleotide and allowing up to two mismatches in total."
                                                                                     )
                                                                                    
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
