# help file
# save as sqReplicates_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#sqreplicates"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqreplicates", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=6,
                                                                              shiny::tags$h3("Overview", class="text-success"),
                                                                              shiny::tags$h4(
                                                                                shiny::tags$span(class='label label-primary', "A fast overview")
                                                                              ),
                                                                              shiny::tags$p(class="text-justify",
"Here you will find a boxplot visualization of the log2 Foldchanges or Z-Ratios between all uploaded samples on both gene and sgRNA level.",
"This gives you a fast overview to check for outlying samples, that might have an unexpected influence on the analysis of your data.")
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Pairwise Comparisons", class="text-success"),
                                                                              shiny::tags$h4(
                                                                                shiny::tags$span(class='label label-primary', "For a more detailed view")
                                                                              ),
                                                                              shiny::tags$p(class="text-justify", 
                                                                                            "These plots allow you to directly compare two samples by both log2-Foldchange and Z-Ratio.",
                                                                                            "With this you can inspect how foldchanges differ between your screning replicates."
                                                                                            )
                                                                              
                                                                       )
                                                                ),
                                                                column(width=12,
                                                                       column(width=6,
                                                                              shiny::tags$h3("Scatterplot Matrices", class="text-success"),
                                                                              shiny::tags$h4(
                                                                                shiny::tags$span(class='label label-primary', "A fast overview")
                                                                              ),
                                                                              shiny::tags$p(class="text-justify",
                                                                                            "The grid gives you a fast overview of the correlation of your replicates. You can view the correlation for both sgRNA and Gene read count.
                                                                                            CRISPRAnalyzeR calculates the Pearson as well as the Spearman coefficient.
                                                                                            For a more detailed view, have a look at the compare datasets tab.")
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Scatterplots", class="text-success"),
                                                                              shiny::tags$h4(
                                                                                shiny::tags$span(class='label label-primary', "For a more detailed view")
                                                                              ),
                                                                              shiny::tags$p(class="text-justify", 
                                                                                            "If you are interested in a more detailed view, you can select two datasets or treatment groups and compare them on either gene or sgRNA level.
                                                                                            You can highlight positive or non-targeting controls which will shown in red or blue color respectively.
                                                                                            Moreover you can select multiple genes which will be highlighted in orange color within the scatterplot.
                                                                                            For gene-based levels, please tick the 'Show the plot with gene level read count'. Moreover, you can get all scatter plots with log10-transformed data."
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
