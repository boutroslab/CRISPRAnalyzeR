# help file
# save as sqCoverage_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#sqcoverage"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqcoverage", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       
                                                                       column(width=6, style="padding:2%;",
                                                                              shiny::tags$h3("Missing sgRNAs/Genes", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "The missing sgRNA/Genes tab shows you how many sgRNAs/Genes were not present in the sequencing data compared to the uploaded sgRNA library file. 
This means the read count for this particular sgRNA or the sum of sgRNA read counts for a particular genes is 0.
If a read count threshold has been applied, removed sgRNAs are NOT listed as missing."),
                                                                              shiny::tags$dl(class="dl-horizontal",
                                                                                             shiny::tags$dt("Missing Genes"),
                                                                                             shiny::tags$dd(class="text-left","Number of Genes in which all sgRNAs were not present in the dataset compared to the provided sgRNA library."),
                                                                                             shiny::tags$dt("Missing sgRNAs"),
                                                                                             shiny::tags$dd(class="text-left","Number of sgRNAs which revealed a read count of 0 in the dataset."),
                                                                                             shiny::tags$dt("% of missing
                                                                                                            non-targeting sgRNAs"),
                                                                                             shiny::tags$dd(class="text-left","The percentage of sgRNAs, which belong to the non-targeting control, that were not present in the dataset."),
                                                                                             shiny::tags$dt("% of missing
                                                                                                            positive control sgRNAs"),
                                                                                             shiny::tags$dd(class="text-left","The percentage of sgRNAs, which belong to the positive control, that were not present in the dataset.")
                                                                                             )
                                                                              
                                                                              
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Designs per Gene", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify", "Distribution of sgRNA presence for all genes in the dataset. In case all sgRNAs for all genes are present, you will expect only one bar."),
                                                                              shiny::tags$img(class="img-responsive", src="images/CRISPRAnalyzeR_coverage_designs.png")
                                                                              )
                                                                              )
                                                                
                                                                
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                                              )
                                                
                                                
                                                
                                                )
                                
                                
                )
                
                
                
)




)
