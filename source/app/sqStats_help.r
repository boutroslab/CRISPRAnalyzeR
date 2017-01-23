# help file
# save as sqStats_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#sqstats"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqstats", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       
                                                                       column(width=6, style="padding:2%;",
                                                                              shiny::tags$h3("Overview", class="text-success"),
                                                                              shiny::tags$dl(class="dl-horizontal",
                                                                                             shiny::tags$dt("Mean"),
                                                                                             shiny::tags$dd(class="text-left","The mean read count of all sgRNAs/Genes in the dataset"),
                                                                                             shiny::tags$dt("Median"),
                                                                                             shiny::tags$dd(class="text-left","The median read count of all sgRNAs/Genes in the dataset"),
                                                                                             shiny::tags$dt("Min"),
                                                                                             shiny::tags$dd(class="text-left","The minimum read count of all sgRNAs/Genes in the dataset"),
                                                                                             shiny::tags$dt("Max"),
                                                                                             shiny::tags$dd(class="text-left","The maximum read count of all sgRNAs/Genes in the dataset"),
                                                                                             shiny::tags$dt("SD"),
                                                                                             shiny::tags$dd(class="text-left","The calculated Standard Deviation of all sgRNA/Gene read count in the dataset"),
                                                                                             shiny::tags$dt("Missing"),
                                                                                             shiny::tags$dd(class="text-left","The number of sgRNAs/Genes which have a read count of 0. In case the uploaded
                                                                              sgRNA FASTA library contains more items than the screening data, these sgRNAs/Genes will reported as missing.")
                                                                                             )
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Readcounts", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify", "The Readcounts tab allows you to browse through your readcount data. You can also sort according to the different columns and search for specific
                                                                                            genes or sgRNAs.
                                                                                            On the left you can select whether you wish to browse readcounts per sgRNA
                                                                                            or get a summed-up readcount per gene.")
                                                                       )
                                                                       ),
                                                                column(width=12,
                                                                       
                                                                       column(width=6,
                                                                              shiny::tags$h3("Distribution", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "Described the log2 read count distribution for all sgRNAs of each dataset. "),
                                                                              shiny::tags$h4(
                                                                                shiny::tags$span(class='label label-danger',"Did you know?")
                                                                                ),
                                                                                shiny::tags$p(class="lead","You can zoom into the plot by keeping the left-mouse button pushed. Moreover, you can select/deselect any dataset by clicking on the dataset name below the plot.")
                                                                              
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Controls", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify", "If you have set either positive or non-targeting controls in the Set Analysis Parameter section, read statistics for these controls are shown.")
                                                                              )
                                                                       ),
                                                                column(width=12,
                                                                       column(width=6#,
                                                                              #shiny::tags$h3("Normalized Overview", class="text-success"),
                                                                              #shiny::tags$p(class="text-justify", "A boxplot representation of log2-transformed read counts for each dataset. Reads considered as population outliers are highlighted in blue color.")
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Read Depth", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "Shows the read count of each gene divided by the number of sgRNAs that contributed to it. This is used to give you an idea of read count depth normalized to the number of sgRNAs.
                                                                                            The Read Depth plot shows you how many reads fall onto every gene present in your dataset. Positive controls (if set) are highlighted in red, non-targeting control (if set) are highlighted in blue color.")
                                                                              ),
                                                                       shiny::tags$h4(
                                                                         shiny::tags$span(class='label label-danger',"Did you know?")
                                                                         ),
                                                                         shiny::tags$p(class="lead","You can zoom into this plot for a closer inspection.")
                                                                         
                                                                       )
                                                                
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                                )
                                                
                                                
                                                
                                                )
                                
                                
         )
         
         
         
)




)
