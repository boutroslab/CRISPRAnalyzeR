# help file
# save as sqHeatmap_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#sqHeatmap"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqHeatmap", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=8, offset=2,
                                                                       column(width=12,
                                                                              shiny::tags$h3("What Heatmap Visualizations can I use?", class="text-success"),
                                                                              shiny::tags$p(class="lead", "CRISPRAnalyzeR provides you different settings to adjust the heatmap visualization."),
                                                                              
                                                                              shiny::tags$p(class="text-justify",
                                                                                            "CRISPRAnalyzeR offers you six different types of heatmaps visualization. In addition, you can use K-Means clustering within selected heatmap types.
                                                                                            Heatmaps allow you to look at your data in a structured way in order to check how your screen performed.")
                                                                              ),
                                                                       column(width=12,
                                                                         shiny::tags$dl(class="dl-horizontal",
                                                                                        shiny::tags$dt("Gene Abundance"),
                                                                                        shiny::tags$dd(class="text-left","The gene abundance heatmap takes the read count of all sgRNAs for a given gene and calculates its fraction (in %) of the whole dataset."),
                                                                                        shiny::tags$dt("Gene Read Count"),
                                                                                        shiny::tags$dd(class="text-left","With this type, the normalized read count for all sgRNAs of a given gene is summed up and visualized."),
                                                                                        shiny::tags$dt("Gene Top/Low"),
                                                                                        shiny::tags$dd(class="text-left","Describes you how many sgRNAs for each gene where among the top or lowest 5 % of the dataset. This can be used to find potentially enriched or depleted genes."),
                                                                                        #shiny::tags$dt("Missing Genes"),
                                                                                        #shiny::tags$dd(class="text-left","Every gene for which not a single sgRNA in the dataset shows a read count above 0 will be highlighted in blue color."),
                                                                                        shiny::tags$dt("sgRNA Abundance"),
                                                                                        shiny::tags$dd(class="text-left","The sgRNA abundance heatmap shows you the percentage of read counts wihtin the dataset accounted for each sgRNA."),
                                                                                        shiny::tags$dt("sgRNA Read Count"),
                                                                                        shiny::tags$dd(class="text-left","This type describes the normalized read counts for each sgRNA in the dataset.")
                                                                                        ),
                                                                       
                                                                        shiny::tags$h4(shiny::tags$span(class="label label-danger", "Did you know?")),
                                                                        shiny::tags$p("You can zoom into the heatmap to inspect interesting regions more closely. In addition, you can save an image of the current view using the upper right menu.")
                                                                       )
                                                                )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                )
                                                
                                                
                                                
                                                )
                                
                                
                )
                
                
                
                )
         
         
         
         
)
