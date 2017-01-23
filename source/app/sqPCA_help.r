# help file
# save as sqPCA_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#sqPCA"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="sqPCA", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=8, offset=2,
                                                                              shiny::tags$h3("What is Principal Component Analysis", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify",
                                                                                            "A principal component analysis is a method to reduce multi-dimensional data into two-dimensional projection by keeping the information about the variance in the dataset.
                                                                                            It tries to identify directions, for which the variation is maximum. These directions are called principal components."),
                                                                              shiny::tags$strong("Please check out the following article, in which PCA on genomic data is nicely explained."),
                                                                              HTML("<a href='http://www.nature.com/nbt/journal/v26/n3/full/nbt0308-303.html' target='_blank'><i class='fa fa-external-link  fa-fw'></i>Nature Computational Biology</a>")
                                                                              )
                                                                      
                                                                              )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                                )
                                                
                                                
                                                
                                )
                                
                                
         )
         
         
         
)




         )
