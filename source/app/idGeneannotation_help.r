# help file
# save as idGeneannotation_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#idGeneannotation"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="idGeneannotation", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              
                                                                              shiny::tags$p(class="lead", "Select multiple genes as a variety of external information to annotate your genes of interest."),
                                                                              
                                                                              
                                                                              
                                                                              # image what is shown
                                                                              # Figure what crispranalyzer will do (Genes / Annotations -> ask Ensembl -> show the user)
                                                                              #shiny::tags$img(src="images/", class="img-responsive", width="60%"),
                                                                              column(width=6,
                                                                                     
                                                                                     
                                                                                     shiny::tags$h3("Selecting Genes", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "You can select multiple genes for which you would like to retrieve additional information. Just start typing the gene identifier and CRISPRAnalyzeR will show you the matching genes of your screen."
                                                                                     )
                                                                                     
                                                                              ),
                                                                              column(width=6,
                                                                                     shiny::tags$h3("How many Annotations can I add?", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "CRISPRAnalyzeR allows you to enter up to 20 annotation features. Just start typing and CRISPRAnalzyeR will tell you which annotations match your input."
                                                                                     )
                                                                                     
                                                                              )
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h3(class="text-success", "Which Annotations are available?"),
                                                                              shiny::tags$p(class="lead", "CRISPRAnalyzeR offers you all annotations available via the Ensembl biomaRt service"),
                                                                              shiny::tags$p(class="justify", "just start typing what you would like to know and CRISPRAnalyzeR will show you mathcing annotations directly. Moreover you will find a list of all available annotations below."),
                                                                              
                                                                              # output of list of annotatiopns (do an output that constructs table with 3 cols)
                                                                              shiny::uiOutput("listavailable_annotations")
                                                                              )
                                                                )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                )
                                )
                )
         )
)
