# help file
# save as setup_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#groups"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="groups", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=8, offset=2,
                                                                              shiny::tags$h2("Why do I need to set groups?", class="text-success"),
                                                                              # IMAGE
                                                                              shiny::tags$p(class="lead", "CRISPRAnalyzeR performs hit analysis by comparing two groups, whith each group containing the corresponding samples."),
                                                                              shiny::tags$p(class="text-justify", "All hit analysis methods that are implemented in CRISPRAnalyzeR require the use of two groups that are compared.
                                                                                            This means, you need to assign each sample to either one of the groups you would like to compare.",
                                                                                            "As an example, this could be a Day0 vs a Day12 group or a Control vs a Drug Treatment group.")
                                                                              ),
                                                                       
                                                                       column(width=6,
                                                                              shiny::tags$h3("How can I setup my treatment groups?", class="text-success"),
                                                                              shiny::tags$p(class="lead", "CRISPRAnalyzeR will always compare two treatment groups"),
                                                                              shiny::tags$p(class="text-justify","You can increase the number of groups, but the minimum required number is 2. If you like you can also name each group.")
                                                                             
                                                                              
                                                                       ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("How can I assign my samples to the groups?", class="text-success"),
                                                                              # IMAGE
                                                                              shiny::tags$p(class="text-justify", "You can assign each file to one group by clicking on the sample name. If you like to have more than one file per group, keep the CMD or CTRL button pushed while selecting the samples.",
                                                                                            HTML("<strong>Please note that your are not allowed to use the same sample in more than one treatment group.</strong>")
                                                                                            ),
                                                                              shiny::tags$img(src="./images/CRISPRAnalyzeR_groups3.png", class="img-responsive")
                                                                            
                                                                              
                                                                              
                                                                              ),
                                                                       # column(width=12,
                                                                       #        #shiny::tags$img(src="images/CRISPRAnalyzeR_data-review_1.png", class="img-responsive")
                                                                       # ),
                                                                       column(width=12,
                                                                              shiny::tags$hr() ),
                                                                       column(width=12,
                                                                              shiny::tags$h3(class="text-sucess", "Why do I need to set gene identifiers?"),
                                                                              # image with gene ID conversion/enrichment
                                                                              shiny::tags$p(class="lead",
                                                                                            "CRISPRAnalyzeR needs to know the type of gene identifier used for the screen - and can automatically convert it for you"),
                                                                              shiny::tags$p(class="text-justify", width="70%", "CRISPRAnalyzeR needs to know in which organism your screen was performed and which gene identifier is used in the sgRNA library file.",
                                                                                            "With this information, CRISPRAnalyzeR can offer you extensive gene annotation and visualizations."
                                                                                            ),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR expects the type of organism, the gene identifier and which identifier you would like to convert it to."),
                                                                              shiny::tags$strong("If you do not want to convert your gene identifier, please select the same identifer twice."),
                                                                              shiny::tags$br(),
                                                                              shiny::tags$h4(class="text-primary", "Supported Gene Identifiers"),
                                                                              shiny::tags$dl(class="dl-horizontal",
                                                                                             shiny::tags$dt("Ensembl Gene ID"),
                                                                                             shiny::tags$dd("e.g. ENSG00000141510", class="text-left"),
                                                                                             shiny::tags$dt("EntrezGene ID (also NCBI Gene ID)"),
                                                                                             shiny::tags$dd("e.g. 7157", class="text-left"),
                                                                                             shiny::tags$dt("HGNC ID"),
                                                                                             shiny::tags$dd("e.g. 11998", class="text-left"),
                                                                                             shiny::tags$dt("HGNC symbol"),
                                                                                             shiny::tags$dd("e.g. TP53", class="text-left"),
                                                                                             shiny::tags$dt("Unigene ID"),
                                                                                             shiny::tags$dd("e.g. 7157", class="text-left"),
                                                                                             shiny::tags$dt("Uniprot Gene Name"),
                                                                                             shiny::tags$dd("e.g. TP53", class="text-left")
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
