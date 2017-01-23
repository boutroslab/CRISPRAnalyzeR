# help file
# save as hcOverview_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#idOverview"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="idOverview", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              
                                                                              shiny::tags$img(src="images/CRISPRAnalyzeR_gene_annotation.png", class="img-responsive", width="500px"),
                                                                              
                                                                              shiny::tags$p(class="lead", "Once you select a gene, CRISPRAnalyzeR will gather all information for you"),
                                                                              shiny::tags$h3("Why does CRISPRAnalyzeR reannotate sgRNAs?", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR reannotates every sgRNA within your screen using E-CRISP.",
                                                                                            "With this information, CRISPRAnalyzeR can generate additional information and plots for, e.g. the genomic model or potential genomic binding sites."),
                                                                              
                                                                              
                                                                              
                                                                              shiny::tags$h3("Gene Information", class="text-success"),
                                                                              shiny::tags$img(src="images/empowered_lg.png", class="img-responsive", width="200px"),
                                                                              shiny::tags$p(class="text-justify", "General information about the selected gene are retrieved from Ensembl, EnrichR and KEGG."
                                                                                            ),
                                                                              
                                                                              shiny::tags$h3("Gene/sgRNA Model", class="text-success"),
                                                                              shiny::tags$p(class="text-justify",
                                                                                            "CRISPRAnalyzeR creates a genomic view similar to a genome browser, which includes gene or sgRNA information."
                                                                                            ),
                                                                              shiny::tags$h3("Published Screens", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "You can check whether you gene of interest has been used in previously published CRISPR screens.",
                                                                                            "Moreover, information about observed phenotypes is presented."),
                                                                              shiny::tags$h3("Gene Peformance", class="text-success"),
                                                                              shiny::tags$p(class="text-justify",
                                                                                            "CRISPRAnalyzeR gives you a brief overview of how the selected gene performed in your screen compared to all other genes."),
                                                                              shiny::tags$h3("COSMIC Mutation Database", class="text-success"),
                                                                              shiny::tags$img(src="images/logo_cosmic.png", class="img-responsive"),
                                                                              shiny::tags$p(class="text-justify","CRISPRAnalyzeR retrieves additional information about somatic cancer mutation from the Sange COSMIC database.",
                                                                                            "Please visit the ",shiny::tags$a(href="https://cancer.sanger.ac.uk/cosmic", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i><strong>COSMIC website</strong>'))," to find more information about the COSMIC database."),
                                                                              
                                                                              shiny::tags$h3("Gene Ontology", class="text-success"),
                                                                              shiny::tags$p(class="text-jusitfy","CRISPRAnalyzeR retrieves additional information from the the Gene Ontology Consortium.",
                                                                                            "Please visit the ",shiny::tags$a(href="http://geneontology.org/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i><strong>Gene Ontology website</strong>'))," to find more information about Gene Ontology.")
                                                                              
                                                                              
                                                                              )
                                                                )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                                )
                                                
                                                
                                                
                                )
                                
                                
         )
         
         
         
         )
         
         
         
         
         )
