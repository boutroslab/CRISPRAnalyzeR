# help file
# save as idSgrna_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#idSgrna"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="idSgrna", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              
                                                                              shiny::tags$p(class="lead", "The sgRNA section of the In-Depth Analysis allows you to have a closer look the performance and properties of individual sgRNAs targeting the selected gene."),
                                                                              
                                                                              column(width=6,
                                                                                     
                                                                                     
                                                                                     shiny::tags$h3("Readcount", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "After you have selected a gene on the left, you can check the read counts for each sgRNA targeting the selected gene.",
                                                                                                   "Moreover, you can select to get the normalized or non-normalized, raw read counts plotted.",
                                                                                                   "Remember that you can zoom into the plot as well as select/deselect the samples by clicking on their names in the legend below the plot."
                                                                                                   ),
                                                                                     shiny::tags$h3("Log2 Foldchange", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "This shows the fold changes of individual sgRNAs between your treatment groups in a log2-transformed visualization.",
                                                                                                   "You can sort the sgRNAs according to the log2 fold change."
                                                                                                   ),
                                                                                     shiny::tags$h3("Genomic Binding Sites", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "CRISPRAnalyzeR uses the re-evaluation feature of E-CRISP.org to reannotate your sgRNAs. This includes the analysis of predicted genomic binding sites, for which each sgRNAs is checked for potential genomic binding sites with a maximum of two mismatches and ignoring the first nucleotide base."
                                                                                     ),
                                                                                     shiny::tags$h3("Z-Score", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "The Z-Score or standard score is a z-transformation according to ", shiny::tags$a(href="https://en.wikipedia.org/wiki/Standard_score", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>this calculation</strong>.'))
                                                                                     ),
                                                                                     shiny::tags$h3("Efficiency Scores", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "E-CRISP.org also provides some external efficiency scores. For more information, please visit the ", shiny::tags$a(href="http://www.e-crisp.org/E-CRISP/aboutpage.html", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>E-CRISP website</strong>.'))
                                                                                     ),
                                                                                     shiny::tags$dl(class="dl-horizontal",
                                                                                                    shiny::tags$dt("Seed GC %"),
                                                                                                    shiny::tags$dd(class="text-left","The GC content in per cent of the 8 basepairs proximal to the PAM sequence"),
                                                                                                    shiny::tags$dt("Doench"),
                                                                                                    shiny::tags$dd(class="text-left","Efficacy scoring as introduced by Doench et al. 2014 Nat. Biotech."),
                                                                                                    shiny::tags$dt("XU"),
                                                                                                    shiny::tags$dd(class="text-left","Efficacy scoring as introduced by Xu et al. 2015 Gen.Res.")
                                                                                     )
                                                                                     
                                                                                     ),
                                                                              column(width=6,
                                                                                     shiny::tags$h3("E-CRISP Scores", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "CRISPRAnalyzeR provides you with the scores that E-CRISP.org calculates for each sgRNA. An overview of the presented scores can be found at the ", shiny::tags$a(href="http://www.e-crisp.org/E-CRISP/aboutpage.html", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>E-CRISP website</strong>.'))
                                                                                     ),
                                                                                     shiny::tags$dl(class="dl-horizontal",
                                                                                                    shiny::tags$dt("Specificity"),
                                                                                                    shiny::tags$dd(class="text-left","Specificity Score"),
                                                                                                    shiny::tags$dt("Annotation"),
                                                                                                    shiny::tags$dd(class="text-left","Annotation Score"),
                                                                                                    shiny::tags$dt("Efficiency"),
                                                                                                    shiny::tags$dd(class="text-left","E-CRISP Efficacy Score"),
                                                                                                    shiny::tags$dt("CDS"),
                                                                                                    shiny::tags$dd(class="text-left","Coding Sequence Score"),
                                                                                                    shiny::tags$dt("Exon"),
                                                                                                    shiny::tags$dd(class="text-left","Exon Targeting Score")
                                                                                     ),
                                                                                     shiny::tags$h3("sgRNA Sequence", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "Here you can find an overview of your sgRNA target sequences, the fold changes, the Z-Score and the number of predicted targets for each sgRNA. Don't forget that you can download the table using the buttons at the upper left of the table."
                                                                                     ),
                                                                                     shiny::tags$h3("Predicted sgRNA Binding Sites", class="text-success"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "Similar to the genomic binding sites, the predicted sgRNA binding sites tells you if the sgRNA has predicted target within an annotated gene or between two genes (intergenic).",
                                                                                                   "In case a sgRNA targets the same target more than once, the predicted target is only listed once."
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
