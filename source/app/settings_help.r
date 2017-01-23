# help file
# save as settings_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#settings"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="settings", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=8, offset=2,
                                                                              shiny::tags$h3("How do I setup screening controls?", class="text-success"),
                                                                              # IMAGE
                                                                              shiny::tags$p(class="lead", "You can set positive and non-targeting controls which will help you to judge your screen performance")
                                                                              
                                                                       ),
                                                                       
                                                                       column(width=6,
                                                                              shiny::tags$h4("What are positive controls?", class="text-success"),
                                                                              shiny::tags$p(class="text-justify","Positive controls are genes, for which the screening outcome is known.",
                                                                                            "These will tell you whether your screening setup worked and are very important to judge your screening performance."),
                                                                              shiny::tags$p(class="text-justify","Which gene can be used as a positive controls strongly depends on the type of screen.",
                                                                                            "For Viability/Dropout screen, essential genes can be used as positive control.")
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=6,
                                                                              shiny::tags$h4("What are non-targeting contols?", class="text-success"),
                                                                              # IMAGE
                                                                              shiny::tags$p(class="text-justify", "Non-targeting controls are articicial genes, which are not present in the screened organism.",
                                                                                            "Since these controls don't have any target in the screening organism, they can be used to judge the screening variability.",
                                                                                            "Common non-targeting controls are GFP or random sgRNA sequences.")
                                                                              )
                                                                              
                                                                              
                                                                              
                                                                       ),
                                                                       # column(width=12,
                                                                       #        #shiny::tags$img(src="images/CRISPRAnalyzeR_data-review_1.png", class="img-responsive")
                                                                       # ),
                                                                       column(width=12,
                                                                              shiny::tags$hr() ),
                                                                       
                                                                column(width=12,
                                                                       shiny::tags$h3("Adjust thresholds used for hit identification", class="text-success"),
                                                                       shiny::tags$p(class="lead", "CRISPRAnalyzeR uses different algorithms to analyze your data"),
                                                                       shiny::tags$p(class="text-justify", "For each algorithm, you can set a p-value threshold that is used to discriminate between significant and non-significant hit candidates."
                                                                                     ),
                                                                       shiny::tags$br(),
                                                                       # methods
                                                                       shiny::tags$dl(class="dl-horizontal",
                                                                                      shiny::tags$dt("Wilcox"),
                                                                                      shiny::tags$dd(class="text-left", HTML("<span class='badge'>Analysis on sgRNA read count</span></br>"),
                                                                                                     "Wilcoxon analysis is based on a Mann-Whitney test between a random/non-targeting sgRNA set and all sgRNAs for each targeted gene.",
                                                                                                     "If no non-targeting controls have been set, randomly picked sgRNAs are used as test reference instead."),
                                                                                      shiny::tags$dt("DESeq2"),
                                                                                      shiny::tags$dd( class="text-left", HTML("<span class='badge'>Analysis on Gene read count</span></br>"), 
                                                                                                      "For this analysis, the DESeq2 package is employed on gene-level read count to find potential hit candidates between the treatment groups."
                                                                                                      ),
                                                                                      shiny::tags$dt("MAGeCK"),
                                                                                      shiny::tags$dd(class="text-left", HTML("<span class='badge'>Analysis on sgRNA read count</span></br>"),
                                                                                                     "MAGeCK analysis is performed on sgRNA readcount comparing the selected treatment groups.",
                                                                                                     "For this, MAGeCK is implemented as described ", HTML("<a href='https://sourceforge.net/p/mageck/wiki/Home/' target='_blank'><i class='fa fa-external-link fa-fw text-success '></i> on the MAGeCK website</a>")),
                                                                                      shiny::tags$dt("edgeR"),
                                                                                      shiny::tags$dd(class="text-left",HTML("<span class='badge'>Analysis on sgRNA read count</span></br>"),
                                                                                                     "EdgeR hit analysis is performed on sgRNA read counts followed by a gene enrichment analysis.",
                                                                                                     "EdgeR has been implemented as described" , HTML("<a href='http://bioinf.wehi.edu.au/shRNAseq/' target='_blank'><i class='fa fa-external-link fa-fw text-success '></i> here</a>"),
                                                                                                     HTML("</br>"),
                                                                                                     "Dai et al. (2014) edgeR: a versatile tool for the analysis of shRNA-seq and CRISPR-Cas9 genetic screens, F1000Research, 3:95."
                                                                                                     ),
                                                                                      shiny::tags$dt("sgRSEA"),
                                                                                      shiny::tags$dd(class="text-left", HTML("<span class='badge'>Analysis on sgRNA read count</span></br>"),
                                                                                                     "SgRSEA (single-guide RNA Set Enrichment Analysis) has been implemented as described in the corresponding R package available at ", HTML("<a href='https://cran.r-project.org/web/packages/sgRSEA/index.html' target='_blank'><i class='fa fa-external-link fa-fw text-success '></i> CRAN</a>")
                                                                                                     )
                                                                                      
                                                                          ),
                                                                       shiny::tags$hr(),
                                                                       shiny::tags$h3("Can I remove low / high read counts from the analysis?", class="text-success"),
                                                                       shiny::tags$p(class="text-justify", "If you like to remove sgRNAs with low or even high read counts from your analysis, just activate the checkbox and tell CRISPRAnalyzeR the desired threshold.
                                                                                     Each sgRNA having a read count below/above or equal to the threshold will be removed from the analysis."),
                                                                       shiny::tags$img(src="images/settings_2.png", class="img-responsive", width="70%")
                                                                       
                                                                       
                                                                       ),
                                                                column(width=12,
                                                                       shiny::tags$hr() ),
                                                                column(width=12,
                                                                       shiny::tags$h3("How can I analyze two groups?" , class="text-success"),
                                                                       shiny::tags$p(class="lead","CRISPRAnalyzeR will run the analysis between two groups"),
                                                                       shiny::tags$p(class="text-justify",
                                                                                     "You can select the two groups out of all groups you have defined on the Set Groups And Identifier page."
                                                                                     ),
                                                                       
                                                                       shiny::tags$hr(),
                                                                       shiny::tags$h3("Can I run multiple analysises?", class="text-success"),
                                                                       shiny::tags$p(class="text-justify",
                                                                                     "CRISPRAnalyzeR always runs a pairwise analysis between two groups. If you want to compare additional groups, you can always come back to the Set Analysis page and click on the Change Setting button at the bottom.
                                                                                     This allows you to change all analysis parameters and start a new analysis without uploading the data again."),
                                                                      shiny::tags$img(src="images/settings_1.png", class="img-responsive", width="70%")
                                                                       )
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                                )
                                                
                                                
                                                
                                )
                                
                                
         )
         
         
         
         )
         
         
         
         
)
