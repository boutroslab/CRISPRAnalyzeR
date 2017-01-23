# help file
# save as hcCandidates_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#hcCandidates"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="hcCandidates", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              shiny::tags$h3("Which hit calling algorithms are implemented in CRISPRAnalyzeR?", class="text-success"),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR offers you six different hit calling algorithm implementations to give you the chance to find the most robust hit within your screen.
                                                                                            Since each algorithm performs the hit calling in a different way, we suggest you to have a look to all of them."),
                                                                              shiny::tags$p(class="lead", "Before CRISPRAnalyzer applies hit calling, all data is normalized using DESeq2."),
                                                                              column(width=6,
                                                                                     shiny::tags$h4("Wilcox"),
                                                                                     shiny::tags$span( class='badge', "Analysis on sgRNA foldchange"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "The Wilcox implementation is based on a two-sided Mann-Whitney-U test, which compares the sgRNA population foldchange (between your treatment groups) of each gene to either the population of the non-targeting control (if specified) or randomly picked sgRNAs.",
                                                                                                   "Finally a P-value correction according to Benjamini-Hochberg is applied to correct for multiple testing."),
                                                                                     # add some image here
                                                                                     shiny::tags$h4("MAGeCK"),
                                                                                     shiny::tags$span( class='badge', "Analysis on sgRNA read counts"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "MAGeCK is a stand-alone algorithm to perform hit calling in pooled CRISPR screens. MAgeCK is based on a RRA ranking algorithm to identify hit candidates."
                                                                                                   ),
                                                                                     shiny::tags$p("Li, et al. MAGeCK enables robust identification of essential genes from genome-scale CRISPR/Cas9 knockout screens. Genome Biology 15:554 (2014)"),
                                                                                     # add some image here
                                                                                     
                                                                                     shiny::tags$h4("edgeR"),
                                                                                     shiny::tags$span( class='badge', "Analysis on sgRNA read counts"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                  "In brief, all read count data is modelled using an overdispersed Poisson model. Gene dispersions are then estimated by conditional maximum likelihood and shrunk using an empirical Bayes procedure. Finally, differential expression is assessed using an adapted Fisher's exact test.",
                                                                                                  "EdgeR analysis has been implemented as previously published in"
                                                                                                   ),
                                                                                     shiny::tags$p("Dai,Z. et al. (2014) edgeR: a versatile tool for the analysis of shRNA-seq and CRISPR-Cas9 genetic screens. F1000Research, 3, 95.")
                                                                                     
                                                                                     
                                                                                     
                                                                                     ),
                                                                              column(width=6,
                                                                                     shiny::tags$h4("DESeq2"),
                                                                                     shiny::tags$span( class='badge', "Analysis on Gene-level read counts"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "The DESeq2 implementation is based on the DESeq2 R package. It uses the summed read counts of all sgRNAs for a given gene and tests for differential effects based on a negative binomial distribution model.",
                                                                                                   "In brief, eead counts for all sgRNAs are summed up to obtain the total read count per gene. DESeq2 analysis is performed on these read counts, which includes normalization, estimation of size-factors and variance stabilization using a parametric fit. A Wald test for difference in log2-foldchanges between both conditions is done to determine enrichment/depletion effects.",
                                                                                                   "For more information about DESeq2, please see the DESeq2 manual available at ", shiny::tags$a(target="_blank", href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html", HTML('<i class="fa fa-external-link fa-fw text-success "></i><strong> Bioconductor</strong>') ),' or the publication.'),
                                                                                     shiny::tags$p("Love MI, Huber W and Anders S (2014). “Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2.” Genome Biology, 15, pp. 550. doi: 10.1186/s13059-014-0550-8."),
                                                                                     # add some image here
                                                                                     shiny::tags$h4("sgRSEA"),
                                                                                     shiny::tags$span( class='badge', "Analysis on sgRNA read counts"),
                                                                                     shiny::tags$p(class="text-justify",
                                                                                                   "sgRSEA is based on the sgRSEA R package (Enrichment Analysis of CRISPR/Cas9 Knockout Screen Data), which is available at the ", shiny::tags$a(target="_blank", href="https://cran.r-project.org/web/packages/sgRSEA/index.html", HTML('<i class="fa fa-external-link fa-fw text-success "></i><strong> R CRAN page</strong>.')),
                                                                                                   "in brief, sgRSEA is based on a single-guide RNA Set Enrichment Analysis. First, sgRNAs are ranked by a signal-to-noise ratio. Then, the distribution of sgRNA ranks of a sgRNA set is compared with the overall distribution using a so-called enrichment score, which is based on a one-sided Kolmogorov Smirnov statistic and reflects the degree to which a sgRNA set is overrepresented at the top/the bottom of the ranked list."
                                                                                      ),
                                                                                    shiny::tags$h4("Z-Ratio"),
                                                                                    shiny::tags$span( class='badge', "Analysis on sgRNA/Gene-level read counts"),
                                                                                    shiny::tags$p(class="text-justify",
                                                                                                  "A Z-Ratio is calculated between your two treatment groups for each sample and was originally used for microarray data. The Z-Ratio has been implemented as previously published."
                                                                                    ),
                                                                                    shiny::tags$p("Analysis of Microarray Data Using Z Score Transformation Chris Cheadle, Marquis P. Vawter, William J. Freed, Kevin G. Becker J Mol Diagn. 2003 May; 5(2): 73–81. doi: 10.1016/S1525-1578(10)60455-2")
                                                                              
                                                                                     
                                                                              )
                                                                              
                                                                              ),
                                                                column(width=12,
                                                                       shiny::tags$h3("What visualization does CRISPRAnalyzeR offer?", class="text-success"),
                                                                       shiny::tags$p(class="text-justify", "CRISPRAnalyzeR offers you three different visualizations for all hit calling methods that have been implemented."),
                                                                       shiny::tags$ul(class="list-group",
                                                                         shiny::tags$li(class="list-group-item","Ranked P-Values by significance"),
                                                                         shiny::tags$li(class="list-group-item","Distribution of unadjusted P-Values"),
                                                                         shiny::tags$li(class="list-group-item","Log2-transformed foldchanges plotted against the -log10 transformation of the corresponding adjusted p-values")
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
