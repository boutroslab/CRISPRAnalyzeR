# sourced by 'ui.r'
# save as 'hcPerformance_ui.r'
# ui elements of 'Performance' sub tab in 'Hit Calling' tab






tabItem(tabName = "hc_performance", align = "center",
  
  shinyjs::useShinyjs(),
        
        
  ## Welcome message
  fluidRow( style="width:80%;",
    HTML("
         <div class='section'>
         <div class='container'>
         <div class='row'>
         <div class='col-md-12'>
         <h1 class='text-success text-left'>
         <i class='fa fa-angle-double-right  fa-fw'></i>Hit Calling
         <font color='#777777'>&nbsp;
         <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Algorithm Performance</span>
         </font>
         </h1>
         <hr>
         <p class='lead'>caR uses 6 different hit calling methods to analyze your screen - check out the performance of each below.</p>
         </div>
         </div>
         </div>
         </div>
         <div class='section'>
         <div class='container'>
         <div class='row'>
         <div class='col-md-12'>
         <div class='panel panel-default'>
         <div class='panel-heading'>
         <h3 class='panel-title'>
         <a data-toggle='collapse' href='#collapsehcperformance'><i class='fa fa-arrow-circle-down fa-fw'></i>Additional Information</a>&nbsp;
         <small>Click to extend</small>
         </h3>
         </div>
         <div class='panel-collapse collapse' id='collapsehcperformance'>
         <div class='panel-body'>
         <div class='col-md-12'>
         <div class='col-md-12'>
         <h3 class='text-success'>Various Hit Calling Algorithms are implemented in caR</h3>
         <p>caR offers you five different hit calling algorithm implementations, which
         all behave differently depending on the dataset quality or screening setup.
         <br>We do not want to make your analysis all to complicated, so we give you
         the chance to browser each result indepently first.
         <br>
         <b>Please note:</b>caR is meant to be used for explorative analysis your
         data, so there is no best hit calling algorithm!</p>
         <ul class='list-group'>
         <li class='list-group-item' style='text-align:left;'>Wilcox Implementation - Mann-Whitney-U Test
         <span class='badge'>Analysis on sgRNA readcount</span>
         </li>
         <li class='list-group-item' style='text-align:left;'>DESeq2 Implementation on gene-level readcount
         <span class='badge'>Analysis on Gene readcount</span>
         </li>
         <li class='list-group-item' style='text-align:left;'>MAGeCK Implementation as published
         <br>
         <i>Li,W. et al. (2014), Genome Biology, 15, 554.</i>
         <span class='badge'>Analysis on sgRNA ReadCount</span>
         </li>
         <li class='list-group-item' style='text-align:left;'>edgeR Implementation as published
         <br>
         <i>Dai,Z. et al. (2014) edgeR: a versatile tool for the analysis of shRNA-seq
         and CRISPR-Cas9 genetic screens. F1000Research, 3, 95.</i>
         <span class='badge'>Analysis on sgRNA readcount</span>
         </li>
         <li class='list-group-item' style='text-align:left;'>sgRSEA Implementation as described here
         <br>
         <i>https://cran.r-project.org/web/packages/sgRSEA/index.html</i>
         <span class='badge'>Analysis on sgRNA readcount</span>
         </li>
         </ul>
         </div>
         <div class='col-md-6'>
         <h3 class='text-success'>Gene List Table</h3>
         <p>The gene list table allows you to browse the result obtained by the algorithm.
         <br>This table is supposed to give you the chance to explore the results in
         an easy way.
         <br>
         <br>You can select whether to look at genes with a positive fold change (
         <b>enriched</b>) or negative fold change (
         <b>depleted</b>) between your treatment and control.
         <br>Depending on the P-Value you defined for each algorithm in the settings
         tab, you can decide whether only genes below the p-value threshold are
         shown.
         <br>You can sort the table according to any of the columns and also do a full
         search within the table.</p>
         </div>
         <div class='col-md-6'>
         <h3 class='text-success'>Sorted P-Value Plot</h3>
         <p>This plot visualizes the algorithm performance.
         <br>It plots the -log10 pvalue obtained for all genes.
         <br>Genes that revealed an adjusted p-value below the p-value threshold defined
         in the settings page will be highlighted in red color.
         <br>
         <br>
         <b>Please remember:</b>You can zoom into the plot as well as create an image.
         Hover with the mouse above any data point and it will show you the gene
         name and the corrected p-value obtained.</p>
         </div>
         </div>
         </div>
         </div>
         </div>
         </div>
         </div>
         </div>
         </div>")
  ),
  
  
  # Tabset Panel
  fluidRow(
    column(width=10, offset=1,
    tabBox(width = 12,

      
      # Wilcox
      # tabPanel(
      #   "Wilcox",
      #   shiny::tags$p(class="lead", "Wilcox analysis is based on the Wilcoxon Rank-Sum test."),
      #   shiny::tags$p(style="width:60%","First all sgRNA read counts are normalized with DESeq2 across the samples.", br(),
      #       "Then, fold changes of each population of sgRNAs for one gene is compared to either non-targeting controsl (if specified) or a number of randomly pciked sgRNAs using a two-sided Mann-Whitney test with FDR correction according to Benjamini Hochberg."),
      #   br(),
      #   
      #   fluidRow(
      #     column(7, 
      #       #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
      #         h3("Gene List"),
      #         radioButtons("hcPerformance_wilcox_radio", NULL, 
      #           inline = TRUE, choices = list("enriched" = "en", "depleted" = "de")),
      #         checkboxInput("hcPerformance_wilcox_sign", "Show only genes below the p-value threshold", value = TRUE),
      #         DT::dataTableOutput("hcPerformance_wilcox_data")
      #       #)
      #     ),
      #     
      #   )
      # ),
      # 
      
      # # DESeQ2
      # tabPanel(
      #   "DESeq2",
      #   shiny::tags$p(class="lead","DESEq2 analysis is performed using the DESEq2 Differentially Expressed Genes workflow on read counts per gene."),
      #   shiny::tags$p(style="width:60%","Read counts for all sgRNAs are summed up to obtain read counts per gene.", 
      #       "Then, DESeq2 analysis is performed, which includes normalization, estimation of size-factors and variance stabilization using a parametric fit.", br(),
      #       "A Wald test for difference in log2-foldchanges between both conditions is done."),
      #   br(),
      #   
      #   fluidRow(
      #     column(7, 
      #       #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
      #         h3("Gene List"),
      #         radioButtons("hcPerformance_deseq_radio", NULL, inline = TRUE,
      #                    choices = list("enriched" = "en", "depleted" = "de")),
      #         checkboxInput("hcPerformance_deseq_sign", "show only significantly differential genes", value = TRUE),
      #         DT::dataTableOutput("hcPerformance_deseq_data")
      #       #)
      #     ),
      #     tabBox(width=5,
      #            tabPanel(title = "Ranked by P-Value",
      #           shiny::tags$div(width="100%",
      #                           highchartOutput("hcPerformance_deseq_plot")
      #                           )
      #         
      #           ),
      #           tabPanel(title = "P-Value Distribution",
      #                    shiny::tags$div(width="100%",
      #                                    highchartOutput("hcPerformance_deseq_plot2")
      #                               
      #                    )
      #           )
      #     )
      #   )
      # ),
      # 
      # 
      # MAGeCK
      # tabPanel(
      #   "MAGeCK",
      #   shiny::tags$p(class="lead","MAGeCK analysis is performed using the MAGeCK algorithm."),
      #   shiny::tags$p(style="width:60%","This method uses a rank-based model to test for a change in abundance of sgRNAs after DESeq2 data normalization."),
      #   br(),
      #   
      #   fluidRow(
      #     column(7, 
      #       #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
      #         h3("Gene List"),
      #         radioButtons("hcPerformance_mageck_radio", NULL, inline = TRUE,
      #                    choices = list("enriched" = "en", "depleted" = "de")),
      #         checkboxInput("hcPerformance_mageck_sign", "Show only genes below the p-value threshold", value = TRUE),
      #         DT::dataTableOutput("hcPerformance_mageck_data")
      #       #)
      #     ),
      #     tabBox(width=5,
      #            tabPanel(title = "Ranked by P-Value",
      #                     shiny::tags$div(width="100%",
      #         h3("Sorted P Values for Enrichment"),
      #         highchartOutput("hcPerformance_mageck_plotEnr"),
      #         shiny::tags$hr(),
      #         h3("Sorted P Values for Depletion"),
      #         highchartOutput("hcPerformance_mageck_plotDep")
      #                     )
      #          ),
      #         tabPanel(title = "P-Value Distribution",
      #                  shiny::tags$div(width="100%",
      #                                  h3("Enrichment"),
      #                                  highchartOutput("hcPerformance_mageck_plotEnr2"),
      #                                  shiny::tags$hr(),
      #                                  h3("Depletion"),
      #                                  highchartOutput("hcPerformance_mageck_plotDep2")
      #                  )
      #         )
      #     )
      #   )
      # ),
      # 
      # 
      # # sgRSEA
      # tabPanel(
      #   "sgRSEA",
      #   shiny::tags$p(class="lead","This method uses the published sgRSEA method which is designed specificially to determine dropouts/essential genes."),
      #   shiny::tags$p(style="width:60%","sgRSEA is based on a single-guide RNA Set Enrichment Analysis. First, sgRNAs are ranked by a signal-to-noise ratio.",
      #   "Then, the distribution of sgRNA ranks of a sgRNA set is compared with the overall distribution using the enrichment score.", br(),
      #   "This score is based on a one-sided Kolmogorov Smirnov statistic and reflects the degree to which a sgRNA set is overrepresented at the top/the bottom of the ranked list.", br(),
      #   "sgRSEA has its strength in detecting depleted genes, e.g. in a dropout or essential genes CRISPR assay."),
      #   br(),
      #   
      #   fluidRow(
      #     column(7, 
      #       #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
      #         h3("Gene List"),
      #         radioButtons("hcPerformance_sgrsea_radio", NULL, inline = TRUE,
      #                    choices = list("enriched" = "en", "depleted" = "de")),
      #         checkboxInput("hcPerformance_sgrsea_sign", "Show only genes below the p-value threshold", value = TRUE),
      #         DT::dataTableOutput("hcPerformance_sgrsea_data")
      #       #)
      #     ),
      #     tabBox(width=5,
      #            tabPanel(title = "Ranked by P-Value",
      #                     shiny::tags$div(width="100%",
      #                                     h3("Ranked for Enrichment"),
      #                                     highchartOutput("hcPerformance_sgrsea_plotEnr"),
      #                                     shiny::tags$hr(),
      #                                     h3("Ranked for Depletion"),
      #                                     highchartOutput("hcPerformance_sgrsea_plotDep")
      #                                     )
      #             ),
      #            tabPanel(title = "P-Value Distribution",
      #                     shiny::tags$div(width="100%",
      #                                     h3("Enrichment"),
      #                                     highchartOutput("hcPerformance_sgrsea_plotEnr2"),
      #                                     shiny::tags$hr(),
      #                                     h3("Depletion"),
      #                                     highchartOutput("hcPerformance_sgrsea_plotDep2")
      #                     )
      #            )
      #     )
      #   )
      # ),
      # 
      
      # EdgeR
      tabPanel(
        "edgeR",
        shiny::tags$p(class="lead","The edgeR implementation uses the edgeR bioconductor package according to Dai,Z. et al."),
        shiny::tags$p(style="width:60%","Readcount data is modelled using an overdispersed Poisson model.",
            "Gene dispersions are estimated by conditional maximum likelihood and dispersions are shrunk using an empirical Bayes procedure.", br(),
            "Then, differential expression is assessed using an adapted Fisher's exact test."),
        br(),
        
        fluidRow(
          column(7, 
            #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
              h3("Gene List"),
              radioButtons("hcPerformance_edger_radio", NULL, inline = TRUE,
                         choices = list("enriched" = "en", "depleted" = "de")),
              checkboxInput("hcPerformance_edger_sign", "Show only genes below the p-value threshold", value = TRUE),
              DT::dataTableOutput("hcPerformance_edger_data")
            #)
          ),
          tabBox(width=5, 
                 tabPanel(title = "Ranked by P-Value",
                  shiny::tags$div(width="100%",
                                  #h3("Sorted P Values"),
                                  highchartOutput("hcPerformance_edger_plot")
                                  )
                ),
                tabPanel(title = "P-Value Distribution",
                         shiny::tags$div(width="100%",
                                         #h3("Sorted P Values"),
                                         highchartOutput("hcPerformance_edger_plot2")
                         )
                         
                )
          )
        )
      ),
      
      # Z-Ratio
      tabPanel(
        "Z-Ratio",
        shiny::tags$p(class="lead","The Z-Ratio is a variance normalized way to show a foldchange."),
        shiny::tags$p(style="width:60%","Z-Ratio is calculated between your treated and untreated samples and was originally used for microarray data.",
            "The Z-Ratio has been implemented as described in the paper"),
        shiny::tags$p(style="width:60%",
            shiny::tags$b("Analysis of Microarray Data Using Z Score Transformation"), 
            shiny::tags$i("Chris Cheadle, Marquis P. Vawter, William J. Freed, Kevin G. Becker
J Mol Diagn. 2003 May; 5(2): 73–81. doi: 10.1016/S1525-1578(10)60455-2")
              ),
        shiny::tags$hr(width="50%"),
        shiny::tags$p(class="lead","A Z-Ratio higher than 1.96 and lower than -1.96 indicates a possible screening candidate acccording to the publication."),
        
        br(),
        fluidRow(
          column(7, 
                 #box(title = "gene list", width = NULL, solidHeader = FALSE, status = "primary",
                 h3("Gene List"),
                 DT::dataTableOutput("hcPerformance_zratio_data")
                 #)
          ),
          tabBox(width=5,
                 tabPanel(title = "Ranked by Z-Ratio",
              shiny::tags$div(width="100%",
                              
                              highchartOutput("hcPerformance_zratio_plot"))
                  ),
              tabPanel(title = "Z-Ratio Distribution",
                       shiny::tags$div(width="100%",
                                
                                highchartOutput("hcPerformance_zratio_plot2"))
                       )
              )
          )
        )
      )
      
      
      
      
    )
   
  ),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
   
) # close tab
