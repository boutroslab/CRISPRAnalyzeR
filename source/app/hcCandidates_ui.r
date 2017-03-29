# sourced by 'ui.r'
# save as 'hcCandidates_ui.r'
# ui elements of 'Candidates' sub tab in 'Hit Calling' tab






tabItem(tabName = "hc_candidates", align = "center",
  
  shinyjs::useShinyjs(),
        
        
  ## Welcome message
  fluidRow(
   style="width:80%;",
   HTML("<div class='section'><div class='container'><div class='row'><div class='col-md-12'>
        <h1 class='text-success text-left'><i class='fa fa-angle-double-right  fa-fw'></i>Hit Calling<font color='#777777'>&nbsp;
        <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Gene Ranking</span></font></h1>
        <hr><p class='lead'>CRISPRAnalyzeR performs data analysis using 8 different algorithms, which gives you a more sophisticated way to check for screening candidates. Take your time and have a look at each individual analysis method and compare them.</p></div></div></div></div>")),
   
  # HELP as including of hcCandidates_help.r
  source(file.path(config$appDir, "hcCandidates_help.r"))[1],
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  # Tabset Panel
  fluidRow(
    column(width=10,offset=1,
           
           shiny::tags$p(
             "CRISPRAnalyzeR performs the analysis to find more abundant (enriched) or less abundant (depleted) genes between your treatment groups.",
             "Genes which have shown a significant change, according to the p-value thresholds you have specified, are highlighted in red color in all plots."
             
           ),
           # figure showing what enriched/depleted means exactly
           
    tabBox(width = 12, 

      
      # Wilcox
      tabPanel(
        "Wilcox",
        shiny::tags$p(style="width:60%", "Wilcox analysis is based on the Wilcoxon Rank-Sum test."),
       # shiny::tags$p(style="width:60%","First all sgRNA read counts are normalized with DESeq2 across the samples.", br(),
      #                "Then, fold changes of each population of sgRNAs for one gene is compared to either non-targeting controsl (if specified) or a number of randomly pciked sgRNAs using a two-sided Mann-Whitney test with FDR correction according to Benjamini Hochberg."),
      shiny::tags$hr(width="50%"),  
      shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 radioButtons("hcPerformance_wilcox_radio", NULL, 
                              inline = TRUE, choices = list("enriched" = "en", "depleted" = "de")),
                 checkboxInput("hcPerformance_wilcox_sign", "Show only genes below the p-value threshold", value = TRUE),
                 shiny::div(width="100%",
                            DT::dataTableOutput("hcPerformance_wilcox_data")
                 )
              
          ),
          column(width=10,offset=1,
                 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_wilcox_plot")
                                 )
                                 
                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="60%",
                                                 highchartOutput("hcPerformance_wilcox_plot2") )
                        ),
                        tabPanel(title = "Foldchange vs. adjusted P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcCandidates_wilcox_plot", height = "500px") )
                        )
                        
                 )
              
            
          )
        )
      ),
      
      
      # DESeq2
      tabPanel(
        "DESeq2",
        shiny::tags$p(style="width:60%","DESeq2 analysis is performed using the DESeq2 Differentially Expressed Genes workflow on gene-level read counts."),
       # shiny::tags$p(style="width:60%","Read counts for all sgRNAs are summed up to obtain read counts per gene.", 
      #                "Then, DESeq2 analysis is performed, which includes normalization, estimation of size-factors and variance stabilization using a parametric fit.", br(),
      #                "A Wald test for difference in log2-foldchanges between both conditions is done."),
      shiny::tags$hr(width="50%"),  
      shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 radioButtons("hcPerformance_deseq_radio", NULL, inline = TRUE,
                              choices = list("enriched" = "en", "depleted" = "de")),
                 checkboxInput("hcPerformance_deseq_sign", "Show only genes below the p-value threshold", value = TRUE),
                 shiny::tags$div(width="100%",
                                 DT::dataTableOutput("hcPerformance_deseq_data")
                                 ),
                 shiny::tags$br()
                 
          ),
          column(width=10,offset=1, 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_deseq_plot")
                                 )
                                 
                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_deseq_plot2")
                                                 
                                 )
                        ),
                        tabPanel(title = "Foldchange vs. adjusted P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcCandidates_deseq_plot", height = "500px") )
                        )
                        
                 )
          )

        )
        ),
        
      
      # MAGeCK
      tabPanel(
        "MAGeCK",
        shiny::tags$p(style="width:60%",
                      "MAGeCK analysis is performed using the MAGeCK algorithm as described in "),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("Li,W. et al. (2014) MAGeCK enables robust identification of essential genes from genome-scale CRISPR/Cas9 knockout screens."), 
                      shiny::tags$i("Genome Biology, 15, 554.")
                      ),
        shiny::tags$hr(width="50%"),
        shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 radioButtons("hcPerformance_mageck_radio", NULL, inline = TRUE,
                              choices = list("enriched" = "en", "depleted" = "de")),
                 checkboxInput("hcPerformance_mageck_sign", "Show only genes below the p-value threshold", value = TRUE),
                 shiny::tags$div(width="100%",
                                 DT::dataTableOutput("hcPerformance_mageck_data")
                 ),
                 shiny::tags$br()
          ),
          column(width=10,offset=1, 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by P-Value",
                                 shiny::tags$div(width="100%",
                                                 shiny::tags$h3("Enriched"),
                                                 highchartOutput("hcPerformance_mageck_plotEnr"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Depleted"),
                                                 highchartOutput("hcPerformance_mageck_plotDep")
                                 )
                                 
                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="100%",
                                                 shiny::tags$h3("Enriched"),
                                                 highchartOutput("hcPerformance_mageck_plotEnr2"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Depleted"),
                                                 highchartOutput("hcPerformance_mageck_plotDep22")
                                 )
                        ),
                        tabPanel(title = "Foldchange vs. adjusted P-Value",
                                 shiny::tags$div(width="100%",
                                                 shiny::tags$h3("Enriched"),
                                                 highchartOutput("hcCandidates_mageck_plotEnr", height = "500px"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Depleted"),
                                                 highchartOutput("hcCandidates_mageck_plotDep", height = "500px")
                                                 )
                        )
                        
                 )
          )
        )
        ),
        
      
      
      # sgRSEA
      tabPanel(
        "sgRSEA",
        shiny::tags$p(style="width:60%",
                      "This method uses the sgRSEA R package as described in the workflow at"),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("https://cran.r-project.org/web/packages/sgRSEA/index.html")
        ),
        shiny::tags$hr(width="50%"),
        shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 radioButtons("hcPerformance_sgrsea_radio", NULL, inline = TRUE,
                              choices = list("enriched" = "en", "depleted" = "de")),
                 checkboxInput("hcPerformance_sgrsea_sign", "Show only genes below the p-value threshold", value = TRUE),
                 shiny::div(width="100%",
                            DT::dataTableOutput("hcPerformance_sgrsea_data")
                 ),
                 shiny::tags$br()
                 
                 
          ),
          column(width=10,offset=1,
                 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by P-Value",
                                 shiny::tags$div(width="100%",
                                                 shiny::tags$h3("Ranked for Enrichment"),
                                                 highchartOutput("hcPerformance_sgrsea_plotEnr"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Ranked for Depletion"),
                                                 highchartOutput("hcPerformance_sgrsea_plotDep")
                                 )
                                 
                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="60%",
                                                 shiny::tags$h3("Enrichment"),
                                                 highchartOutput("hcPerformance_sgrsea_plotEnr2"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Depletion"),
                                                 highchartOutput("hcPerformance_sgrsea_plotDep2")
                                                 )
                        ),
                        tabPanel(title = "Foldchange vs. adjusted P-Value",
                                 shiny::tags$div(width="100%",
                                                 shiny::tags$h3("Enriched"),
                                                 highchartOutput("hcCandidates_sgrsea_plotEnr", height = "500px"),
                                                 shiny::tags$hr(),
                                                 shiny::tags$h3("Depleted"),
                                                 highchartOutput("hcCandidates_sgrsea_plotDep", height = "500px")
                                                 )
                        )
                        
                 )
                 
                 
          )
          
        )
        
        
      ), 
        

      # EdgeR
      tabPanel(
        "edgeR",
        shiny::tags$p(style="width:60%",
                      "The edgeR implementation uses the edgeR Bioconductor package using a previously published workflow in "),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("Dai,Z. et al. (2014) edgeR: a versatile tool for the analysis of shRNA-seq and CRISPR-Cas9 genetic screens."), 
                      shiny::tags$i("F1000Research, 3, 95.")
        ),
        shiny::tags$hr(width="50%"),
        shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 radioButtons("hcPerformance_edger_radio", NULL, inline = TRUE,
                              choices = list("enriched" = "en", "depleted" = "de")),
                 checkboxInput("hcPerformance_edger_sign", "Show only genes below the p-value threshold", value = TRUE),
                 shiny::tags$div(width="100%",
                                 DT::dataTableOutput("hcPerformance_edger_data")
                 )
                 
          ),
          column(width=10,offset=1, 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_edger_plot")
                                 )
                                 
                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_edger_plot2")
                                                 
                                 )
                        ),
                        tabPanel(title = "Foldchange vs. adjusted P-Value",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcCandidates_edger_plot", height = "500px")
                                                 )
                        )
                        
                 )
          )
          
          
        )
      ),
        
      
      # Z-Ratio
      tabPanel(
        "Z-Ratio",
        shiny::tags$p(style="width:60%",
                      "The Z-Ratio has been implemented as described in the paper"),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("Analysis of Microarray Data Using Z Score Transformation"), 
                      shiny::tags$i("Chris Cheadle, Marquis P. Vawter, William J. Freed, Kevin G. Becker
                                    J Mol Diagn. 2003 May; 5(2): 73–81. doi: 10.1016/S1525-1578(10)60455-2")
                      ),
        shiny::tags$hr(width="50%"),
        #shiny::tags$p(class="lead","The Z-Ratio is a variance normalized way to show a foldchange."),
        shiny::tags$p(class="text","According to the publication, a Z-Ratio higher than 1.96 and lower than -1.96 indicates a possible candidate."),
        
        shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 
                 shiny::tags$div(width="100%",
                                 DT::dataTableOutput("hcPerformance_zratio_data")
                 )
                 
          ),
          column(width=10,offset=1, 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by Z-Ratio",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_zratio_plot")
                                 )
                                 
                        ),
                        tabPanel(title = "Z-Ratio Distribution",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_zratio_plot2")
                                                 
                                 )
                        ),
                        tabPanel(title = "Treated Z-Score vs. Z-Ratio",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcCandidates_zratio_plot", height = "500px")
                                 )
                        )
                        
                 )
          )
          
            
        )
      ),
        
      # Bagel
      tabPanel(
        "BAGEL",
        shiny::tags$p(style="width:60%",
                      "BAGEL analysis for essential genes has been implemented as described in"),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("Hart,T. et al. (2015) Systematic discovery and classification of human cell line essential genes."), 
                      shiny::tags$i(" Cold Spring Harbor Labs Journals")
                      ),
        shiny::tags$hr(width="50%"),
        
        shiny::tags$br(),
        
        fluidRow(
          column(width=10,offset=1, 
                 
                 checkboxInput("hcPerformance_bagel_sign", HTML(paste("Show only genes above the determined log2 Bayes Factor threshold of", shiny::textOutput("hcPerformance_bagel_cutoff"), sep=" ")), value = TRUE, width="100%"),
                 shiny::div(width="100%",
                            DT::dataTableOutput("hcPerformance_bagel_data")
                 )
                 
          ),
          column(width=10,offset=1, 
                 tabBox(width=12,
                        tabPanel(title = "Ranked by log2 Bayes Factor",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_bagel_plot")
                                 )
                                 
                        ),
                        tabPanel(title = "Log2 Bayes Factor Distribution",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_bagel_plot2")
                                                 
                                 )
                        )
                        
                 )
          )
          
          
        )
        ),
      
      # ScreenBEAM
      # too slow for implementation yet
      tabPanel(
        "ScreenBEAM",
        shiny::tags$p(style="width:60%",
                      "ScreenBEAM has been implemented as described in the paper"),
        shiny::tags$p(style="width:60%",
                      shiny::tags$b("Yu,J. et al. (2015) ScreenBEAM: a novel meta-analysis algorithm for functional genomics screens via Bayesian hierarchical modeling."),
                      shiny::tags$i("Bioinformatics, btv556.")
                      ),
        shiny::tags$hr(width="50%"),

        shiny::tags$br(),

        fluidRow(
          column(width=10,offset=1,
                 checkboxInput("hcPerformance_screenbeam_sign", HTML(paste("Show only genes above the determined threshold of", shiny::textOutput("hcPerformance_screenbeam_cutoff"), sep=" ")), value = TRUE, width="100%"),
                 shiny::tags$div(width="100%",
                                 DT::dataTableOutput("hcPerformance_screenbeam_data")
                 )

          ),
          column(width=10,offset=1,
                 tabBox(width=12,
                        tabPanel(title = "Ranked by FDR",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_screenbeam_plot")
                                 )

                        ),
                        tabPanel(title = "P-Value Distribution",
                                 shiny::tags$div(width="100%",
                                                 highchartOutput("hcPerformance_screenbeam_plot2")

                                 )
                        )

                 )
          )


        )
        )
      
    
    )
    
  )
  ),
  
  shiny::tags$br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
   
) # close tab
