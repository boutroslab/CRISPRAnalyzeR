# sourced by 'ui.r'
# save as 'idOverview_ui.r'
# ui elements for "In-Depth Analysis" tab "Overview" sub tab




tabItem(tabName = "id_overview", align = "center",
        
        shinyjs::useShinyjs(),
        
        
        fluidRow(style="width:80%",
                 
                 HTML("
  <body>
                      <div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Hit Confirmation
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Gene Overview</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>The Hit Confirmation section will provide you with
                      more detailed information about your genes of interest.
                      <br>This includes individual sgRNA performance, a genomic model,
                      predicted genome binding sites, predicted sgRNA scores as well as a brief overview
                      for each individual gene of your screen.
                      <br>
                      </p>
                      </div>
                      </div>
                      </div>
                      </div>")),
        
        # HELP as including of idOverview_help.r
        source(file.path(config$appDir, "idOverview_help.r"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        
        ##### Now we plot the following information IN A DATA TABLE with sprakling plot, must be COLLAPSIBLE for more information
        ### General Gene Informaiton
        ## HGNC symbol (link to genecards), EmsemblID (link to ensembl), description, go term, go domain, gene length, number of transcripts, number of exons, annotated diseases
        
        ### sgRNA performance overview
        ## number of sgrnas in screens
        ## small columnplot with log2FC in analysis
        
        fluidRow(style="width:85%",
                 column(4, offset=4,
                 shinydashboard::box(title = NULL, width = 12, solidHeader = FALSE, color = "gray",
                     h4("Please select your gene of interest"),
                     div(style="display:inline-block;", uiOutput("indepthOverviewGene")),
                     
                     uiOutput("info_error_gView"),
                     uiOutput("hit_error"),
                     uiOutput("hitResults_error"), br(),
                     helpText("After selecting a gene, CRISPRAnalyzeR will automatically start to retrieve all information."),
                     actionButton("addReport_overview", "Add to Report")
                     
                 )
                 )
        ),
        br(),
        # General Overview
        
        fluidRow(
                 column(12,
                    tabBox(width=12,
                           
                    tabPanel("Gene Information",  
                        shiny::tags$p(class="lead","Just a quick overview about your gene of interest."),
                     # HTML('<h3 class="text-success">
                     #      <i class="fa fa-angle-right fa-fw"></i>Gene Information</h3>'),
                      # First generl infos
                     
                     HTML('<div class="col-md-6">'),
                     HTML('
                          <table class="table">
                          <tbody>
                          <tr>
                          <td><strong>Gene Symbol</strong></td>
                          <td>'),
                  htmlOutput("HGNC_SYMBOL", inline=TRUE),
                  HTML('</td>
                          </tr>
                          <tr>
                          <td><strong>Ensembl Gene ID</strong></td>
                          <td>'),
                  htmlOutput("ENSEMBL_GENE_ID", inline=TRUE),
                  HTML('</td>
                          </tr>
                          <tr>
                          <td><strong>Gene Description</strong></td>
                          <td>'),
                  htmlOutput("GENE_DESCRIPTION", inline=TRUE),
                  HTML('</td>
                          </tr>
                          <tr>
                   <td><strong>Uniprot ID</strong></td>
                          <td>'),
                  htmlOutput("UNIPROT_ID", inline=TRUE),
                  HTML('</td>
                          </tr>
                          <tr>
                          <td><strong>NCBI Gene ID</strong></td>
                          <td>'),
                  htmlOutput("ENTREZGENE", inline=TRUE),
                  HTML('</td>
                          </tr>
                          <tr>
                          <td></td>
                          <td></td>
                          </tr>
                          </tbody>
                          </table>'),
                     
                     HTML('</div>'),
                     
                     # pvals etc
                     HTML('<div class="col-md-6">'),
                    HTML('<table class="table">
                      <tbody>
                         <tr>
                         <td>
                         <strong>Log2 Foldchange</strong>
                         </td>
                         <td>'),
                  textOutput("indepth_gene.log2fc"),
                  HTML('</td>
                        </tr>
                        <tr>
                       <td><strong>Z-Ratio</strong></td>
                       <td>'),
                  textOutput('indepth_gene.zratio'),
                  HTML('</td>
                      </tr>
                      <tr>
                      <td><strong>Wilcoxon p-value</strong></td>
                      <td>'),
                  textOutput("indepth_gene.wilcoxpval"),
                  HTML('</td>
                      </tr>
                      <tr>
                      <td><strong>DESeq2 p-value</strong></td>
                      <td>'),
                  textOutput("indepth_gene.deseq2pval"),
                  HTML('</td>
                      </tr>
                      <tr>
                      <td><strong>MAGeCK p-value</strong></td>
                      <td>'),
                  htmlOutput("indepth_gene.mageckpval"),
                  HTML('</td>
                      </tr>
                      <tr>
                      <td><strong>sgRSEA p-value</strong></td>
                      <td>'),
                  htmlOutput("indepth_gene.rseapval"),
                  HTML('</td>
                      </tr>
                      <tr>
                      <td><strong>edgeR p-value</strong></td>
                      <td>'),
                  textOutput("indepth_gene.edgerpval"),
                  HTML('</td>
                      </tr>
                      </tbody>
                      </table>'),
                     HTML('</div>'),
                     
                  #br(),
                     #shiny::tags$hr(width="50%", style="margin-top:40px;margin-bottom:40px;"),
                 # KEGG general information
                 fluidRow(class="text-left", style="width:80%",
                   shiny::tags$br(),
                   shiny::tags$p(class="text-left",HTML("<strong>Other Gene Names</strong>"),HTML("</br>") , htmlOutput("kegg_name")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left",HTML("<strong>Associated Pathways</strong>"),HTML("</br>") , htmlOutput("kegg_pathway")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left",HTML("<strong>Associated Diseases</strong>"),HTML("</br>") , htmlOutput("kegg_disease")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left",HTML("<strong>Motifs</strong>"),HTML("</br>") , textOutput("kegg_motif")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left",HTML("<strong>Presence in other Databases</strong>"),HTML("</br>") , htmlOutput("kegg_linkdb")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left",HTML("<strong>Associated Crystal Structures</strong>"),HTML("</br>") , htmlOutput("kegg_structure")),
                   shiny::tags$hr(width="50%"),
                   shiny::tags$p(class="text-left", width="85%", shiny::tags$span(width="20%" , HTML("<strong>Amino Acid Sequence</strong>")), shiny::tags$span(width="80%" , htmlOutput("kegg_aa")) )
                   
                 ),
                 # EnrichR Information
                 fluidRow(class="text-left", style="width:80%",
                          # Chip Seq data from ENCODE and ChEA Consensus TFs from ChIP-X and Genome Browser PWMs
                          shiny::tags$p(class="text-left",HTML("<strong>", "The following genes bind to the promoter region of your gene","</strong>"), HTML("</br>"), htmlOutput("chipseqbinding") ),
                          shiny::tags$hr(width="50%"),
                          # TargetScan microRNAs
                          shiny::tags$p(class="text-left",HTML("<strong>", "Your gene is a predicted target of the following miRNAs","</strong>"), HTML("</br>"), htmlOutput("targetscanmirna") ),
                          shiny::tags$hr(width="50%"),
                          ## ENCODE TF ChIP-seq 2015
                          #shiny::tags$p(class="text-left",HTML("<strong>", "The following genes showed promotor binding of your gene according to the ENCODE TF ChIP-seq 2015 dataset","</strong>"), HTML("</br>") ),
                          #shiny::tags$hr(width="50%"),
                          # Transcription factor PPIs
                          shiny::tags$p(class="text-left",HTML("<strong>", "Physical interaction with your gene has been reported for","</strong>"), HTML("</br>"), htmlOutput("TFppi") ),
                          shiny::tags$hr(width="50%")
                          
                          
                 )
                 
                 
                 ),
        
        
        ## GVIZ Plots for Genomic View
               
               tabPanel("Gene Model",
                        shiny::tags$div(align="center",
                        shiny::tags$p(class="lead","Check out the gene model based on the Ensembl database."),
                        
                         # Gviz Plot 1
                        plotOutput(width="85%", height="1000px", "indepth_GVIZ_gene"),
                        shiny::tags$br()
                        )
                 ),
        tabPanel("sgRNA Model",
                 shiny::tags$div(align="center",
                 shiny::tags$p(class="lead","Find out the location and effect of individual sgRNAs for your gene of interest."),
                 # Gviz Plot 2
                 plotOutput(width="85%", height="1000px","indepth_GVIZ_sgrna"),
                 shiny::tags$br()
                 )
        ),
        
        
        ## GenomeCRISPR Information
        # Screens
        tabPanel("Published Screens", width="85%",
        
            ## DT Overview
            shiny::tags$div(style="text-align: center;",
            shiny::tags$p(class="lead","In previous screens, authors reported the following phenotypes for your gene of interest"),
            shiny::tags$br(),
            dataTableOutput("indepth_DT_overview"),
            shiny::tags$br()
            ),
            
            ## DT sgRNA
            shiny::tags$br(),
            shiny::tags$div(style="text-align: center;",
            shiny::tags$hr(width="50%"),
            shiny::tags$p(class="lead","Your sgRNAs have been used in the following previously published screens"),
            shiny::tags$br(),
            dataTableOutput("indepth_DT_sgrna")
            ),
    
            # Powered by genomecrispr
            shiny::tags$div(style="text-align: center;",
            shiny::tags$p(class="lead","Your gene of interest has already been assayed in the following published CRISPR screens"),
            htmlOutput("indepth_screens"),
            shiny::tags$br(),     
            shiny::tags$hr(width="50%"),
            shiny::tags$br()
            )
        ),
          
         ##### Screen information
        
        tabPanel("Gene Performance", width="85%",
                 shiny::tags$p(class="lead","Your gene of interest showed the following log2-foldchange/Z-Ratio"),
                 #HTML('<h4 class="text-primary">
                #      <i class="fa fa-angle-down fa-fw"></i>How did the selected gene perform in your screen?</h4>'),
                 # Highcharts TABBED BOX
                 # Make a tabbed box for HC Plots
                 tabBox(width=12,
                         tabPanel("Log2 FoldChange",
                                  shiny::tags$div(align="center",
                                  highchartOutput("indepth_hc_gene_log2fc", height = "500px"))
                                  ),
                         tabPanel("Z-Ratio",
                                  shiny::tags$div(align="center",
                                  highchartOutput("indepth_hc_gene_zratio", height = "500px"))
                         )
                 ),
                shiny::tags$br(),
                shiny::tags$hr(width="50%"),
                shiny::tags$br(),
                shiny::tags$div(align="center",
                shiny::tags$p(class="lead","Individual sgRNAS for your gene of interest showed the following log2-foldchange"),
                #HTML('<h4 class="text-primary">
                          #    <i class="fa fa-angle-down fa-fw"></i>How did the sgRNAs of the selected gene perform in your screen?</h4>'),
                         
                             # sgRNA information as DT
                            shiny::tags$br(),
                            dataTableOutput("indepth_DT_sgrna2"),
                            shiny::tags$br(),
                             # sgRNA Plot
                            highchartOutput("indepth_hc_sgrna_log2fc")
                         )
        ),
        
        # COSMIC
        
        tabPanel("COSMIC Mutation Database",
                 
                 shiny::tags$p(class="lead","Your gene of interest is annotated with the following mutations in the COSMIC database"),
                 shiny::tags$br(),
                 
                 shiny::tags$br(),
                 
                 tabBox(width=12,
                        tabPanel("All Data",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Mutation data for the selected gene in the COSMIC database"),
                                 shiny::tags$br(),
                                 dataTableOutput("indepth_DT_cosmic"),
                                 shiny::tags$br()
                                 )
                                 
                                 
                        ),
                        tabPanel("Mutation Types",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","The following mutation types have been associated with the selected gene"),
                                 shiny::tags$br(),
                                 shiny::tags$div(align="center",width="100%", highchartOutput("indepth_Type_cosmic", height = "500px")),
                                 shiny::tags$br()
                                 )
                                 
                        ),
                        tabPanel("Tumor Types",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Mutations for your gene have been found in the following tumor types"),
                                 shiny::tags$br(),
                                 shiny::tags$div(align="center",width="100%", highchartOutput("indepth_Tumor_cosmic", height = "500px")),
                                 shiny::tags$br()
                                 )
                                 
                        ),
                        tabPanel("Tumor Sites",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Mutations where present in the following tumor sites"),
                                 shiny::tags$br(),
                                 shiny::tags$div(align="center",width="100%", highchartOutput("indepth_Primary_cosmic", height = "500px")),
                                 shiny::tags$br()
                                 )
                                 
                        ),
                        tabPanel("Samples",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","The following sample have been associated with mutations in your gene"),
                                 shiny::tags$br(),
                                 shiny::tags$div(align="center", width="100%", highchartOutput("indepth_Sample_cosmic", height = "500px")),
                                 shiny::tags$br()
                                 )
                                 
                        )
                        
                 )
                 
        ),
        
        # Gene Ontology
        
        tabPanel("Gene Ontology",
                 shiny::tags$p(class="lead","Your gene of interest is annotated with the following Gene Ontology Terms"),
                 shiny::tags$br(),
                 # shiny::helpText("Since retrieving Gene Ontology information takes some time, please click on the button to start it."),
                 # shiny::actionButton("GOstart", "Get Gene Ontology information"),
                 shiny::tags$br(),
                 #HTML('<h4 class="text-primary">
                 #      <i class="fa fa-angle-down fa-fw"></i>How did the selected gene perform in your screen?</h4>'),
                 # Highcharts TABBED BOX
                 # Make a tabbed box for HC Plots
                 tabBox(width=12,
                        tabPanel("Cellular Component",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Tabular View"),
                                 shiny::tags$br(),
                                 dataTableOutput("indepth_GO_table1"),
                                 shiny::tags$br(),
                                 shiny::tags$hr(),
                                 shiny::tags$p(class="lead","Image View"),
                                 #downloadButton('download_GO_plot1', 'Download Image'),
                                  shiny::tags$br(),
                                imageOutput("indepth_GO_plot1", inline=TRUE)
                                 )
                 
                                 ),
                        tabPanel("Biological Process",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Tabular View"),
                                 shiny::tags$br(),
                                 dataTableOutput("indepth_GO_table2"),
                                 shiny::tags$br(),
                                 shiny::tags$hr(),
                                 shiny::tags$p(class="lead","Image View"),
                                 #downloadButton('download_GO_plot2', 'Download Image'),
                                 shiny::tags$br(),
                                 imageOutput("indepth_GO_plot2", inline=TRUE)
                                 )
                                 ),
                        tabPanel("Molecular Function",
                                 shiny::tags$div(align="center",
                                 shiny::tags$p(class="lead","Tabular View"),
                                 shiny::tags$br(),
                                 dataTableOutput("indepth_GO_table3"),
                                 shiny::tags$br(),
                                 shiny::tags$hr(),
                                 shiny::tags$p(class="lead","Image View"),
                                 #downloadButton('download_GO_plot3', 'Download Image'),
                                 shiny::tags$br(),
                                 imageOutput("indepth_GO_plot3", inline=TRUE)
                                 )
                                 )
                 )
                 
        )
        
        
        
        # #### KEGG Pathways to view
        # tabPanel("Pathway",
        #          shiny::tags$p(class="lead","Your gene is annotated with the following KEGG pathways"),
        #          shiny::tags$br(),
        #          shiny::helpText("Please select a pathway to view"),
        #          shiny::uiOutput("keggpathwayinput"),
        #          shiny::tags$br()
        #          
        #          
        #          
        # )
        
        ) #Tabbox
        ) # column
        ), # fluidrow
       
          
         
        
        
shiny::tags$div(width="100%", style="display:block;height:10%;"),
# load footer  
source(file.path(config$appDir, "footer.r"))[1]
) # close tab


