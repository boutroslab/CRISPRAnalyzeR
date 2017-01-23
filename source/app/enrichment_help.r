# help file
# save as enrichment_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#enrichment"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="enrichment", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Can I use several genes?"),
                                                                              shiny::tags$p(class="lead", "Yes, you can select single or multiple genes to start gene set analysis."),
                                                                              shiny::tags$img(src="images/enrichment_select.gif", class="img-responsive", width="300px")
                                                                       ),
                                                                       
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "How is the Gene Set Analysis performed?"),
                                                                              shiny::tags$p(class="text-justify", "Gene Set Analysis is carried out using the Enrichr API available at the ", shiny::tags$a(href="http://amp.pharm.mssm.edu/Enrichr", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Enrichr website</strong>.'))),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR presents you each analysis with the Enrichr Combined Score, which is a  combination of both the p-value and z-score:"),
                                                                              shiny::tags$code("c = log(p) * z"),
                                                                              
                                                                              shiny::tags$p(class="text-justify", "If you want to learn more about Enrichr, you can also check out the following publication:"),
                                                                              shiny::tags$pre("
Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR, Ma'ayan A. Enrichr:
interactive and collaborative HTML5 gene list enrichment analysis tool.
BMC Bioinformatics. 2013;128(14)

Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z, Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD, Gundersen GW, Ma'ayan A.
Enrichr: a comprehensive gene set enrichment analysis web server 2016 update.
Nucleic Acids Research. 2016; gkw377.")
                                                                       ),
                                                                       
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Pathways"),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR checks the following databases for pathway information:"),
                                                                              
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","WikiPathways 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.wikipathways.org/index.php/Download_Pathways", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Wiki Pathway Database</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","KEGG 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.kegg.jp/kegg/download/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>KEGG Database</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","Biocarta 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://cgap.nci.nih.gov/Pathways/BioCarta_Pathways", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Biocarta Database</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","Reactome 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.reactome.org/download/index.html", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Reactome Database</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","NCI-Nature 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://pid.nci.nih.gov/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>NCI Nature Database</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","Panther 2016"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.pantherdb.org/pathway/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Panther Database</strong>.')))
                                                                              )
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Transcription"),
                                                                              shiny::tags$p(class="text-justify", "Gene Set Analysis is performed using different databases for transcriptional information provided by the Enrichr service"),
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","ChEA 2015"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://amp.pharm.mssm.edu/lib/cheadownload.jsp", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Enrichr Download Page</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","TRANSFAC and JASPAR PWMs"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://jaspar.genereg.net/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>JASPAR Transcription Factor Binding Sites</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","ENCODE and ChEA Consensus TFs from ChIP-X"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://amp.pharm.mssm.edu/Enrichr/geneSetLibrary?mode=text&libraryName=ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Download Dataset from Enrichr</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","TargetScan microRNA Targets"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.targetscan.org/cgi-bin/targetscan/data_download.cgi?db=vert_61", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>TargetScan Website</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","Transcription Factor PPIs"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://amp.pharm.mssm.edu/Enrichr/geneSetLibrary?mode=text&libraryName=Transcription_Factor_PPIs", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Download Dataset from Enrichr</strong>.')))
                                                                              )
                                                                           
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Ontologies"),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzer uses Enrichr to get Gene Ontology Gene set analysis from the Gene Ontology Consortium."),
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","GO Biological Process 2015"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.geneontology.org/GO.downloads.annotations.shtml", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Gene Ontology Website</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","GO Cellular Component 2015"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.geneontology.org/GO.downloads.annotations.shtml", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Gene Ontology Website</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","GO Molecular Function 2015"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.geneontology.org/GO.downloads.annotations.shtml", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Gene Ontology Website</strong>.')))
                                                                              )
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Diseases"),
                                                                              shiny::tags$p(class="text-justify", "CRISPRAnalyzeR asks Enrichr to retrieve data from the Online Mendelian Inheritance in Man® Database."),
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","OMIM"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.omim.org/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Online Mendelian Inheritance in Man®</strong>.')))
                                                                              )
                                                                              
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Cell Types"),
                                                                              shiny::tags$p(class="text-justify", "Retrieve genetic information about cell lines."),
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","Cancer Cell Line Encyclopedia"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://www.broadinstitute.org/ccle/data/browseData", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>Cancer Cell Line Encyclopedia</strong>.'))),
                                                                                             shiny::tags$dt(class="text-left","NCI-60 Cancer Cell Lines"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://biogps.org/downloads/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>BioGPS Portal</strong>.')))
                                                                              )
                                                                              
                                                                       ),
                                                                       column(width=12,
                                                                              shiny::tags$h2(class="text-success", "Protein Interactions"),
                                                                              shiny::tags$p(class="text-justify", "You can get a protein interaction network using the String Database."),
                                                                              shiny::tags$dl(class="text-left",
                                                                                             shiny::tags$dt(class="text-left","String Database"),
                                                                                             shiny::tags$dd(class="text-left", shiny::tags$a(href="http://string-db.org/", target="blank", HTML('<i class="fa fa-external-link fa-fw text-success "></i> <strong>String Database</strong>.')))
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
