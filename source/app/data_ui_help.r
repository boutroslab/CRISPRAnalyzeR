# help file
# save as data_ui_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#data_ui"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                               )
                                                ),
                                shiny::tags$div(id="data_ui", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                

                                                                column(width=12,
                                                                       shiny::tags$h2("Sample Data", class="text-success"),
                                                                       shiny::tags$p(class="text-justify","In case you like to try out CRISPRAnalyzeR without your own data, we have some sample data ready for you.
                                                                                     All you need to do is download one of the sample data files,
                                                                                     extract them - everything you need is included. Each data package consists of a sgRNA library file (.fasta) and NGS rawd ata (.fastq.gz) or read count files (.txt)."),
                                                                       shiny::tags$h3("Focused Screen (12000 sgRNAs, custom library)"),
                                                                       shiny::tags$p(class="lead", " This 12k custom sgRNA library was published before in"),
                                                                       shiny::tags$pre("
F. Heigwer*, T. Zhan*, M. Breinig, J. Winter, D. Brügemann, S. Leible, M. Boutros,
CRISPR library designer (CLD): software for multispecies design of single guide RNA libraries,
Genome Biol., 2016, DOI:10.1186/s13059-016-0915-2
                                                                                       "),
                                                                       column(width=6,
                                                                              shiny::tags$h4("Sample Data with Read Count Files"),
                                                                              shiny::tags$p("In this package you will find 4 readcount files and a sgRNA library FASTA
                                                                                            file."),
                                                                              shiny::tags$table(class="table table-striped",
                                                                                             
                                                                                             shiny::tags$thead(
                                                                                               shiny::tags$th("File Name"),
                                                                                               shiny::tags$th("Description")
                                                                                             ),
                                                                                             shiny::tags$tbody(
                                                                                               shiny::tags$tr(
                                                                                                 shiny::tags$td("PBS-Replicate1.txt"),
                                                                                                 shiny::tags$td("Read Count file for untreated replicate 1")
                                                                                               ),
                                                                                               shiny::tags$tr(
                                                                                                 shiny::tags$td("PBS-Replicate2.txt"),
                                                                                                 shiny::tags$td("Read Count file for untreated replicate 2")
                                                                                               ),
                                                                                               shiny::tags$tr(
                                                                                                 shiny::tags$td("TRAIL-Replicate1.txt"),
                                                                                                 shiny::tags$td("Read Count file for TRAIL-treated replicate 1")
                                                                                               ),
                                                                                               shiny::tags$tr(
                                                                                                 shiny::tags$td("TRAIL-Replicate2.txt"),
                                                                                                 shiny::tags$td("Read Count file for TRAIL-treated replicate 2")
                                                                                               ),
                                                                                               shiny::tags$tr(
                                                                                                 shiny::tags$td("pilotscreen.fasta"),
                                                                                                 shiny::tags$td("sgRNA library FASTA file with 12000 sgRNAs")
                                                                                               )
                                                                                             )
                                                                              ),
                                                                              HTML('<a class="btn btn-block btn-danger btn-md" href="https://cdn.rawgit.com/boutroslab/CRISPRAnalyzeR/f77fdab0/sampledata/caR_Readcount_sample-data.zip"
                                                                                   target="_blank"><i class="fa fa-download fa-fw"></i>Download Read Count Package&nbsp;&nbsp;<span class="badge">0.5 MB</span></a>')
                                                                              
                                                                              
                                                                              ),
                                                                       column(width=6,
                                                                              shiny::tags$h4("Sample Data with NGS FASTQ.gz Files"),
                                                                              shiny::tags$p("In this package you will find 4 .fastq.gz NGS files and a sgRNA library FASTA
                                                                                            file."),
                                                                              shiny::tags$table(class="table table-striped",
                                                                                                
                                                                                                shiny::tags$thead(
                                                                                                  shiny::tags$th("File Name"),
                                                                                                  shiny::tags$th("Description")
                                                                                                ),
                                                                                                shiny::tags$tbody(
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("PBS-Replicate1.fastq.gz"),
                                                                                                    shiny::tags$td("NGS FASTQ.gz file for untreated replicate 1")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("PBS-Replicate2.fastq.gz"),
                                                                                                    shiny::tags$td("NGS FASTQ.gz file for untreated replicate 2")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("TRAIL-Replicate1.fastq.gz"),
                                                                                                    shiny::tags$td("NGS FASTQ.gz file for TRAIL-treated replicate 1")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("TRAIL-Replicate2.fastq.gz"),
                                                                                                    shiny::tags$td("NGS FASTQ.gz file for TRAIL-treated replicate 2")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("pilotscreen.fasta"),
                                                                                                    shiny::tags$td("sgRNA library fasta file with 12000 sgRNAs")
                                                                                                  )
                                                                                                )
                                                                              ),
                                                                              HTML('<a class="btn btn-block btn-danger btn-md center" href="http://www.dkfz.de/signaling/crispranalyzer/CRISPRAnalyzeR_NGSFASTQ_sample-data.zip"
                                                                                   target="_blank"><i class="fa fa-download fa-fw"></i>Download Raw Data Package&nbsp;&nbsp;<span class="badge">1.38 GB</span></a>')
                                                                              ),
                                                                       column(width=12,
                                                                              shiny::tags$br(),
                                                                              shiny::tags$hr(width="50%"),
                                                                              
                                                                              shiny::tags$h3("Whole Genome Essential Genes Screen (90000 sgRNAs, TKOv1 library)", style="padding-top:20px;"),
                                                                              shiny::tags$p(class="lead", "This screening data was published in"),
                                                                              shiny::tags$pre("
Steinhart,Z. et al. (2016)
Genome-wide CRISPR screens reveal a Wnt-FZD5 signaling circuit as a druggable vulnerability of RNF43-mutant pancreatic tumors.
Nat. Med.
                                                                                              "),
                                                                              shiny::tags$h4("Sample Data with Read Count Files"),
                                                                              shiny::tags$p("In this package you will find 3 read count files and a sgRNA library fasta
                                                                                            file."),
                                                                              shiny::tags$table(class="table table-striped",
                                                                                                shiny::tags$thead(
                                                                                                  shiny::tags$th("File Name"),
                                                                                                  shiny::tags$th("Description")
                                                                                                ),
                                                                                                shiny::tags$tbody(
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("TKO_readcount_initial1"),
                                                                                                    shiny::tags$td("Read Count file for Day 0")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("TKO_readcount_final1"),
                                                                                                    shiny::tags$td("Read Count file after 20 cell doublings (replicate 1)")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("TKO_readcount_final2"),
                                                                                                    shiny::tags$td("Read Count file after 20 cell doublings (replicate 2)")
                                                                                                  ),
                                                                                                  
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("FASTA_TKO_90K_library.fasta"),
                                                                                                    shiny::tags$td("sgRNA library FASTA file for this screen with 90000 sgRNAs")
                                                                                                  )
                                                                                                )
                                                                              ),
                                                                              HTML('<a class="btn btn-block btn-danger btn-md" href="https://cdn.rawgit.com/boutroslab/CRISPRAnalyzeR/f77fdab0/sampledata/CRISPRAnalyzeR_TKO_FZD5_CRISPR_SampleData.zip"
                                                                                   target="_blank"><i class="fa fa-download fa-fw"></i>Download TKO Read Count Package&nbsp;&nbsp;<span class="badge">4.1 MB</span></a>')
                                                                              
                                                                              
                                                                              
                                                                              )
                                                                       
                                                                              ),




column(width=12,  
       shiny::tags$br(),
       shiny::tags$hr(),
       shiny::tags$br(),
       shiny::tags$h2("Data Upload", class="text-success"),
       shiny::tags$p(class="lead", "You can upload NGS FASTQ.gz sequencing data or Read Count Files (.txt) in addition to your sgRNA library file (.fasta)."),
column(width=6,
       shiny::tags$h3(class="text-success","sgRNA Library File in FASTA format"),
         shiny::tags$p(class="text-justify" ,"The sgRNA library file must be provided in a ", HTML("<i>FASTA</i>")," format.
       This library file is not only used to identify the genes to which your
       sgRNAs belong but also allows CRISPRAnalyzeR to annotate your screen correctly."),
       shiny::tags$p(class="text-justify", style="font-weight:bold;","Example files for the most common sgRNA libraries can be downloaded from below at the end of the help section."
         ),
       shiny::tags$pre(class="text-left",
">ENSG00000006042_0_4299.561
GGAGCCCTCTGAGTTAGAAC")
       ),
column(width=6,
       shiny::tags$h3(class="text-success","Sequencing Data (NGS .FASTQ files)"),
       shiny::tags$p(class="text-justify" ,"Your screening data can be uploaded either as ", HTML("<strong>gzipped NGS FASTQ</strong> (<i>.fastq.gz</i>)"), "
                     or ", HTML("<b>tab-separated Readcount</b>(<i>.txt)</i>") ,"files.",
                     "In case you upload FASTQ files, CRISPRAnalyzeR uses bowtie2 to map the
                     FASTQ reads against the provided sgRNA library file. More advanced users can adjust
                     bowtie2 parameters in the FASTQ Options Box at Step 3.",
                     "Since FASTQ files are large, we would ask you only upload gzip-compressed files as they are usually
                     provided by your sequencing machine. In this case, the files should end
                     with .fastq.gz. Otherwise, please compress them using gzip first."
       )
  ),
column(width=6,
       shiny::tags$h3(class="text-success", "Tab-Separated Read Count Files"),
       shiny::tags$p(class="text-justify",
                     "You can upload tab-separated read count files, in which every line contains a unique sgRNA identifier as well as the number of reads assigned to it.",
                     "Please note that the sgRNA identifiers must be the same as in the sgRNA library file you provide.",
                     "Since CRISPRAnalyzeR performes the the normalization, you can upload raw read count files."
                     ),
       shiny::tags$p("Example of a tab-separated read count file:"),
       shiny::tags$pre(class="text-left",
"sgRNA Count
AAK1_104_0 0
AAK1_105_0 597
AAK1_106_0 145
AAK1_107_0 0
AAK1_108_0 0
AAK1_109_0 142"
       )

       ),
column(width=12,
       shiny::tags$h3(class="text-success","Extraction of your uploaded FASTQ.gz files"),
       shiny::tags$p(class="text-justify" ,"In case you upload FASTQ files instead of Read count files, your FASTQ
                     data is extracted before the identification of your single sgRNA reads.
                     For this purpose, CRISPRAnalyzeR needs to know how your FASTQ files do look like
                     and how it can identify the barcode of each single sgRNA.",
                     "By default, this barcode is the 20 nt target sequence as it is defined in the sgRNA library file
                     you uploaded.",
                     "To extract this information from your sequencing data, CRISPRAnalyzeR requires a so-called regular expression."
       ),shiny::tags$br(),
       shiny::tags$p(class="text-justify lead","For your convenience, CRISPRAnalyzeR already provides you with pre-defined settings for the most common libraries and vector systems.
                     As an alternative for more advanced users, you can also type in your own regular expression.
                     "
       )
       )

       

       
),






column(width=12,
       shiny::tags$br(),
       shiny::tags$hr(),
       shiny::tags$br(),
        shiny::tags$h2("Pre-Made sgRNA library files for download", class="text-success"),
        shiny::tags$p(class="lead", "CRISPRAnalyzeR offers you pre-made sgRNA library fasta files for commonly used CRISPR libraries."),
        shiny::tags$p("Please email us if you want your sgRNA library file to be distributed with CRISPRAnalyzeR."),
        
        # Table with files
        shiny::tags$table(class="table",
                          shiny::tags$thead(
                            shiny::tags$tr(
                              shiny::tags$th("Library Name"),
                              shiny::tags$th("Lab"),
                              shiny::tags$th("Pubmed ID"),
                              shiny::tags$th("Addgene"),
                              shiny::tags$th("Download")
                            )
                          ),
                          shiny::tags$tbody(
                            # Boutros Data
                            shiny::tags$tr(
                              shiny::tags$td("CLD Benchmarking"),
                              shiny::tags$td("Boutros"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/27013184", target="_blank", "27013184")),
                              shiny::tags$td("NA"),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_CLD_library.fasta" target="_blank">FASTA</a>'))
                            ),
                            # Gecko V2
                            shiny::tags$tr(
                              shiny::tags$td("Gecko V2"),
                              shiny::tags$td("Zhang"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/25075903", target="_blank", "25075903")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/crispr/libraries/geckov2/" target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_all.fasta" target="_blank">A+B FASTA</a>'),HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_HGLib_A.fasta" target="_blank">A only FASTA</a>') , HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_HGLib_B.fasta" target="_blank">B only FASTA</a>'))
                            ),
                            
                            # GeckoV2 MOUSE
                            shiny::tags$tr(
                              shiny::tags$td("Gecko V2 MOUSE"),
                              shiny::tags$td("Zhang"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/25075903", target="_blank", "25075903")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/pooled-library/zhang-mouse-gecko-v2/" target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              #shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_all.fasta" target="_blank">A+B FASTA</a>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_MGLib_A.fasta" target="_blank">A only FASTA</a>'), HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_GeckoV2_MGLib_B.fasta" target="_blank">B only FASTA</a>'))
                            ),
                            
                            # TKO
                            shiny::tags$tr(
                              shiny::tags$td("Torronto KnockOut Library (TKO)"),
                              shiny::tags$td("Moffat"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/26627737", target="_blank", "26627737")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/pooled-library/moffat-crispr-knockout/"
                                                  target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('         <a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_TKO_90K_library.fasta" target="_blank">90K FASTA</a>
                                                  <a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_TKO_85Ksupp_library.fasta" target="_blank">85K FASTA</a>'))
                              ),
                            
                            # Brunello
                            shiny::tags$tr(
                              shiny::tags$td("Brunello"),
                              shiny::tags$td("Doench"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/26780180", target="_blank", "26780180")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/pooled-library/broadgpp-crispr-knockout/"
                                                  target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_brunello_library.fasta" target="_blank">FASTA</a>'))
                              ),
                            
                            
                            
                            # CrisprA/I
                            shiny::tags$tr(
                              shiny::tags$td("CRISPRa / CRISPRi"),
                              shiny::tags$td("Weissmann"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/25307932", target="_blank", "25307932")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/crispr/libraries/" target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs ">not available yet</a>'))
                            ),
                            
                            
                            # Sabatini
                            shiny::tags$tr(
                              shiny::tags$td("Human Lentiviral sgRNA library high cleavage activity"),
                              shiny::tags$td("Sabatini"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/26472758", target="_blank", "26472758")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/crispr/libraries/" target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs " href="https://github.com/boutroslab/CRISPRAnalyzeR/blob/master/fasta/FASTA_Wang_v2__185K_library.fasta" target="_blank">185K FASTA</a>'))
                            ),
                            
                            
                            # Sabatini
                            shiny::tags$tr(
                              shiny::tags$td("Human Lentiviral sgRNA sub libraries"),
                              shiny::tags$td("Sabatini"),
                              shiny::tags$td(shiny::tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/24336569", target="_blank", "24336569")),
                              shiny::tags$td(HTML('<a href="https://www.addgene.org/crispr/libraries/" target="_blank"><i class="fa fa-external-link fa-fw text-success "></i>')),
                              shiny::tags$td(HTML('<a class="btn btn-success btn-xs ">not available yet</a>'))
                            )
                            
                              )
                          
                          
                          
                          
          )# end of table
),


# sgRNA indeph help
column(width=12,
       shiny::tags$br(),
       shiny::tags$hr(),
       shiny::tags$br(),
       
       shiny::tags$h2("sgRNA Library - How To", class="text-success"),
       shiny::tags$p(class="lead", "sgRNA library files must be in FASTA format and include a unique sgRNA
       identifier as well as the guide target sequence (typically 19-21 nt) in ",HTML("5'-&gt;3'"), " direction."),
       shiny::tags$p("An example of a FASTA file structure"),
       shiny::tags$pre(class="text-left",
">ENSG00000006042_0_4299.561
GGAGCCCTCTGAGTTAGAAC 
>ENSG00000006042_5_4299.588
GAAGATGCCTCGTAAGGCCA 
>ENSG00000006042_6_4299.588
GAGATGCCTCGTAAGGCCAT 
"),
       
       column(width=6,
              shiny::tags$h4("sgRNA library file structure"),
              shiny::tags$p(class="text-justify",
                            "A single item within the FASTA file consists of two lines. In this case it is the the sgRNA identifier with its target
                            sequence.
                            The first line is used to identify the item. It always starts with a
                            ",shiny::tags$strong(">"), "followed by the sgRNA identifier.
                            The second line provides the corresponding sgRNA target sequence
                             ",shiny::tags$strong("without the PAM sequence"),".")
              
              ),
       column(width=6,
              shiny::tags$h4("How to select the correct Regular Expressions for sgRNA libraries"),
              shiny::tags$p(class="text-justify",
                            "Regular Expressions are used to extract the gene identifier from your sgRNA identifier.
                            This is important as we want to provide you not only a gene-based analysis,
                            but also additional visualizations and information about your candidate
                            gene.
                            To find a matching regular expression, you need to know the structure
                            of such an expression and of course, the structure of your sgRNA identifier
                            as it is used within your library file.
                            ", shiny::tags$strong("Regular expressions for our pre-made libraries can be found below"), "."),
              shiny::tags$br(),
              shiny::tags$p(class="lead", "CRISPRAnalyzeR provides you with pre-defined regular expressions for commonly used vector systems. Alternatively, see below how to find the correct expression for your custom library.")
              ),
       column(width=12,
       shiny::tags$div(class="media",
                       shiny::tags$div(class="media-left media-middle",
                                       shiny::tags$img(class="img-responsive", src = "images/regex1.png", style="width:100%;"),
                                       shiny::tags$img(class="img-responsive", src = "images/regex2.png", style="width:100%;")
                                        ),
                       shiny::tags$div(class="media-body",
                                       shiny::tags$h3(class="text-success", "How to find the right Regular Expression"),
                                       shiny::tags$p("A Regular Expression is used to detect both the gene and sgRNA identifier
       for each sgRNA within your library file."),
                                       shiny::tags$p("Note the brackets which are necessary. Regular
                                                     Expression for the pre-made library FASTA files are provided below.")
                       )
                       
                       
                       )
       ),
       
       shiny::tags$hr(width="50%", style="padding:2%"),
       
      
      column(width=12,
                    shiny::tags$h2(class="text-success", "NGS Sequencing Files (.fastq.gz)"),
                    shiny::tags$p(class="text-justify", 
                                  "You can also provide raw data NGS files directly from your sequencing machine in a FASTQ format. Since FASTQ data can be large, the files which must be compressed using GZIP (.fastq.gz file ending).
                                  CRISPRAnalyzeR extracts all information from your FASTQ NGS file and performs the mapping against the sgRNA library file.",
                                  shiny::tags$b("Please make sure the FASTQ extraction pattern in Step3 matches the vector system you used for screening.")
                                  ),
                    shiny::tags$p(class="text-left", "Example of a FASTQ file"),
                    shiny::tags$pre(class="text-left",
"
@M01100:63:000000000-AMJC7:1:1101:21768:1401 1:N:0:1
TTGGATTCTTGTGGAAAGGACGAAACACCGTTCCCTGCAGCCCTCATGCGTTTTAGAGCTAGAAAT
+
CCCCCGGGGGGGGGGGGGGGGGGFGGGGGGGGGGGGGGFGFDGGGGGGGGGGGGGGGGG
"
                    ),
                    
shiny::tags$b("Please provide FASTQ files in a gzipped format (.fastq.gz)"),
                    
                   shiny::tags$h3(class="text-success"," CRISPRAnalyzeR needs to know which plasmid you used"),
                   shiny::tags$p(class="text-justify",
                                 "In order to find the sgRNA target sequence (which is used as a barcode to identify the sgRNA), CRISPRAnalyzeR needs to know where to find it within your sequencing data.
                                 For this purpose, regular expressions are used to extract this information
                                 from your sequencing rawdata.",
                                 shiny::tags$br(),
                                 "You can have a look at the plasmid map of the vector you used and try
                                 to identify the left and right flanking sequences next to the sgRNA target
                                 sequence (as illustrated below).",
                                 shiny::tags$b("Alternatively you can use one of the pre-defined settings depending on your
                                 vector system.")
                   ),
                   shiny::tags$p(class="lead", "You can find the required regular expression for your screening vector the following way"),
                   shiny::tags$img(src='images/U6-Regex.png', class='img-responsive'),
                   shiny::tags$p(class="text-justify", 
                                 "Regular Expressions are used to extract the sgRNA target sequence barcode from your NGS FASTQ file.
                                 To find a matching regular expression, you need to know the sequence or
                                 name of the vector you used for performing the lentiviral transduction.
                                 All you need to do is select a couple of bases flanking your sgRNA target
                                 sequence on the left and right side.",
                                 shiny::tags$b("CRISPRAnalyzeR provides with pre-defined settings for the most common screening systems (see below), but you can use your custom system as well."),
                                 shiny::tags$br(),
                                 "In this case, select CUSTOM in Step3 and enter a custom regular expression by the addition of
                                 2-4 bases of both the left and right flanking sequences on the left and
                                 right site of the bracket. For the above shown example you could use the following regular expression for a sgRNA target sequence of either 20 or 21 nt in length:"
                                 ),
                   shiny::tags$p(class="text-center",
                                 shiny::tags$span("The expression to detect the sgRNA target sequence is shown in blue color. Don't hesitate to ask us for help."),
                                 shiny::tags$span(style="font-color: red;", "CACC"),
                                 shiny::tags$span(style="font-color: blue;", "(.{20,21})"),
                                 shiny::tags$span(style="font-color: red;", "GTTTT")
                                 ),
                   shiny::tags$br(),
shiny::tags$p(class="lead",
              "You can modify the pre-defined settings or add your own custom regular expression"),
shiny::tags$p("Just tick the checkbox and enter a custom regular expression."),

                    shiny::tags$br(),
                   shiny::tags$p(class="lead",
                                 "You can use the pre-defined settings for the most common screening vectors:"),
                   
                   shiny::tags$table(class="table",
                     shiny::tags$thead(
                       shiny::tags$th("Plasmid Name"),
                       shiny::tags$th("Lab"),
                       shiny::tags$th("Addgene ID"),
                       shiny::tags$th("Expression required for Step3")
                     ),
                     shiny::tags$tbody(
                       shiny::tags$tr(
                         shiny::tags$td("Lenticrisp V2"),
                         shiny::tags$td("Zhang"),
                         shiny::tags$td("52961"),
                         shiny::tags$td("Default Setting", shiny::tags$code("ACC(.{20,21})G"))
                       ),
                       shiny::tags$tr(
                         shiny::tags$td("Lentiguide (Puro)"),
                         shiny::tags$td("Zhang"),
                         shiny::tags$td("52963"),
                         shiny::tags$td("Default Setting", shiny::tags$code("ACC(.{20,21})G"))
                       ),
                       shiny::tags$tr(
                         shiny::tags$td("Human Lentivirus Library V1"),
                         shiny::tags$td("Haoquan Wu"),
                         shiny::tags$td("69763"),
                         shiny::tags$td(shiny::tags$code("GTTT(.{20})GT"))
                       ),
                       shiny::tags$tr(
                         shiny::tags$td("pLCKO (TKO Library)"),
                         shiny::tags$td("Moffat"),
                         shiny::tags$td("73311"),
                         shiny::tags$td(shiny::tags$code("ACCG(.{20,21})G"))
                       ),
                       shiny::tags$tr(
                         shiny::tags$td("pU6-sgRNA EF1Alpha-puro-T2A-BFP (CRISPRa/i)"),
                         shiny::tags$td("Weissman"),
                         shiny::tags$td("60955, 62217, 60956"),
                         shiny::tags$td("Default Setting", shiny::tags$code("GTTG(.{20})G"))
                       )
                     )
                     
                   )
                   
             ) 
       
       
       
       
       
       
       
       )







                                                                ### END OF HELP PAGE INSERT
                                                                 )
                                                
                                                
                                                
                                                )
                                
                                
                                )
                
                
                
                )
  
  
  
  
)

