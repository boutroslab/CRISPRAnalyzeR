# help file
# save as sqFastq_help.r


fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left", 
                                                               HTML('<a data-toggle="collapse" href="#fastqqc"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="fastqqc", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       column(width=8, offset=2,
                                                                              shiny::tags$h3("What do the plots tell me?"),
                                                                              # IMAGE
                                                                              shiny::tags$p(class="lead", "CRISPRAnalyzeR provides you with a set of FASTQ quality plots once you uploaded FASTQ sequencing files"),
                                                                              shiny::tags$p(class="text-justify","CRISPRAnalyzeR uses the Rqc Bioconductor package to provide you with FASTQ quality information."),
                                                                              shiny::tags$pre("http://www.bioconductor.org/packages/release/bioc/vignettes/Rqc/inst/doc/Rqc.html
Souza W and Carvalho B (2016). Rqc: Quality Control Tool for High-Throughput Sequencing Data.
R package version 1.8.0, https://github.com/labbcb/Rqc.")
                                                                              
                                                                       ),
                                                                       
                                                                       column(width=6,
                                                                              shiny::tags$h3("Cycle-sepcific quality plots", class="text-success"),
                                                                              shiny::tags$h4("GC content"),
                                                                              shiny::tags$p(class="text-justify","Shows the GC-base content for each sequencing cycle"),
                                                                              shiny::tags$h4("Quality Distribution"),
                                                                              shiny::tags$p(class="text-justify","Shows the quality score proportion per cycle. Colors are presented in a gradient Red-Blue, where red identifies calls of lower quality."),
                                                                              shiny::tags$h4("Average Quality"),
                                                                              shiny::tags$p(class="text-justify","Describes the average quality score for each cycle of sequencing"),
                                                                              shiny::tags$h4("Basecall Proportion"),
                                                                              shiny::tags$p(class="text-justify","Describes the proportion of each nucleotide present at every cycle of sequencing.")
                                                                              
                                                                              
                                                                              
                                                                       ),
                                                                       column(width=6,
                                                                              shiny::tags$h3("Read Frequency and Width", class="text-success"),
                                                                              # IMAGE
                                                                              shiny::tags$h4("Read Frequency"),
                                                                              shiny::tags$p(class="text-justify", "Shows the proportion and frequency of sequencing reads."),
                                                                              shiny::tags$h4("Read Width"),
                                                                              shiny::tags$p(class="text-justify", "Describes the length of reads within the sequencing file.")
                                                                       )
                                                                       
                                                                       
                                                                       
                                                                )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                )
                                                
                                                
                                                
                                                )
                                
                                
                )
                
                
                
                )
         
         
         
         
         )
