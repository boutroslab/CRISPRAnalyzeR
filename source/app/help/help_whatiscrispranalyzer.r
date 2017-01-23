# save as help_whatiscrispranalyzer.r
shiny::tags$div(class="panel-body",
                # here comes the content
                column(width=6,
                       ## CRISPRAnalyzeR Description
                       shiny::tags$p(class="lead", "Explore your Data, Explore your Analysis."),
                       shiny::tags$p(class="text-justify",
                                     "CRISPRAnalyzeR is a web-based analysis platform for pooled CRISPR screens.",
                                     "It is easy-to-use and provides you with an extensive data analysis that includes everything you need to know - without the need to install any software.",
                                     "You will be guided through all the steps and in case you need help - there is a help on every page.",
                                     "And once you are finished, you can download all the data as well as your analysis as an interactive HTML file."),
                       shiny::tags$br(),
                       shiny::tags$p(class="text-justify",
                                     "Data analysis is fun when you can literally play with your data - and CRISPRAnalyzeR gives you exactly that opportunity.
                                     Since we also "),
                       shiny::tags$br(),
                       shiny::tags$p(class="text-justify",
                                     "In case you don't like uploading screening data, you can use CRISPRAnalyzeR as a local installation within your lab.",
                                     "Just head to the help page, download a Docker container and put it on a compatible machine."),
                       shiny::tags$p(class="lead","It's not just easy to use - it also comes packed with loads of features"),
                       
                       shiny::tags$ul(class="list-group", style="width:80%;",
                                      shiny::tags$li(class="list-group-item", "Fully interactive and Web-based Interface"),
                                      shiny::tags$li(class="list-group-item", "Easy-to-use, streamlined Workflow"),
                                      shiny::tags$li(class="list-group-item", "FASTQ/NGS Rawdata Quality Information"),
                                      shiny::tags$li(class="list-group-item", "Screening Quality Information"),
                                      shiny::tags$li(class="list-group-item", "6 different Hit-Calling methods"),
                                      shiny::tags$li(class="list-group-item", "Explorative Analysis with in-depth information about Genes and sgRNAs"),
                                      shiny::tags$li(class="list-group-item", "Annotate your gene with additional information"),
                                      shiny::tags$li(class="list-group-item", "Automatic re-Annotation of your sgRNAs"),
                                      shiny::tags$li(class="list-group-item", "Downloadable, fully-interactive Report"),
                                      shiny::tags$li(class="list-group-item", "... and much more")
                                      
                                      
                       )
                       ),
                column(width=6,
                       ## CRISPRAnalyzeR Workflow
                       shiny::tags$img(class="img-responsive", src="/images/workflow3.png")
                )
                
)