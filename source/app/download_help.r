# save as download_help.r

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left text-success", 
                                                               HTML('<a data-toggle="collapse" href="#download"><i class="fa fa-question fa-fw"></i> Click here for help</a>')
                                                )
                                ),
                                shiny::tags$div(id="download", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       
                                                                       shiny::tags$h2("What is included in the report?", class="text-success"),
                                                                       #shiny::tags$img(src="", class="img-responsive"),
                                                                       shiny::tags$p(class="text-justify", "CRISPRAnalyzeR automatically includes all plots and data of the screen quality section and the hit calling section.",
                                                                                     "For all additional information, you can specifically add those information by clicking on the 'Add to Report' button to tell CRISPRAnalyzeR that you would like to have this included in the report."),
                                                                       shiny::tags$img(src="images/addtoreport.png", class="img-responsive", width = "100px"),
                                                                       shiny::tags$p(class="lead", "Tell CRISPRAnalyzeR which additional information should be added to the report"),
                                                                       
                                                                       shiny::tags$h3("You can select which information is included in the report", class="text-success"),
                                                                       shiny::tags$p(class="text-justify", "The download page will tell you which information was be added to the report. Here you also have the chance to include or not include complete sections, which can be usefull if you would like to generate a report that only contains specific parts of the analysis."),
                                                                       shiny::tags$img(class="img-responsive", src="images/report_select_qc.png")
                                                                       
                                                                ),
                                                                column(width=12,
                                                                       
                                                                       shiny::tags$h2("Can I add additional information to the report?", class="text-success"),
                                                                       shiny::tags$p(class="text-justify", "In step2, you can add additional screening information and comments to the report, such as a screening title, the screening workflow procedure, a hypothesis, the used cell line and many more.",
                                                                                     "We advise you to include as much information to have a complete documentation of the screen."),
                                                                       shiny::tags$img(class="img-responsive", src="images/report_information.png", width="70%")
                                                                ),
                                                                column(width=12,
                                                                       
                                                                       shiny::tags$h2("How can I download my report?", class="text-success"),
                                                                       shiny::tags$p(class="text-justify", "The report is generated once you click on the 'Create HTML Report' button at the bottom of the page.",
                                                                                     "The report generation will take some time, dependend on the amount of included data and plots - please be patient.",
                                                                                     "In the meantime you can use CRISPRAnalyzeR, as the report is generated in the background and will be downloaded to your device automatically."),
                                                                       shiny::tags$img(class="img-responsive", src="images/report_button.png", width="70%")
                                                                       
                                                                )
                                                                
                                                                
                                                                ###############
                                                                ### END OF HELP PAGE INSERT
                                                )
                                )
                )
         )
)
