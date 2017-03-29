##impressum
# sourced by ui.r

## save as imrpessum_ui.r

tabItem(tabName = "impressum", align = "center",
        
        
        fluidRow(style="width:80%;",
                 
                 HTML("
        <div class='section'>
      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right fa-fw'></i>About CRISPRAnalyzeR
                      <small>&nbsp;About us / Imprint</small>
                      </h1>
                      </div>
                      </div>
                      </div>
                      </div>"),
                 
                 shiny::tags$br(),
                 shiny::tags$hr(width="85%"),
                 
fluidRow(style="margin-top:50px;",
  column(width=10,offset=1,
         column(width=6,
                shiny::tags$p(class="lead", "This application is a non-commercial project, which is funded and hosted by the German Cancer Research Center (dkfz).", HTML("</br>"), 
                "It is developed by Jan Winter and Marc Schwering.")
                 ),
         column(width=6,
                shiny::tags$img(class="img-responsive", src="images/logo_e_rgb.png")
                )
         
         )
),

fluidRow(style="margin-top:50px;",
  column(width=10, offset=1,
         column(width=6,
                shiny::tags$div(class="media",
                                shiny::tags$div(class="media-left media-middle",
                                                shiny::tags$img(src='images/jwin.jpg', class='img-responsive img-thumbnail', width="200px")
                                ),
                                shiny::tags$div(class="media-body",
                                                shiny::tags$b("Jan Winter"), 
                                                #shiny::tags$br(),
                                                shiny::tags$p("PhD Student"),
                                                #shiny::tags$br(),
                                                shiny::tags$p("Division of Signaling and Functional Genomics"),
                                               # shiny::tags$br(),
                                                shiny::tags$p("jan.winter@dkfz-heidelberg.de"),
                                               # shiny::tags$br(),
                                                HTML("<a href='https://de.linkedin.com/in/jan-winter-726aab54'
                                                     target='blank'><i class='fa fa-3x fa-linkedin fa-fw'></i>LinkedIn</a>
                                                     <a href='https://twitter.com/winterj86' target='blank'><i class='fa fa-3x fa-twitter fa-fw'></i>Twitter</a>")
                                )
                )
         ),
                
         column(width=6,
                shiny::tags$div(class="media",
                                shiny::tags$div(class="media-left media-middle",
                                                shiny::tags$img(src='images/mschwering.jpeg', class='img-responsive img-thumbnail', width="200px")
                                ),
                                shiny::tags$div(class="media-body",
                                                shiny::tags$b("Marc Schwering"), 
                                                #shiny::tags$br(),
                                                shiny::tags$p("Master Student"),
                                                #shiny::tags$br(),
                                                shiny::tags$p("Division of Signaling and Functional Genomics"),
                                                #shiny::tags$br(),
                                                shiny::tags$p("m.schwering@dkfz-heidelberg.de"),
                                                #shiny::tags$br(),
                                                HTML("<a href='https://de.linkedin.com/in/marc-schwering-139914103'
                                                     target='blank'><i class='fa fa-3x fa-linkedin fa-fw'></i>LinkedIn</a>
                                                     <a href='https://twitter.com/schweringMarc' target='blank'><i class='fa fa-3x fa-twitter fa-fw'></i>Twitter</a>")
                                                )
                )
              )
         )
),

column(width=10, offset=1,
       shiny::tags$hr(),
  shiny::tags$p(class="lead", "
                  CRISPRAnalyzeR does not store personal information.
                  All uploaded data is deleted immediately once the browser is closed or the connection is lost for any reason.")
),
column(width=10,offset=1,
       shiny::tags$hr(),
  shiny::tags$h2(class="text-success text-left", "Impressum / Imprint (German Only)"),
  shiny::tags$h4(class="text-left","Angaben gemäß § 5 TMG:"),
  
  shiny::tags$p(class="text-left",
                "Deutsches Krebsforschungszentrum (dkfz)", shiny::tags$br(),
                "Im Neuenheimer Feld 280",shiny::tags$br(),
                "69120 Heidelberg", shiny::tags$br()
                ),
  shiny::tags$br(),
  shiny::tags$h4(class=" text-left", "Vertreten durch:"),
  shiny::tags$p(class="text-left", "Prof. Dr. Michael Baumann (wissenschaftlicher Vorstand)", shiny::tags$br(),
                "Prof. Dr. Josef Puchta (Kaufmännischer Vorstand)"),
  shiny::tags$br(),
  shiny::tags$h4(class=" text-left", "Kontakt:"),
  shiny::tags$p(class="text-left",
                "Telefon:", shiny::tags$br(), shiny::tags$strong("06221421953"), shiny::tags$br(),
                "Telefax:", shiny::tags$br(), shiny::tags$strong("06221421959") , shiny::tags$br(),
                "Email:", shiny::tags$br(), shiny::tags$strong("jan.winter@dkfz-heidelberg.de")
                ),
  shiny::tags$br(),
  shiny::tags$h4(class=" text-left", "Umsatzsteuer-Identifikationsnummer gemäß §27 a Umsatzsteuergesetz"),
  shiny::tags$p(class="text-left","DE 143293537")
),
column(width=10,offset=1,
       shiny::tags$hr(),
       shiny::tags$h2(class="text-success text-left", "What data is stored?"),
       shiny::tags$p(class="lead text-left",
                     "CRISPRAnalyzeR does not store personal information.
                     All uploaded data is deleted immediately once the browser is closed or the connection is lost for any reason."),
       shiny::tags$p(class="text-left",
                     "CRISPRAnalyzeR only stores uploaded data for the time you visit the website.", shiny::tags$br(),
                     "Moreover, CRISPRAnalyzeR logs the following information ", shiny::tags$strong("anonymously"), " for maintenance purposes:"
                     ),
       shiny::tags$p(class="text-left", "Occuring error messages, file names, selected gene identifier, number of uploaded files, submit button activity and timestamps")
),
column(width=10,offset=1,
       shiny::tags$hr(),
       shiny::tags$h2(class="text-success text-left", "Does CRISPRAnalyzeR use Google Analytics?"),
       shiny::tags$p(class="lead text-left",
                     "This website uses Google Analytics, to turn it off please ", HTML("<strong><a href='javascript:gaOptout()' >click here</a></strong>"))
       
),

column(width=10,offset=1,
       shiny::tags$hr(),
       shiny::tags$h2(class="text-success text-left",
                      "CRISPRAnalyzeR uses the following technologies"),
       shiny::tags$h3(class="text-left", "Tools"),
       shiny::tags$table(class="table", style="width:60%",
                         shiny::tags$thead(
                           shiny::tags$tr(
                             shiny::tags$th(""),
                             shiny::tags$th("Version")
                           )
                           
                         ),
                         shiny::tags$tbody(
                           shiny::tags$tr(
                             shiny::tags$td(shiny::tags$strong("Bowtie2")),
                             shiny::tags$td("2.29")
                           ),
                           shiny::tags$tr(
                             shiny::tags$td(shiny::tags$strong("Highcharts")),
                             shiny::tags$td("5.04")
                           ),
                           shiny::tags$tr(
                             shiny::tags$td(shiny::tags$strong("MAGeCK")),
                             shiny::tags$td("0.53")
                           ),
                           shiny::tags$tr(
                             shiny::tags$td(shiny::tags$strong("R")),
                             shiny::tags$td("3.32")
                           ),
                           shiny::tags$tr(
                             shiny::tags$td(shiny::tags$strong("")),
                             shiny::tags$td("")
                           )
                         )
       ),
      
       shiny::tags$h3(class="text-left", "R Packages"),
       tableOutput("sessioninfo")
       
       )

),
        
        
        
        # load footer  
        source(file.path(config$appDir, "footer.r"))[1]
)

