# saved as footer.r




shiny::fluidRow(style="margin-top:100px",
  # google Analytics
  #shiny::tags$head(shiny::tags$script(src="js.cookie.js")),
  
  
  shiny::tags$br(),
  column(width=10,offset=1,
         # Footer with Logos and Google Analytics Opt-Out
         shiny::tags$hr(),
         column(width=4,
                shiny::tags$p(class="lead text-left", style="color:#4CAF50;", "CRISPRAnalyzeR"),
                # GA opt out
                # verbatimTextOutput("results"),actionButton("go","click me"))
                shiny::tags$p(class="text-left" , style="font:weight:200'", HTML("<a href='javascript:gaOptout()' >Click once to turn off Google Analytics</a>") ),
                # CRISPRAnalyzeR dependend shortcuts to help pages
                
                shiny::tags$p(class="text-left","How to use CRISPRAnalyzeR on your local machine"),
                shiny::tags$p(class="text-left","What data does CRISPRAnalyzeR store?"),
                shiny::tags$p(class="text-left","Help - I am lost!")
         ),
         column(width=4,
                # Other Resources
                shiny::tags$p(class="lead text-left", style="color:#4CAF50;", "Other Resources"),
                shiny::tags$p(class="text-left", HTML("<a href='http://www.e-crisp.org' target='_blank'>E-CRISP</a>")),
                shiny::tags$p(class="text-left", HTML("<a href='http://www.genomecrispr.org' target='_blank'>GenomeCRISPR</a>")),
                shiny::tags$p(class="text-left", HTML("<a href='http://www.genomernai.org' target='_blank'>GenomeRNAI</a>")),
                shiny::tags$p(class="text-left", HTML("<a href='http://www.boutroslab.org' target='_blank'>Boutroslab</a>"))
         ),
         column(width=4,
                # Logos
                shiny::tags$p(shiny::tags$br()),
                shiny::tags$p(
                  shiny::tags$img(src="./images/CRISPRAnalyzR_logo3_small.png", style="width:200px;")),
                shiny::tags$p(
                  shiny::tags$img(src="./images/genomecrispr_logo_blue_transparent.png", style="width:200px;")),
                shiny::tags$p(
                  shiny::tags$img(src="./images/logo_e_rgb.png", style="width:200px;"))
         )
         
         
  ))