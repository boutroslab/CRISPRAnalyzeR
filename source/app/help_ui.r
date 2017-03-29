# sourced by 'ui.r'
# save as 'help_ui.r'
# ui elements for help tab




tabItem(tabName = "help_ticket", align = "center",

  shinyjs::useShinyjs(),
  
  ## CSS
  #tags$head(
  #  tags$style(HTML('#help_submit{background-color:#4682B4; font-weight:bold; color:white}'))
  #),

  fluidRow(style="width:80%;",
    HTML("<div class='section'>
          <div class='container'>
         <div class='row'>
         <div class='col-md-12'>
         <h1 class='text-success'>
         <i class='fa fa-angle-double-right  fa-fw'></i>Help&nbsp;
         <small>&nbsp;Ask us for help - send us a ticket</small>
         </h1>
         <hr>
         <p class='lead'>Our main goal was to keep CRISPRAnalyzeR easy-to-use and straight-forward.
         <br>We apologize for any inconvenience and aks you to please let us know what
         struggled you.
         <br>
         <br>Just send us a ticket with your
         <b>Name, Email-Adress and some description</b> and we will get back to you
         as soon as possible.
         <br>In the meantime you can join the Google Forum or read the FAQs.</p>
         </div>
         </div>
         </div>
         </div>")
  ),
  
  uiOutput("help_inputForm", width="80%"),
  
  actionButton("help_submit", "Submit", icon("exclamation")),
  helpText("You can only submit one ticket."),
  
  
  br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
)# close tab

