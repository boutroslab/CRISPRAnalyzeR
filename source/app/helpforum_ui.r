

tabItem(tabName = "help_forum", align = "center",
        
        shinyjs::useShinyjs(),
        
        ## CSS
        #tags$head(
        #  tags$style(HTML('#help_submit{background-color:#4682B4; font-weight:bold; color:white}'))
        #),
        
  fluidRow(style="width:80%;",
       HTML(" <div class='container'>
        <div class='row'>
            <div class='col-md-12'>
            <h1 class='text-success'>
            <i class='fa fa-angle-double-right  fa-fw'></i>Help&nbsp;
            <small>&nbsp;Google Forum</small>
            </h1>
            <hr>
            <p class='lead'>Our main goal was to keep CRISPRAnalyzeR easy-to-use and straight-forward.
              <br>But - to be honest - with each new user things might come up that might
            not be clear to the user or just don't work as intended.
            <br>We apologize for any inconvenience and ask you to please let us know what
            struggled you.
            <br>
            <br>To provide you a fast and convenient way of asking questions, please join
            our Google Forum below.</p>
            </div>
            </div>
            </div>
            "),
        
        HTML("<iframe id='forum_embed'
  src='javascript:void(0)'
             scrolling='no'
             frameborder='0'
             width='900'
             height='700'>
             </iframe>
             <script type='text/javascript'>
             document.getElementById('forum_embed').src =
             'https://groups.google.com/forum/embed/?place=forum/crispr-analyzer'
             + '&showsearch=true&showpopout=true&showtabs=false'
             + '&parenturl=' + encodeURIComponent(window.location.href);
             </script>"),
       br()
        ),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
        
        
        
        
        ) # close tab
