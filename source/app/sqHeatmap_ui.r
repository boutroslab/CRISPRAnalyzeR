# sourced by 'ui.r'
# save as 'sqHeatmap_ui.r'
# ui elements 'Heatmap' sub tab of 'Screen Quality' tab


tabItem(tabName = "sq_heatmap", align = "center",

  shinyjs::useShinyjs(),
  
  #shiny::tags$head(
  #  shiny::tags$style(HTML('#sqHeatmap_go{background-color:#4682B4; font-weight:bold; color:white}'))
  #),
    

  ## Welcome message
  fluidRow(style="width:80%;",
           HTML("<div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>Screen Quality
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Heatmap Visualization</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>Large datasets can be visualized using a heatmap, which gives you an overview
                of how your data looks like.
                <br>CRISPRAnalyzeR offers different heatmaps to visualize your data.</p>
                </div>
                </div>
                </div>
                </div>")
    
    
    
  ),
  # HELP as including of sqheatmap_help.r
  source(file.path(config$appDir, "sqHeatmap_help.r"))[1],
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  
  fluidRow(
    column(width=10,offset=1,
    box(title = "Step 1: Adjust the heatmap settings", width = 12, solidHeader = TRUE, collapsible=TRUE, status = "primary",
        
        shiny::tags$p(class="lead", "You can adjust the heatmap using various parameters"),
        shiny::tags$p("You can download the plot via the upper right menu within the plot and add it to your CRISPRAnalyzeR Report by clicking on the Add Report button."),
        
      column(width=4,
             shiny::tags$div(
              
               selectInput("sqHeatmaps_show", "Please select the type of information", choices = list( 
                 "Gene Abundance" = "geneabundance", "Gene Readcount" = "genereadcount", "Gene Top/Lowest 5%" = "genethreshold",
                 "sgRNA Abundance" = "sgrnaabundance", "sgRNA Readcount" = "sgrnareadcount")),
               helpText("See Additional Information for a detailed explanation of all settings.")
             )),
      column(width=4,
             shiny::tags$div(
               
               radioButtons("sqHeatmaps_type", "Please select the direction of effect", choices = list("enriched", "depleted"), inline = TRUE),
               helpText("In case you selected Gene Top/Lowest 5%, you can set whether you want to see TOP (enriched) or LOWEST (depleted) genes.")
             )),
      column(width=4,
             shiny::tags$div(
               checkboxGroupInput("sqHeatmap_checkboxes", "Log10", selected = "log", choices = list("show on decadic logarithm" = "log")),
               helpText("This is useful if you have some outliers. With the logged color scale you will not only see those.")
               )
             ),
      column(width=12,
             uiOutput("heatmap_error"),
             div(style="display:inline-block", actionButton("sqHeatmap_go", "Create Heatmap")),
             div(style="display:inline-block", actionButton("addReport_heatmap", "Add to Report"))
             )
      
                  
    )
    )
  ),
  fluidRow(
    shiny::tags$hr(width="50%"),
    column(10,offset=1, 
           column(width=8, offset=2, class="alert alert-info", style="margin-top:10px;",
                  shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                  shiny::tags$span(
                    shiny::tags$strong("Depending on the library size, heatmap calculation can take some time.", shiny::tags$br(),"Please be patient while CRISPRAnalyzeR generates it for you.")
                  )
           ),
           
      box( title="Get your heatmap",  width = 12, solidHeader = TRUE, collapsible = TRUE , status = "primary", height = 1500,
        shiny::tags$h2(class="text-success","Heatmap"),
        highchartOutput2("sqHeatmap_plot", height = "1400px")
      )
    )
    
  ),
  
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
   
) # close tab
