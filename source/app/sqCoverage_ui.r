# sourced by 'ui.r'
# save as 'sqCoverage_ui.r'
# ui elements for 'screening tab'Coverage' sub tab in 'Screen Quality' tab




tabItem(tabName = "sq_coverage", align = "center",

  shinyjs::useShinyjs(),
    
  
  ## Welcome message
  fluidRow(style="width:80%;",
           HTML("<div class='section'>
      <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>Screen Quality
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Sequencing coverage</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>A gooed coverage is a key to successfull CRISPR screens.
                Here you can check whether all sgRNAs are still present in your samples. It
                shows you how many sgRNAs/Genes were not
                present in the screening data and how many sgRNAs per gene are present at all.</p>
                </div>
                </div>
                </div>
                </div>")
  
  ),
  
  # HELP as including of sqCoverage_help.r
  source(file.path(config$appDir, "sqCoverage_help.r"))[1],
  
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  
  # Tabset Panel
  fluidRow(
    column(width=10,offset=1,
    tabBox(width = 12, #title = tagList(shiny::icon("signal", lib="glyphicon"), "Coverage"),
      
      ## Unmapped sgRNAs
      tabPanel(
        "Missing sgRNAs / Genes",
        shiny::tags$p(class="lead","How many sgRNAs/Genes were not covered in your samples according to your sgRNA FASTA library?"),
        br(),
        fluidRow(
          
          ## barplot of missing genes over datasets
          column(6, #box(title = "Missing Genes", width = NULL, solidHeader = FALSE, status = "primary",
                shiny::tags$h3("Missing Genes"),
                shiny::tags$div(width="100%",highchartOutput("sqCoverage_unmapped_basic1")),
                
                helpText("This number of genes did not result in any read count and are therefore considered missing in the respective sample.")
            #)
          ),
        
          ## barplot of missing sgRNAs over datasets
          column(6, 
            #box(title = "Missing sgRNAs", width = NULL, solidHeader = FALSE, status = "primary",
            shiny::tags$h3("Missing sgRNAs"),
            shiny::tags$div(width="100%", highchartOutput("sqCoverage_unmapped_basic2")),
              
              helpText("This number of sgRNAs did not result in read counts and are therefore considered missing in the respective sample.")
            #)
          )
        
        ), fluidRow(
        
          ## barplot of % of missing non-coding sgRNAs 
          column(6, 
            #box(title = "% missing non-targeting sgRNAs", width = NULL, solidHeader = FALSE, status = "primary",
            shiny::tags$h3("% missing non-targeting sgRNAs"),
            shiny::tags$div(width="100%", highchartOutput("sqCoverage_unmapped_non")),
              
              helpText("Percentage of missing sgRNAs among the genes specified as non-targeting controls.")
            #)
          ),
          
          ## barplot of % of missing positive sgRNAs 
          column(6, #box(title = "% missing positive sgRNAs", width = NULL, solidHeader = FALSE, status = "primary",
              h3("% missing positive sgRNAs"),
              shiny::tags$div(width="100%", highchartOutput("sqCoverage_unmapped_pos")),
              
              helpText("Percentage of missing sgRNAs among the genes specified as positive controls.")
            #)
          )
            
        )
      ),
      
      
      
      
      ## Design per Gene
      tabPanel(
        "Designs per Gene",
        shiny::tags$p(class="lead","These plots provide and overview of the representation of sgRNAs per gene within your data."),
        shiny::tags$p(
            "In case every gene is covered by all sgRNAs, you will see just a single column at 100%.", shiny::tags$br(),
            "Moreover, genes with a low percentage of present sgRNAs might also show a reduced read count."),
        shiny::tags$br(),
        
        fluidRow(
          column(4, box(title = "Available Datasets", width = NULL, solidHeader = FALSE, color = "gray",
            uiOutput("sqCoverage_designs_select")
          )),
          
          column(8, 
            #box(title = "sgRNA coverage in dataset", width = NULL, solidHeader = FALSE, status = "primary",
              shiny::tags$h3("sgRNA coverage in dataset"),
            shiny::tags$div(width="100%",highchartOutput("sqCoverage_designs_plot"))
              
            #)
          )
        )
      )
      
    )
    )
  ),
  
  
  
   
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  ) # close tab
