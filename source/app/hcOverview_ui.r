# sourced by 'ui.r'
# save as 'hcOverview_ui.r'
# ui elements of 'Overview' sub tab of 'Hit Calling' tab






tabItem(tabName = "hc_overview", align = "center",
  
  shinyjs::useShinyjs(),
        
        
  ## Welcome message
  fluidRow(
   style="width:80%;",
   HTML("<div class='section'>
      <div class='container'>
        <div class='row'>
        <div class='col-md-12'>
        <h1 class='text-success text-left'>
        <i class='fa fa-angle-double-right  fa-fw'></i>Hit Calling
        <font color='#777777'>&nbsp;
        <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Overview</span>
        </font>
        </h1>
        <hr>
        <p class='lead'>This page gives you the overall view for each analysis method and allows
        you to do a compare-on-a-glance.
        <br>Venn diagrams illustrate a hit candidate overlap for those that showed
        up below the defined p-value threshold in the different algorithms.</p>
        </div>
        </div>
        </div>
        </div>")
  ),
  
  # HELP as including of hcOverview_help.r
  source(file.path(config$appDir, "hcOverview_help.r"))[1],
  
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  
  # Tabset Panel
  fluidRow(
    
    
      column(width=10,offset=1,
      ## Table
      column(7, 
        box(title = "Find signficant candidates across all methods",width = NULL, solidHeader = FALSE,color = "gray",
          radioButtons("hcOverview_radio", NULL, 
            inline = TRUE, choices = list("enriched" = "en", "depleted" = "de")),
          br(),
          div(style="display:inline-block; margin-right:10px", 
              shinyWidgets::awesomeCheckbox("hcOverview_wilcox", "Wilcox", value = TRUE)),
          div(style="display:inline-block; margin-right:10px", 
              shinyWidgets::awesomeCheckbox("hcOverview_deseq", "DESeq2", value = TRUE)),
          div(style="display:inline-block; margin-right:10px", 
              shinyWidgets::awesomeCheckbox("hcOverview_mageck", "MAGeCK", value = TRUE)),
          div(style="display:inline-block; margin-right:10px", 
              shinyWidgets::awesomeCheckbox("hcOverview_rsea", "sgRSEA", value = TRUE)),
          div(style="display:inline-block", shinyWidgets::awesomeCheckbox("hcOverview_edger", "EdgeR", value = TRUE)),
          helpText("Exclude genes which did not show significant enrichment/depletion 
                 according to the checked methods."),
          DT::dataTableOutput("hcOverview_data")
        )
      ),
      
      ## Venn Diagrams
      column(5, 
        #box(title = "overview",width = NULL,solidHeader = FALSE,status = "primary",
          plotOutput("hcOverview_venn"),
          helpText("significantly enriched/depleted genes across all methods")
        #)
      )
      )
      
  ),
  
  br(),
  # load footer  
  source(file.path(config$appDir, "footer.r"))[1]
  
   
) # close tab
