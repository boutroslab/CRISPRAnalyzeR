# save as 'ui.r'
# shiny ui





# load packages
library(shinydashboard)
library(shiny)
library(shinyjs)
library(ggplot2)
library(highcharter)
library(DT)
library(VennDiagram)
library(shinyBS)


# load configuration
source("config.r", local = TRUE)


# load ui functions
source(file.path(config$appDir, "functions_ui.r"), local = TRUE)

# repeated ui stuff for modals
addReport_modelTrivia <- tagList(br(), br(), "The report can be downloaded in the downloads section.")



###############
#### header####
###############
header <- dashboardHeader( 
  
  title = HTML(paste(img(src='./images/CRISPRAnalyzR_logo4_small.png', align = "middle", class="img-responsive", style="height:55px !important;"), "")),
  
  dropdownMenuOutput("infoMenu"),
  
  dropdownMenuOutput("logMenu")

)




#################
#### sidebar ####
#################
sidebar <- dashboardSidebar(sidebarMenu(
  
  ## Welcome
  menuItem("Welcome", tabName = "welcome", icon = icon("home")),
  
  
  ## Data
  menuItem("Setup your Screen", tabName = "data_overview", icon = icon("gear"),
           collapsible = TRUE,
           menuSubItem("Upload your Data", tabName = "data"),
           menuSubItem("Review your Data", tabName = "data_review"),
           menuSubItem("Set Groups and Gene Identifier", tabName = "setup"),
           menuSubItem("Set Analysis Parameters", tabName = "settings")
          ),
  
  
  ## Setup
  #menuItem("Setup", tabName = "setup", icon = icon("cog")),  
  
  
  ## Screening
  #menuItem("Settings", tabName = "settings", icon = icon("play")),  

  
  ## Screen Quality
  menuItem("Screen Quality Control", tabName = "sq", icon = icon("thumbs-up"),
    collapsible = TRUE,
      menuSubItem("FASTQ Data Quality", tabName = "sq_fastq"),
      menuSubItem("Screen Readcount", tabName = "sq_stats"),
      menuSubItem("sgRNA Coverage", tabName = "sq_coverage"),
      menuSubItem("Sample Comparison", tabName = "sq_samples"),
      menuSubItem("PCA", tabName = "sq_pca"),
      menuSubItem("Heatmaps", tabName = "sq_heatmap")
  ),
  

  ## Hit Calling
  menuItem("Hit Calling", tabName = "hc", icon = icon("list"),
    collapsible = TRUE,
      menuSubItem("Gene Ranking", tabName = "hc_candidates"), 
      menuSubItem("Overview", tabName = "hc_overview")
  ),
  
  
  
  ## Hit Confirmation
  menuItem("Hit Confirmation", tabName = "id", icon = icon("search"),
    collapsible = TRUE,
      menuSubItem("Gene Overview", tabName = "id_overview"),
      menuSubItem("sgRNAs Performance", tabName = "id_sgRNAs"),
      menuSubItem("Gene Set Enrichment", tabName = "enrichment"),
      menuSubItem("Compare Genes", tabName = "id_genecompare"),
      menuSubItem("Annotate Genes", tabName = "id_geneannotation")
  ), 
  
  ## Gene Comparison
  # menuItem("Gene Set Analysis", tabName = "annotate", icon = icon("info"),
  #          collapsible = TRUE,
  #          
  # ),
  
  
  ## Downloads
  menuItem("Report", tabName = "downloads", icon = icon("floppy-o")),  
  
  
  ## Help
  menuItem("Help", tabName = "help", icon = icon("question"),
           collapsible = TRUE,
             menuSubItem("Ask us for help", tabName = "help_ticket"),
             menuSubItem("Forum", tabName = "help_forum"),
           menuSubItem("Help", tabName = "help_analyzer")
           ),
  
  
  ## Impressum
  menuItem("Imprint/Impressum", tabName = "impressum", icon = icon("legal")),
  
  ## About CRISPRAnalyzer
  #menuItem("About CRISPRAnalyzeR", tabName = "aboutcar", icon = icon("graduation-cap")),
  
  menuItem("Download CRISPRAnalyzeR", tabName = "getCRISPRAnalyzeR", icon = icon("graduation-cap"))
  
  

)#,

#shiny::bookmarkButton(label = "Save Session", title= "Save your Session on the server for later use")
)




##############
#### body ####
##############
#tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "test.css")), 
body <- dashboardBody(
  shiny::tags$head(
    shiny::tags$style(HTML(config$stylesheet)),
    
    shiny::includeScript("tooltip-delay.js")
  ),
  
  tabItems(
    
    ## Welcome
    source(file.path(config$appDir, "welcome_ui.r"), local = TRUE)$value,
    
    ## Data
    source(file.path(config$appDir, "data_ui.r"), local = TRUE)$value,
    source(file.path(config$appDir, "data_review_ui.r"), local = TRUE)$value,
    
    ## Setup
    source(file.path(config$appDir, "setup_ui.r"), local = TRUE)$value,
    
    ## Settings
    source(file.path(config$appDir, "settings_ui.r"), local = TRUE)$value,
    
  
    ## Screen Quality
  
    # FASTQ QC
    source(file.path(config$appDir, "sqFastq_ui.r"), local = TRUE)$value,
    
    # Basic Statistics
    source(file.path(config$appDir, "sqStats_ui.r"), local = TRUE)$value,
    
    # Coverage
    source(file.path(config$appDir, "sqCoverage_ui.r"), local = TRUE)$value,
  
    # Samples
    source(file.path(config$appDir, "sqSamples_ui.r"), local = TRUE)$value,
    
    # PCA
    source(file.path(config$appDir, "sqPca_ui.r"), local = TRUE)$value,
    
    # Heatmap
    source(file.path(config$appDir, "sqHeatmap_ui.r"), local = TRUE)$value,
      
  
    ## Hit Calling
    
    # Gene Ranking
    source(file.path(config$appDir, "hcCandidates_ui.r"), local = TRUE)$value,
    
    # Overview
    source(file.path(config$appDir, "hcOverview_ui.r"), local = TRUE)$value,
    
    # Essential Genes
    
    
    
    ## Hit Confirmation
    
    # Overview
    source(file.path(config$appDir, "idOverview_ui.r"), local = TRUE)$value,
    
    # sgRNAs
    source(file.path(config$appDir, "idSgrna_ui.r"), local = TRUE)$value,
    
    # Gene Set Enrichment
    source(file.path(config$appDir, "enrichment_ui.r"), local = TRUE)$value,
    
    # Gene Compare
    source(file.path(config$appDir, "idGenecompare_ui.r"), local = TRUE)$value,
    
    # Gene Annotation
    source(file.path(config$appDir, "idGeneannotation_ui.r"), local = TRUE)$value,
    
    
  
    ## Report
    source(file.path(config$appDir, "download_ui.r"), local = TRUE)$value,
    
    ## Help
    source(file.path(config$appDir, "help_ui.r"), local = TRUE)$value,
    source(file.path(config$appDir, "helpforum_ui.r"), local = TRUE)$value,
    source(file.path(config$appDir, "help_analyzer.r"), local = TRUE)$value,
    
    # Impressum
    source(file.path(config$appDir, "impressum_ui.r"), local = TRUE)$value,
    
    # About CRISPRAnalyzeR
    source(file.path(config$appDir, "about.r"), local = TRUE)$value,
    source(file.path(config$appDir, "getCRISPRAnalyzeR.r"), local = TRUE)$value

  ),

#### MODALS
### So that modals appear on any page

shinyBS::bsModal("addReport_SPLOM_modal", "SPLOM added to report", "addReport_SPLOM", 
                 p("Scatter plot matrix was added to the report", addReport_modelTrivia)),
shinyBS::bsModal("addReport_replicates_modal", "Scatter plot added to report", "addReport_replicates", 
                 p("Scatter plot was added to the report", addReport_modelTrivia)),
shinyBS::bsModal("addReport_heatmap_modal", "Heatmap added to report", "addReport_heatmap", 
                 p("Heatmap was added to the report", addReport_modelTrivia)),
shinyBS::bsModal("addReport_sgRNA_modal", "sgRNA plots added to report", "addReport_sgRNA", 
                 p("sgRNA plots about the selected gene were added to the report.", addReport_modelTrivia)),
shinyBS::bsModal("addReport_overview_modal", "Gene overview plots added to report", "addReport_overview", 
                 p("Overview plots about the selected gene were added to the report.", addReport_modelTrivia)),
shinyBS::bsModal("addReport_compare_modal", "Gene comparisons added to report", "addReport_compare", 
                 p("Comparisons about the selected genes were added to the report.", addReport_modelTrivia)),
shinyBS::bsModal("addReport_anno_modal", "Gene annotations added to report", "addReport_anno", 
                 p("Annotations about the selected genes were added to the report.", addReport_modelTrivia)),
shinyBS::bsModal("addReport_enr_modal", "Gene set enrichment added to report", "addReport_enr", 
                 p("Gene set enrichment analysis about the selected genes were added to the report.", addReport_modelTrivia)),
shinyBS::bsModal("reportCreated_modal", "Report Created", "", p("The report can now be downloaded with the", strong("Download HTML Report"), "button")),

shinyBS::bsModal(id = "fastqextraction_finished", title = "Data Upload and Data Check finished", trigger = "test", size = "large", 
                 fluidRow(
                   style="width100%;",
                   column(width=8, offset=2, class="alert alert-success",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-check fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center", "Your data files have been uploaded and checked successfully."),
                          shiny::tags$p(class="text-center","As a next step, please go to the Data Review section.")
                          )
                      )
                 )
),
shinyBS::bsModal(id = "libfileerror", title = "Warning: sgRNA library file", trigger = "libfileerrormodal", size = "large", 
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-danger text-center",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(shiny::tags$p(class="lead text-center", uiOutput("libFile_error")))
                   )
                  )
),
shinyBS::bsModal(id = "seqfileerror", title = "Warning: Sequencing Files", trigger = "seqfileerrormodal", size = "large", 
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-danger text-justify",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(
                   shiny::tags$p(class="lead text-center", uiOutput("seqFiles_error"))
                   )
                   )
                 )
),
shinyBS::bsModal(id = "extractfileerror", title = "Warning", trigger = "extractfileerrormodal", size = "large", 
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-danger text-justify",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center",uiOutput("extractedFiles_error"))
                          )
                   )
                 )
),
shinyBS::bsModal(id = "extracterror", title = "Warning", trigger = "extracterrormodal", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-warning",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center", uiOutput("extract_error"))
                          )
                   )
                 )
),
shinyBS::bsModal(id = "grouperror", title = "Attention", trigger = "grouperrormodal", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-warning",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center", uiOutput("groups_error"))
                          )
                   )
                 )
),
shinyBS::bsModal(id = "groups_finished", title = "Groups set", trigger = "test", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-success",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-check fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center", "Your groups have been set succesfully."),
                          shiny::tags$p(class="text-center","As a next step, please set your Analysis Parameters.")
                          )
                   )
                 )
),

shinyBS::bsModal(id = "analysis_finished", title = "Analysis finished", trigger = "test", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-success",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-check fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center", "CRISPRAnalyzeR has successfully run the analysis.")
                          )
                   )
                 )
),
shinyBS::bsModal(id = "analysis_error", title = "Attention", trigger = "test", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-warning",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-exclamation-triangle fa-4x"></i>')),
                          shiny::tags$span(
                          shiny::tags$p(class="lead text-center",  uiOutput("results_errormodal"))
                          )
                   )
                 )
),
shinyBS::bsModal(id = "reannotation_finished", title = "Reannotation finished", trigger = "test", size = "large",
                 fluidRow(
                   style="width:100%;",
                   column(width=8,offset=2, class="alert alert-success",
                          shiny::tags$span(style="float:left;", HTML('<i class="fa fa-check fa-4x"></i>')),
                          shiny::tags$span(
                            shiny::tags$p(class="lead text-center", "CRISPRAnalyzeR has successfully re-annotated your sgRNAs.")
                          )
                   )
                 )
)



)




################
#### run UI ####
################
# compatible with bookmarking state
#function(request) {
shinyUI(dashboardPage( 
  header,
  sidebar,
  body ))

#}






