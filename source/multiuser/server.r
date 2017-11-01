# save as 'server.r'
# shiny server






# load packages
library(ggplot2)
library(highcharter)
library(reshape2)
library(data.table)
#library(tidyr)
library(DT)
library(httr)
#library(dplyr)
library(shinydashboard)
library(VennDiagram)
library(openxlsx)
library(shiny)
library(shinyjs)
library(jsonlite)
library(BiocGenerics)
library(biomaRt)
library(seqinr)
library(MESS)
library(DESeq2)
library(sm)
library(reshape2)
library(DEoptim)
library(ScreenBEAM)
library(tidyverse)
library(caTools)
library(BiocParallel)
#library(Rqc) # is referenced where necessary
library(ShortRead)

library(BiocParallel)

# found in sendmail_server
library(gmailr)





options(shiny.reactlog=FALSE) 
options(shiny.sanitize.errors = FALSE)

# load configuration
source("config.r", local = TRUE)

# load app functions
source(file.path(config$appDir, "functions_server.r"))



############################
## Load language messages ##
############################
config$messages <- as.data.frame(readr::read_tsv(file = "messages.txt", col_names = TRUE), stringsAsFactors = FALSE)
rownames(config$messages) <- config$messages$ID
config$messages$ID <- NULL
config$messages <- setNames(split(config$messages, seq(nrow(config$messages))), rownames(config$messages))



############################
## Load COSMIC database ####
############################


if(!is.null(config$COSMIC_database)){
  
  cosmicreadable <- file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4)
  
  if(cosmicreadable == 0)
  {
      COSMICDB <- try(readr::read_tsv(file = file.path(config$database_path, config$COSMIC_database), col_names = TRUE))
      if(class(COSMICDB) == "try-error")
      {
        COSMICDB <- NULL
      }
  } else
  {
    COSMICDB <- NULL
  }
} else {
  COSMICDB <- NULL
}



#############################


# start shiny session
shinyServer(function(input, output, session) {
  # 4096MB upload limit per file
  options(shiny.maxRequestSize = as.numeric(config$max_upload) * 1024^2)
  
  # create objects needed throughout this session
  source(file.path(config$appDir, "init_server.r"), local = TRUE)

  
  #### Input Modules
  
  # uploading sequencing and library fiels
  source(file.path(config$appDir, "fileUpload_server.r"), local = TRUE)
  
  # retrieving information about optional file extraction
  source(file.path(config$appDir, "extraction_server.r"), local = TRUE)
  
  # creating and arranging groups
  source(file.path(config$appDir, "grouping_server.r"), local = TRUE)
  
  # set gene annotations and convert gene identifier
  source(file.path(config$appDir, "annotations_server.r"), local = TRUE)
  
  # set gene annotations
  source(file.path(config$appDir, "enrichment_server.r"), local = TRUE)
  
  # group1 vs group2
  source(file.path(config$appDir, "analysisSettings_server.r"), local = TRUE)
  
  
  
  
  #### Core Analysis
  
  # handling analysis.r start and finish
  source(file.path(config$appDir, "analysis_server.r"), local = TRUE)
  
  # fetch results from get_info.r
  source(file.path(config$appDir, "info_server.r"), local = TRUE)
  
  
  
  
  #### Screen Quality
  
  # rendering general statistics
  source(file.path(config$appDir, "sqStats_server.r"), local = TRUE)
  
  # rendering coverage statistics
  source(file.path(config$appDir, "sqCoverage_server.r"), local = TRUE)
  
  # rendering replicate statistics
  source(file.path(config$appDir, "sqReplicates_server.r"), local = TRUE)
  
  # PCA
  source(file.path(config$appDir, "sqPca_server.r"), local = TRUE)
  
  # creating and rendering heatmaps
  source(file.path(config$appDir, "sqHeatmap_server.r"), local = TRUE)
  
  source(file.path(config$appDir, "essentials_server.r"), local = TRUE)
  
  
  #### Hit Calling
  
  # phenotype and analysis statistics for each method
  source(file.path(config$appDir, "hcCandidates_server.r"), local = TRUE)
  
  # Venn diagrams and comparison across methods
  source(file.path(config$appDir, "hcOverview_server.r"), local = TRUE)
  
  
  # Essential Genes
  
  
  
  #### In-depth Analysis

  # handle hit_candidate.r
  #source(file.path(config$appDir, "hit_server.r"), local = TRUE)
  
  # render plots
  source(file.path(config$appDir, "inDepth_server.r"), local = TRUE)
  
  
  
  
  #### other
  
  # facilitate downloads
  source(file.path(config$appDir, "download_server.r"), local = TRUE)
  
  # input form for help tab
  source(file.path(config$appDir, "help_server.r"), local = TRUE)
  
  # render notification menues
  source(file.path(config$appDir, "notifications_server.r"), local = TRUE)
  

  # Email Support?
  if(identical(config[["activate.mail"]],TRUE))
  {
    source(file.path(config$appDir, "sendmail_server.r"), local = TRUE)
  }
 

  
  #shiny::showReactLog()
})



