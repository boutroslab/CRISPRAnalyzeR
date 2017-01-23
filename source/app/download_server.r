# save as 'downlaod_server.r'
# sources by 'server.r'
# server side for Downloads tab









##################################
#### toggle addReport Buttons ####
##################################
# these buttons are actually scatter throughout the app
observeEvent(status$heatmap, {
  if(status$heatmap == TRUE) shinyjs::enable("addReport_heatmap") else shinyjs::disable("addReport_heatmap")
})
observeEvent(status$geneAnnotation, {
  if(status$geneAnnotation == TRUE) shinyjs::enable("addReport_anno") else shinyjs::disable("addReport_anno")
})
observeEvent(status$results, {
  if( status$results == TRUE ){
    shinyjs::enable("addReport_replicates")
    shinyjs::enable("addReport_SPLOM")
    shinyjs::enable("addReport_enr")
  } else {
    shinyjs::disable("addReport_replicates")
    shinyjs::disable("addReport_SPLOM")
    shinyjs::disable("addReport_enr")
  }
})
observeEvent(status$info, {
  if( status$results == TRUE && status$info == TRUE ){
    shinyjs::enable("addReport_overview")
    shinyjs::enable("addReport_compare")
    shinyjs::enable("addReport_sgRNA")
  } else {
    shinyjs::disable("addReport_overview")
    shinyjs::disable("addReport_compare")
    shinyjs::disable("addReport_sgRNA")
  }
})




#######################
#### fill geneList ####
#######################
# each addReport button appends a list of a reactive value geneList
# information to recreate and describe the plot has to be saved
# in some cases the actual plot object is saved (gView)
# a shinyjs info text will be shown for confirmation

## SPLOM
# SPLOM is plotted through side effects, so cannot save plot as 1 obj
# has to be recreated in report
observeEvent(input$addReport_SPLOM, {
  baggro <- input$sqReplicates_overview_aggro
  blog <- input$sqReplicates_overview_log
  cctrl <- input$sqReplicates_overview_ctrl
  dsNames <- c(results()$compare[[1]], results()$compare[[2]])
  geneList$SPLOM <- append(geneList$SPLOM, list(list(baggro = baggro, blog = blog, cctrl = cctrl, dsNames = dsNames)))
}, ignoreNULL = TRUE)


## replicates
observeEvent(input$addReport_replicates, {
  if( length(input$sqReplicates_dataset_set1) != 0 && length(input$sqReplicates_dataset_set2) != 0 ) {
    XX <- input$sqReplicates_dataset_set1 # x axis dataset
    YY <- input$sqReplicates_dataset_set2 # y axis dataset
    blog <- input$sqReplicates_dataset_log # devadic log
    baggro <- input$sqReplicates_dataset_aggro # aggregated to genes?
    ctrl <- input$sqReplicates_dataset_ctrl # pos or neg?
    highlighted <- input$sqReplicates_labelgene_select
    geneList$replicates <- append(geneList$replicates, list(list(XX = XX, YY = YY, blog = blog, baggro = baggro, ctrl = ctrl, highlighted = highlighted)))
  }
}, ignoreNULL = TRUE)

## heatmap
# plot object saved here
observeEvent(input$addReport_heatmap, {
  plot <- heatmap()
  geneList$heatmap <- append(geneList$heatmap, list(list(plot = plot, checks = input$sqHeatmap_checkboxes, 
                                                         type = input$sqHeatmaps_type, show = input$sqHeatmaps_show)))
}, ignoreNULL = TRUE)

## sgRNA
# a lot of plots are created, so only gene name is saved
observeEvent(input$addReport_sgRNA, {
  gene <- input$hit_select
  if( length(gene) != 0 ) geneList$sgRNA <- append(geneList$sgRNA, list(list(gene = gene)))
}, ignoreNULL = TRUE)

## overview 
observeEvent(input$addReport_overview, {
  geneList$overview <- append(geneList$overview,
                              list(list(
                                gene = input$indepthOverviewGene,
                                indepth_geneOverview = indepth_geneOverview(),
                                indepth_GVIZ_gene_data = indepth_GVIZ_gene_data(),
                                indepth_GVIZ_sgrna_data = indepth_GVIZ_sgrna_data()
                              )))
}, ignoreNULL = TRUE)

## compare
observeEvent(input$addReport_compare, {
  genes <- input$hit_select_violine
  if( length(genes) != 0 ) geneList$compare <- append(geneList$compare, list(list(genes = genes)))
}, ignoreNULL = TRUE)

## annotations
observeEvent(input$addReport_anno, {
  data <- idanno_annotation_react()
  geneList$anno <- append(geneList$anno, list(list(data = data)))
}, ignoreNULL = TRUE)

## gene set enrichment
observeEvent(input$addReport_enr, {
  geneList$GSE <- append(geneList$GSE, 
                    list(list(
                      genes = input$enrichmentselectgene,
                      data = enrichment$data,
                      status = enrichment$status,
                      enrichr = enrichment$enrichr,
                      stringdbplot = enrichment$stringdbplot      
                    ))
                  )
}, ignoreNULL = TRUE)









##########################
#### geneList entries ####
##########################
# show summary of geneList entries in the download section
# right now, just some text, could be fancier
output$report_sqList <- renderUI({
  p(length(geneList$SPLOM), "Scatter Plot Matrices,", length(geneList$replicates), "Scatter Plots",
    "and", length(geneList$heatmap), "Heatmaps are added to the report.")
})
output$report_ovList <- renderUI({
  if(length(geneList$overview) > 0){
    p("Information for", length(geneList$overview), "gene overviews were saved and are added to the report.")
  } else {
    p("No gene overviews added yet. You can do that in the In-Depth Analysis tab.")
  }
})
output$report_sgList <- renderUI({
  if(length(geneList$sgRNA) > 0){
    p("Plots regarding genes", paste(unlist(geneList$sgRNA), collapse = ", "), "were saved and are added to the report.")
  } else {
    p("No genewise sgRNA plots added yet. You can do that in the In-Depth Analysis tab.")
  }
})
output$report_coList <- renderUI({
  if(length(geneList$compare) > 0){
    p("Comparisons for a total of", length(unlist(geneList$compare)), "genes were saved and are added to the report.")
  } else {
    p("No gene comparisons saved yet. You can do that in the Gene Set Analysis tab.")
  }
})
output$report_anList <- renderUI({
  if(length(geneList$anno) > 0){
    p(length(geneList$anno), "annotations were saved and are added to the report.")
  } else {
    p("No gene annotations saved yet. You can do that in the Gene Set Analysis tab.")
  }
})
output$report_enList <- renderUI({
  if(length(geneList$GSE) > 0){
    p(length(geneList$GSE), "gene set enrichments were saved and are added to the report.")
  } else {
    p("No gene set enrichments saved yet. You can do that in the Gene Set Analysis tab.")
  }
})

observeEvent(input$report_sqCheck, 
             if(input$report_sqCheck) shinyjs::show("report_sq", TRUE) else shinyjs::hide("report_sq", TRUE))
observeEvent(input$report_hcCheck, 
             if(input$report_hcCheck) shinyjs::show("report_hc", TRUE) else shinyjs::hide("report_hc", TRUE))
observeEvent(input$report_sgCheck, 
             if(input$report_sgCheck) shinyjs::show("report_sg", TRUE) else shinyjs::hide("report_sg", TRUE))
observeEvent(input$report_ovCheck, 
             if(input$report_ovCheck) shinyjs::show("report_ov", TRUE) else shinyjs::hide("report_ov", TRUE))
observeEvent(input$report_coCheck, 
             if(input$report_coCheck) shinyjs::show("report_co", TRUE) else shinyjs::hide("report_co", TRUE))
observeEvent(input$report_anCheck, 
             if(input$report_anCheck) shinyjs::show("report_an", TRUE) else shinyjs::hide("report_an", TRUE))
observeEvent(input$report_enCheck, 
             if(input$report_enCheck) shinyjs::show("report_en", TRUE) else shinyjs::hide("report_en", TRUE))





#######################
#### create Report ####
#######################
# react to action button 
# delete old file if exist
# a big report.info file is written
# .Rmds are copied form scripts into the userDir
# geneList is written as .rds
# report.Rmd is rendered with location of report.info as argument
# report.html is written then zipped
# while creating report, reactive value is set FALSE
reportFile <- reactiveValues(status = FALSE, error = FALSE, msg = "")
observeEvent(input$createReport, {
    if( status$results == FALSE ) reportFile$status <- FALSE else {
      reportFile$status <- FALSE
      reportFile$error <- FALSE
      reportFile$msg <- ""
      shinyjs::disable("createReport")
      
      wd <- getwd()
      bookDir <- paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "report", sep = "_")
      unlink(file.path(userDir, "report.zip"))
      unlink(file.path(userDir, "_book"), recursive = TRUE)
      unlink(file.path(userDir, "_bookdown_files"), recursive = TRUE)
      unlink(file.path(userDir, bookDir), recursive = TRUE)
      
      group <- c()
      for( i in 1: length(groups()) ){
        group[i] <- paste(names(groups()[i]), paste(groups()[[i]], collapse = ";"), sep = ";")
      }
      
      info <- c(paste("progress", 0, sep = ";"),
                paste("info", "", sep = ";"),
                paste("logDir", config$logDir, sep = ";"),
                paste("userID", userID, sep = ";"),
                paste("userDir", userDir, sep = ";"),
                paste("scriptDir", config$scriptpath, sep = ";"),
                paste("appDir", config$appDir, sep = ";"),
                paste("funDir", config$Fundir, sep = ";"),
                
                paste("libName", extractedFiles()$libName, sep = ";"),
                paste("libPath", extractedFiles()$libPath, sep = ";"),
                
                paste("seqNames", paste(extractedFiles()$names, collapse = ";"), sep = ";"),
                paste("seqPaths", paste(extractedFiles()$paths, collapse = ";"), sep = ";"),
                paste("seqGenNames", paste(extractedFiles()$gen_names, collapse = ";"), sep = ";"),
                
                paste("g.extractpattern", as.character(libFile()$regex), sep = ";"),
                
                paste("extracted", paste(extract()$extract, collapse = ";"), sep = ";"),
                paste("targetRegex", paste(extract()$targetRegex, collapse = ";"), sep = ";"),
                paste("reverse", paste(extract()$reverse, collapse = ";"), sep = ";"),
                paste("bt2Quality", paste(input$seqFiles_bt2quali, collapse = ";"), sep = ";"),
                paste("bt2Sensitivity", paste(extract()$bt2Sensitivity, collapse = ";"), sep = ";"),
                
                paste(">>Groups start<<", "", sep = ";"),
                group,
                paste(">>Groups end<<", "", sep = ";"),
                
                paste("annoDataset", annos()$dataset, sep = ";"),
                paste("annoID", annos()$ID, sep = ";"),
                paste("annoIDnew", annos()$IDnew, sep = ";"),
                
                paste("compareGroup1", compare()$groups[1], sep = ";"),
                paste("compareGroup2", compare()$groups[2], sep = ";"),
                paste("comparePos", paste(compare()$pos, collapse = ";"), sep = ";"),
                paste("compareNeg", paste(compare()$neg, collapse = ";"), sep = ";"),
                
                paste("analysisWilcoxPval", analysis()$wilcoxPval, sep = ";"),
                paste("analysisWilcoxRand", analysis()$wilcoxRand, sep = ";"),
                paste("analysisDeseq2Pval", analysis()$deseq2Pval, sep = ";"),
                paste("analysisMageckPval", analysis()$mageckPval, sep = ";"),
                paste("analysisSgrseaPval", analysis()$sgrseaPval, sep = ";"),
                paste("analysisEdgerPval", analysis()$edgerPval, sep = ";"),
                
                paste("scope", input$report_scope, sep = ";"),
                paste("procedure", input$report_procedure, sep = ";"),
                paste("comment", input$report_comments, sep = ";"),
                
                paste("report_organism", input$report_organism, sep = ";"),
                paste("report_cellline", input$report_cellline, sep = ";"),
                paste("report_experimentator", input$report_experimentator, sep = ";"),
                paste("report_plasmid", input$report_plasmid, sep = ";"),
                paste("report_library", input$report_library, sep = ";"),
                paste("report_coverage", input$report_coverage, sep = ";"),
                paste("report_treatment", input$report_treatment, sep = ";"),
                
                paste("report_seqprimer", input$report_seqprimer, sep = ";"),
                paste("report_seqkit", input$report_seqkit, sep = ";"),
                
                paste("inclSQ", input$report_sqCheck, sep = ";"),
                paste("inclHC", input$report_hcCheck, sep = ";"),
                paste("inclOV", input$report_ovCheck, sep = ";"),
                paste("inclSG", input$report_sgCheck, sep = ";"),
                paste("inclCO", input$report_coCheck, sep = ";"),
                paste("inclAN", input$report_anCheck, sep = ";"),
                paste("inclGS", input$report_enCheck, sep = ";"))
      write(info, infoFiles$report)
      write(info, paste0(infoFiles$report, ".bak"))
      
      file.copy(file.path(config$scriptpath, "report.Rmd"), file.path(userDir, "report.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "green_report.css"), file.path(userDir, "green_report.css"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "init.Rmd"), file.path(userDir, "init.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "info.Rmd"), file.path(userDir, "info.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "screenQuality.Rmd"), file.path(userDir, "screenQuality.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "sqFastQqcChild.Rmd"), file.path(userDir, "sqFastQqcChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "hitCalling.Rmd"), file.path(userDir, "hitCalling.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "overview.Rmd"), file.path(userDir, "overview.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "overviewChild.Rmd"), file.path(userDir, "overviewChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "cosmicChild.Rmd"), file.path(userDir, "cosmicChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "sgRNA.Rmd"), file.path(userDir, "sgRNA.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "sgRNAChild.Rmd"), file.path(userDir, "sgRNAChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "compare.Rmd"), file.path(userDir, "compare.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "compareChild.Rmd"), file.path(userDir, "compareChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "anno.Rmd"), file.path(userDir, "anno.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "annoChild.Rmd"), file.path(userDir, "annoChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "gse.Rmd"), file.path(userDir, "gse.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "gseChild.Rmd"), file.path(userDir, "gseChild.Rmd"), overwrite = TRUE)
      file.copy(file.path(config$scriptpath, "_bookdown.yml"), file.path(userDir, "_bookdown.yml"), overwrite = TRUE)
      
      saveRDS(
        list(SPLOM = geneList$SPLOM, replicates = geneList$replicates, heatmap = geneList$heatmap,
        overview = geneList$overview, sgRNA = geneList$sgRNA, compare = geneList$compare, anno = geneList$anno, GSE = geneList$GSE), 
      file.path(userDir, "geneList.rds"))
      
      wd <- getwd()
      setwd(userDir)
      
      res <- try( bookdown::render_book( file.path(userDir, "report.Rmd"), 
                             bookdown::gitbook(split_by = "section", self_contained = FALSE, number_sections = TRUE,
                                               config = list(sharing = NULL, toc = list(collapse = "section"))),
                             params = list(info = infoFiles$report) ) )
      if( inherits(res, "try-error") ){
        reportFile$status <- FALSE
        reportFile$error <- TRUE
        reportFile$msg <- "<div style='color:red;'>Error occurred during report rendering.</div>"
      } else {
        dir.create(bookDir)
        file.copy(c("_book", "_bookdown_files"), bookDir, recursive = TRUE)
        system2("zip", args = c("-r", file.path(userDir, "report.zip"), bookDir)) 
        reportFile$status <- TRUE
        shinyBS::toggleModal(session, "reportCreated_modal", toggle = "open")
      }
      shinyjs::enable("createReport")
      setwd(wd)
    }
  }
)

output$reportGen_error <- renderUI(
  return(HTML(reportFile$msg))
)







#########################
#### download Report ####
#########################
# download handler for presenting html report
# report.zip is presented to download handler
# this button should only be active when there is such a file
observe({
  if( reportFile$status == TRUE ){
    shinyjs::enable("downloadReport")
  } else {
    shinyjs::disable("downloadReport")
  }
})

output$downloadReport <- downloadHandler(
  filename = paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "report.zip", sep = "_"),
  content = function(file) {
    if( reportFile$status == FALSE ) NULL else {
      file.copy(file.path(userDir, "report.zip"), file, overwrite = TRUE)
    }
  }
)





#################################
#### toggle Download Buttons ####
#################################
observeEvent(status$results, {
  if( status$results ){
    shinyjs::enable("createReport") 
    shinyjs::enable("downloads_sqData") 
    shinyjs::enable("downloads_hcData") 
    shinyjs::enable("report_sqCheck") 
    shinyjs::enable("report_hcCheck")
    shinyjs::enable("report_ovCheck")
    shinyjs::enable("report_sgCheck")
    shinyjs::enable("report_coCheck")
    shinyjs::enable("report_anCheck")
    shinyjs::enable("report_enCheck")
    shinyjs::show("report_remarks")
  } else {
    shinyjs::disable("createReport")
    shinyjs::disable("downloads_sqData")
    shinyjs::disable("downloads_hcData")
    shinyjs::disable("report_sqCheck")
    shinyjs::disable("report_hcCheck")
    shinyjs::disable("report_ovCheck")
    shinyjs::disable("report_sgCheck")
    shinyjs::disable("report_coCheck")
    shinyjs::disable("report_anCheck")
    shinyjs::disable("report_enCheck")
    shinyjs::hide("report_remarks")
  }
})





# Sesioninfo
output$sessioninfo <- renderTable({
  
  r <- devtools::session_info()
  # write into file
  readr::write_tsv(r$packages,file.path(userDir, "Sessioninfo.txt"))
  return(r$packages)
  
})


