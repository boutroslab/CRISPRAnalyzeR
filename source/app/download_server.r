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
# a modal will open with info text
# part of the modal is generated here

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
output$addReport_SPLOM_modal_info <- renderUI({
  if (length(geneList$SPLOM) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 scatter plot matrices can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 scatter plot matrices was reached")),
      p("Now whenever you add a new scatter plot matrix to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

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
output$addReport_replicates_modal_info <- renderUI({
  if (length(geneList$replicates) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 scatter plots can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 scatter plots was reached")),
      p("Now whenever you add a new scatter plot to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## heatmap
# plot object saved here
observeEvent(input$addReport_heatmap, {
  plot <- heatmap()
  geneList$heatmap <- append(geneList$heatmap, list(list(plot = plot, checks = input$sqHeatmap_checkboxes, 
                                                         type = input$sqHeatmaps_type, show = input$sqHeatmaps_show)))
}, ignoreNULL = TRUE)
output$addReport_heatmap_modal_info <- renderUI({
  if (length(geneList$heatmap) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 heatmaps can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 heatmaps was reached")),
      p("Now whenever you add a new heatmap to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## sgRNA
# a lot of plots are created, so only gene name is saved
observeEvent(input$addReport_sgRNA, {
  gene <- input$hit_select
  if( length(gene) != 0 ) geneList$sgRNA <- append(geneList$sgRNA, list(list(gene = gene)))
}, ignoreNULL = TRUE)
output$addReport_sgRNA_modal_info <- renderUI({
  if (length(geneList$sgRNA) <= 30) {
    el <- tagList(
      p(strong("Note:"), "sgRNA plots for a maximum of 30 genes can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("sgRNA plots for maximum of 30 genes was reached")),
      p("Now whenever you add a new set of plots to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## overview 
observeEvent(input$addReport_overview, {
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Not Available'),
    shiny::need(indepth_geneOverview()$data, 'Not Available'),
    shiny::need(!is.na(indepth_geneOverview()$data), 'BiomaRt does not work. Please try again later or check the proxy configuration.')
  )
  geneList$overview <- append(geneList$overview,
                              list(list(
                                gene = input$indepthOverviewGene,
                                indepth_geneOverview = indepth_geneOverview(),
                                indepth_GVIZ_gene_data = indepth_GVIZ_gene_data(),
                                indepth_GVIZ_sgrna_data = indepth_GVIZ_sgrna_data()
                              )))
}, ignoreNULL = TRUE)
output$addReport_overview_modal_info <- renderUI({
  if (length(geneList$overview) <= 20) {
    el <- tagList(
      p(strong("Note:"), "Overview sections for a maximum of 20 genes can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Overview sections for a maximum of 20 genes was reached")),
      p("Now whenever you add an overview section for a new gene to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## compare
observeEvent(input$addReport_compare, {
  genes <- input$hit_select_violine
  if( length(genes) != 0 ) geneList$compare <- append(geneList$compare, list(list(genes = genes)))
}, ignoreNULL = TRUE)
output$addReport_compare_modal_info <- renderUI({
  if (length(geneList$compare) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 gene comparisons can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 gene comparisons was reached")),
      p("Now whenever you add a new gene comparison to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## annotations
observeEvent(input$addReport_anno, {
  data <- idanno_annotation_react()
  geneList$anno <- append(geneList$anno, list(list(data = data)))
}, ignoreNULL = TRUE)
output$addReport_anno_modal_info <- renderUI({
  if (length(geneList$anno) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 annotation tables can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 annotation tables was reached")),
      p("Now whenever you add a new annotation table to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})

## gene set enrichment
observeEvent(input$addReport_enr, {
  geneList$GSE <- append(geneList$GSE, 
                    list(list(
                      genes = input$enrichmentselectgene,
                      data = enrichment$data,
                      status = enrichment$status,
                      enrichr = enrichment$enrichr,
                      stringDBthreshold = input$stringDBthreshold      
                    ))
                  )
}, ignoreNULL = TRUE)
output$addReport_enr_modal_info <- renderUI({
  if (length(geneList$GSE) <= 20) {
    el <- tagList(
      p(strong("Note:"), "A maximum of 20 gene set enrichments can be added.")
    ) 
  } else {
    el <- tagList(
      img(src='images/warning.png', align = "center"),
      p(strong("Maximum of 20 gene set enrichments was reached")),
      p("Now whenever you add a new gene set enrichment to the report,",
        "it will replace the oldest one still in the report.")
    )
  }
  el
})









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
    if( status$results == FALSE )
      {
        reportFile$status <- FALSE
        } else {
      reportFile$status <- FALSE
      reportFile$error <- FALSE
      reportFile$msg <- ""
      shinyjs::disable("createReport")
      
      wd <- config$wd
      wd <- userDir
      
      #bookDir <- paste("CRISPR-AnalyzeR", "report", sep = "_")
      bookDir <- paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "report", sep = "_")
      
      system2("echo", args = c(wd), stdout = "/tmp/wd")
      system2("echo", args = c(bookDir), stdout = "/tmp/bookDir")
      system2("echo", args = c(userDir), stdout = "/tmp/userDir")
      
      
      # if(file.exists(file.path(userDir, "report.zip"))) unlink(file.path(userDir, "report.zip"))
      # if(dir.exists(file.path(userDir, "_book"))) unlink(file.path(userDir, "_book"), recursive = TRUE)
      # if(dir.exists(file.path(userDir, "_bookdown_files"))) unlink(file.path(userDir, "_bookdown_files"), recursive = TRUE)
      # if(dir.exists(file.path(userDir, bookDir))) unlink(file.path(userDir, bookDir), recursive = TRUE)
      
      if(file.exists(file.path(userDir, "report.zip")))
      {
        system2(command = "rm", args = c("-f",file.path(userDir, "report.zip")))  
        #unlink(file.path(userDir, "report.zip"))
      }
      if(dir.exists(file.path(userDir, "_book")))
      {
        system2(command="rm", args = c("-f", "-r", file.path(userDir, "_book")))
        #unlink(file.path(userDir, "_book"), recursive = TRUE)
      }
      if(dir.exists(file.path(userDir, "_bookdown_files")))
      {
        system2(command="rm", args = c("-f", "-r", file.path(userDir, "_bookdown_files")))
        #unlink(file.path(userDir, "_bookdown_files"), recursive = TRUE)
      }
      
      system2("echo", args = c("check_bookdir"), stdout = "/tmp/check_bookDir1")
      system2("echo", args = c(file.path(userDir, bookDir)), stdout = "/tmp/check_bookDir2")
      
      if(dir.exists(file.path(userDir, bookDir)))
      {
        system2("echo", args = c("Remove bookdir"), stdout = "/tmp/remove_bookDir")
        system2(command="rm", args = c("-f", "-r", file.path(userDir, bookDir)))
        #unlink(file.path(userDir, bookDir), recursive = TRUE)
      }
        
      system2("echo", args = c("Set Groups"), stdout = "/tmp/set_groups")
      group <- c()
      for( i in 1: length(groups()) ){
        group[i] <- paste(names(groups()[i]), paste(groups()[[i]], collapse = ";"), sep = ";")
      }
      
      # init log
      logReport <- file.path(config$logDir, "report.log")
      write(paste(userID, ": Start report generation at", Sys.time()), logReport, append = TRUE)
      
      system2("echo", args = c("Write Info"), stdout = "/tmp/write_info")
      
      # check for existence of the cosmic DB
      # For some reasing, file.access sometimes results in an empty value
      cosmicfile <- -1
      print(config$database_path)
      print(config$COSMIC_database)
      print(file.path(config$database_path, config$COSMIC_database))
      print(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4))
      print(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 0))
      print(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 1))
      print(file.access(names = file.path(config$database_path, "testwrong"), mode = 4))
      cosmicfile <- try(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4))
      print(cosmicfile)
      
      if(cosmicfile == 0)
      {
        cosmicDB = "yes"
      } else
      { 
        cosmicDB = "no"
      }
      
      info <- c(paste("progress", 0, sep = ";"),
                paste("info", "", sep = ";"),
                paste("logDir", config$logDir, sep = ";"),
                paste("userID", userID, sep = ";"),
                paste("userDir", userDir, sep = ";"),
                paste("scriptDir", config$scriptpath, sep = ";"),
                paste("appDir", config$appDir, sep = ";"),
                paste("funDir", config$Fundir, sep = ";"),
                paste("logReport", logReport, sep = ";"),
                paste("proxyurl", config$car.proxy.url, sep = ";"),
                paste("proxyport", config$car.proxy.url, sep = ";"),
                paste("proxy", config$car.proxy, sep = ";"),
                paste("database_path", config$database_path, sep = ";"),
                
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
                
                paste("cosmicDB", cosmicDB, sep = ";"),
                
                paste("inclSQ", input$report_sqCheck, sep = ";"),
                paste("inclHC", input$report_hcCheck, sep = ";"),
                paste("inclOV", input$report_ovCheck, sep = ";"),
                paste("inclSG", input$report_sgCheck, sep = ";"),
                paste("inclCO", input$report_coCheck, sep = ";"),
                paste("inclAN", input$report_anCheck, sep = ";"),
                paste("inclGS", input$report_enCheck, sep = ";"))
      
      write(info, infoFiles$report)
      
      print(info)
      
      system2("echo", args = c("Copy file"), stdout = "/tmp/copy_files")
      write(paste(userID, ": Copy files to ", userDir), logReport, append = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "report.Rmd"), file.path(userDir, "report.Rmd")))
      #file.copy(file.path(config$scriptpath, "report.Rmd"), file.path(userDir, "report.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "green_report.css"), file.path(userDir, "green_report.css")))
      #file.copy(file.path(config$scriptpath, "green_report.css"), file.path(userDir, "green_report.css"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "init.Rmd"), file.path(userDir, "init.Rmd")))
      #file.copy(file.path(config$scriptpath, "init.Rmd"), file.path(userDir, "init.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "info.Rmd"), file.path(userDir, "info.Rmd")))
      #file.copy(file.path(config$scriptpath, "info.Rmd"), file.path(userDir, "info.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "screenQuality.Rmd"), file.path(userDir, "screenQuality.Rmd")))
      #file.copy(file.path(config$scriptpath, "screenQuality.Rmd"), file.path(userDir, "screenQuality.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "sqFastQqcChild.Rmd"), file.path(userDir, "sqFastQqcChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "sqFastQqcChild.Rmd"), file.path(userDir, "sqFastQqcChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "hitCalling.Rmd"), file.path(userDir, "hitCalling.Rmd")))
      #file.copy(file.path(config$scriptpath, "hitCalling.Rmd"), file.path(userDir, "hitCalling.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "overview.Rmd"), file.path(userDir, "overview.Rmd")))
      #file.copy(file.path(config$scriptpath, "overview.Rmd"), file.path(userDir, "overview.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "overviewChild.Rmd"), file.path(userDir, "overviewChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "overviewChild.Rmd"), file.path(userDir, "overviewChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "cosmicChild.Rmd"), file.path(userDir, "cosmicChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "cosmicChild.Rmd"), file.path(userDir, "cosmicChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "sgRNA.Rmd"), file.path(userDir, "sgRNA.Rmd")))
      #file.copy(file.path(config$scriptpath, "sgRNA.Rmd"), file.path(userDir, "sgRNA.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "sgRNAChild.Rmd"), file.path(userDir, "sgRNAChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "sgRNAChild.Rmd"), file.path(userDir, "sgRNAChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "compare.Rmd"), file.path(userDir, "compare.Rmd")))
      #file.copy(file.path(config$scriptpath, "compare.Rmd"), file.path(userDir, "compare.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "compareChild.Rmd"), file.path(userDir, "compareChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "compareChild.Rmd"), file.path(userDir, "compareChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "anno.Rmd"), file.path(userDir, "anno.Rmd")))
      #file.copy(file.path(config$scriptpath, "anno.Rmd"), file.path(userDir, "anno.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "annoChild.Rmd"), file.path(userDir, "annoChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "annoChild.Rmd"), file.path(userDir, "annoChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "gse.Rmd"), file.path(userDir, "gse.Rmd")))
      #file.copy(file.path(config$scriptpath, "gse.Rmd"), file.path(userDir, "gse.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "gseChild.Rmd"), file.path(userDir, "gseChild.Rmd")))
      #file.copy(file.path(config$scriptpath, "gseChild.Rmd"), file.path(userDir, "gseChild.Rmd"), overwrite = TRUE)
      system2(command = "cp", args = c("-R", file.path(config$scriptpath, "_bookdown.yml"), file.path(userDir, "_bookdown.yml")))
      #file.copy(file.path(config$scriptpath, "_bookdown.yml"), file.path(userDir, "_bookdown.yml"), overwrite = TRUE)
      
      write(paste(userID, ": Save RDS files"), logReport, append = TRUE)
      
      saveRDS(
        list(SPLOM = geneList$SPLOM, replicates = geneList$replicates, heatmap = geneList$heatmap,
        overview = geneList$overview, sgRNA = geneList$sgRNA, compare = geneList$compare, anno = geneList$anno, GSE = geneList$GSE), 
      file.path(userDir, "geneList.rds"))
      
      write(paste(userID, ": Start to render files in ", userDir), logReport, append = TRUE)
      system2("echo", args = c(getwd()), stdout = "/tmp/set_wd")
      wd <- getwd()
      setwd(userDir)
      
      system2("echo", args = c("start rendering"), stdout = "/tmp/render")
      res <- try(withProgress(
        bookdown::render_book( file.path(userDir, "report.Rmd"), 
          bookdown::gitbook(split_by = "section", self_contained = FALSE, number_sections = TRUE,
            config = list(sharing = NULL, toc = list(collapse = "section"))),
          params = list(info = infoFiles$report)),
          message = "Compiling Report") )
      
      if( inherits(res, "try-error") ){
        reportFile$status <- FALSE
        reportFile$error <- TRUE
        reportFile$msg <- "<div style='color:red;'>Error occurred during report rendering.</div>"
        
        shinyBS::toggleModal(session, "reportError_modal", toggle = "open")
        
      } else {
        # make bookdir again
        bookDir <- paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "report", sep = "_")
        
        system2("echo", args = c(bookDir), stdout = "/tmp/make_bookdir")
        #make direcotry
        system2(command = "mkdir", args = c(bookDir))
        #dir.create(bookDir)
        system2("echo", args = c(bookDir), stdout = "/tmp/copyto_bookdir")
        system2("echo", args = bookDir, stdout = "/tmp/copyto_bookdir2")
        # copy file
        argcp <- c("-R", "_book", bookDir)
        system2(command = "cp", args = argcp)
        #file.copy("_book", bookDir, recursive = TRUE)
        # Zip it
        system2("echo", args = c("-r", file.path(userDir, "report.zip"), bookDir), stdout = "/tmp/zip_bookdir")
        system2("zip", args = c("-r", file.path(userDir, "report.zip"), bookDir)) 
        reportFile$status <- TRUE
        # Tell that it is finished
        shinyBS::toggleModal(session, "reportCreated_modal", toggle = "open")
      }
      shinyjs::enable("createReport")
      #set back working dir
      setwd(config$wd)
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

##############################################
###### Download other Data in Data Review TAB
#############################################

observe({
  if( status$info == TRUE ){
    shinyjs::enable("downloadSGRNA")
    shinyjs::enable("downloadALL")
  } else {
    shinyjs::disable("downloadSGRNA")
    shinyjs::disable("downloadALL")
  }
  
  if( status$results == TRUE ){
    shinyjs::enable("downloadanalysisdata")
    shinyjs::enable("downloadHC_TSV")
    shinyjs::enable("downloadHC_XLSX")
  } else {
    shinyjs::disable("downloadanalysisdata")
    shinyjs::disable("downloadHC_TSV")
    shinyjs::disable("downloadHC_XLSX")
  }
  
})

# sgRNA re-evaluation

output$downloadSGRNA <- downloadHandler(
  filename = paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "sgRNA_ReEvaluation.tsv", sep = "_"),
  content = function(file) {
    if( status$info == FALSE ) NULL else {
      file.copy(file.path(userDir, "sgRNA_ReEvaluation.tsv"), file, overwrite = TRUE)
    }
  }
)

# Hit Calling Data

output$downloadHC_TSV <- downloadHandler(
  filename = paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "HitCalling_all.tsv", sep = "_"),
  content = function(file) {
    if( status$results == FALSE ) NULL else {
      file.copy(file.path(userDir, "HitCalling_all.tsv"), file, overwrite = TRUE)
    }
  }
)

output$downloadHC_XLSX <- downloadHandler(
  filename = paste("CRISPR-AnalyzeR", format(startTime, format = "%y-%m-%d"), "HitCalling_all.xlsx", sep = "_"),
  content = function(file) {
    if( status$results == FALSE ) NULL else {
      file.copy(file.path(userDir, "HitCalling_all.xlsx"), file, overwrite = TRUE)
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





# Sessioninfo
output$sessioninfo <- renderTable({
  
  r <- devtools::session_info()
  # write into file
  readr::write_tsv(r$packages,file.path(userDir, "Sessioninfo.txt"))
  return(r$packages)
  
})






## Make downloadable files

observe({
  # check first that:
  # status$results == TRUE
  # which is the case when analysis is done
  if(status$results == TRUE){
    # Download all generated analysis data as TSV or XLSX files combined
    
    # 1:
    # provide TSV and ONE XLSX for all individually (xlsx with several layers)
    
    ## create tibble
    df_all <- tibble::tibble("gene" = results()$aggregatedReadcount$design
    )
    # join Data
    ## DESEq2
    df_all <- dplyr::left_join(x=df_all, y=as.data.frame(results()$deseq$data$genes[,c("genes", "log2FoldChange", "padj")]), by = c("gene" = "genes") )
    names_col <- c("gene", "log2FoldChange", "DESeq2 adjusted p-value")
    
    colnames(df_all) <- names_col
    
    # MAGECK
    df_all <- dplyr::left_join(x=df_all, y=as.data.frame(results()$mageck$data$genes[,c("genes", "rank.pos", "pos" , "rank.neg", "neg")]), by = c("gene" = "genes") )
    names_col <- c(names_col,"MAGeCK Rank Enriched", "MAGeCK adjusted p-value enriched" , "MAGeCK Rank depleted", "MAGeCK adjusted p-value depleted")
    colnames(df_all) <- names_col
    
    # WILCOX
    wilcox_df <- results()$wilcox$data
    wilcox_df$genes <- rownames(wilcox_df)
    df_all <- dplyr::left_join(x=df_all, y=wilcox_df[,c("genes", "p.value")], by = c("gene" = "genes") )
    
    names_col <- c(names_col, "Wilcox adjusted p-value")
    colnames(df_all) <- names_col
    
    # sgRSEA
    sgrsea_pos <- as.data.frame(results()$rsea$data$gene.pos)
    sgrsea_pos$genes <- rownames(sgrsea_pos)
    sgrsea_neg <- as.data.frame(results()$rsea$data$gene.neg)
    sgrsea_neg$genes <- rownames(sgrsea_neg)
    
    df_all <- dplyr::left_join(x=df_all, y=sgrsea_pos[,c("genes", "FDR.pos", "rank.pos")], by = c("gene" = "genes") )
    names_col <- c(names_col, "sgRSEA FDR enriched", "sgRSEA Rank enriched")
    
    
    df_all <- dplyr::left_join(x=df_all, y=sgrsea_neg[,c("genes", "FDR.neg", "rank.neg")], by = c("gene" = "genes") )
    names_col <- c(names_col, "sgRSEA FDR depleted", "sgRSEA Rank depleted")
    
    colnames(df_all) <- names_col
    
    # EDGER
    edger_df <- results()$edger$data$genes
    edger_df$genes <- rownames(edger_df)
    df_all <- dplyr::left_join(x=df_all, y=edger_df[,c("genes", "FDR", "FDR.Mixed")], by = c("gene" = "genes") )
    names_col <- c(names_col, "edgeR FDR", "edgeR FDR Mixed")
    
    
    # BAGEL if available
    if(!is.null(results()$bagel$info))
    {
      # bagel is in
      # results()$bagel$data
      df_all <- dplyr::left_join(x=df_all, y=results()$bagel$data[,c("GENE", "BF", "Essential")], by = c("gene" = "GENE") )
      names_col <- c(names_col, "BAGEL Bayes Factor", "BAGEL Essential")
      
    }
    
    # ScreenBEAM if available
    
    if(!is.null(results()$screenbeam$info))
    {
      # Data is in results()screenbeam$data
      df_all <- dplyr::left_join(x=df_all, y=results()$screenbeam$data[,c("gene", "FDR", "pval")], by = c("gene" = "gene") )
      names_col <- c(names_col, "SCREENBEAM FDR", "SCREENBEAM not-adjusted p-value")
      
    }
    
    # Z-Ratio
    df_all <- dplyr::left_join(x=df_all, y=results()$zratio[,c("gene", "zscore.untreated", "zscore.treated", "zratio")], by = c("gene" = "gene") )
    names_col <- c(names_col, "Z-Score UNTREATED", "Z-Score TREATED", "Z-Ratio")
    
    colnames(df_all) <- names_col
    # 2:
    # combined:
    # gene IDs + log2 FC from DESeq2 + all pvals/scores
    
    # make TSV
    readr::write_tsv(df_all, path = file.path(userDir, "HitCalling_all.tsv"))
    
    
    # make XLSX
    
    library(openxlsx)
    l_all <- list("HitCalling ALL" = df_all, "DESeq2" = as.data.frame(results()$deseq$data$genes), "MAGeCK" = as.data.frame(results()$mageck$data$genes), "Wilcoxon" = as.data.frame(results()$wilcox$data), "edgeR" = as.data.frame(results()$edger$data$genes), "sgRSEA enriched" = as.data.frame(results()$rsea$data$gene.pos), "sgRSEA depleted" = as.data.frame(results()$rsea$data$gene.neg), "Z-Ratio" = as.data.frame(results()$zratio))
    
    # add BAGEL and Screenbeam if available
    if(!is.null(results()$screenbeam$info))
    {
      l_all <- c(l_all, "ScreenBEAM" = as.data.frame(results()$screenbeam$data))
    }
    
    if(!is.null(results()$bagel$info))
    {
      bageldf <-  data.frame(
        "Gene" =  results()$bagel$data$BAGEL.GENE,
        "Bayes Factor" =  results()$bagel$data$BAGEL.BF,
        "STD" =  results()$bagel$data$BAGEL.STD,
        "Essential?" =  results()$bagel$data$BAGEL.Essential,
        stringsAsFactors = FALSE
      )
      l_all <- c(l_all, "BAGEL" = bageldf)
      
      bageldf <- NULL
    }
    
    res <- try(openxlsx::write.xlsx(l_all, file.path(userDir, "HitCalling_all.xlsx"), asTable = TRUE, colNames = TRUE, rowNames = TRUE, overwrite = TRUE))
    if(class(res) == "try-error")
    {return(NULL)}
  } else {NULL}
  
})


observe({
  if(status$info == TRUE) {
    readr::write_tsv(info()$ecrisp, path = file.path(userDir, "sgRNA_ReEvaluation.tsv"))
  } else {NULL}
})