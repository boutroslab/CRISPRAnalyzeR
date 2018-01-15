# sourced by 'server.r'
# save as 'analysis_server.r'
# starting analysis.r and reading its results







########################
#### start analysis ####
########################
# start analysis if everything ok
# upon clicking on start Screen button
# disable submit button
# disable reset button (only one job per session)
# send write info file and run Rscript
observeEvent(input$startAnalysis, {
  write(paste(userID, ": clicked on startAnalysis at", Sys.time()), logFile, append = TRUE)
  
  
  test <- Check_final( status$seqFiles, status$libFile, status$extract, status$groups, status$anno,
    status$compare, status$analysis, status$extractedFiles, messages = config$messages )
  if( test$error == TRUE ){
    error$final <- test$message
    write(paste(userID, ": status of seqFiles, libFile, extract, groups, anno, compare, analysis, extractedFiles: error"), 
          logFile, append = TRUE)
    return()
  }
  
  status$final <- TRUE
  error$final <- test$message
  shinyjs::disable("startAnalysis")
  shinyjs::disable("resetAnalysis")
  shinyjs::disable(id="restartevaluation")
  shinyjs::disable("reset_data")
  shinyjs::disable("reset_data2")
  
  shinyjs::disable("geneID_pos")
  shinyjs::disable("geneID_neg")
  shinyjs::disable("wilcox_pval")
  shinyjs::disable("wilcox_rand")
  shinyjs::disable("deseq2_pval")
  shinyjs::disable("mageck_pval")
  shinyjs::disable("sgrsea_pval")
  shinyjs::disable("edger_pval")
  
  shinyjs::disable("remove_low")
  shinyjs::disable("remove_threshold_low")
  shinyjs::disable("remove_high")
  shinyjs::disable("remove_threshold_high")
  shinyjs::disable("removegroup")
  
  shinyjs::disable("bagel_lower")
  shinyjs::disable("bagel_higher")
  shinyjs::disable("screenbeam_iterations")
  shinyjs::disable("screenbeam_burnin")
  shinyjs::disable("screenbeam_run")
  shinyjs::disable("screenbeam_pval")
  
  

  
  scriptpath <- file.path(config$scriptpath, "analysis.r")
  
  signature <- paste(sample(0:9, 4), collapse = "")
  status$signature <- signature
  write(signature, file.path(userDir, "analysis.sign"))
  
  log <- c(paste(userID, ": status of seqFiles, libFile, extract, groups, anno, compare, analysis, extractedFiles: good"),
           paste(userID, ": Proxy Information", config$car.proxy.url, config$car.proxy.port),
           paste(userID, ": starting analysis.r at", Sys.time()),
           paste(userID, ": signature is", signature),
           paste(userID, ": executing:", "Rscript", scriptpath, infoFiles$analysis))
  write(log, logFile, append = TRUE)
  
  
  # set proxy 
  if(is.null(config$car.proxy.url))
  {
    proxurl <- "NULL"
  } else {
    proxurl <- config$car.proxy.url
  }
  
  if(is.null(config$car.proxy.port))
  {
    proxport <- "NULL"
  } else {
    proxport <- config$car.proxy.port
  }
  
  group <- c()
  for( i in 1: length(groups()) ){
    group[i] <- paste(names(groups()[i]), paste(groups()[[i]], collapse = ";"), sep = ";")
  }
  files <- list.files(config$Fundir)
  info <- c(paste("progress", 0, sep = ";"),
          paste("info", "", sep = ";"),
          paste("logDir", config$logDir, sep = ";"),
          paste("userID", userID, sep = ";"),
          paste("signature", signature, sep = ";"),
          paste("userDir", userDir, sep = ";"),
          paste("scriptDir", config$scriptpath, sep = ";"),
          paste("funDir", config$Fundir, sep = ";"),
          paste("extfiles", paste(files, collapse = ","), sep = ";"),
          paste("proxy", config$car.proxy, sep = ";"),
          paste("bmDatabase", config$car.bm.database, sep = ";"),
          paste("ecrisp", config$ecrisp, sep = ";"),
          paste("databasepath", config$databasepath, sep = ";"),
          paste("bt2Threads", paste(config$car.bt2.threads, collapse = ";"), sep = ";"),
          
          paste("proxyurl", proxurl, sep = ";"),
          paste("proxyport", proxport, sep = ";"),
          # Add library information for pre-made re-evaluation files
          paste("libsAvailable", paste(unique(config$screeninglibraries$Library) , collapse = ";"), sep = ";"),
          
          paste("libSelected", input$screeninglibrary, sep = ";"),
          paste("libName", extractedFiles()$libName, sep = ";"),
          paste("libPath", extractedFiles()$libPath, sep = ";"),
          paste("libregex", libFile()$regex, sep = ";"),
          paste("optimizeFASTA", libFile()$optimizeFASTA, sep = ";"),
          
          paste("seqNames", paste(extractedFiles()$names, collapse = ";"), sep = ";"),
          paste("seqPaths", paste(extractedFiles()$paths, collapse = ";"), sep = ";"),
          paste("seqGen_names", paste(extractedFiles()$gen_names, collapse = ";"), sep = ";"),
          
          paste("g.extractpattern", as.character(libFile()$regex), sep = ";"),
          
          #
          
          #">>Groups start<<",
          paste(">>Groups start<<", "", sep = ";"),
          group,
          #">>Groups end<<",
          paste(">>Groups end<<", "", sep = ";"),
          
          paste("annoDataset", annos()$dataset, sep = ";"),
          paste("annoID", annos()$ID, sep = ";"),
          paste("annoIDnew", annos()$IDnew, sep = ";"),
          
          paste("compareGroup1", compare()$groups[1], sep = ";"),
          paste("compareGroup2", compare()$groups[2], sep = ";"),
          paste("comparePos", paste(compare()$pos, collapse = ";"), sep = ";"),
          paste("compareNeg", paste(compare()$neg, collapse = ";"), sep = ";"),
          
          # Analysis parameters
          paste("removeLow", analysis()$removeLow, sep = ";"),
          paste("removeThresholdLow", analysis()$removeThresholdLow, sep = ";"),
          paste("removeHigh", analysis()$removeHigh, sep = ";"),
          paste("removeThresholdHigh", analysis()$removeThresholdHigh, sep = ";"),
          paste("removeGroups", analysis()$removeGroups, sep = ";"),
          paste("analysisWilcoxPval", analysis()$wilcoxPval, sep = ";"),
          paste("analysisWilcoxRand", analysis()$wilcoxRand, sep = ";"),
          paste("analysisDeseq2Pval", analysis()$deseq2Pval, sep = ";"),
          paste("analysisMageckPval", analysis()$mageckPval, sep = ";"),
          paste("analysisSgrseaPval", analysis()$sgrseaPval, sep = ";"),
          paste("analysisEdgerPval", analysis()$edgerPval, sep = ";"),
          paste("analysisBagelLower", analysis()$bagel_lower, sep = ";"),
          paste("analysisBagelHigher", analysis()$bagel_higher, sep = ";"),
          paste("analysisScreenBEAMRun", analysis()$screenbeam_run, sep = ";"),
          paste("analysisScreenBEAMIterations", analysis()$screenbeam_iterations, sep = ";"),
          paste("analysisScreenBEAMBurnin", analysis()$screenbeam_burnin, sep = ";"),
          paste("analysisScreenBEAMPval", analysis()$screenbeam_pval, sep = ";")
          )

  
  
  write(info, infoFiles$analysis)
  write(info, paste(infoFiles$analysis,".bak", sep="") )
  
  
  
  time$startAnalysis <- Sys.time()

  system2("Rscript", args = c(scriptpath, infoFiles$analysis), wait = FALSE, stdout = NULL, stderr = NULL)

  #show progress bar
  shinyjs::show(id="analysis-progress")
  
  
  
  
}, ignoreNULL = TRUE)


# create error message
output$final_error <- renderUI(HTML(
  paste0("<div style='color:red;'>", error$final, "</div>")
))






#################
#### Results ####
#################
# info and results from analysis.r script are read
# info file with progress and possible notification is updated regularly
# when batch script is finished, progress = 1
# if there was no problem 'info'line is empty and result files will be read
# if there was a problem 'info'line is not empty, and error message is displayed
# results will be loaded by results module
# for info retrieval and visualization reactive objects are created
# reulsts()$    list of all objects as stated in 'out'


## get updates from analysis.r
progress_analysis <- reactivePoll(500, NULL, Info_trigger_analysis, Info_read_analysis)


## results module
results <- eventReactive(progress_analysis(),{
  if( progress_analysis()$progress == 1 ){
      write(paste(userID, ": analysis.r finished at", Sys.time()), logFile, append = TRUE)
      shinyjs::enable("resetAnalysis")   
      shinyjs::enable("reset_data") 
      shinyjs::enable("reset_data2") 
      
      error$results <- ""
      
      x <- scan(infoFiles$analysis, what="", sep="\n", quiet = TRUE)
      xlist <- strsplit(x, split = ";", fixed = TRUE) 
      out <- list()
      for( i in 1:length(xlist) ){
        out[[xlist[[i]][1]]] <- xlist[[i]][-1]
      }
      
      # Check for Error using the info file
      test <- Check_results( out$info, userID, messages = config$messages )
      if( test$error == TRUE ){
        error$results <- test$message
        write(paste(userID, ": results tested: error"), logFile, append = TRUE)
        
        if(identical(config[["activate.mail"]],TRUE))
        {
          # Send Email
          text <- paste("Analysis Error<br/> for User",userID,"<br/>", out$info, sep=" ")
          title <- paste("[CRISPRAnalyzeR][error][analysis]", userID, sep=" ")
          attach <- file.path(config$logDir, "analysis.log") # Attach logfile AND analysis.info for more details
          
          sendmail_car(message = text, title = title, from=NULL, to=NULL, attach=attach, type = "error")
        }
        # open modal to show error
        shinyBS::toggleModal(session, "analysis_error", toggle = "open")
        shinyjs::hide(id="analysis-progress")
        return(NA)
      } else {
        write(paste(userID, ": results tested: good"), logFile, append = TRUE)
      }
      
      
      time$finishAnalysis <- Sys.time()
      
      # COSMIC DB loaded?
      if(is.null(COSMICDB)){
        COSMICDB <- NULL
      }
     
      
      out <- list(
        "pca" = readRDS(file.path(userDir, "PCA.rds")),
        "statsGeneral" = readRDS(file.path(userDir, "statsGeneral.rds")),
        "unmappedGenes" = readRDS(file.path(userDir, "unmappedGenes.rds")),
        "readDistribution" = readRDS(file.path(userDir, "readDistribution.rds")),
        "essentialDistribution" = readRDS(file.path(userDir, "essentialDistribution.rds")),
        #"readDistributionBox" = readRDS(file.path(userDir, "readDistributionBox.rds")),
        "readDistributionBoxNorm" = readRDS(file.path(userDir, "readDistributionBoxNorm.rds")),
        "CDF_list" = readRDS(file.path(userDir, "CDF_list.rds")),
        "readDepth" = readRDS(file.path(userDir, "readDepth.rds")),
        "geneDesigns" = readRDS(file.path(userDir, "geneDesigns.rds")),
        "readCountVS" = readRDS(file.path(userDir, "readCountVS.rds")),
        "COSMICDB" = COSMICDB,
        
        "wilcox" = readRDS(file.path(userDir, "wilcox.rds")),
        "deseq" = readRDS(file.path(userDir, "deseq.rds")),
        "mageck" = readRDS(file.path(userDir, "mageck.rds")),
        "rsea" = readRDS(file.path(userDir, "rsea.rds")),
        "edger" = readRDS(file.path(userDir, "edger.rds")),
        "zratio" = readRDS(file.path(userDir, "zratio.rds")),
        "bagel" = readRDS(file.path(userDir, "bagel.rds")),
        "screenbeam" = readRDS(file.path(userDir, "screenbeam.rds")),
        "libFILE" = readRDS(file.path(userDir, "libFILE.rds")),
        
        
        "hitOverview" = readRDS(file.path(userDir, "hitOverview.rds")),
        "hitOverview_info" = readRDS(file.path(userDir, "hitOverview_info.rds")),
        "vennEnriched" = readRDS(file.path(userDir, "vennEnriched.rds")),
        "vennDepleted" = readRDS(file.path(userDir, "vennDepleted.rds")),
                         
        "readcount" = readRDS(file.path(userDir, "readcount.rds")),
        "normalizedReadcount" = readRDS(file.path(userDir, "normalizedReadcount.rds")),
        "aggregatedReadcount" = readRDS(file.path(userDir, "aggregatedReadcount.rds")),
        
        "compare" = readRDS(file.path(userDir, "compare.rds")),
        "ctrls" = readRDS(file.path(userDir, "ctrls.rds")),
        
        "uniqueGenes" = readRDS(file.path(userDir, "uniqueGenes.rds")),
        "sampleList" = readRDS(file.path(userDir, "sampleList.rds")),
        "error" = test$error,
        
        "GenomeCRISPR_essentials" = readRDS(file.path(config$scriptpath, "GenomeCRISPR_Essentials.rds")),
        "DAISY_essentials" = readRDS(file.path(config$scriptpath, "DAISY_Essentials.rds")),
        
        "GSE_methodlist" = readRDS(file.path(userDir, "GSE_methodlist.rds"))
        )
      
      ### Open MODAL when Analysis Extraction is done
      if( test$error != TRUE )
      {
        shinyBS::toggleModal(session, "reannotation_started", toggle = "open")
        shinyBS::toggleModal(session, "analysis_finished", toggle = "open")
        shinyjs::show("downloadanalysisdata")
      }
      
      
      status$results <- TRUE
      shinyjs::show(id="reevaluation-progress")
      out
  }
})


## trigger reactive
observe(results())


## draw progress bar
output$analysis_progressBar <- renderUI({
  if( progress_analysis()$progress == 0 ){
    return() 
  } else {
    perc <- round(progress_analysis()$progress * 100)
    
    if(progress_analysis()$progress >= 0.1 && progress_analysis()$progress < 0.4)
    { 
      title = "Performing Screen Quality Calculations"
    } else if(progress_analysis()$progress >= 0.05 && progress_analysis()$progress < 0.08 )
    {
      title = "Converting Gene Identifiers"
    } else if(progress_analysis()$progress >= 0.08 && progress_analysis()$progress < 0.1 )
    {
      title = "Check Gene and sgRNA Readcount for consistency"
    }  else if(progress_analysis()$progress >= 0.4 && progress_analysis()$progress < 0.45)
    {
      title = "Performing Hit Calculations - Wilcoxon"
    } else if(progress_analysis()$progress >= 0.45 && progress_analysis()$progress < 0.55)
    {
      title = "Performing Hit Calculations - DESeq2"
    } else if(progress_analysis()$progress >= 0.55 && progress_analysis()$progress < 0.60)
    {
      title = "Performing Hit Calculations - MAGeCK"
    } else if(progress_analysis()$progress >= 0.6 && progress_analysis()$progress < 0.65)
    {
      title = "Performing Hit Calculations - sgRSEA"
    } else if(progress_analysis()$progress >= 0.65 && progress_analysis()$progress < 0.7)
    {
      title = "Performing Hit Calculations - Z-Ratio"
    } else if(progress_analysis()$progress >= 0.7 && progress_analysis()$progress < 0.73)
    {
      title = "Performing Hit Calculations - edgeR"
    } else if(progress_analysis()$progress >= 0.73 && progress_analysis()$progress < 0.75)
    {
      title = "Performing Hit Calculations - BAGEL - Please be patient"
    }  else if(progress_analysis()$progress >= 0.75 && progress_analysis()$progress < 0.78)
    {
      title = "Performing Hit Calculations - ScreenBEAM - Please be patient"
    } else if(progress_analysis()$progress >= 0.78 && progress_analysis()$progress < 0.8)
    {
      title = "Performing Hit Calculations - Overlaps"
    } else if(progress_analysis()$progress >= 0.8 && progress_analysis()$progress <= 0.94)
    {
      title = "Finish Analysis"
    } else if(progress_analysis()$progress ==1)
    {
      title = "Analysis finished"
    } else {title=""}
    
    HTML(paste0("<div id = 'analysis-progress' style='width:70%'><br/>
                <div id='analysis_r_progress_title' class='text-center'><h4 class='text-center'>",title,"<h4></div>
                <div id='analysis_r_progress' class='progress progress-striped shiny-file-input-progress' style='visibility: visible;'>
                <div class='progress-bar' style='width:", perc, "%;'>Progress ", perc, "%</div></div></div>"
    ))
  }
})


## write error messages
output$results_error <- renderUI(HTML(
  paste0("<div class='text-center' style='color:red;'>", error$results, "</br></div>")
))
output$results_errormodal <- renderUI(
  return(HTML(error$results))
)


## reset sets final back
observeEvent(input$resetAnalysis, {
  shinyjs::hide(id="reevaluation-progress")
  shinyjs::hide(id="analysis-progress")
  # close modals so we can open them later on
  if(!is.na(results)){
    shinyBS::toggleModal(session, "analysis_reset", toggle = "open") 
  } else {
    shinyBS::toggleModal(session, "analysis_error", toggle = "close")
  }
  
  
  
  
  status$final <- FALSE
  status$results <- FALSE
  error$results <- ""
  shinyjs::enable("startAnalysis")
  shinyjs::enable("geneID_pos")
  shinyjs::enable("geneID_neg")
  shinyjs::enable("wilcox_pval")
  shinyjs::enable("wilcox_rand")
  shinyjs::enable("deseq2_pval")
  shinyjs::enable("mageck_pval")
  shinyjs::enable("sgrsea_pval")
  shinyjs::enable("edger_pval")
  shinyjs::enable("remove_low")
  shinyjs::enable("remove_threshold_low")
  shinyjs::enable("remove_high")
  shinyjs::enable("remove_threshold_high")
  shinyjs::enable("removegroup")
  shinyjs::enable("bagel_lower")
  shinyjs::enable("bagel_higher")
  shinyjs::enable("screenbeam_iterations")
  shinyjs::enable("screenbeam_burnin")
  shinyjs::enable("screenbeam_run")
  shinyjs::enable("screenbeam_pval")
  
  
  write(paste(userID, ": clicked on resetAnalysis at", Sys.time()), logFile, append = TRUE)
})


## Info how long analysis might take
output$analysisduration <- renderUI({
  shiny::validate(
    need(extractedFiles()$toplength, "CRISPRAnalyzeR provides you with a time estimation once all data has been checked successfully.")
  )
  
 
  # we guess extraction of 300 MB zipped is minimum 1 minute
  timeduration <- round(extractedFiles()$toplength / 3000, digits=0)+1
  if(input$screenbeam_run)
  { # Add ScreenBEAM information
    timeduration <- timeduration * 5
  }
  
  text <- paste("The <strong>minimum expected time</strong> to check and analyse your data <strong>is", timeduration, "minutes</strong>.</br>
                Once the analysis is finished, you will be notified via a popup.</br>
                Please note that all upcoming pages require a successfully finished analysis.", sep=" ")
  return(HTML(text))
  
})




