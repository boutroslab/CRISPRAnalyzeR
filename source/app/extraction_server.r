# save as 'extraction_server.r'
# getting input on whether uploaded files are to be extracted and if so, how





#########################
#### file extraction ####
#########################
# give info about whether and how to extract files
# extract()$seqFiles_b = boolean seqFiles need to be extracted?
# extract()$seqFiles_target = chr string of regex for target sequences
# extract()$seqFiles_machine = chr string of regex for machine ID


#### extract_fastq
# reactive boolean with info whether fastq extraction is needed
extract_fastq <- reactiveValues("needed" = FALSE)


#### switch elements
# control regex ui elements for seqFiles part
# if any '.fastQ' extension in file names -> extraction needed
# also set extract_fastq
observe({
  status$seqFiles
  if( is.null(input$seqFiles_upload$name) || is.na(input$seqFiles_upload$name) || 
    length(input$seqFiles_upload$name) == 0 ) {
    extract_fastq$needed <- FALSE
    shinyjs::disable("seqFiles_regexTarget")
    shinyjs::disable("seqFiles_rev")
    shinyjs::disable("seqFiles_bt2Sens")
    shinyjs::disable("seqFiles_bt2quali")
  } else if( any(grepl(".*\\.fastq\\.gz$", tolower(input$seqFiles_upload$name), perl = TRUE)) ){
    extract_fastq$needed <- TRUE
    shinyjs::enable("seqFiles_regexTarget")
    shinyjs::enable("seqFiles_rev")
    shinyjs::enable("seqFiles_bt2Sens")
    shinyjs::enable("seqFiles_bt2quali")
  } else {
    extract_fastq$needed <- FALSE
    shinyjs::disable("seqFiles_regexTarget")
    shinyjs::disable("seqFiles_rev")
    shinyjs::disable("seqFiles_bt2Sens")
    shinyjs::disable("seqFiles_bt2quali")
  } 
})


#### extract
# write extract if ok
extract <- reactive({
  status$extract <- FALSE
  
  needed <- extract_fastq$needed
  target <- input$seqFiles_regexTarget
  
  #Calculate bt2 quality for perfect
  # M{20,21}$ -> perfect
  # [M,X]+M{18}$ <- high
  # [M,X]+M{14}$ <- seed
  length.regex <- sub(pattern = ".+?({.+?}).+", x = target, perl=TRUE, replacement = "\\1")

  if(input$seqFiles_bt2quali == "perfect")
  {
    quali <- paste("M",length.regex,"$", sep="")
  } else if(input$seqFiles_bt2quali == "high")
  {
    quali <- "[M,X]+M{18}$"
  }else if(input$seqFiles_bt2quali == "seed")
  {
    quali <- "[M,X]+M{14}$"
  }
  
  #quali <- input$seqFiles_bt2quali
  rev <- input$seqFiles_rev
  sens <- input$seqFiles_bt2Sens
  
  
  test_seq <- Check_extract(target, needed, messages = config$messages)
  if( test_seq$error == TRUE ){
    error$extract <- test_seq$message
    return()
  }

  status$extract <- TRUE
  error$extract <- test_seq$message
  list("extract" = needed, "targetRegex" = target, "reverse" = rev,
       "bt2Quality" = paste("'", quali,"'", sep=""), "bt2Sensitivity" = sens)
})

# Modal
observeEvent(error$extract, {
  if(error$extract !="" && !is.null(error$extract))
  {
    shinyBS::toggleModal(session, "extracterror", toggle = "open")
  }
})

#### write error messages
output$extract_error <- renderUI(
  return(HTML(error$extract))
)


#### trigger reactives
observe(extract())








#############################
#### Start fastQ extract ####
#############################
# upon clicking on submit button
# if status for lib and seq files and for extraction is good
# disable submit and reset button (to disable starting same job again)
# send file and extraction info to batch process
observeEvent(input$submit_seqFiles, {
  write(paste(userID, ": clicked on submit_seqFiles at", Sys.time()), logFile, append = TRUE)
  
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$extract == TRUE){
    shinyjs::disable("submit_seqFiles") 
    shinyjs::disable("reset_data") 
    shinyjs::disable("seqFiles_regexTarget")
    shinyjs::disable("seqFiles_rev")
    shinyjs::disable("seqFiles_bt2Sens")
    shinyjs::disable("seqFiles_bt2quali")
    shinyjs::disable("libFile_regex")
    
    info2 <- c(paste("progress", paste(0, collapse = ";"), sep = ";"),
              paste("info", paste("", collapse = ";"), sep = ";"),
              paste("libName", paste(libFile()$name, collapse = ";"), sep = ";"),
              paste("libPath", paste(libFile()$path, collapse = ";"), sep = ";"),
              paste("libRegex", paste(libFile()$regex, collapse = ";"), sep = ";"),
              paste("logDir", paste(config$logDir, collapse = ";"), sep = ";"),
              paste("userID", paste(userID, collapse = ";"), sep = ";"),
              paste("userDir", paste(userDir, collapse = ";"), sep = ";"),
              paste("scriptDir", paste(config$scriptpath, collapse = ";"), sep = ";"),
              paste("names", paste(seqFiles()$names, collapse = ";"), sep = ";"),
              paste("paths", paste(seqFiles()$paths, collapse = ";"), sep = ";"),
              paste("gen_names", paste(seqFiles()$gen_names, collapse = ";"), sep = ";"),
              paste("extract", paste(extract()$extract, collapse = ";"), sep = ";"),
              paste("targetRegex", paste(extract()$targetRegex, collapse = ";"), sep = ";"),
              paste("reverse", paste(extract()$reverse, collapse = ";"), sep = ";"),
              paste("bt2Quality", paste(extract()$bt2Quality, collapse = ";"), sep = ";"),
              paste("bt2Sensitivity", paste(extract()$bt2Sensitivity, collapse = ";"), sep = ";"),
              paste("bt2Threads", paste(config$car.bt2.threads, collapse = ";"), sep = ";"))
    write(info2, infoFiles$fastq)
    
    scriptpath <- file.path(config$scriptpath, "fastq_extraction.r") 
    
    log <- c(paste(userID, ": status of input seqFiles, libFile, extract: good"),
      paste(userID, ": starting fastq_extraction.r"),
      paste(userID, ": executing:", paste("Rscript", scriptpath, infoFiles$fastq)))
    write(log, logFile, append = TRUE)
    
    time$startFastq <- Sys.time()
    
    system2("Rscript", args = c(scriptpath, infoFiles$fastq), wait = FALSE, stdout = NULL, stderr = NULL)
  } else {
    write(paste(userID, ": status of input seqFiles, libFile, extract: error"), logFile, append = TRUE)
  }
  
}, ignoreNULL = TRUE)








#########################
#### extracted Files ####
#########################
# info and results from fastq_extraction.r script are read
# info file with progress and possible notification is updated regularly
# when batch script is finished, progress = 1, and result files will be read
# results will be loaded by extractedFiles module
# for info retrieval and visualization reactive objects are created
# extractedFiles()$names = chr array of sequencing file names
# extractedFiles()$paths = chr array of sequencing file paths
# extractedFiles()$gen_names = chr array of user created sequencing file names
# extractedFiles()$libName = chr of sgRNA library file name
# extractedFiles()$libPath = chr os sgRNA library file path
# extractedFiles()$rqc = list with RQC Fastq quality plots


#### get updates from fastq_extraction.r
progress_fastq <- reactivePoll(500, NULL, Info_trigger_fastq, Info_read_fastq)


#### extractedFiles
# write extracted files if ok
extractedFiles <- eventReactive(progress_fastq(), {
  if(progress_fastq()$progress == 1){
      write(paste(userID, ": fastq_extraction.r finished at", Sys.time()), logFile, append = TRUE)
      shinyjs::enable("reset_data") 
      
      error$extractedFiles <- ""
      info <- progress_fastq()$info
      
      x <- scan(infoFiles$fastq, what="", sep="\n", quiet = TRUE)
      xlist <- strsplit(x, split = ";", fixed = TRUE) 
      out <- list()
      for( i in 1:length(xlist) ){
        out[[xlist[[i]][1]]] <- xlist[[i]][-1]
      }
      
      test <- Check_extractedFiles(out$names, out$paths, out$gen_names, info, userID, messages = config$messages)
      if( test$error == TRUE ){
        error$extractedFiles <- test$message
        write(paste(userID, ": extractedFiles tested: error"), logFile, append = TRUE)
        
        if(identical(config[["activate.mail"]],TRUE))
        {
          # Send Email
          message <- paste("FASTQ Extraction Error<br/> for User",userID,"<br/>", out$info, sep=" ")
          title <- paste("[CRISPRAnalyzeR][error][FASTQ Extraction]", userID, sep=" ")
          attach <- file.path(config$logDir, "fastq_extraction.log") # Attach logfile AND analysis.info for more details
          sendmail_car(message = message, title = title, from=NULL, to=NULL, attach=attach, type = "error")
        }
        
        return()
      }
      
        if(out[["rqc"]] != "empty")
        {
          rqc.qa <- readRDS(file = out$rqc)
          
          out$rqc <- rqc.qa
        } else
        {
          out$rqc <- ""
        }
      
    
      
      write(paste(userID, ": extractedFiles tested: good"), logFile, append = TRUE)
      time$finishFasq <- Sys.time()
      
      #########
      # Add stuff for data reviewing
      ## total number sgRNAs
      con <- file(out$libPath)
      top <- readLines(con)
      close(con)
      out$toplength <- as.numeric(length(top))/2
      
      ## total file size
      
      filesize <- file.size(out$oldpaths)
      
      out$totalfilesize <- round((sum(filesize)/1024)/1024, digits = 2) # in MB
      
      out$filesize <- lapply(as.list(out$oldpaths), function(y){
        size <- round((file.size(y)/1024)/1024, digits = 2)
      })
        

      ## get bt2 mapping information
      # stored in log file seqFiles()$names_bt2.log
      out$bt2mapping <- list()
      
      for(i in 1: length(out$names))
      {
        out$bt2mapping[[i]] <- list(NA)
        # check if log file is there
        if(file.access(file.path(userDir, paste(out$names[i],"_bt2.log", sep=""))) == 0)
        {
          # read file
          con <- file(file.path(userDir, paste(out$names[i],"_bt2.log", sep="")))
          top <- readLines(con)
          close(con)
          
          # grep
          # ^\s+\d+\s\((.+?)\)\s.*$ will catch all mapping information
          # Will return a vector, we just need 2,3,4,5
          mappinginfo <- sub(pattern = "^\\s+\\d+\\s\\((.+?)\\)\\s.*$", replacement = "\\1", x = top, perl=TRUE, fixed=FALSE)
          #mappinginfo <- mappinginfo[c(,3,4,5)]
          
          out$bt2mapping[[i]] <- list(
            "notalign" = mappinginfo[3],
            "singlealign" = mappinginfo[4],
            "multiplealign" = mappinginfo[5])
          
          # since mapping was done we have a large FASTQ file, which we want to delte for space purposes.
          # This is not done in case readcount files are uploaded, as wee need them later on.
          command <- "rm"
          arguments <- file.path(userDir, "*.seqFile") 
          system2(command, arguments)
          
        }
        
      }
     
    
      #########
      
      ### Delete all FASTQ extracted files to save disk space - file names are stored in oldpaths
      # Only do this in case FASTQ files have been uploaded!
      
        # for(u in 1:length(out$oldpaths))
        # {
        #   
        #   if(as.character(out$paths[u]) != as.character(out$oldpaths[u]))
        #   {
        #     
        #     if(file.access(file.path(out$oldpaths[u])) == 0)
        #     {
        #       file.remove(file.path(out$oldpaths[u]))
        #     }
        #     
        #     if(file.access(file.path(out$oldextractedpaths[u])) == 0)
        #     {
        #       file.remove(file.path(out$oldextractedpaths[u]))
        #     }
        #   }
        #   
        #   # extracted files
        #   
        # }
      # SAM FILES
      command <- "rm"
      arguments <- file.path(userDir, "*.sam") 
      system2(command, arguments)
      
      # FASTQ
      command <- "rm"
      arguments <- file.path(userDir, "*.fastq.gz") 
      system2(command, arguments)
      
      command <- "rm"
      arguments <- file.path(userDir, "*.fastq") 
      system2(command, arguments)
      
      
      status$extractedFiles <- TRUE
      return(out)
  }
})


#### draw progress bar
output$fastq_progressBar <- renderUI({
  if( progress_fastq()$progress == 0 ){
    return() 
  } else {
    
    if(progress_fastq()$progress >= 0.1 && progress_fastq()$progress < 0.9)
    { 
      title = "Extracting and Mapping Files - Please be patient"
    } else if(progress_fastq()$progress >= 0.9 && progress_fastq()$progress < 0.91 )
    {
      title = "Get FASTQ Quality Information"
    } else if(progress_fastq()$progress >= 0.91 && progress_fastq()$progress < 0.93)
    {
      title = "Creating FASTQ Quality Report"
    } else if(progress_fastq()$progress >= 0.93 && progress_fastq()$progress < 0.97)
    {
      title = "Calculating FASTQ Quality Plots"
    } else if(progress_fastq()$progress >= 0.97 )
    {
      title = "Finish Data Handling"
    } else {title=""}
    
    
    
    perc <- round(progress_fastq()$progress * 100)
    HTML(paste0("<div style='width:70%'><br/>
      <div id='analysis_r_progress_title' class='text-center'><h4 class='text-center'>",title,"<h4></div>
      <div id='fastq_extraction_progress' class='progress progress-striped shiny-file-input-progress' style='visibility: visible;'>
      <div class='progress-bar' style='width:", perc, "%;'>Progress ", perc, "%</div></div></div>"
    ))
  }
})

output$fastq_progressBar2 <- renderUI({
  if( progress_fastq()$progress == 0 ){
    return() 
  } else {
    perc <- round(progress_fastq()$progress * 100)
    HTML(paste0("<div style='width:70%'><br/>
                <div id='fastq_extraction_progress' class='progress progress-striped shiny-file-input-progress' style='visibility: visible;'>
                <div class='progress-bar' style='width:", perc, "%;'>Progress ", perc, "%</div></div></div>"
    ))
  }
  })

### Open MODAL when FASTQ Extraction is done
observeEvent(progress_fastq()$progress, {
  if(progress_fastq()$progress == 1 && (error$extractedFiles == "" || is.null(error$extractedFiles) ) && (error$extract == "" || is.null(error$extract) ) )
  {
    shinyBS::toggleModal(session, "fastqextraction_finished", toggle = "open")
  }
  
 
})

# Modal
observeEvent(error$extractedFiles, {
  if(error$extractedFiles !="" && !is.null(error$extractedFiles))
  {
    shinyBS::toggleModal(session, "extractfileerror", toggle = "open")
  }
})


#### write error messages
output$extractedFiles_error <- renderUI(
  return(HTML(error$extractedFiles))
)


#### trigger reactive
observe(extractedFiles())


#### reset button
# tabset file upload
observeEvent(input$reset_data, {
  status$seqFiles <- FALSE
  status$libFile <- FALSE
  status$groups <- FALSE
  status$extractedFiles <- FALSE
  status$final <- FALSE
  status$results <- FALSE
  error$extractedFiles <- ""
  shinyjs::enable("libFile_regex")
  shinyjs::enable("submit_seqFiles")
  shinyjs::enable("startAnalysis")
  shinyjs::enable("submit_groups")
  write(paste(userID, ": clicked on reset_data at", Sys.time()), logFile, append = TRUE)
})

#### Fastq QC RQC plots

# rqc <- reactive({
#   shiny::validate(
#     shiny::need(!exists(extractedFiles()$rqc), "No Fastq.gz files uploaded or an error occured during FASTQ analysis."),
#     shiny::need(extractedFiles()$rqc != "", "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
#   )
#   extractedFiles()$rqc
# })

output$rqcQCperCycle <- renderPlot({
  shiny::validate(
  shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
 )
  print(extractedFiles()$rqc[["QCperCycle"]])
})

output$rqcGCcontent <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["GCcontent"]])
})

output$rqccycleqcmap <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["cycleqcmap"]])
})

output$rqcQCcycle <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["QCcycle"]])
})

output$rqcCycleBasecall <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["CycleBasecall"]])
})

output$rqcCycleBasecallLine <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["CycleBasecallLine"]])
})

output$rqcReadQualityBoxPlot <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["ReadQualityBoxPlot"]])
})

output$rqcReadQualityPlot <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["ReadQualityPlot"]])
})

output$rqcAverageQualityPlot <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["AverageQualityPlot"]])
})

output$rqcReadFrequency <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["ReadFrequency"]])
})


output$rqcWidth <- renderPlot({
  shiny::validate(
    shiny::need(extractedFiles()$rqc, "No Fastq.gz files uploaded or an error occured during FASTQ analysis.")
  )
  print(extractedFiles()$rqc[["Width"]])
})




##################################
############### Data Review
##################################

# Valueboxes

output$review_numberfiles <- shinydashboard::renderValueBox({
  shiny::validate(
    shiny::need(extractedFiles()$names, "")
  )
  valueBox(width = 2,
    length(extractedFiles()$names), "Samples uploaded", icon = icon("open-file", lib = "glyphicon"),
    color = "blue"
  )
})

output$review_numbersgrnas <- renderValueBox({
  
  shiny::validate(
    shiny::need(extractedFiles()$libPath, "")
  )
  
 
  
  valueBox(width=2,
    extractedFiles()$toplength, "sgRNAs in the library", icon = icon("book", lib = "glyphicon"),
    color = "blue"
  )
})

output$review_filesize <- renderValueBox({
  
  shiny::validate(
    shiny::need(extractedFiles()$paths, "")
  )
  
  
  
  valueBox(width=2,
           extractedFiles()$totalfilesize, "MegaByte total data uploaded", icon = icon("book", lib = "glyphicon"),
           color = "blue"
  )
})

# Download of readcount files
# uses extractedFiles()$paths for processed files and $oldpaths for originally uploaded files


output$download_readcounts <- downloadHandler(
  #shiny::validate(
  #  shiny::need(extractedFiles()$paths, message=FALSE)
  #), 
  filename = function(file) {
    paste('Readcount_',userID,'.tar.gz', sep="")
  },
  
  content = function(con) {
    
    # Rename files
    for(i in 1:length(extractedFiles()$paths))
    {
      file.copy(extractedFiles()$paths[i], file.path(userDir, paste(extractedFiles()$gen_names[i],".txt", sep="")) )
      if(i==1)
      {
        filestozip <- file.path(paste(extractedFiles()$gen_names[i],".txt", sep=""))
      }
      else
      {
        filestozip <- c(filestozip, file.path(paste(extractedFiles()$gen_names[i],".txt", sep="")) )
      }
    }
    
    arguments <- c("-czf", file.path(userDir, paste("Readcount_",userID,".tar.gz", sep="")), paste("-C", file.path(userDir), sep=" "), paste(filestozip, collapse = " "))
    
    gzipped <- try(system2("tar", args = arguments))
  
    file.copy(file.path(userDir, paste("Readcount_",userID,".tar.gz", sep="")), con)
  }
)


### Download of FASTQ Report


output$download_fastq_report <- downloadHandler(
  #shiny::validate(
  #  shiny::need(extractedFiles()$rqc, message="Download FASTQ QC Report (if available")
  #), 
  filename = function(file) {
    paste('FASTQ_QA_Report','.html', sep="")
  },
  content = function(con) {
    file.copy(file.path(userDir, "FASTQ_QA_Report.html"), con)
  }
)


## Overview of Files

output$overview_files <- renderUI ({
  shiny::validate(
    shiny::need(extractedFiles()$paths, "Please wait until CRISPRAnalyzeR has uploaded and checked the data.")
  )
  
  top <- "<div class='section'>
      <div class='container'>
  <div class='row'>
  <div class='col-md-12'>
  <h4 class='text-success text-left'>
  <i class='fa fa-files-o fa-fw'></i>Uploaded / Generated Rawdata Files</h4>
  <table class='table'>
  <thead>
  <tr>
  <th>Filename Original</th>
  <th>Provided Name</th>
  <th>Type</th>
  <th>Size</th>
  <th>Extraction Ratio</th>
  <th>Mapping to Reference</th>
  </tr>
  </thead>
  <tbody>"
  
  bottom <- "</tbody>
                      </table>
  
  </div>
  </div>
  </div>
  </div>"
  
  fileinfo <- data.frame("NameOriginal" = "",
                         "NameNew" = "",
                         "Type" = "",
                         "Size" = "",
                         "extractRatio" = "not available",
                         "bt2mapping" = "not available",
    stringsAsFactors = FALSE
  )
  # loop through files and give back rdy to use HTML
  
  for(i in 1:length(extractedFiles()$paths))
  {
    # Set initals
    fileinfo[i,"extractRatio"] <- "not available"
    fileinfo[i,"bt2mapping"] <- "not available"
    
    # check if paths are different, if it is it is a readcount file as well as a FASTQ file available
    if(extractedFiles()$paths[i] == extractedFiles()$oldpaths[i])
    {
      fileinfo[i,"Type"] <- "Readcount"
    }
    else # it is a fastq files uploaded, so we need to change things a bit
    {
      fileinfo[i,"Type"] <- "Fastq"
      # get extractRatio
      fileinfo[i,"extractRatio"] <- paste((as.numeric(seqFiles()$extractRatio[i])), "%", sep="")
      
      # get bt2mapping
      if(!is.na(extractedFiles()$bt2mapping[[i]][1]))
      {
        #fileinfo[i,"extractRatio"] <- extractedFiles()bt2mapping[i]
        mappinginfo <- c(paste("Not aligned:", extractedFiles()$bt2mapping[[i]][["notalign"]], sep= " "), paste("Aligned once:", extractedFiles()$bt2mapping[[i]][["singlealign"]], sep= " "), paste("Aligned multiple times:", extractedFiles()$bt2mapping[[i]][["multiplealign"]], sep= " "))
        mapping <- paste(mappinginfo, collapse = "<br/>")
        
        fileinfo[i,"bt2mapping"] <- mapping
      }
      
      
      
    }
    # get Size of file
    fileinfo[i,"Size"] <-  paste(extractedFiles()$filesize[[i]],"MegaByte", sep=" ")
    
    # Uploaded name
    fileinfo[i,"NameOriginal"] <- extractedFiles()$names[i]
    
    # New name
    fileinfo[i,"NameNew"] <- extractedFiles()$gen_names[i]
    
    
    # make HTML
    if(i==1)
    {
      fileinfohtml <- paste("<tr>
                      <td>",fileinfo[i,"NameOriginal"],"</td>
      <td>",fileinfo[i,"NameNew"],"</td>
      <td>",fileinfo[i,"Type"],"</td>
      <td>
      ",fileinfo[i,"Size"],"
      </td>
      <td>
      ", fileinfo[i,"extractRatio"],"
      </td>
      <td>
      ", fileinfo[i,"bt2mapping"],"
      </td>
      </tr>", sep="")
    } else
    {
      fileinfohtml <- c(fileinfohtml, paste("<tr>
                      <td>",fileinfo[i,"NameOriginal"],"</td>
                           <td>",fileinfo[i,"NameNew"],"</td>
                           <td>",fileinfo[i,"Type"],"</td>
                           <td>
                           ",fileinfo[i,"Size"],"
                            </td>
                            <td>
                            ", fileinfo[i,"extractRatio"],"
                            </td>
                            <td>
                            ", fileinfo[i,"bt2mapping"],"
                            </td>
                           </tr>", sep="") )
    }
  }
  lengthfiles <- nrow(fileinfo)
  # Add Library
  fileinfo[lengthfiles+1,"Type"] <- "Library File"
  fileinfo[lengthfiles+1,"Size"] <-  paste(round((file.size(extractedFiles()$libPath)/1024)/1024, digits = 2),"MegaByte", sep=" ")
  fileinfo[i,"NameOriginal"] <- extractedFiles()$libName
  fileinfo[i,"NameNew"] <- extractedFiles()$libName
  
  outputhtml <- paste(fileinfohtml, sep ="", collapse= " ")
  outputhtml <- paste(top, outputhtml, bottom, sep="", collapse = " ")
  HTML(outputhtml)
  
})








