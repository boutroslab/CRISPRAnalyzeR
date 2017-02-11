# sourced by 'server.r '
# save as 'fileUpload_server.r'
# handling sequencing and library files upload







#################################
#### Sequencing Files Upload ####
#################################
# uploading multiple sequencing files
# renaming these files
# checking for errors (possible error message)
# seqFiles()$names = actual File names
# seqFiles()$paths = data path of files
# seqFiles()$gen_names = generic names
# seqFiles()$regex = regex for FASTQ files


#### create interface for renaming Files
output$seqFiles_rename <- renderUI({
  if( is.null(input$seqFiles_upload$name) ){
    "no files uploaded yet"
  } else if( status$seqFiles == FALSE ){
  
    file_names <- input$seqFiles_upload$name
    n <- length(file_names)
    out <- tagList()
    for( i in 1:n ){
      out[[i]] <- textInput(paste0("seqFile_gen_name_", i), file_names[i], value = file_names[i])
    }
    out
  } else {
    n <- nrow(seqFiles())
    counter <- 1
    out <- tagList()
    for( i in 1:n ){
      out[[counter]] <- shiny::tags$strong(seqFiles()$names[i])
      out[[counter + 1]] <- shiny::tags$br()
      out[[counter + 2]] <- shiny::tags$div(seqFiles()$gen_names[i])
      out[[counter + 3]] <- shiny::tags$br()
      out[[counter + 4]] <- shiny::tags$br()
      counter <- counter + 5
    }
    out
  }
})


#### sequencing files
# write seqFiles if no errors
seqFiles <- eventReactive(input$submit_seqFiles, {
        if( is.null(input$seqFiles_upload$name) ){
        error$seqFiles <- "No sequencing files have been uploaded yet."
        return()
       }
  
  names <- input$seqFiles_upload$name
  paths <- input$seqFiles_upload$datapath
  regex <- input$seqFiles_regexTarget
  
  gen_names <- character()
  for( i in 1:length(names) ){
    gen_names <- c(gen_names, input[[paste0("seqFile_gen_name_", i)]])
  }
    #as.numeric(config[["car.bt2.threads"]])
  test <- Check_seqFiles(names, paths, gen_names, regex, messages = config$message, threads = 1, userdir = userDir, ID = userID)
  
  if( test$error == TRUE ){
    error$seqFiles <- test$message
    return()
  }
  status$seqFiles <- TRUE
  error$seqFiles <- test$message
  
  outputfile <- data.frame("names" = names, "paths" = paths, "gen_names" = gen_names, "extractRatio" = test$extractRatio, stringsAsFactors = FALSE)
  colnames(outputfile) <- c("names", "paths", "gen_names","extractRatio")
  return(outputfile)
}, ignoreNULL = TRUE)

## Modal
# Modal for libfile error
observeEvent(error$seqFiles, {
  if(error$seqFiles !="" && !is.null(error$seqFiles))
  {
    shinyBS::toggleModal(session, "seqfileerror", toggle = "open")
  }
  
})

#### create error Messages
output$seqFiles_error <- renderUI(
  return(HTML(error$seqFiles))
)


#### trigger reactives
observe(seqFiles())








###################################
#### sgRNA library file upload ####
###################################
# upload a sgRNA lib
# also check and give errors
# libFile()$name = actual file name
# libFile()$path = data path of file
# libFile()$regex = regex for gene extraction



#### sgRNA library file 

# write libFile if no error
libFile <- eventReactive(input$submit_seqFiles, {
  if( is.null(input$libFile_upload$name) ){
    error$libFile <- "You have not uploaded a sgRNA library yet"
    return()
  }
  
  name <- input$libFile_upload$name
  path <- input$libFile_upload$datapath
  regex <- input$libFile_regex

  test <- Check_libFile(name, path, regex, messages = config$message)

  
  if( test$error == TRUE ){
    error$libFile <- test$message
    return()
  }
  
  status$libFile <- TRUE
  error$libFile <- test$message
  list("name" = name, "path" = path, "regex" = regex)
})


#### trigger reactives
observe(libFile())

# Modal for libfile error
observeEvent(error$libFile, {
  if(error$libFile !="" && !is.null(error$libFile))
  {
    shinyBS::toggleModal(session, "libfileerror", toggle = "open")
  }
  
})



#### create error messages
output$libFile_error <- renderUI(
  return(HTML(error$libFile))
)


#### toggle helper UI
observe({
  if(!is.null(input$libFile_upload)){ shinyjs::show("libFile_upload_exampleUI", anim = TRUE) 
  } else shinyjs::hide("libFile_upload_exampleUI", anim = TRUE) 
})


#### visualize regex
output$libFile_upload_example <- renderUI({
  file <- input$libFile_upload$datapath
  pat <- input$libFile_regex
  
  # n = lines to show, colors will be recycled
  n <- 5
  cols <- c("#4285f4", "#0f9d58", "#f4b400", "#db4437")
  
  if(!is.null(file) && file.exists(file) && sum(grepl(".*\\(.+\\).*\\(.+\\).*", pat, perl = TRUE)) > 0){ 
    con <- file(file)
    x <- readLines(con)
    close(con)
    if(length(x) < n) return(HTML("<div class='text-center' style='color:red;'>The file has fewer than", n, "lines. There must be something wrong.</div>"))
    x <- x[grepl(pat, x)]
    if(length(x) < n) return(HTML("<div class='text-center' style='color:red;'>The selected regular expression fits to less than", n, "sgRNA identifiers. There must be something wrong.</div>"))
    x <- x[sample(1:length(x), n)]
    
    txt <- character()
    for( i in 1:n ){
      m <- GetCaptures(x[i], pat)
      m <- apply(rbind(m[1, ], apply(m, 2, function(s) substr(x[i], s[2], s[3]))), 2, function(ss){
        if(as.integer(ss[1]) != 0) paste0('<font color="', cols[as.integer(ss[1])], '">', ss[2], "</font>") else ss[2]})
      txt <- c(txt, paste(m, collapse = ""))
    }
    HTML(paste0("<tt>", paste( txt, collapse = "<br/>"), "</tt>"))
  } else NULL
})


## Info how long extraction might take
output$extractiontime <- renderUI({
  shiny::validate(
    shiny::need(input$seqFiles_upload$datapath, "Please upload your files.")
  )

  filesize <- file.size(input$seqFiles_upload$datapath)
  totalfilesize <- round((sum(filesize)/1024)/1024, digits = 2) # in MB
  # we guess extraction of 100 MB zipped is minimum 1 minute
  timeduration <- round(totalfilesize / 100, digits=0)
  
  text <- paste("The <strong>minimum expected time</strong> to check and extract your data <strong>is", timeduration, "minutes</strong>.</br>
                You will be notified once CRISPRAnalyzeR has performed these steps.", sep=" ")
  return(HTML(text))
  
})

output$removeLow_warning <- renderUI({
  shiny::validate(
    shiny::need(identical(analysis()$removeLow, TRUE), message = FALSE  ),
    shiny::need(identical(analysis()$removeLow, TRUE), message = FALSE  )
  )
  
  if(analysis()$removeLow == FALSE && analysis()$removeHigh == FALSE){
    return(HTML(""))
  } else
  {
    out = ""
    if(analysis()$removeLow == TRUE){
      out = HTML('<div class="col-sm-8 col-sm-offset-2 alert alert-warning" style="margin-top:40px;">
    <span style="float:left;"><i class="fa fa-info fa-4x" aria-hidden="true"></i></span>
                 <span>
                 <p><strong>sgRNAs with read count equal to or below ', analysis()$removeThresholdLow ,' have been removed from the data and the analysis.</strong></p>
                 </span>
                 </div>')
    }
    if(analysis()$removeHigh == TRUE){
      out = paste(out, HTML('<div class="col-sm-8 col-sm-offset-2 alert alert-warning" style="margin-top:40px;">
                 <span style="float:left;"><i class="fa fa-info fa-4x" aria-hidden="true"></i></span>
                 <span>
                 <p><strong>sgRNAs with read count equal to or higher ', analysis()$removeThresholdHigh ,' have been removed from the data and the analysis.</strong></p>
                 </span>
                 </div>'), sep=" " )
    }
    return(HTML(out))
  }
  

})


