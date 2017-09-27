# sourced by 'server.r '
# save as 'fileUpload_server.r'
# handling sequencing and library files upload



################################################
#### User interface with Pre-defined settings ##
################################################

# Users can select from a pre-defined list of screening libraries located in ./scripts/predefinedsettings.txt
# If user wants to make their own individual selection, they have to go for CUSTOM

# Required input:
## input$screeninglibrary -> will be the chosen predefined setting

# uiOutput for pre-defined settings
output$input_screeninglibrary <- renderUI({
  
  # Pre-defined settings are stored in config$screeninglibraries as a tibble
  # 
  libs_available <- unique(config$screeninglibraries$Library)
  #out <- selectInput(inputId = "screeninglibrary",multiple = FALSE,width = "80%", selected = "Example",
  #                         label = "Please select the screening Library", choices = libs_available
  #                         )
  
  
  out <- radioGroupButtons2(inputId = "screeninglibrary", label = "Please select the screening Library", choices = libs_available, selected = "Example",
                       status = "default", size = "normal", direction = "horizontal",
                       justified = FALSE, individual = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")) )
  
  return(out)
  
})

output$selectedFASTA <- renderUI({
  shiny::validate(
    shiny::need(input$screeninglibrary, message = FALSE)
  )

  if(input$screeninglibrary == "CUSTOM")
  {
    # show the link to Addgene and tell the user that pre-defined settings have been loaded
    HTML <- column(width=12,
                   column(width=8, offset=2, class="alert alert-danger", style="margin-top:40px;",
                          shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                          shiny::tags$span(
                            shiny::strong("You have chosen to use a CUSTOM library."),
                            shiny::tags$br(),
                            shiny::tags$p("Please upload a FASTA library file and adjust all settings on this and the next pages accordingly.")
                          )
                   )
    )
  } else if(input$screeninglibrary == "Example")
  {
    # show the link to Addgene and tell the user that pre-defined settings have been loaded
    HTML <- column(width=12,
                   column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
                          shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                          shiny::tags$span(
                            shiny::strong("You have chosen to use the sample library.", shiny::tags$br(), "Please go ahead and download the sample data below."),
                            shiny::tags$br()
                            
                          )
                   )
    )
  } else {
    print("library selected")
    URL  <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "URL") %>% select(Value) %>% .[[1]]
    # show the link to Addgene and tell the user that pre-defined settings have been loaded
    HTML <- column(width=12,
                   column(width=8, offset=2, class="alert alert-info", style="margin-top:40px;",
                          shiny::tags$span(style="float:left;" , shiny::HTML('<i class="fa fa-info fa-4x" aria-hidden="true"></i>')),
                          shiny::tags$span(
                            shiny::strong("You have chosen to use a ", paste(input$screeninglibrary,sep=""), " library."),
                            shiny::tags$br(),
                            shiny::tags$p("CRISPRAnalyzeR has automatically set pre-defined settings for this library."),
                            shiny::tags$br(),
                            shiny::tags$p("More information about this library can be found at ", HTML('<a href="',URL,'" target="_blank" class="text" style="font-weight:bold"><i class="fa fa-external-link fa-fw text "></i> Addgene</a>'))
                          )
                   )
    )
  }
  
  return(HTML)
  
})


observe({
  shiny::validate(
    shiny::need(input$screeninglibrary, message = FALSE)
  )
  if(input$screeninglibrary == "CUSTOM")
  {
    shinyjs::show("dataUpload_step1a")
    # shinyjs::show("fastqsettings")
    shinyjs::show("libFile_upload")
    shinyjs::hide("example_data1")
    
  } else if(input$screeninglibrary == "Example")
  {
    shinyjs::show("example_data1")
    shinyjs::hide("dataUpload_step1a")
    shinyjs::hide("libFile_upload")
    
  } else {
    shinyjs::hide("dataUpload_step1a")
    # shinyjs::hide("fastqsettings")
    shinyjs::hide("libFile_upload")
    shinyjs::hide("example_data1")
  }
  
})


### FASTQ Regex Settings

output$InputseqFiles_regexTarget <- renderUI({
  
  shiny::validate(
    shiny::need(input$screeninglibrary, message = "Please select a screening library.")
  )
  
  if(input$screeninglibrary != "CUSTOM")
  {
    regex <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "regexFASTQ") %>% select(Value) %>% .[[1]]
    
     out <- selectizeInput('seqFiles_regexTarget', 'Regular Expression for sgRNA target sequence extraction from FASTQ files)',
                          choices = regex, options = list(create=TRUE, maxItems = 1))
  } else {
    # user might have selected custom library
    out <- selectizeInput('seqFiles_regexTarget', 'Regular Expression for sgRNA target sequence extraction from FASTQ files)',
                          choices = config[["fastq_regex"]], options = list(create=TRUE, maxItems = 1))
  }
  
  
  
  return(out)
})



## FASTA REGEx setting
output$inputlibFile_regex <- renderUI({
  shiny::validate(
    shiny::need(input$screeninglibrary, message = "Please select a screening library.")
  )
  
  if(input$screeninglibrary != "CUSTOM")
  {
    regex <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "regexFASTA") %>% select(Value) %>% .[[1]]
    out <- selectizeInput( 'libFile_regex', 'Please select a regular expression which matches your sgRNA library', 
                           choices = regex, options = list(create = TRUE))
  } else {
    # user might have selected custom library
    out <- selectizeInput( 'libFile_regex', 'Please select a regular expression which matches your sgRNA library', 
                           choices = config[["sgrna_regex"]], options = list(create = TRUE))
  }
  
  return(out)
  
  
})




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
    "No files uploaded yet"
  } else if( status$seqFiles == FALSE ){
  
    file_names <- input$seqFiles_upload$name
    n <- length(file_names)
    out <- tagList()
    for( i in 1:n ){
      # also replace unwanted characters and file endings
      if(grep(pattern = "\\..*$", x =  file_names[i]))
      {
        file_names[i] <- gsub("\\..*$", "" , file_names[i])
      }
      
      out[[i]] <- textInput(paste0("seqFile_gen_name_", i), file_names[i], value = gsub("[[:space:][:blank:][:punct:]]", "_",file_names[i]))
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
    
    # Get file names and paths
    names <- input$seqFiles_upload$name
    paths <- input$seqFiles_upload$datapath
    
 
  # get fastq regex or custom regex
  if(input$seqFiles_regexTargetcustom != "")
  {
    regex <- input$seqFiles_regexTargetcustom
  } else {
    regex <- input$seqFiles_regexTarget
  }
  
  
  needed <- extract_fastq$needed
  
  test <- Check_extract(regex, needed, messages = config$messages)
  if( test$error == TRUE ){
    status$seqFiles <- FALSE
    error$extract <- test$message
    return()
  }
  
  status$seqFiles <- TRUE
  error$seqFiles <- test$message
  
  gen_names <- character()
  for( i in 1:length(names) ){
    gen_names <- c(gen_names, input[[paste0("seqFile_gen_name_", i)]])
  }
    #as.numeric(config[["car.bt2.threads"]])
  test <- Check_seqFiles(names, paths, gen_names, regex, messages = config$message, threads = 1, userdir = userDir, ID = userID, overridealignment = input$override_low_alignment)
  
  if( test$error == TRUE ){
    status$seqFiles <- FALSE
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
  
  # disbale sgRNA re-evaluation button
  shinyjs::disable(id="restartevaluation")
  
})

#### create error Messages
output$seqFiles_error <- renderUI(
  return(HTML(error$seqFiles))
)


#### trigger reactives
observe(seqFiles())


observe(
  if(input$custom_fastqregex == TRUE)
  {
    shinyjs::show(id = "customfastqregex")
    shinyjs::enable(id = "seqFiles_regexTargetcustom")
    shinyjs::disable("seqFiles_regexTarget")
  
  } else {
    shinyjs::hide(id = "customfastqregex")
    shinyjs::disable(id = "seqFiles_regexTargetcustom")
    shinyjs::enable("seqFiles_regexTarget")
    
  }
)

observe(
  if(input$custom_libregex == TRUE)
  {
    shinyjs::show(id = "customlibregex")
    shinyjs::enable(id = "libFile_regexCustom")
    shinyjs::disable("libFile_regex")
    shinyjs::enable("optimizeFASTA")
  } else {
    shinyjs::hide(id = "customlibregex")
    shinyjs::disable(id = "libFile_regexCustom")
    shinyjs::enable("libFile_regex")
    shinyjs::enable("optimizeFASTA")
  }
)




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
  # check if pre-defined library is used
  if(input$screeninglibrary == "CUSTOM")
  {
    if( is.null(input$libFile_upload$name) ){
      error$libFile <- "You have not uploaded a sgRNA library yet"
      return()
    }
    
    name <- input$libFile_upload$name
    path <- input$libFile_upload$datapath
    
    organism <- NULL
    ID <- NULL
    URL <- NULL
    
    # get fastq regex or custom regex
    if(input$libFile_regexCustom != "")
    {
      regex <- input$libFile_regexCustom
    } else {
      regex <- input$libFile_regex
    }
    
  } else {
    
    # A pre-defined screening library has been chosen
    #name <- GETFILENAME HERE
    # path <- put the PATH in here including the filename, so it can be loaded
    
    organism <- NULL
    ID <- NULL
    URL <- NULL
    
    name <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "FASTA") %>% select(Value) %>% .[[1]]
    path <- file.path(config$scriptpath,"external",config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "FASTA") %>% select(Value) %>% .[[1]])
    organism <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "organism") %>% select(Value) %>% .[[1]]
    ID <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "symbol") %>% select(Value) %>% .[[1]]
    URL  <- config$screeninglibraries %>% filter(Library == input$screeninglibrary) %>% filter(Setting == "URL") %>% select(Value) %>% .[[1]]
    regex <- input$libFile_regex
    
  }
  
  test <- Check_libFile(name, path, regex, messages = config$message)

  if( test$error == TRUE ){
    error$libFile <- test$message
    return()
  }
  
  status$libFile <- TRUE
  error$libFile <- test$message
  print(input$optimizeFASTA)
  list("name" = name, "path" = path, "regex" = regex, "organism" = organism, "ID" = ID, "URL" = URL, "optimizeFASTA" = input$optimizeFASTA)
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
  
  # get fastq regex or custom regex
  if(input$libFile_regexCustom != "")
  {
    pat <- input$libFile_regexCustom
  } else {
    pat <- input$libFile_regex
  }
 
  
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
  # we guess extraction of 60 MB zipped is minimum 1 minute
  timeduration <- round(totalfilesize / 60, digits=0)
  
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


