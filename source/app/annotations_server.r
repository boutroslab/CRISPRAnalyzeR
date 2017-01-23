# sourced by 'server.r'
# save as 'annotations_server.r'
# handling annotations and their error messages








##########################
#### Gene Annotations ####
##########################
# read entries for gene annotations and biomart
# check for errors, set error messages
# return reactive annos() if no error, set status$geneAnnotation = true
# annos() list of "checks" and "attribs"
# annos()$checks  chr array of chosen checkboxes 'convert' and 'anno'
# annos()$attribs chr array of chosen annotations (list of possible values in code) 


## Gene Annotation
# Output for Gene Annoation
output$hit_select_annotation <- renderUI({
  if( status$results == TRUE ){
    selectizeInput("hit_select_annotation", label = "Select (multiple) genes",
                   choices = results()$aggregatedReadcount$design, width = "200px",
                   multiple = TRUE, options = list(create=FALSE, maxItems = 10))
  } else {
    HTML(config$messages$statusanalysis$String)
  }
})

output$hit_select_anno <- renderUI({
  if( status$results == TRUE ){
    selectizeInput(
      'hit_select_anno', 'Please select (multiple) annotations',
      choices = config[["biomart.attributes"]], options = list(create=FALSE, maxItems = 20)
    )
  } else {
    HTML(config$messages$statusanalysis$String)
  }
})



#### start gene annotation
# observeEvent for starting hit_candidate.r
# set error$hit to ""
# test for results, info (actually should not be clickable without results and info anyway)
# run check function and give error to error$hit
# otherwise start hit_candidate.r and det error$hit to ""
observeEvent(input$hit_submit_annotation, {
  write(paste(userID, ": clicked on hit_submit_annotation at", Sys.time()), logFile, append = TRUE)
  
  status$geneAnnotation <- FALSE
  
  geneName <- input$hit_select_annotation
  options.anno <- input$hit_select_anno

  wd <- getwd()
  
  write(paste(userID, ": Selected Genes were", paste(geneName, collapse = ", ")), logFile, append = TRUE)
  write(paste(userID, ": Selected Annotations were", paste(options.anno, collapse = ", ")), logFile, append = TRUE)
  test <- Check_hit_annotation( geneName, options.anno )
  if( test$error == TRUE ){
    error$geneAnnotation <- test$message
    status$annohit <- FALSE
    
    write(paste(userID, ": tested hit_select_annotation: error"), 
          logFile, append = TRUE)
    return()
  }
  
  status$annohit <- TRUE
  error$annohit <- ""
  error$geneAnnotation <- ""
  shinyjs::disable("hit_submit_annotation")
  shinyjs::disable("hit_select_annotation")
  shinyjs::disable("hit_select_anno")
  
  scriptpath <- file.path(config$scriptpath, "gene_annotation.r")
  
  log <- c(paste(userID, ": tested hit_select_annotation: good"),
           paste(userID, ": starting gene_annotation.r at", Sys.time()),
           paste(userID, ": executing:", "Rscript", scriptpath, infoFiles$anno))
  write(log, logFile, append = TRUE)
  
  info <- c(paste("progress", 0, sep = ";"),
            paste("info", "", sep = ";"),
            paste("logDir", config$logDir, sep = ";"),
            paste("userID", userID, sep = ";"),
            paste("userDir", userDir, sep = ";"),
            paste("scriptDir", config$scriptpath, sep = ";"),
            paste("funDir", config$Fundir, sep = ";"),
            paste("ecrisp", config$ecrisp, sep = ";"),
            paste("annoDataset", annos()$dataset, sep = ";"),
            paste("annoID", annos()$ID, sep = ";"),
            paste("annoIDnew", annos()$IDnew, sep = ";"),
            paste("geneName", paste(geneName, collapse =";"), sep = ";"),
            paste("annotation", paste(options.anno, collapse=";"), sep = ";"),
            paste("proxyurl", config$car.proxy.url, sep = ";"),
            paste("proxyport", config$car.proxy.port, sep = ";")
            
  )
  write(info, infoFiles$anno)
  write(info, paste0(infoFiles$anno, ".bak"))
  time$startHit <- Sys.time()
  
  system2("Rscript", args = c(scriptpath, infoFiles$anno), wait = FALSE, stdout = NULL, stderr = NULL)
  
}, ignoreNULL = TRUE)


#### write error messages
# for whether the selection was valid
output$hit_error_anno <- renderUI(HTML({
  paste0("<div style='color:red;'>", error$annohit, "</div>")
}))



#### enable/ disable button
# gets enabled if get_info.r is done (sucessfully)
observe({
  if( status$results == TRUE){
    shinyjs::enable("hit_submit_annotation")
  } else {
    shinyjs::disable("hit_submit_annotation")
  }
})



###############################
#### Annotation Results #######
###############################
# fetch results from gene_annotation.r in reactive object 'geneAnnotation'
# test them and render error message
# store results in reactive 'geneAnnotation'


#### get updates from hit_candidate.info
progress_anno <- reactivePoll(500, NULL, Info_trigger_anno, Info_read_anno)


#### Gene Annotation
# fetch results from gene_annotation.r
# Check them and create an error message if sth is wrong
# value         dataframe for genomic view plot
# side effects  writes on status$geneAnnotation and error$geneAnnotation
geneAnnotation <- eventReactive(progress_anno(), {
  if( progress_anno()$progress == 1 ){
    write(paste(userID, ": gene_annotation.r finished at", Sys.time()), logFile, append = TRUE)
    
    shinyjs::enable("hit_submit_annotation")
    shinyjs::enable("hit_select_annotation")
    shinyjs::enable("hit_select_anno")
    
    x <- scan(infoFiles$anno, what="", sep="\n", quiet = TRUE)
    xlist <- strsplit(x, split = ";", fixed = TRUE) 
    out <- list()
    for( i in 1:length(xlist) ){
      out[[xlist[[i]][1]]] <- xlist[[i]][-1]
    }
    
    test <- Check_annotationResults( out$info, userID )
    if( test$error == TRUE ){
      error$geneAnnotation <- test$message
      status$geneAnnotation <- FALSE
      write(paste(userID, ": Gene Annotation tested: error"), logFile, append = TRUE)
      
      # Send out Email
      if(identical(config[["activate.mail"]],TRUE))
      {
        # Send Email
        message <- paste("Gene Annotation Error<br/> for User",userID,"<br/>", info, sep=" ")
        title <- paste("[CRISPRAnalyzeR][error][Gene Annotation]", userID, sep=" ")
        attach <- c(file.path(config$logDir, "gene_annotation.log"), logFile )# Attach logfile AND annotaiton logfor more details
       
        sendmail_car(message = message, title = title, from=NULL, to=NULL, attach=attach, type = "error")
      }
      
      return(NULL)
    }
    
    write(paste(userID, ": Gene Annotation tested: good"), logFile, append = TRUE)
    time$finishHit <- Sys.time()
    
    error$geneAnnotation <- ""
    status$geneAnnotation <- TRUE
    
    list("geneAnnotation" = readRDS(file.path(userDir, "geneAnnotation.rds")))
  } else {
    if( progress_anno()$progress != 0 ){
    
    ## draw progress bar
        shiny::withProgress(message = 'Retrieving Data from biomaRt', value = 0,{
          if( progress_anno()$progress != 1 ){
            #progress_anno()$progress
            setProgress(value = progress_anno()$progress)
          }
        })
    } else {
      return(NULL)
      }
    

  }
})


#### trigger reactive
observe(geneAnnotation())


#### error message
# this error message is for the case that something went wrong in
# gene_annotation.r script
output$geneAnnotation_error <- renderUI(HTML({
  paste0("<div style='color:red;'>", error$geneAnnotation, "</div>")
}))




#### annos
# write annos if no errors
annos <- reactive({
  status$anno <- FALSE
  
  #attribs <- input$biomart_attribs$right
  dataset <- input$biomart_dataset
  ID <- input$biomart_ID
  IDnew <- input$biomart_IDnew
  
  test <- Check_annos(dataset, ID, IDnew)
  if( test$error == TRUE ){
    error$anno <- test$message
    return()
  }
  
  status$anno <- TRUE
  error$anno <- test$message
  list("dataset" = dataset, "ID" = ID, "IDnew" = IDnew)
})
#annos2 <- reactive({
#  status$anno2 <- FALSE
#  
#  #attribs <- input$biomart_attribs$right
#  gene <- input$hit_select_annotation
#  annotation <- input$hit_select_anno
#  
#  test <- Check_hit_annotation(gene, annotation)
#  if( test$error == TRUE ){
#    error$anno2 <- test$message
#     return()
#   }
#   
#   status$anno2 <- TRUE
#   error$anno2 <- test$message
#   list("Genes" = gene, "Annotation" = annotation)
# })


#### trigger reactives
observe(annos())
#observe(annos2())

#### create error Messages
output$annos_error <- renderUI(HTML(
  paste0("<div style='color:red;'>", error$anno, "</div>")
))
#output$annos_error2 <- renderUI(HTML(
#  paste0("<div style='color:red;'>", error$anno2, "</div>")
#))

########################################
########################################
##### Gene Annotation ##################
########################################
##### idGeneannotation.r ###############
########################################

idanno_annotation_react <- reactive({
  if(status$geneAnnotation == FALSE ){
    NULL
  } else {
    geneAnnotation <- geneAnnotation()$geneAnnotation
    df <- geneAnnotation$table
    annos <- colnames(df)
    cnames <- as.character(names(config[["biomart.attributes"]][config[["biomart.attributes"]] %in% annos]))
    colnames(df) <- cnames
    df    
  }
})

output$idanno_annotation <- DT::renderDataTable({
  if( is.null(idanno_annotation_react()) ){
    return(Table_blank( msg = config$messages$noanalysisrunyet$String))
  } else {
    df <- idanno_annotation_react()
    Table_DT(df, colNames = colnames(df), bRownames = FALSE, class = "stripe hover", bScroll = TRUE, filename = "")
  }
})
  
  
  
# output types of annotations
output$listavailable_annotations <- renderUI({
  # this is the Table
  # name and description
  # set up ui element
  
  return_ui <- '  <table class="table table-striped">
    <thead>
  <tr>
  <th>Annotation</th>
<th></th>
<th></th>
  </tr>
  </thead>
  <tbody>'
  
  
  length_bm <- length(config[["biomart.attributes"]])
  for(i in seq(1, length_bm, 3)) {
    
    # get 3 elements
    el1 <- names(config[["biomart.attributes"]])[i]
    el2 <- names(config[["biomart.attributes"]])[i+1]
    el3 <- names(config[["biomart.attributes"]])[i+2]
    
    if(!is.na(el1)) {
      el1 <- paste("<td>",el1,"</td>", sep="")
    } else {
      el1 <- paste("<td>","","</td>", sep="")
    }
    if(!is.na(el2)) {
      el2 <- paste("<td>",el2,"</td>", sep="")
    } else {
      el2 <- paste("<td>","","</td>", sep="")
    }
    if(!is.na(el3)) {
      el3 <- paste("<td>",el3,"</td>", sep="")
    } else {
      el3 <- paste("<td>","","</td>", sep="")
    }
    
    return_ui <- paste(return_ui,
      '<tr>
        ',el1, el2, el3,'
      </tr>'
      , sep="")
    
  }
  return_ui <- paste(return_ui, '</tbody>
  </table>', sep="")
  
  return(HTML(return_ui))
  
})

  
  