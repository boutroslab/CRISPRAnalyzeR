# sourced by 'server.r'
# save as 'hit_server.r'
# handling hit_candidate.r process








#############################
#### start hit candidate ####
#############################
# creating selectize input covering all genes of sgRNA library
# collect user input from selectize input and test it (possible error message)
# if user input ok, start hit_candidate.r


#### create selectize
# select one gene from gene list
# one ui for genomic view, one for sgRNAs
output$hit_select <- renderUI({
  if( status$results == TRUE && status$info == TRUE ){
    selectizeInput("hit_select", label = "Select a gene",
                   choices = results()$aggregatedReadcount$design, width = "300px",
                   multiple = TRUE, options = list(maxItems = 1))
  } else {
    HTML("No analysis run yet.")
  }
})








#### start hit candidate
# observeEvent for starting hit_candidate.r
# set error$hit to ""
# test for results, info (actually should not be clickable without results and info anyway)
# run check function and give error to error$hit
# otherwise start hit_candidate.r and det error$hit to ""
observeEvent(input$hit_submit, {
  write(paste(userID, ": clicked on hit_submit at", Sys.time()), logFile, append = TRUE)
  
  status$hitResults <- FALSE
  
  geneName <- input$hit_select_gView
  options.anno <- input$hit_options.anno
  options.scores <- input$hit_options.scores
  
  write(paste(userID, ": Selected Annotations were", options.anno, options.scores), logFile, append = TRUE)
  test <- Check_hit( geneName, options.anno, options.scores )
  if( test$error == TRUE ){
    error$hit <- test$message
    status$hit <- FALSE
    write(paste(userID, ": tested hit_select: error"), 
          logFile, append = TRUE)
    return()
  }
  
  status$hit <- TRUE
  error$hit <- ""
  error$hitResults <- ""
  shinyjs::disable("hit_submit")
  
  
  scriptpath <- file.path(config$carpools.scriptpath, "hit_candidate.r")
  
  log <- c(paste(userID, ": tested hit_select: good"),
           paste(userID, ": starting hit_candidate.r at", Sys.time()),
           paste(userID, ": executing:", "Rscript", scriptpath, infoFiles$hit))
  write(log, logFile, append = TRUE)
  
  info <- c(paste("progress", 0, sep = ";"),
          paste("info", "", sep = ";"),
          paste("logDir", config$logDir, sep = ";"),
          paste("userID", userID, sep = ";"),
          paste("userDir", userDir, sep = ";"),
          paste("scriptDir", config$carpools.scriptpath, sep = ";"),
          paste("funDir", config$carpoolsFundir, sep = ";"),
          paste("ecrisp", config$ecrisp, sep = ";"),
          
          paste("geneName", geneName, sep = ";"),
          paste("anno.all", options.anno, sep = ";"),
          paste("anno.score", options.scores, sep = ";")
  )
  write(info, infoFiles$hit)
  time$startHit <- Sys.time()
  
  system2("Rscript", args = c(scriptpath, infoFiles$hit), wait = FALSE, stdout = NULL, stderr = NULL)
  
}, ignoreNULL = TRUE)


#### write error messages
# for whether the selection was valid
output$hit_error <- renderUI(HTML({
  paste0("<div style='color:red;'>", error$hit, "</div>")
}))
output$hit_error2 <- renderUI(HTML({
  paste0("<div style='color:red;'>", error$hit, "</div>")
}))


#### enable/ disable button
# gets enabled if get_info.r is done (sucessfully)
observe({
  if( status$results == TRUE && status$info == TRUE ){
    shinyjs::enable("hit_submit")
  } else {
    shinyjs::disable("hit_submit")
  }
})








###############################
#### Hit Candidate Results ####
###############################
# fetch results from hit_candidate.r in reactive object 'hitResults'
# test them and render error message
# store results in reactive 'hitResults'


#### get updates from hit_candidate.info
progress_hit <- reactivePoll(500, NULL, Info_trigger_hit, Info_read_hit)


#### gview
# fetch results from hit_candidate.r
# Check them and create an error message if sth is wrong
# value         dataframe for genomic view plot
# side effects  writes on status$hitResults and error$hitResults
hitResults <- eventReactive( progress_hit(), {
  if( progress_hit()$progress == 1 ){
      write(paste(userID, ": hit_candidate.r finished at", Sys.time()), logFile, append = TRUE)

      shinyjs::enable("hit_submit")
      shinyjs::enable("hit_select_gView")
      
      x <- scan(infoFiles$hit, what="", sep="\n", quiet = TRUE)
      xlist <- strsplit(x, split = ";", fixed = TRUE) 
      out <- list()
      for( i in 1:length(xlist) ){
        out[[xlist[[i]][1]]] <- xlist[[i]][-1]
      }
    
      test <- Check_hitResults( out$info, userID )
      if( test$error == TRUE ){
        error$hitResults <- test$message
        status$hitResults <- FALSE
        write(paste(userID, ": gview tested: error"), logFile, append = TRUE)
        
        
        if(identical(config[["activate.mail"]],TRUE))
        {
          # Send Email
          message <- paste("Genomic View Error<br/> for User",userID,"<br/>", out$info, sep=" ")
          title <- paste("[caR][error][Genomic View]", userID, sep=" ")
          attach <- c(file.path(config$logDir, "hit_candidate.log"), logFile) # Attach logfile AND analysis.info for more details
          
          sendmail_car(message = message, title = title, from=NULL, to=NULL, attach=attach, type = "error")
        }
        
        
        return()
      }
      
      write(paste(userID, ": gview tested: good"), logFile, append = TRUE)
      time$finishHit <- Sys.time()
    
      error$hitResults <- ""
      status$hitResults <- TRUE
      
      list("gView" = readRDS(file.path(userDir, "gView.rds")))
  } else {
    return(NULL)
  }
})


#### trigger reactive
observe(hitResults())


#### error message
# this error message is for the case that something went wrong in
# hit_candidate.r script
output$hitResults_error <- renderUI(HTML({
  paste0("<div style='color:red;'>", error$hitResults, "</div>")
}))


## draw progress bar
output$hit_progressBar <- renderUI({
  if( progress_hit()$progress == 0 ){
    return() 
  } else {
    if( progress_hit()$progress != 1 ){
      HTML(paste0("working... ", progress_hit()$progress * 100, "%"))
    } else {
      HTML("Done")
    }
  }
})




