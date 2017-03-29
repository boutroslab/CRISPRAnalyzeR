# save as 'info_server.r'
# sourced by 'server.r'
# fetching results from 'get_info.r' script








##############
#### Info ####
##############
# fetch results from get_info.r script 
# create error message if not done yet
# if done, test results and show and create error message if necessary
# errors are rendered in both idGview_server.r and idEffects_server.r


#### get updates from analysis.r
progress_info <- reactivePoll(500, NULL, Info_trigger_info, Info_read_info)


#### info
# fetch results from get_info.r
# Check them and create an error message if not done yet or if sth is wrong
# value         NULL
# side effects  writes on status$info and error$info
info <- eventReactive( progress_info(), {
  if( progress_info()$progress == 1 ){
      write(paste(userID, ": get_info.r finished at", Sys.time()), logFile, append = TRUE)

      #error$info <- ""
      
      x <- scan(infoFiles$info, what = "", sep = "\n", quiet = TRUE)
      xlist <- strsplit(x, split = ";", fixed = TRUE) 
      out <- list()
      for( i in 1:length(xlist) ){
        out[[xlist[[i]][1]]] <- xlist[[i]][-1]
      }
    
      test <- Check_info( out$info, userID, messages = config$messages )
      if( test$error == TRUE ){
        error$info <- test$message
        status$info <- FALSE
        write(paste(userID, ": info tested: error"), logFile, append = TRUE)
        
        if(identical(config[["activate.mail"]],TRUE))
        {
          # Send Email
          message <- paste("Analysis Error<br/> for User",userID,"<br/>", out$info, sep=" ")
          title <- paste("[CRISPRAnalyzeR][error][E-CRISP Data Retrieval]", userID, sep=" ")
          attach <- c(file.path(config$logDir, "get_info.log"), logFile) # Attach logfile AND get_info.log for more details
          
          sendmail_car(message = message, title = title, from=NULL, to=NULL, attach=attach, type = "error")
        }
        
        # open modal to show error
        shinyBS::toggleModal(session, "info_error", toggle = "open")
        shinyjs::hide(id="reevaluation-progress")
        
        return()
      }
      
      write(paste(userID, ": info tested: good"), logFile, append = TRUE)
      time$finishInfo <- Sys.time()
    
      error$info <- ""
      ### Open MODAL when Reannotation Extraction is done
      shinyBS::toggleModal(session, "reannotation_finished", toggle = "open")
      status$info <- TRUE
      
      list("rawGenes" = readRDS(file.path(userDir, "rawGenes.rds")), "ecrisp" = readRDS(file.path(userDir, "ecrisp.rds")))
  } else {
      
    error$info <- config$messages$getinfo1$String
    status$info <- FALSE
    return(NULL)
  }
})


#### trigger reactive
observe(info())


## draw progress bar
output$info_progressBar <- renderUI({
  if( progress_info()$progress == 0 ){
    return() 
  } else {
    perc <- round(progress_info()$progress * 100)
    
    if(progress_info()$progress >= 0.1 && progress_info()$progress < 0.3)
    { 
      title = "Setting Up Parameters"
    } else if(progress_info()$progress >= 0.3 && progress_info()$progress < 0.5 )
    {
      title = "Starting with sgRNA Re-Evaluation - Please be patient"
    }else if(progress_info()$progress >= 0.5 && progress_info()$progress < 0.7 )
    {
      title = "sgRNA Re-Evaluation - Please be patient"
    } else if(progress_info()$progress >= 0.8 && progress_info()$progress < 0.9 )
    {
      title = "Finish sgRNA Re-Evaluation and write files"
    } else if(progress_info()$progress == 1)
    {
      title = "sgRNA Re-Evaluation finished"
    } else {title=""}
    
    HTML(paste0("<div id = 'reevaluation-progress' style='width:70%'><br/>
                <div id='info_r_progress_title' class='text-center'><h4 class='text-center'>",title,"<h4></div>
                <div id='info_r_progress' class='progress progress-striped shiny-file-input-progress' style='visibility: visible;'>
                <div class='progress-bar' style='width:", perc, "%;'>Progress ", perc, "%</div></div></div>"
    ))
  }
  })

#### error message
# showing if get_info.r is done
# or whether there was a problem
# render one for sgRNAs and one for Genomic View
output$info_error <- renderUI(HTML(
  if( status$results == TRUE ) {
    
    paste0("<div style='color:red;'>", error$info, "</div>")
  } else {
    NULL
  }
))



output$info_errormodal <- renderUI(
  return(HTML(error$info))
)

