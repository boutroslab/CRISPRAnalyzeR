# save as 'help_server.r'
# sourced by 'server.r'
# server side for help tab







################
#### Ticket ####
################
# give the user the chance to complain about errors
# a text input is rendered
# the user can write some text and submit it
# it is saved as a file stored in config$ticketDir
# userID is used as file name
# this ID can be used to browse the logging files and recreate the error


## submit ticket
# disable submit button
# write log
# write ticket
observeEvent(input$help_submit, {
  shinyjs::disable("help_submit")
  write(paste(userID, ": clicked on help_submit at", Sys.time()), logFile, append = TRUE)
  write(paste(userID, ": " , file.path(config$ticketDir, paste(userID,".txt",sep="") )), logFile, append = TRUE)
  
  out <- input$help_inputForm
  write(x = out, file = file.path(config$ticketDir, paste(userID,".txt",sep="") ), append = FALSE)
  
  # Send Email
  if(identical(config[["activate.mail"]],TRUE))
  {
    # Send Email
    logstoattach <- c(file.path(config$logDir, "analysis.log"), file.path(config$logDir, "app.log"),file.path(config$logDir, "fastq_extraction.log"), file.path(config$logDir, "gene_annotation.log"), file.path(config$logDir, "get_info.log"), file.path(config$logDir, "heatmap.log"), file.path(config$logDir, "hit_candidate.log"), file.path(config$ticketDir, paste(userID,".txt",sep="")) )
    text <- paste("New Ticket<br/> for User",userID,"<br/><br/>", out, sep=" ")
    title <- paste("[CRISPRAnalyzeR][ticket]", userID, sep=" ")
    
    sendmail_car(message = text, title = title, from=NULL, to=NULL, attach=NULL, type = "error")
    #sendmail_car(message = text, title = title, from=NULL, to=NULL, attach=logstoattach, type = "error")
  }
 
  # Disable input form
  shinyjs::disable("help_inputForm") # doesnt work
})


## create input form
# after help_submit observer, so that message is read, before
# the acutal input form dissapears
output$help_inputForm <- renderUI({
  if( input$help_submit < 1 ){
    HTML('<textarea id="help_inputForm" rows="10" cols="50">Dear CRISPRAnalyzeR User, please let us know in case you had any trouble. We will come back to you as soon as possible. Please delete this text and enter your own.</textarea>')
  } else {
    HTML("<strong>Thank you for submitting.</strong>")
  }  
})




