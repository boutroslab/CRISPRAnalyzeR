# save as 'sqHeatmap_server.r'
# sourced by 'server.r'
# handles heatmap creation and rendering








######################
#### Switch Input ####
######################
# if results == FALSE, nothing is selectable
# if 'unmapped' is selected, the other parameters are obsolete
# 'type' only makes sense if 'threshold' is selected
observe({
  if( status$results == FALSE ){
    shinyjs::disable("sqHeatmaps_show")
    shinyjs::disable("sqHeatmaps_type")
    shinyjs::disable("sqHeatmap_checkboxes")
    shinyjs::disable("sqHeatmap_ncenter")
  } else if( input$sqHeatmaps_show == "unmapped" ){
    shinyjs::disable("sqHeatmaps_type")
    shinyjs::enable("sqHeatmaps_show")
    shinyjs::disable("sqHeatmap_checkboxes")
    shinyjs::disable("sqHeatmap_ncenter")
  } else if( input$sqHeatmaps_show == "genethreshold" ){
    shinyjs::enable("sqHeatmaps_show")
    shinyjs::enable("sqHeatmaps_type")
    shinyjs::enable("sqHeatmap_checkboxes")
    shinyjs::enable("sqHeatmap_ncenter")    
} else {
    shinyjs::disable("sqHeatmaps_type")
    shinyjs::enable("sqHeatmaps_show")
    shinyjs::enable("sqHeatmap_checkboxes")
    shinyjs::enable("sqHeatmap_ncenter")    
  }
})








#######################
#### start Heatmap ####
#######################
# upon clicking action button
# check whether analysis was already done sucessfully
# if no write log and error message
# if yes, disable action button, write log and send info to batch process

observeEvent(input$sqHeatmap_go, {
  write(paste(userID, ": clicked on sqHeatmap_go at", Sys.time()), logFile, append = TRUE)
  if( status$results != TRUE ){
    error$heatmap <- "You haven't run any analysis yet. Please do this first.<br/>"
    write(paste(userID, ": status of results: error"), logFile, append = TRUE)    
  } else {
    
    status$heatmap <- FALSE
    withProgress(message = 'Performing Heatmap Calculations', value = 0.1, {
    shinyjs::disable("sqHeatmap_go")
    
    info1 <- c(paste("progress", 0, sep = ";"),
              paste("logDir", config$logDir, sep = ";"),
              paste("userID", userID, sep = ";"),
              paste("userDir", userDir, sep = ";"),
              paste("scriptDir", config$scriptpath, sep = ";"),
              paste("funDir", config$Fundir, sep = ";"),
              paste("show", input$sqHeatmaps_show, sep = ";"),
              paste("type", input$sqHeatmaps_type, sep = ";"),
              paste("checks", paste(input$sqHeatmap_checkboxes, collapse = ";"), sep = ";"))
    write(info1, infoFiles$heatmap)
    })
    scriptpath <- file.path(config$scriptpath, "heatmap.r")
                    
    log <- c(paste(userID, ": status of results: good"),
      paste(userID, ": starting heatmap.r at", Sys.time()),
      paste(userID, ": executing: Rscript", scriptpath, infoFiles$heatmap))
    write(log, logFile, append = TRUE)
    
    system2("Rscript", args = c(scriptpath, infoFiles$heatmap), wait = FALSE, stdout = NULL, stderr = NULL)
  }
})


## write error messages
output$heatmap_error <- renderUI(HTML(
  paste0("<div style='color:red;'>", error$heatmap, "</div>")
))










#################
#### heatmap ####
#################
# info and result from heatmap.r script are read
# info file with progress and possible notification is updated regularly
# when batch script is finished, progress = 1, and result file is read
# result will be loaded by heatmap module
# heatmap will eventually be rendered

## get updates from heatmap.r
progress_heatmap <- reactivePoll(500, NULL, Info_trigger_heatmap, Info_read_heatmap)

## heatmap module
heatmap <- eventReactive(progress_heatmap(), {
  if(progress_heatmap()$progress == 1){
      write(paste(userID, ": heatmap.r finished at", Sys.time()), logFile, append = TRUE)
      
      shinyjs::enable("sqHeatmap_go") 
      
      
      error$heatmap <- ""
      info <- progress_heatmap()$info
      
      test <- Check_heatmap(info)
      if(test$error == TRUE ){
        error$heatmap <- test$message
        write(paste(userID, ": heatmap tested: error"), logFile, append = TRUE)
        
        
        if(identical(config[["activate.mail"]],TRUE))
        {
          # Send Email
          message <- paste("Heatmap Error<br/> for User",userID,"<br/>", out$info, sep=" ")
          title <- paste("[CRISPRAnalyzeR][error][Heatmap]", userID, sep=" ")
          attach <- c(file.path(config$logDir, "heatmap.log"),logFile) # Attach logfile AND analysis.info for more details
          
          sendmail_car(message = message, title = title, from=NULL, to=NULL, attach=attach, type = "error")
        }
        
        return()
      }
      withProgress(message = 'Performing Heatmap Calculations', value = 0.6, {
      write(paste(userID, ": heatmap tested: good"), logFile, append = TRUE)
      status$heatmap <- TRUE
      })
      out <- readRDS(file.path(userDir, "heatmap.rds"))
      out
  }
})


## trigger reactive
observe(heatmap())


## plot
output$sqHeatmap_plot <- renderHighchart2({
  if( status$heatmap == FALSE ){
    Plot_blank(device = "base", msg = config$messages$noanalysisrunyet$String)
  } else {
    withProgress(message = 'Generating Heatmap Plot (please be patient)', value = 0.9, {
      heatmap()
    })
  }
})








