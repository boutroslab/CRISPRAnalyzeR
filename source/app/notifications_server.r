# sourced from 'server.r'
# save as 'log_server.r'
# notifications and info about status of CRISPRAnalyzeR app




############
### Log ####
############
# intended to give user information about status of app in short form
# will be rendered as a 'tasks'  menu
# it appears after the first kind of input is done
output$logMenu <- renderMenu({
  
  counter <- 0
  noti <- list()

  # if file extraction with bowtie2 was started
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$extract == TRUE &&
      extract()$extract ){
    counter <- counter + 1
    noti[[counter]] <- menuItem(text = paste0("fastQ extraction started at ", 
                                                      format(time$startFastq, format = "%H:%M:%S"), "..."))
    if( progress_fastq()$progress != 1 ){
      counter <- counter + 1
      noti[[counter]] <- taskItem(text = "Progress", value = progress_fastq()$progress * 100)
    }
  }
  
  # if file extraction with bowtie2 has finished
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$extract == TRUE &&
      extract()$extract && status$extractedFiles == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- menuItem(text = paste0("... finished at ", 
                                                      format(time$finishFasq, format = "%H:%M:%S")))
  }
    
  # tasks of data tab sucessfully done
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$extract == TRUE &&
      status$extractedFiles == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "Data tab is ready", icon("th"))
  }
  
  # tasks of setup tab sucessfully done
  if( status$groups == TRUE && status$anno == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "Setup tab is ready", icon("cog"))
  }
  
  # tasks of screening tab sucessfully done
  if( status$compare == TRUE && status$analysis == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "Settings tab is ready", icon("play"))
  }

  # analysis started
  input$startScreen
  if( status$final == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- menuItem(text = paste0("analysis started at ", 
                          format(time$startAnalysis, format = "%H:%M:%S"), "..."))
    if( progress_analysis()$progress != 1 ){
      counter <- counter + 1
      noti[[counter]] <- taskItem(text = "Progress", value = progress_analysis()$progress * 100)
    }
  }
  
  # if analysis finished successfully
  if( status$results == TRUE && status$final == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- menuItem(text = paste0("... finished at ", 
                          format(time$finishAnalysis, format = "%H:%M:%S")))
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "Screen Quality is ready", icon("thumbs-up"))
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "Hit Calling is ready", icon("list"))
  }
  
  # if get_info finished successfully
  if( status$results == TRUE && status$info == TRUE ){
    counter <- counter + 1
    noti[[counter]] <- notificationItem(text = "In-depth Analysis is ready", icon("search"))
  }
    
  dropdownMenu(type = "tasks", badgeStatus = "success", .list = noti) 
})






##############
#### Info ####
##############
# this is a 'messages' dropdown menu
# it appears after the analysis was started
# it has some basic information like what gorups where compared an which ctrls were used
# the idea is, that if you browse around in some other tab
# you can always lookup some basics, you set for the analysis
output$infoMenu <- renderMenu({

  # short info about analysis
  input$startScreen
  if( status$final == TRUE ){
    grps <- character()
    for( i  in 1:length(groups()) ){
      grps <- paste0(grps, "<strong>", names(groups())[i], "</strong><br/>", 
                    paste0(groups()[[i]], collapse = "<br/>"), "<br/>")
    }
    
    info <- messageItem(from = "Screen Overview",
      HTML(paste0( "<br/><br/><h4>", compare()$groups[1], " VS ", compare()$groups[2], "</h4>",
      "<br/>Positive Ctrl: ", if( length(compare()$posCtrl) == 0 ) "none" else paste(compare()$posCtrl, collapse = ", "), 
      "<br/>Non-target Ctrl: ", if( length(compare()$negCtrl) == 0 ) "none" else paste(compare()$negCtrl, collapse = ", "),
      "<br/><br/>Wilcox P Value: ", analysis()$wilcoxPval, ", random Picks: ", analysis()$wilcoxRand, 
      "<br/>DESeq2 P Value: ", analysis()$deseq2Pval, "<br/>MAGeCK P Value: ", analysis()$mageckPval,
      "<br/>sgRSEA P Value: ", analysis()$sgrseaPval, "<br/>Edger P Value: ", analysis()$edgerPval,
      "<br/><br/>", grps
      )), icon = icon("navicon")
    )
  } else {
    return(NULL)
  }
  dropdownMenu(type = "messages", icon = icon("navicon"), .list = list(info)) 
})





