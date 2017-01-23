# sourced by 'server.r'
# save as 'grouping_server.r'
# creating groups and allocating uploaded files to them








####################
#### Goup Names ####
####################
# defining sample groups
# use this to allocate files with groups
# groupNames()$names  chr array of group names      


#### create input field for defining groups 
# if files were uploaded, otherwise show warning
output$groups_names <- renderUI({
  n <- input$groups_n
  defaults <- c("Treated", "Untreated")
  
  out <- tagList()
  for( i in 1:n ){
    out[[i]] <- textInput(paste0("group_name_", i), paste0("Group ",i), value = defaults[i%%2 + 1])
  }
  
  out
})


#### write groupNames 
# write group names if no errors
groupNames <- reactive({
  status$groupNames <- FALSE
  
  input$submit_seqFiles
  input$reset_data2
  
  names <- character()
  for( i in 1:input$groups_n ){
    names <- c(names, input[[paste0("group_name_", i)]])
  }
    
  test <- Check_groupNames(names, messages = config$messages)
  if( test$error == TRUE ){
    error$groupNames <- test$message
    return()
  }
  
  status$groupNames <- TRUE
  error$groupNames <- test$message
  data.frame("names" = names)
})


#### trigger reactives
observe(groupNames())


#### create error Messages
output$groupNames_error <- renderUI(HTML(
  paste0("<div class='text-center' style='color:red;'>", error$groupNames, "</div>")
))








################
#### Groups ####
################
# allocate files with groups
# this is the actual groups reactive which will be used for analysis
# groups()          list of chr arrays named by groups
# groups()$group1   chr arry of file names belonging to 'group1'
#                   gen_names are taken (file names given by user, not actual file names)


#### disable buttons
# only enable grouping submit button if files and group names are all ok
observe({
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$groupNames == TRUE && status$extract == TRUE &&
      status$groups == FALSE ){
    shinyjs::enable("submit_groups2")
  } else {
    shinyjs::disable("submit_groups2")  
  }
})


#### group file allocation
# create columns to arrange groups and files
output$groups_arrange <- renderUI({
  
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$groupNames == TRUE && status$extract == TRUE &&
      status$groups == FALSE ){
    file_names <- as.character(seqFiles()$gen_names)
    n <- input$groups_n
    group_names <- as.character(groupNames()$names)
  
    out <- tagList()
    for( i in 1:n ){
      out[[i]] <- selectInput(paste0("groups_arrange_", i), label = paste("Group", i), choices = file_names, 
                              multiple = TRUE, selectize = FALSE, size = 4)
    }
    
  } else { # different kind of error messages to appear
    if( status$seqFiles == FALSE || status$libFile == FALSE || status$extract == FALSE ){
      out <- HTML("<div class='text-center' style='color:red;'>Files were not sucessfully uploaded yet.<br/>Please go back to the 'Data' tab to upload files.</div>")  
    } else {
      out <- HTML("<div class='text-center' style='color:red;'>Please define group names first.</div>")  
    }
  }
  
  # if grouping was sucessfully entered already
  if( status$seqFiles == TRUE && status$libFile == TRUE && status$groupNames == TRUE && 
      status$groups == TRUE && status$extract == TRUE ){
    out <- "<div>"
    n <- input$groups_n
    for( i  in 1:n){
      out <- paste0(out, "<strong>", names(groups())[i], "</strong><br/>", 
                    paste0(groups()[[i]], collapse = "<br/>"), "<br/><br/>")
    }
    out <- HTML(paste0(out, "</div>"))
  }
  
  out
})


#### groups
# write groups if no errors
groups <- eventReactive(input$submit_groups2, {
  write(paste(userID, ": clicked on submit_groups2 at", Sys.time()), logFile, append = TRUE)
  
  if( status$groupNames == FALSE ){
    return()
  }
  
  n <- input$groups_n
  group_names <- groupNames()$names
  out <- list() 
  for( i in 1:n ){
    out[[as.character(group_names[i])]] <- input[[paste0("groups_arrange_",i)]]  #$right
  }
  
  test <- Check_groups(out, n, messages = config$messages)
  if( test$error == TRUE ){
    error$groups <- test$message
    write(paste(userID, ": groups tested: error"), logFile, append = TRUE)
    return()
  }
  
  status$groups <- TRUE
  error$groups <- test$message
  
  shinyjs::disable("groups_n")
  shinyjs::disable("biomart_dataset")
  shinyjs::disable("biomart_ID")
  shinyjs::disable("biomart_IDnew")
  
  shinyBS::toggleModal(session, "groups_finished", toggle = "open")
  
  write(paste(userID, ": groups tested: good"), logFile, append = TRUE)
  
  out
}, ignoreNULL = TRUE)


#### trigger reactives
observe(groups())

# Modal
observeEvent(error$groups, {
  if(error$groups !="" && !is.null(error$groups))
  {
    shinyBS::toggleModal(session, "grouperror", toggle = "open")
  }
})

#### create error Messages
output$groups_error <- renderUI(
  paste0(HTML(error$groups)) 
)


#### reset
observeEvent(input$reset_data2, {
  status$groupNames <- FALSE
  status$groups <- FALSE
  status$final <- FALSE
  status$results <- FALSE
  shinyjs::enable("groups_n") 
  shinyjs::disable("biomart_dataset")
  shinyjs::disable("biomart_ID")
  shinyjs::disable("biomart_IDnew")
  shinyjs::enable("submit_groups")
  shinyjs::enable("startAnalysis")
  write(paste(userID, ": clicked on reset_data2 at", Sys.time()), logFile, append = TRUE)
})


