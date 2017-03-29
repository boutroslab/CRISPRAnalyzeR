# sourced by 'server.r'
# save as 'analysisSettings_server.r'
# comparison of 2 groups, positive and negative controls
# normalization methods and analysis settings (pvals and such)








#################
#### Compare ####
#################
# treatment groups which are to be compared in analysis, and positive and negative controls for analysis
# test entries, write error messages, change status and write reactive 'compare' if ok
# compare()$groups    chr array of groups to be compared
# compare()$posCtrl   chr array for pos Ctrls entered (sep by ",", ";" space removed)
# compare()$negCtrl   chr array for neg Ctrls entered (sep by ",", ";" space removed)


#### read sgRNA content and enable Ctrl inputs if libFile ready
libContent <- eventReactive(status$libFile,{
  if( status$libFile == TRUE ){
    shinyjs::enable("geneID_pos")
    shinyjs::enable("geneID_neg")
    #con <- file(libFile()$path)
    #top <- readLines(con)
    #close(con)
    #top <- as.character(top)
    top <- libfile_gene
    #tolower(top)
  } else {
    shinyjs::disable("geneID_pos")
    shinyjs::disable("geneID_neg")
    return()
  }
})


#### create selections for choosing 2 groups
output$groups_compare <- renderUI({
  if( status$groups == FALSE ){
    HTML("<div style='color:red;'><br/>Groups were not defined yet.<br/>Please go back to the 'Set Groups' tab to define treatment groups.</div>")  
  } else if( status$final == TRUE ){
    tagList(fluidRow(
        column(5, h4(input$group_select1)), 
        column(2, h4("VS")),
        column(5, h4(input$group_select2))
      ))
  } else {
    tagList(
      fluidRow(
        column(5,
          selectInput("group_select1", "Untreated Group", choices = names(groups()), selected = names(groups()[1]))
        ), 
        column(2, h2("VS")),
        column(5,
          selectInput("group_select2", "Treated Group", choices = names(groups()), selected = names(groups()[2]))
        )
      ),
      helpText("Untreated left, treated right. Otherwise results will be confusing.")
    )
  }
})


#### compare
# write compare if ok
compare <- reactive({
  status$compare <- FALSE
  
  group1 <- input$group_select1
  group2 <- input$group_select2
  pos <- strsplit(gsub(" ", "", input$geneID_pos, fixed = TRUE), "[,;]", perl = TRUE)[[1]]
  neg <- strsplit(gsub(" ", "", input$geneID_neg, fixed = TRUE), "[,;]", perl = TRUE)[[1]]
  top <- libContent()
  
  test <- Check_compare(group1, group2, pos, neg, top, messages = config$messages)
  if( test$error == TRUE ){
    error$compare <- test$message
    return()
  }
  
  status$compare <- TRUE
  error$compare <- test$message
  out <- list("groups" = c(group1, group2), "posCtrl" = pos, "negCtrl" = neg)
})


#### trigger reactive
observe(compare())


#### write error messages
output$compare_error <- renderUI(HTML(
  paste0("<div style='color:red;'>", error$compare, "</div>")
))


### read count removal

output$remove_in_group <- renderUI({
  shiny::validate(
    shiny::need(input$group_select1, "Please select which groups you would like to compare."),
    shiny::need(input$group_select1, "Please select which groups you would like to compare.")
  )
  selected_groups <- list("all", input$group_select1, input$group_select2)
  names(selected_groups) <- c("Remove based on all datasets (recommended)", paste("Remove only based on datasets in the",input$group_select1,"group", sep=" "), paste("Remove only based on datasets in the",input$group_select2,"group", sep=" "))
  
  
  selectInput("removegroup", "Please select on which groups the removal of sgRNAs should be based on", choices = selected_groups, selected = selected_groups[[1]])
  
})





###########################
#### Analysis Settings ####
###########################
# combine many analysis settings in one reactive
# I didn't see the need for a input check here
# if needed check function can be added later on
# creates reactive 'analysis'
# look at code to see values. they are like
# analysis()$normal   chr string, boolean or numeric


#### analysis
# write analysis if no errors
analysis <- reactive({
  status$analysis <- FALSE
  
  #Initialize
  wilcoxPval <- NA
  wilcoxRand <- NA
  deseq2Pval <- NA
  mageckPval <- NA
  sgrseaPval <- NA
  edgerPval <- NA
  bagel_lower <- NA
  bagel_higher <- NA
  screenbeam_iterations <- NA
  screenbeam_burnin <- NA
  screenbeam_run <- NA
  screenbeam_pval <- NA
  
  wilcoxPval <- input$wilcox_pval
  wilcoxRand <- input$wilcox_rand
  deseq2Pval <- input$deseq2_pval
  mageckPval <- input$mageck_pval
  sgrseaPval <- input$sgrsea_pval
  edgerPval <- input$edger_pval
  bagel_lower <- input$bagel_lower
  bagel_higher <- input$bagel_higher
  screenbeam_iterations <- input$screenbeam_iterations
  screenbeam_burnin <- input$screenbeam_burnin
  screenbeam_run <- input$screenbeam_run
  screenbeam_pval <- input$screenbeam_pval
  
  
  # remove Threshold
  removeLow <- input$remove_low
  removeThresholdLow <- input$remove_threshold_low
  
  removeHigh <- input$remove_high
  removeThresholdHigh <- input$remove_threshold_high
  
  removeGroups <- input$removegroup
  
  
  test <- Check_analysisSettings(wilcoxPval, wilcoxRand, deseq2Pval, mageckPval, sgrseaPval, edgerPval, removeLow, removeHigh, removeThresholdLow, removeThresholdHigh, bagel_lower, bagel_higher, screenbeam_iterations, screenbeam_burnin, screenbeam_run, screenbeam_pval, messages = config$messages)
  if( test$error == TRUE ){
    error$analysis <- test$message
    return()
  }
  
  status$analysis <- TRUE
  error$analysis <- test$message
  out <- list(  
    "wilcoxPval" = wilcoxPval, "wilcoxRand" = wilcoxRand, "deseq2Pval" = deseq2Pval,
    "mageckPval" = mageckPval, "sgrseaPval" = sgrseaPval, "edgerPval" = edgerPval, "removeLow" = removeLow, "removeHigh" = removeHigh, "removeThresholdLow" = removeThresholdLow, "removeThresholdHigh" = removeThresholdHigh, "removeGroups" = removeGroups,
    "bagel_lower" = bagel_lower,"bagel_higher" = bagel_higher, "screenbeam_iterations" = screenbeam_iterations, "screenbeam_burnin" = screenbeam_burnin, "screenbeam_run" =  screenbeam_run, "screenbeam_pval" = screenbeam_pval
  )
  
  
})


#### trigger reactive
observe(analysis())


#### write error messages
output$analysis_error <- renderUI(HTML(
  paste0("<div style='color:red;'>", error$analysis, "</div>")
))



