# save as 'sqReplicates_server.r'
# sourced by 'server.r'
# creates plots and ui elements comparing datasets








##################
#### Overview ####
##################
# readcounts of all datasets
# highlight either non-targeting or positive controls
# show plot of all dataset tupels with pearson and spearman 
# correlation coefficients in matrix layout (paired plot)
# can be switched to log10 scale and to aggregated genes
# bAggro = boolean, show aggregated genes
# bLog = show log10 of data
# cCtrl = "pos"/"neg" highlight positive/ non-targeting controls
output$sqReplicates_overview_plot <- renderPlot({
  if( status$results == FALSE ){
    Plot_blank("base", pos = c(0.5, 0.5), msg = config$messages$noanalysisrunyet$String)
  } else {
    
    baggro <- input$sqReplicates_overview_aggro
    blog <- input$sqReplicates_overview_log
    cctrl <- "pos"  #input$sqReplicates_overview_ctrl removed UI element
    dsNames <- c(results()$compare[[1]], results()$compare[[2]])
    data <- results()$readCountVS

    Plot_Replicates_SPLOM( data, dsNames, bLog = blog, bAggro = baggro, cCtrl = cctrl )
  }
})


#### Disable radio buttons and checkboxes
observe({
  if( status$results == TRUE ){
    #if( results()$ctrls$non == TRUE || results()$ctrls$pos == TRUE ){
      #shinyjs::enable("sqReplicates_overview_ctrl")
    #}
    shinyjs::enable("sqReplicates_overview_aggro")
    shinyjs::enable("sqReplicates_overview_log")
  } else {
    #shinyjs::disable("sqReplicates_overview_ctrl")
    shinyjs::disable("sqReplicates_overview_aggro")
    shinyjs::disable("sqReplicates_overview_log")
  }
})








##################
#### Datasets ####
##################
# view correlation of 2 datasets in a scatterplot
# which datasets to compare can be selected by 2 dropdown lists


#### Selection
# create 2 selection lists for choosing which datasets to compare
output$sqReplicates_dataset_select <- renderUI({
  if( status$results == FALSE ){
    NULL
  } else {
    dsNames <- c(results()$compare[[1]], results()$compare[[2]], names(results()$compare)[1],names(results()$compare)[2])
    sel1 <- dsNames[1]   
    if( length(dsNames) > 1 ){ 
      sel2 <- dsNames[2]
    }else{
      sel2 <- dsNames[1]
    }
    
    out <- tagList()
    out[[1]] <- selectInput("sqReplicates_dataset_set1", "Dataset 1", 
                            choices = as.list(as.character(dsNames)), 
                            selected = sel1)
    out[[2]] <- h2("VS")
    out[[3]] <- selectInput("sqReplicates_dataset_set2", "Dataset 2", 
                            choices = as.list(as.character(dsNames)),
                            selected = sel2)    
    out
  }
})


# Output for violine plots effects
output$sqReplicates_labelgene_select <- renderUI({
  if( status$results == TRUE){
    selectizeInput("sqReplicates_labelgene_select", label = "Select (multiple) genes to highlight in the plot",
                   choices = results()$aggregatedReadcount$design, width = "400px",
                   multiple = TRUE, options = list(maxItems = 20))
  } else {
    NULL
  }
})


#### Scatterplot
# scatterplots are created where 2 datasets can be compared
# there are options for log10 and aggregated genes
# either positive or non-targeting ctrls are highlighted
# xx and yy are selections from dropdownlists
# bLog = boolean, log10 scale
# aggro = boolean, aggregated
# ctrl = chr "pos", "neg" positive/ nontargeting
output$sqReplicates_dataset_plot <- renderHighchart2({
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else if( length(input$sqReplicates_dataset_set1) != 0 &&
             length(input$sqReplicates_dataset_set2) != 0 ) {
    XX <- input$sqReplicates_dataset_set1
    YY <- input$sqReplicates_dataset_set2
    
    # #log10?
    # if(input$sqReplicates_dataset_log)
    # {
    #   XX <- log10(XX+0.01)
    #   YY <- log10(YY+0.01)
    # }
    
    highlight <- input$sqReplicates_labelgene_select
    blog <- input$sqReplicates_dataset_log
    baggro <- input$sqReplicates_dataset_aggro
    ctrl <- "pos" #input$sqReplicates_dataset_ctrl removed UI element
    d <- results()$readCountVS
  
    
    Plot_replicates_pair(XX, YY, d, ctrl, bLog = blog, aggro = baggro, bApp = TRUE, filename = paste("QualityControl_Replicates_", XX, "_VS_", YY, sep=""), labelgene = highlight, results = results()$compare )
  }
})


#### Disable radio buttons and check boxes
observe({
  if( status$results == TRUE ){
    # if( results()$ctrls$non == TRUE || results()$ctrls$pos == TRUE ){
    #   shinyjs::enable("sqReplicates_dataset_ctrl")
    # }
    shinyjs::enable("sqReplicates_dataset_log")
    shinyjs::enable("sqReplicates_dataset_aggro")
  } else {
    #shinyjs::disable("sqReplicates_dataset_ctrl")
    shinyjs::disable("sqReplicates_dataset_log")
    shinyjs::disable("sqReplicates_dataset_aggro")
  }
})










# save as 'hcCompareSamples_server.r'
# sourced by 'server.r'
# rendering venn diagrams and table for comparing hits of different methods

######## Input for selecting SampleComparison
########


output$hcCompareSample_select <-  renderUI({
  if( status$results == TRUE ){
    selectizeInput("hcCompareSample_select", label = "Select the Samples to compare",
                   choices = names(results()$sampleList), width = "300px",
                   multiple = FALSE, options = list(maxItems = 1))
  } else {
    HTML(config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Overview log2 foldchange
output$hcCompareSamplesOverviewLOG <- renderHighchart({
  if( status$results == TRUE){
    highcharts.sampleComp(results()$sampleList, sample = NULL, type = "genes", plot = "boxplot", sorted = TRUE, info = "log2fc" , theme = TRUE, addline=FALSE, filename = "HitCalling_Log2FCOverview")
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Genes Z-Ratio
output$hcCompareSamplesOverviewZRATIO <- renderHighchart({
  if( status$results == TRUE){
    highcharts.sampleComp(results()$sampleList, sample = NULL, type = "genes", plot = "boxplot", sorted = TRUE, info = "zratio" , theme = TRUE, addline=FALSE, filename = "HitCalling_Z-RatioOverview")
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Overview LOG2 sgRNA
output$hcCompareSamplesOverviewLOGsgrna <- renderHighchart({
  if( status$results == TRUE){
    highcharts.sampleComp(results()$sampleList, sample = NULL, type = "sgrna", plot = "boxplot", sorted = TRUE, info = "log2fc" , theme = TRUE, addline=FALSE, filename = "HitCalling_Log2FCOverview_sgRNA")
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Overview z-atio sgRNA
output$hcCompareSamplesOverviewZRATIOsgrna <- renderHighchart({
  if( status$results == TRUE){
    highcharts.sampleComp(results()$sampleList, sample = NULL, type = "sgrna", plot = "boxplot", sorted = TRUE, info = "zratio" , theme = TRUE, addline=FALSE, filename = "HitCalling_Z-RatioOverview_sgRNA")
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})



######### USER SELECTS GENE
##### Boxplot log2 foldchange
output$hcCompareSamplesOverviewLOG_sample <- renderHighchart({
  if( status$results == TRUE){
    if(length(input$hcCompareSample_select !=0 ))
    {
      highcharts.sampleComp(results()$sampleList, sample = input$hcCompareSample_select, type = "genes", plot = "column", sorted = TRUE, info = "log2fc" , theme = TRUE, addline=FALSE, filename = paste("HitCalling_Log2FC_",input$hcCompareSample_select , sep=""))
    } else {
      Plot_blank("hc", msg = "Data is being generated.")
    }
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Genes Z-Ratio
output$hcCompareSamplesOverviewZRATIO_sample <- renderHighchart({
  if( status$results == TRUE){
    if(length(input$hcCompareSample_select !=0 ))
    {
      highcharts.sampleComp(results()$sampleList, sample = input$hcCompareSample_select, type = "genes", plot = "column", sorted = TRUE, info = "zratio" , theme = TRUE, addline=FALSE, filename = paste("HitCalling_Z-Ratio_",input$hcCompareSample_select , sep=""))
    } else {
      Plot_blank("hc", msg = "Data is being generated.")
    }
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})

##### Boxplot Overview LOG2 sgRNA
output$hcCompareSamplesOverviewLOGsgrna_sample <- renderHighchart({
  if( status$results == TRUE){
    if(length(input$hcCompareSample_select !=0 ))
    {
      highcharts.sampleComp(results()$sampleList, sample = input$hcCompareSample_select, type = "sgrna", plot = "column", sorted = TRUE, info = "log2fc" , theme = TRUE, addline=FALSE, filename = paste("HitCalling_Log2FC_",input$hcCompareSample_select, "_sgRNA" , sep=""))
    } else {
      Plot_blank("hc", msg = "Data is being generated.")
    }
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})



##### DataTable Output
# Gene
output$hcCompareSamplesTableGene <- DT::renderDataTable({
  shiny::validate(shiny::need(!is.null(results()$sampleList), message = "Data is being generated."))
  d <- Table_DT(results()$sampleList[[input$hcCompareSample_select]]$genes[,c("gene","log2fc","zratio")], colNames = c("Gene", "Log2 Foldchange", "Z-Ratio"), bRownames = FALSE, class = "stripe hover", 
                ordering = list(1, 'asc'), formatRoun = list(cols = c(2,3), digits = 2), filename = paste("HitCalling_",input$hcCompareSample_select , sep="")  )
  return(d)
})

# sgRNA
output$hcCompareSamplesTableSgrna <- DT::renderDataTable({
  shiny::validate(shiny::need(!is.null(results()$sampleList), message = "Data is being generated."))
  d <- Table_DT(results()$sampleList[[input$hcCompareSample_select]]$sgrna[,c("design","gene","log2fc")], colNames = c("sgRNA", "Gene", "Log2 Foldchange"), bRownames = FALSE, class = "stripe hover", 
                ordering = list(1, 'asc'), formatRoun = list(cols = c(2,3), digits = 2) , filename = paste("HitCalling_",input$hcCompareSample_select, "_sgRNA" , sep=""))
  return(d)
})










