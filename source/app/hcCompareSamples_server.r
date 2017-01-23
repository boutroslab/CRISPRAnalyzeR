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







