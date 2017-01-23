# save as 'sqStats_server.r'
# sourced by 'server.r'
# computes tables with general statistics about screen








#######################
#### General Stats ####
#######################
# tabular about basic statistics of screen
# about sgRNA and genes


## Basic Tabular for sgRNAs
output$sqStats_basic_sgRNA <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$statusanalysis$String)
  } else {
    df <- results()$statsGeneral$basic$sgRNA
    Table_small(df, filename = "sgRNA_basic_stats")
  }
})


## Basic Tabular for Genes
output$sqStats_basic_genes <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$statusanalysis$String)
  } else {
    df <- results()$statsGeneral$basic$Gene
    Table_small(df, filename = "Gene_basic_stats")
  }
})








##################
#### Controls ####
##################
# table with statistics about controls


## table for positive Controls
output$sqStats_ctrl_pos <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$statusanalysis$String)
  } else if( results()$ctrls$pos == FALSE ){
    Table_blank(msg = "No positive control defined.")
  } else{
    df <- results()$statsGeneral$ctrl.p
    Table_small( df, type = "ctrl" , filename ="sqStats_ctrl_pos")
  }
})


## table for negative Controls
output$sqStats_ctrl_neg <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$statusanalysis$String)
  } else if( results()$ctrls$non == FALSE ){
    Table_blank(msg = "No non-targeting control defined.")
  } else{
    df <- results()$statsGeneral$ctrl.n
    Table_small( df, type = "ctrl", filename ="sqStats_ctrl_pos")
  }
})








#################
#### Details ####
#################
# in the ui this tabset is called 'Raw Readcounts'
# this table shows either readcount statistics per gene for each dataset or
# raw read counts for each sgRNA
# for the first case a selection for each dataset is created


## Selection
# create selection input
output$sqStats_details_select <- renderUI(
  if( status$results == FALSE) {
    NULL
  } else {
    selectInput("sqCoverage_details_select", label = "dataset", 
                choices = as.list(as.character(extractedFiles()$gen_names)), 
                selected = as.character(extractedFiles()$gen_names[1]))
  }
)


## table
output$sqStats_details_out <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$noanalysisrunyet$String)
  } else if( input$sqStats_details_radio == "sgRNA" ){
    df <- results()$readcount
    Table_big(df)
  } else if( length(input$sqCoverage_details_select != 0) ){
      df <- results()$statsGeneral$dataset
      ds <- input$sqCoverage_details_select
      Table_big( df, ds , filename = "sqStats_dataset_detail")
    }
})


## enable/ disable radio Button and selection dropdown
observe({
  if( status$results == TRUE ){
    shinyjs::enable("sqStats_details_radio")
    if( input$sqStats_details_radio == "gene" ){
      shinyjs::enable("sqCoverage_details_select")
    } else {
      shinyjs::disable("sqCoverage_details_select")
    }
  } else {
    shinyjs::disable("sqStats_details_radio")
  }
})


###########################
#### Read Distribution ####
###########################
# sgRNA read count distribution in each dataset

#### Area curves plot
# this is a plot showing readcount distributions of all datasets as area curves
# datasets can be excluded/included by clicking on legend
# zoom is possible by dragging a rectangle with mouse
output$sqCoverage_distro_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    Plot_coverage_readDistro( results()$readDistribution, bApp = TRUE, filename = "QualityControl_ReadCount_Distribution" )
  }
)

output$sqCoverage_distroBox_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    Plot_coverage_readDistroBox( results()$readDistributionBox, bApp = TRUE, filename = "QualityControl_ReadCount_DistributionBox" )
  }
)

output$sqCoverage_distroBoxNorm_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    Plot_coverage_readDistroBox( results()$readDistributionBoxNorm, bApp = TRUE , type = "Gene", filename = "QualityControl_ReadCount_DistributionBox")
  }
)







####################
#### Read Depth ####
####################
# for each dataset there is a barplot with reads per sgRNA

#### Selection
# create selection input to select dataset
output$sqCoverage_readDepth_select <- renderUI(
  if( status$results == FALSE) {
    NULL
  } else {
    
    opts <- c(results()$compare[[1]], results()$compare[[2]])
    selectInput("sqCoverage_readDepth_select", label = "dataset",
                choices = as.list(as.character(opts)),
                selected = as.character(opts[1]))
  }
)


#### barplot 
# for selected dataset read depth for each gene will be shown
# this will be a very dense bar plot, so usual bars are less visible to reduce overplotting
# non targeting controls are highlighted in blue, positive controls in red
# this plot is x-zoomable
output$sqCoverage_readDepth_plot <- renderHighchart({
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else if( length(input$sqCoverage_readDepth_select) != 0 ){
    
    selection <- as.character(input$sqCoverage_readDepth_select)
    Plot_coverage_readDepth( results()$readDepth, selection, results()$ctrls$non, results()$ctrls$pos , filename = paste("QualityControl_", input$sqCoverage_readDepth_select,"_Readcount_per_sgRNA", sep="") )
  }
})



output$removeLow_warning2 <- renderUI({
  shiny::validate(
    shiny::need(identical(analysis()$removeLow, TRUE), message = FALSE  ),
    shiny::need(identical(analysis()$removeLow, TRUE), message = FALSE  )
  )
  
  if(analysis()$removeLow == FALSE && analysis()$removeHigh == FALSE){
    return(HTML(""))
  } else
  {
    out = ""
    if(analysis()$removeLow == TRUE){
      out = HTML('<div class="col-sm-8 col-sm-offset-2 alert alert-warning" style="margin-top:40px;">
                 <span style="float:left;"><i class="fa fa-info fa-4x" aria-hidden="true"></i></span>
                 <span>
                 <p><strong>sgRNAs with read count equal to or below ', analysis()$removeThresholdLow ,' have been removed from the data and the analysis.</strong></p>
                 </span>
                 </div>')
    }
    if(analysis()$removeHigh == TRUE){
      out = paste(out, HTML('<div class="col-sm-8 col-sm-offset-2 alert alert-warning" style="margin-top:40px;">
                            <span style="float:left;"><i class="fa fa-info fa-4x" aria-hidden="true"></i></span>
                            <span>
                            <p><strong>sgRNAs with read count equal to or higher ', analysis()$removeThresholdHigh ,' have been removed from the data and the analysis.</strong></p>
                            </span>
                            </div>'), sep=" " )
    }
    return(HTML(out))
    }
  
  
    })





