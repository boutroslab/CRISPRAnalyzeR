# save as 'sqCoverage_server.r'
# sourced by 'server.r'
# plots and tables concerning coverage of screen








#########################
#### Unmapped sgRNAs ####
#########################
# plots giving information about how many sgRNAs/gene were not present in datasets
# is in 'Unmapped' tabset
# 4 barplots total


#### Missing Genes
# barplot missing gene read counts vs datasets
output$sqCoverage_unmapped_basic1 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$statusanalysis$String)
  } else {
    Plot_coverage_unmapped( results()$unmappedGenes, "gene", bApp = TRUE , filename = "QualityControl_Genes_Missing")
  }
)


#### Missing sgRNAs
# barplot missing sgRNA readcounts vs datasets
output$sqCoverage_unmapped_basic2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$statusanalysis$String)
  } else {
    Plot_coverage_unmapped( results()$unmappedGenes, "sgRNA", bApp = TRUE , filename  = "QualityControl_sgRNAs_Missing")
  }
)


#### missing non-coding sgRNAs
# ratio of missing sgRNAs vs non-coding sgRNAs
# ctrls are colour coded
output$sqCoverage_unmapped_non <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$statusanalysis$String)
  } else {
    if( results()$ctrls$non == FALSE ){
      Plot_blank("hc", msg = "No non-targeting controls were defined.")
    } else {
      Plot_coverage_unmapped( results()$unmappedGenes, "non", bApp = TRUE, filename = "QualityControl_Non-TargetingControls_Missing" )
    }
  }
)


#### missing positive ctrl sgRNAs
# ratio of missing sgRNAs vs positive ctrl sgRNAs
# controls are colour coded
output$sqCoverage_unmapped_pos <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$statusanalysis$String)
  } else {
    if( results()$ctrls$pos == FALSE ){
      Plot_blank("hc", msg = "No positive controls were defined.")
    } else {
      Plot_coverage_unmapped( results()$unmappedGenes, "pos", bApp = TRUE, filename = "QualityControl_PositiveControls_Missing" )
    }
  }
)









##########################
#### Designs per Gene ####
##########################
# show histograms for coverage
# one histogram for each dataset
# selectable by dropdown list


#### Selection
# create dropdown selection input to select dataset
output$sqCoverage_designs_select <- renderUI(
  if( status$results == FALSE) {
    NULL
  } else {
    
    opts <- c(results()$compare[[1]], results()$compare[[2]])
    selectInput("sqCoverage_designs_select", label = "dataset", 
                choices = as.list(as.character(opts)),
                selected = as.character(opts[1]))
  }
)


#### histogram
# coverage in sgRNAs per gene as histogram
# unfortunately adjusting bins in highchart is very complicated
# so not included
output$sqCoverage_designs_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$statusanalysis$String)
  } else if( length(input$sqCoverage_designs_select) != 0 ){

    selection <- input$sqCoverage_designs_select
    Plot_coverage_geneDesigns(results()$geneDesigns, selection, bApp = TRUE, filename = paste("QualityControl_sgRNACoverage_", selection , sep="") )
  }
)














