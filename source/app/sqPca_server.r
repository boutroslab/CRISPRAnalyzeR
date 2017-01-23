#saved as sqPca_server.r

output$pcaplotgene <- renderHighchart2({
  if( status$results == FALSE ){
    Plot_blank("hc",msg = config$messages$noanalysisrunyet$String)
  } else {
    shiny::withProgress(message = 'Generating PCA Plot', value = 0,{
  d <- Plot_pca(pca = results()$pca$genes, title = "Principal Component Analysis", subtitle = NULL, export = TRUE, legend = TRUE, scale=1, filename = "QualityControl_PrincipleComponentAnalysis") 
  return(d)
    })
  }
})