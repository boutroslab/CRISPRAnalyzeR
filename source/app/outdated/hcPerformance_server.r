# save as 'hcPerformance_server.r'
# sourced by 'server.r'
# rendering "waterfall" plots and tables of each method








################
#### Wilcox ####
################
# a kind of waterfall plot is rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always the adjusted p value
# radio = "en"/"de" to show enriched or depleted genes
# this is decided on whether fold change >/< 1
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# here rownames are shown because they are gene names
output$hcPerformance_wilcox_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$noanalysisrunyet$String)
  } else {
    
    radio <- input$hcPerformance_wilcox_radio
    th <- results()$wilcox$info$pval
    sign <- input$hcPerformance_wilcox_sign
    df <- results()$wilcox$data
    Table_hcPerf( df, sign, th, radio, "wilcox" , filename = "HitCalling_Wilcox")
  }
})


#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_wilcox_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    t <- results()$wilcox$info$pval
    df <- results()$wilcox$data
    Plot_performance( data = df, thresh = t, method = "wilcox", bApp = TRUE , filename = "HitCalling_Wilcox_RankedPValue")
  }
)

output$hcPerformance_wilcox_plot2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc",msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$wilcox$info$pval
    df <- results()$wilcox$data$p.value.unadjusted
    Plot_performance( data = df, thresh = t, bApp =  TRUE, type="distribution", method = "Wilcoxon" , filename = "HitCalling_Wilcox_PValueDistribution")
  }
)








################
#### deseq2 ####
################
# a kind of waterfall plot is rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always adjusted p value
# radio = "en"/"de" to show enriched or depleted genes
# this is decided on whether fold change >/< 1
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
output$hcPerformance_deseq_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$noanalysisrunyet$String)
  } else {
    
    radio <- input$hcPerformance_deseq_radio
    th <- results()$deseq$info$pval
    sign <- input$hcPerformance_deseq_sign
    df <- as.data.frame(results()$deseq$data[[1]])
    Table_hcPerf( df, sign, th, radio, "deseq" , filename = "HitCalling_DESeq2")
  }
})


#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_deseq_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$deseq$info$pval
    df <- as.data.frame(results()$deseq$data[[1]])
    Plot_performance( data = df, thresh = t, bApp=TRUE, method = "deseq", filename = "HitCalling_DESEq2_RankedPValue")
  }
)

output$hcPerformance_deseq_plot2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$deseq$info$pval
    df <- results()$deseq$data$genes$pvalue
    Plot_performance( data = df, thresh = t, bApp= TRUE, type="distribution", method = "DESEq2" , filename = "HitCalling_DESeq2_PValueDistribution")
  }
)








################
#### mageck ####
################
# this method has differing p values for enriched and depleted genes
# so 2 kind of waterfall plots are rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# here are 2 p value arrays, one for depleted one for enriched
# radio = "en"/"de" to order by enriched or depleted genes
# this just orders the gene set on rank of depleted or enriched
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# here rownames are shown because they are gene names
output$hcPerformance_mageck_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank( msg = config$messages$noanalysisrunyet$String)
  } else {
    
    radio <- input$hcPerformance_mageck_radio
    th <- results()$mageck$info$pval
    sign <- input$hcPerformance_mageck_sign
    df <- results()$mageck$data[[1]]
    Table_hcPerf( df, sign, th, radio, "mageck" , filename ="HitCalling_MAGeCK")
  }
})


#### waterfall plot enriched
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_mageck_plotEnr <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data[[1]]
    Plot_performance( data = df, thresh = t, bApp = TRUE, method = "mageckEn" , filename = "HitCalling_MAGeCK_RankedPValue_Enrichment")
  }
)


output$hcPerformance_mageck_plotEnr2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data$genes$pos
    Plot_performance( data = df, thresh = t,bApp=TRUE, type="distribution", method = "MAGeCK Enriched" , filename = "HitCalling_MAGeCK_PValueDistribution_Enrichment")
  }
)

#### waterfall plot depleted
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_mageck_plotDep <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data[[1]]
    Plot_performance( data = df, thresh = t, bApp=TRUE, method =  "mageckDe" , filename = "HitCalling_MAGeCK_RankedPValue_Depletion")
  }
)


output$hcPerformance_mageck_plotDep2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data$genes$neg
    Plot_performance( data = df, thresh = t, bApp = TRUE, type="distribution", method = "MAGeCK Depleted" , filename = "HitCalling_MAGeCK_PValueDistribution_Depletion")
  }
)






################
#### sgRSEA ####
################
# this method has differing p values for enriched and depleted genes
# so 2 kind of waterfall plots are rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always adjusted p value
# there are 2 data frames, 1 for enriched, 1 for depleted
# they are asigned and ordered after FDR according to radio
# radio = "en"/"de" to show enriched or depleted genes
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# again rownames are shown because they are gene names
output$hcPerformance_sgrsea_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank( msg = config$messages$noanalysisrunyet$String)
  } else {
    
    radio <- input$hcPerformance_sgrsea_radio
    th <- results()$rsea$info$pval
    sign <- input$hcPerformance_sgrsea_sign
    df <- results()$rsea$data
    Table_hcPerf( df, sign, th, radio, "sgrsea" , filename ="HitCalling_sgRSEA")
  }
})


#### waterfall plot enriched
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_sgrsea_plotEnr <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.pos)
    Plot_performance( data = df, thresh = t, bApp=TRUE, method = "sgrseaEn" ,  filename = "HitCalling_sgRSEA_RankedPValue_Enrichment")
  }
)

output$hcPerformance_sgrsea_plotEnr2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.pos)
    df <- df$p.value.pos
    Plot_performance( data = df, thresh = t, bApp=TRUE , type="distribution", method = "RSEA Enriched",  filename = "HitCalling_sgRSEA_PValueDistribution_Enrichment")
  }
)


#### waterfall plot depleted
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_sgrsea_plotDep <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.neg)
    Plot_performance( data = df, thresh = t, bApp = TRUE, method = "sgrseaDe",  filename = "HitCalling_sgRSEA_RankedPValue_Depletion")
  }
)

output$hcPerformance_sgrsea_plotDep2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.neg)
    df <- df$p.value.neg
    Plot_performance( data = df, thresh = t, bApp=TRUE , type="distribution", method = "RSEA Depleted",  filename = "HitCalling_sgRSEA_PValueDistribution_Depletion")
  }
)







###############
#### edger ####
###############
# a kind of waterfall plot is rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always adjusted p value
# radio = "en"/"de" to show enriched or depleted genes
# this is decided on whether direction is "Up" or "Down"
# they are ordered by P Value
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# again rownames are shown because they are gene names
output$hcPerformance_edger_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank( msg = config$messages$noanalysisrunyet$String)
  } else {
    
    radio <- input$hcPerformance_edger_radio
    th <- results()$edger$info$pval
    sign <- input$hcPerformance_edger_sign
    df <- results()$edger$data$genes
    Table_hcPerf( df, sign, th, radio, "edger" , filename = "HitCalling_EdgeR")
  }
})


#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_edger_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$edger$info$pval
    df <- results()$edger$data$genes
    Plot_performance( data = df, thresh = t, bApp = TRUE, method =  "edger" ,  filename = "HitCalling_EdgeR_RankedPValue")
  }
)

output$hcPerformance_edger_plot2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$edger$info$pval
    df <- results()$edger$data$genes$PValue
    Plot_performance(data = df, thresh = t,bApp = TRUE, type="distribution", method = "EdgeR", filename = "HitCalling_EdgeR_PValueDistribution" )
  }
)


###############
#### Z-Ratio ####
###############
# a kind of waterfall plot is rendered for this method
# and certain method specific statistical values are shown for each gene in a table


#### table
# data table with information of parameters relevant for this analysis method for each gene
# they are ordered by Z-Ratio

output$hcPerformance_zratio_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank( msg = config$messages$noanalysisrunyet$String)
  } else {
    df <- results()$zratio
    df[,2:4] <- round(df[,2:4],digits=4)
    d <- Table_DT(data = df, colNames = c("Gene","Z-Score Untreated", "Z-Score Treated", "Z-Ratio"), bRownames = FALSE, style = "default", class = "display", 
                          dom = "flrtip", ordering = NULL, alignment = list(centre = NULL, justify = NULL, left = NULL), 
                          formatCurr = NULL, formatPerc = NULL, formatRoun = NULL, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE , filename = "HitCalling_Z-Ratio")
    return(d)
  }
})


#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual Z-Ratio
output$hcPerformance_zratio_plot <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- 1.96
    df <- results()$zratio
    Plot_performance( data = df, thresh = t , bApp = TRUE, method = "zratio", type=NULL, filename = "HitCalling_Z-Ratio_RankedZRatio" )
  }
)

output$hcPerformance_zratio_plot2 <- renderHighchart(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- 1.96
    df <- results()$zratio$zratio
    Plot_performance( data = df, thresh = t , bApp = TRUE, type="distribution", method = "Z-Ratio" , filename = "HitCalling_Z-Ratio_ZRatioDistribution")
  }
)












