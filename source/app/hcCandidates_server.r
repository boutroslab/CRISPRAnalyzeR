# save as 'hcCandidates_server.r'
# sourced by 'server.r'
# rendering volcano plots and accomanying tables








##################
#### Tooltips ####
##################
# HTML code for tooltips is needed in every plot here
# it will show a tabular with info about whether the selected point 
# is significant or not in each of the methods
tooltip_hcCandidates <- shiny::tagList(
  shiny::tags$h3("{point.genes}"),
  shiny::tags$table(style = "width:100%", 
    shiny::tags$tr(shiny::tags$td(strong("Method")), shiny::tags$td(strong("Hit"))),
    shiny::tags$tr(shiny::tags$td("Wilcox"), shiny::tags$td("{point.wilcoxSign}")),
    shiny::tags$tr(shiny::tags$td("DESeq2"), shiny::tags$td("{point.deseqSign}")),
    shiny::tags$tr(shiny::tags$td("MAGeCK"), shiny::tags$td("{point.mageckSign}")),
    shiny::tags$tr(shiny::tags$td("sgRSEA"), shiny::tags$td("{point.rseaSign}")),
    shiny::tags$tr(shiny::tags$td("EdgeR"), shiny::tags$td("{point.edgerSign}"))
  )
) %>% as.character()








################
#### Wilcox ####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization


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


# #### table
# # radio = "en"/"de" controls whether enriched or depleted is shown
# # this is determined by fold change >/< 1
# # thresh = num of p value for this method
# # sign = boolean whether only significant hits are shown
# # for wilcox there is a seperate array for fold change (wilcox.log2fc)
# output$hcCandidates_wilcox_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank( msg = config$messages$statusanalysis$String)
#   } else {
#     
#     radio <- input$hcCandidates_wilcox_radio
#     th <- results()$wilcox$info$pval
#     sign <- input$hcCandidates_wilcox_sign
#     df <- results()$hitOverview
#     Table_hcCandidate( df, sign, th, radio, "wilcox" )
#   }
# })


#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_wilcox_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$wilcox$info$pval
    df <- results()$wilcox$data
    Plot_performance( data = df, thresh = t, method = "wilcox", bApp = FALSE , filename = "HitCalling_Wilcox_RankedPValue")
  }
)

output$hcPerformance_wilcox_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc",msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$wilcox$info$pval
    df <- results()$wilcox$data$p.value.unadjusted
    Plot_performance( data = df, thresh = t, bApp =  FALSE, type="distribution", method = "Wilcoxon" , filename = "HitCalling_Wilcox_PValueDistribution")
  }
)


#### volcano plot
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_wilcox_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview
    t <- results()$wilcox$info$pval
    Plot_candidates( d, d2, t, "wilcox", filename = "HitCalling_Wilcox_Candidates" )
  }
)








################
#### deseq2 ####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization

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
output$hcPerformance_deseq_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$deseq$info$pval
    df <- as.data.frame(results()$deseq$data[[1]])
    Plot_performance( data = df, thresh = t, bApp=FALSE, method = "deseq", filename = "HitCalling_DESEq2_RankedPValue")
  }
)

output$hcPerformance_deseq_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$deseq$info$pval
    df <- results()$deseq$data$genes$pvalue
    Plot_performance( data = df, thresh = t, bApp= FALSE, type="distribution", method = "DESEq2" , filename = "HitCalling_DESeq2_PValueDistribution")
  }
)

# #### table
# # radio = "en"/"de" controls whether enriched or depleted is shown
# # this is determined by fold change >/< 1
# # thresh = num of p value for this method
# # sign = boolean whether only significant hits are shown
# # I use fold change, not ld fold change, in table
# output$hcCandidates_deseq_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank( msg = config$messages$noanalysisrunyet$String)
#   } else {
# 
#     radio <- input$hcCandidates_deseq_radio
#     th <- results()$deseq$info$pval
#     sign <- input$hcCandidates_deseq_sign
#     df <- results()$hitOverview
#     Table_hcCandidate( df, sign, th, radio, "deseq" )
#   }
# })


#### volcano plot
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_deseq_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview
    t <- results()$deseq$info$pval
    Plot_candidates( d, d2, t, "deseq" , filename = "HitCalling_DESeq2_Candidates" )
  }
)








################
#### mageck ####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# there are 2 seperate p values for enriched and depleted in this method
# so 2 volcano plots as representative visualization are shown

# 
# #### table
# # radio = "en"/"de" controls whether enriched or depleted is shown
# # this method renders 2 p value arrays, 1 for enriched, 1 for depleted
# # depending on radio different p values are shown (genes are sorted by this p value array)
# # thresh = num of p value for this method
# # sign = boolean whether only significant hits are shown
# # I use fold change, not ld fold change, in table
# output$hcCandidates_mageck_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank( msg = config$messages$noanalysisrunyet$String)
#   } else {
# 
#     df <- results()$hitOverview
#     radio <- input$hcCandidates_mageck_radio
#     th <- results()$mageck$info$pval
#     sign <- input$hcCandidates_mageck_sign
#     Table_hcCandidate( df, sign, th, radio, "mageck" )
#   }
# })


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
output$hcPerformance_mageck_plotEnr <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data[[1]]
    Plot_performance( data = df, thresh = t, bApp = FALSE, method = "mageckEn" , filename = "HitCalling_MAGeCK_RankedPValue_Enrichment")
  }
)


output$hcPerformance_mageck_plotEnr2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data$genes$pos
    Plot_performance( data = df, thresh = t,bApp=FALSE, type="distribution", method = "MAGeCK Enriched" , filename = "HitCalling_MAGeCK_PValueDistribution_Enrichment")
  }
)

#### waterfall plot depleted
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_mageck_plotDep <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data[[1]]
    Plot_performance( data = df, thresh = t, bApp=FALSE, method =  "mageckDe" , filename = "HitCalling_MAGeCK_RankedPValue_Depletion")
  }
)


output$hcPerformance_mageck_plotDep2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$mageck$info$pval
    df <- results()$mageck$data$genes$neg
    Plot_performance( data = df, thresh = t, bApp = FALSE, type="distribution", method = "MAGeCK Depleted" , filename = "HitCalling_MAGeCK_PValueDistribution_Depletion")
  }
)

#### volcano plot enriched
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_mageck_plotEnr <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview 
    t <- results()$mageck$info$pval
    Plot_candidates( d, d2, t, "mageckEn" , filename = "HitCalling_MAGeCK_Candidates_Enrichment" )
  }
)


#### volcano plot depleted
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_mageck_plotDep <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview 
    t <- results()$mageck$info$pval
    Plot_candidates( d, d2, t, "mageckDe" , filename = "HitCalling_MAGeCK_Candidates_Depletion" )
  }
)








################
#### sgRSEA ####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# there are 2 seperate p values for enriched and depleted in this method
# so 2 volcano plots as representative visualization are shown


# #### table
# # radio = "en"/"de" controls whether enriched or depleted is shown
# # this method renders 2 p value arrays, 1 for enriched, 1 for depleted
# # depending on radio different p values are shown (genes are sorted by this p value array)
# # thresh = num of p value for this method
# # sign = boolean whether only significant hits are shown
# # I use fold change, not ld fold change, in table
# output$hcCandidates_sgrsea_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank(msg = config$messages$noanalysisrunyet$String)
#   } else {
# 
#     df <- results()$hitOverview
#     radio <- input$hcCandidates_sgrsea_radio
#     th <- results()$rsea$info$pval
#     sign <- input$hcCandidates_sgrsea_sign
#     Table_hcCandidate( df, sign, th, radio, "rsea" )
#   }
# })


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
output$hcPerformance_sgrsea_plotEnr <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.pos)
    Plot_performance( data = df, thresh = t, bApp=FALSE, method = "sgrseaEn" ,  filename = "HitCalling_sgRSEA_RankedPValue_Enrichment")
  }
)

output$hcPerformance_sgrsea_plotEnr2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.pos)
    df <- df$p.value.pos
    Plot_performance( data = df, thresh = t, bApp=FALSE , type="distribution", method = "RSEA Enriched",  filename = "HitCalling_sgRSEA_PValueDistribution_Enrichment")
  }
)


#### waterfall plot depleted
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_sgrsea_plotDep <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.neg)
    Plot_performance( data = df, thresh = t, bApp = FALSE, method = "sgrseaDe",  filename = "HitCalling_sgRSEA_RankedPValue_Depletion")
  }
)

output$hcPerformance_sgrsea_plotDep2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$rsea$info$pval
    df <- as.data.frame(results()$rsea$data$gene.neg)
    df <- df$p.value.neg
    Plot_performance( data = df, thresh = t, bApp=FALSE , type="distribution", method = "RSEA Depleted",  filename = "HitCalling_sgRSEA_PValueDistribution_Depletion")
  }
)


#### volcano plot enriched
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_sgrsea_plotEnr <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview
    t <- results()$rsea$info$pval
    Plot_candidates( d, d2, t, "sgrseaEn" , filename = "HitCalling_sgRSEA_Candidates_Enrichment" )
  }
)


#### volcano plot depleted
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_sgrsea_plotDep <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview
    t <- results()$rsea$info$pval
    Plot_candidates( d, d2, t, "sgrseaDe" , filename = "HitCalling_sgRSEA_Candidates_Depletion" )
  }
)








###############
#### edger ####
###############
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization


# #### table
# # radio = "en"/"de" controls whether enriched or depleted is shown
# # this is determined by an array "Direction" with "Up" and "Down" values
# # because this array is in another datafrane I also load this dataframe and merge it by genes
# # enriched/depleted is determined whether direction is up/down
# # this means there can be genes with e.g. fold change < 1 but direction = up
# # therefore I decided that direction column will also be shown for clearity
# # thresh = num of p value for this method
# # sign = boolean whether only significant hits are shown
# # I use fold change, not ld fold change, in table
# output$hcCandidates_edger_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank( msg = config$messages$noanalysisrunyet$String)
#   } else {
#     
#     radio <- input$hcCandidates_edger_radio
#     th <- results()$edger$info$pval
#     sign <- input$hcCandidates_edger_sign
#     df <- results()$hitOverview
#     Table_hcCandidate( df, sign, th, radio, "edger" )
#   }
# })


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
output$hcPerformance_edger_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$edger$info$pval
    df <- results()$edger$data$genes
    Plot_performance( data = df, thresh = t, bApp = FALSE, method =  "edger" ,  filename = "HitCalling_EdgeR_RankedPValue")
  }
)

output$hcPerformance_edger_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- results()$edger$info$pval
    df <- results()$edger$data$genes$PValue
    Plot_performance(data = df, thresh = t,bApp = FALSE, type="distribution", method = "EdgeR", filename = "HitCalling_EdgeR_PValueDistribution" )
  }
)

#### volcano plot
# -lg10 of pvalue vs ld of fold change
# significant p values are shown in red
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_edger_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {

    d <- results()$hitOverview_info
    d2 <- results()$hitOverview
    t <- results()$edger$info$pval
    Plot_candidates( d, d2, t, "edger" , filename = "HitCalling_EdgeR_Candidates" )
  }
)


###############
#### Z-Ratio ####
###############
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization


# #### table
# # I use fold change, not ld fold change, in table
# output$hcCandidates_zratio_data <- DT::renderDataTable({
#   if( status$results == FALSE ){
#     Table_blank( msg = config$messages$noanalysisrunyet$String)
#   } else {
#     
#     df <- results()$zratio
#     df[,2] <- round(df[,2], digits=5)
#     df[,3] <- round(df[,3], digits=5)
#     df[,4] <- round(df[,4], digits=5)
#     
#     d <- Table_DT(data = df, colNames = c("Gene","Z-Score Untreated", "Z-Score Treated", "Z-Ratio"), bRownames = FALSE, style = "default", class = "display", 
#                   dom = "flrtip", ordering = NULL, alignment = list(centre = NULL, justify = NULL, left = NULL), 
#                   formatCurr = NULL, formatPerc = NULL, formatRoun = NULL, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE )
#     return(d)
#   }
# })

### table
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
output$hcPerformance_zratio_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- 1.96
    df <- results()$zratio
    Plot_performance( data = df, thresh = t , bApp = FALSE, method = "zratio", type=NULL, filename = "HitCalling_Z-Ratio_RankedZRatio" )
  }
)

output$hcPerformance_zratio_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    
    t <- 1.96
    df <- results()$zratio$zratio
    Plot_performance( data = df, thresh = t , bApp = FALSE, type="distribution", method = "Z-Ratio" , filename = "HitCalling_Z-Ratio_ZRatioDistribution")
  }
)




#### volcano plot
# xy-zoomable
# tooltip gives cross methods information
output$hcCandidates_zratio_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    d <- results()$hitOverview_info
    d2 <- results()$zratio
    t <- 1.96
    Plot_candidates( d, d2, t, "zratio" , filename = "HitCalling_Z-Ratio_Candidates" )
  }
)




################
#### BAGEL #####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always the adjusted p value
# radio = "en"/"de" to show enriched or depleted genes
# this is decided on whether fold change >/< 1
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# here rownames are shown because they are gene names
output$hcPerformance_bagel_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$noanalysisrunyet$String)
  } else if(status$results == TRUE & is.null(results()$bagel$info))
    {
    Table_blank(msg = config$messages$bagelerror$String)
  } else {
    radio = NULL
    th <- results()$bagel$info$cutoff
    sign <- input$hcPerformance_bagel_sign
    df <- results()$bagel$data
    Table_hcPerf( df, sign, th, radio, "bagel" , filename = "HitCalling_BAGEL")
  }
})

output$hcPerformance_bagel_cutoff <- renderText({
  if( status$results == FALSE ){
   returnval <- "not calculated yet"
  } else if(status$results == TRUE && is.null(results()$bagel$info))
  {
    returnval <- "not available"
  } else {
    returnval <- round(results()$bagel$info$cutoff, digits=3)
  }
  return(returnval)
})



#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_bagel_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } 
  else if(status$results == TRUE && is.null(results()$bagel$info))
  {
    Plot_blank("hc", msg = config$messages$bagelerror$String)
  } else {
    
    t <- results()$bagel$info$cutoff
    df <- results()$bagel$data
    Plot_performance( data = df, thresh = t, method = "bagel", bApp = FALSE , filename = "HitCalling_BAGEL_RankBayesFactors")
  }
)

output$hcPerformance_bagel_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc",msg = config$messages$noanalysisrunyet$String)
  } else if(status$results == TRUE && is.null(results()$bagel$info))
  {
    Plot_blank("hc", msg = config$messages$bagelerror$String)
  } else {
    
    t <- results()$bagel$info$cutoff
    df <- results()$bagel$data$BF
    Plot_performance( data = df, thresh = t, bApp =  FALSE, type="distribution", method = "bagel" , filename = "HitCalling_BAGEL_BayesFactorsDistribution")
  }
)


################
#### ScreenBEAM #####
################
# data table comparing phenotype (fold change) with statistic (p value) for this analysis method
# a volcano plot as representative visualization


#### table
# data table with information of parameters relevant for this analysis method for each gene
# parameter in the very right column is always the adjusted p value
# radio = "en"/"de" to show enriched or depleted genes
# this is decided on whether fold change >/< 1
# thresh = p value threshold for this method
# sign = boolean, whether non significant rows are excluded
# here rownames are shown because they are gene names
output$hcPerformance_screenbeam_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$noanalysisrunyet$String)
  } else if(status$results == TRUE & is.null(results()$screenbeam$info))
  {
    Table_blank(msg = config$messages$screenbeamerror$String)
  } else {
    radio = NULL
    th <- results()$screenbeam$info$cutoff
    sign <- input$hcPerformance_screenbeam_sign
    df <- results()$screenbeam$data
    
    Table_hcPerf( df, sign, th, radio, "screenbeam" , filename = "HitCalling_ScreenBEAM")
  }
})

output$hcPerformance_screenbeam_cutoff <- renderText({
  if( status$results == FALSE ){
    returnval <- "not calculated yet"
  } else if(status$results == TRUE && is.null(results()$screenbeam$info))
  {
    returnval <- "not available"
  } else {
    returnval <- round(results()$screenbeam$info$cutoff, digits=3)
  }
  return(returnval)
})



#### waterfall plot
# plot -log10 of p values from highest to lowest
# thresh = num pvalue threshold for this method
# p values below (virtually above) threshold are coloured red
# this plot is x zoomable
# the tooltip shows the actual p value of a point (not -log10)
output$hcPerformance_screenbeam_plot <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } 
  else if(status$results == TRUE && is.null(results()$screenbeam$info))
  {
    Plot_blank("hc", msg = config$messages$screenbeamerror$String)
  } else {
    t <- as.numeric(results()$screenbeam$info$cutoff)
    print(t)
    df <- results()$screenbeam$data
    Plot_performance( data = df, thresh = t, method = "screenbeam", bApp = FALSE , filename = "HitCalling_ScreenBEAM_RankPval")
  }
)

output$hcPerformance_screenbeam_plot2 <- renderHighchart2(
  if( status$results == FALSE ){
    Plot_blank("hc",msg = config$messages$noanalysisrunyet$String)
  } else if(status$results == TRUE && is.null(results()$screenbeam$info))
  {
    Plot_blank("hc", msg = config$messages$screenbeamerror$String)
  } else {
    
    t <- as.numeric(results()$screenbeam$info$cutoff)
    df <- results()$screenbeam$data$pval
    Plot_performance( data = df, thresh = t, bApp =  FALSE, type="distribution", method = "screenbeam" , filename = "HitCalling_ScreenBEAM_PvalDistribution")
  }
)









output$removeLow_warning3 <- renderUI({
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

