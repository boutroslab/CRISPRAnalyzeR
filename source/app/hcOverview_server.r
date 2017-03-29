# save as 'hcOverview_server.r'
# sourced by 'server.r'
# rendering venn diagrams and table for comparing hits of different methods








#############################
#### Hits across methods ####
#############################
# this is a data table with 2 venn diagrams as overview plots
# the table shows fold change and p values of each analysis method for each gene
# whether enriched or depleted genes are shown can be controlled with a radio button
# for each analysis method there is a checkbox (default = all checked)
# if a method is checked, genes which were not significant according to this method are excluded from the table
# all checked = overlap of all methods; none checked = all enriched/depleted genes 


#### table
# radio controls enriched depleted ("en"/"de")
# bMethod = boolean; whether method is checked
# thesh = list of p value thresholds for each method
# take p values from hitOverview dataframe
# use Fold Change, not ld fold change
# actually for wilcox there is a seperate fold change array
# here I use the same for all to make it more readable
# Enriched/ Depleted: generally determined by Fold Change >/< 1
# MAGeCK/ sgRSEA have seperate p value array for enriched/ depleted
# EdgeR determines by 'Direction' = 'Up'/'Down' whether enriched/depleted
output$hcOverview_data <- DT::renderDataTable({
  if( status$results == FALSE ){
    Table_blank(msg = config$messages$statusanalysis$String)
  } else {
    
    checks <- list(wilcox = input$hcOverview_wilcox, deseq = input$hcOverview_deseq, 
                   mageck = input$hcOverview_mageck, rsea = input$hcOverview_rsea, 
                   edger = input$hcOverview_edger)
    th <- list(wilcox = results()$wilcox$info$pval, deseq = results()$deseq$info$pval, 
           mageck = results()$mageck$info$pval,
           rsea = results()$rsea$info$pval, edger = results()$edger$info$pval)
    radio <- input$hcOverview_radio
    df <- results()$hitOverview
    Table_hcOverview( df, radio, checks, th, filename = "Hit_Calling_Overview" )
  }
})


#### venn diagram
# a static VennDiagram object was produced in 'analysis.r'
# this one shows number of significant hits for each method
# reacts to radio button ("en"/"de") and to check boxes for methods
output$hcOverview_venn <- renderPlot(
  if( status$results == FALSE ){
    Plot_blank("base", pos = c(0.5, 0.5), msg = config$messages$statusanalysis$String)
  } else {
    type <- input$hcOverview_radio 
    df <- switch(type, en = results()$vennEnriched, de = results()$vennDepleted)
    ids <- c("Wilcox", "DESeq2", "MAGeCK", "sgRSEA", "EdgeR")
    ids <- ids[c(input$hcOverview_wilcox, input$hcOverview_deseq, input$hcOverview_mageck, input$hcOverview_rsea, input$hcOverview_edger)]
    # if(all(ids) %in% "FALSE")
    # {
    #   return(Plot_blank("base",msg = "Please select at least two algorithms to compare", pos = c(0.5, 0.5)) )
    # }
    if(length(ids %in% "TRUE") <= 1 )
    {
      Plot_blank("base", msg = "Please select at least two algorithms to compare", pos = c(0.5, 0.5))
    }
    print(str(df))
    print(ids)
    
    Plot_venn( df, ids, "en" )
  }
)









