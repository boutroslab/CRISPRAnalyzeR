# save as 'essentials_server.r'
# sourced by 'server.r'
# Computed information about essential genes



## Input field for cell lines available in GenomeCRISPR Data file structure
output$essentials_cellline <- renderUI({
  shiny::validate(
    shiny::need(results()$DAISY_essentials, message = "No analysis run yet."),
    shiny::need(results()$GenomeCRISPR_essentials, message = "No analysis run yet.")
  )
  shiny::selectInput(inputId = "essentials_cellline_input",label = "Select a Cell Line",choices = unique(results()$GenomeCRISPR_essentials$cellline),multiple=TRUE,width = "100%")
  
  
})

# DAISY Plot

output$essentials_daisy_distribution <- renderHighchart({
  if(status$results == FALSE)
  {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    data <- tibble("gene" = results()$deseq$data$genes$genes,
                   "fc"= results()$deseq$data$genes$log2FoldChange
    )
    # make plot
    Plot_Essentials(data = data, identifier = annos()$IDnew, mode = "daisy", type = "distribution", essentials = results()$DAISY_essentials, celllines = NULL)
  }
  
  
})


## GENOMECRISPR Cell Line Plot
output$essentials_gcrispr_distribution <- renderHighchart({
  if(status$results == FALSE)
  {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  } else {
    data <- tibble("gene" = results()$deseq$data$genes$genes,
                   "fc"= results()$deseq$data$genes$log2FoldChange
    )
    
    shiny::validate(
      shiny::need(input$essentials_cellline_input , message = "Please select a cell line")
    )
    
    # make plot
    Plot_Essentials(data = data, identifier = annos()$IDnew, mode = "gcrispr", type = "distribution", essentials = results()$GenomeCRISPR_essentials, celllines = input$essentials_cellline_input )
  }
  
  
})


## DAISY Scatter Plot with color of DAISY Core Essentials
output$essentials_DT_DAISY <- renderDataTable({
  shiny::validate(
    shiny::need(status$results==TRUE , message = "No Analysis run yet.")
  )
  # Get Data from DESeq2
  data <- tibble("gene" = results()$deseq$data$genes$genes,
                 "fc"= results()$deseq$data$genes$log2FoldChange
  )
  
  Table_essentials(data = data, identifier = annos()$IDnew, mode = "diasy", essentials = results()$DAISY_essentials, celllines = NULL)
  
})

output$essentials_DT_GCRISPR <- renderDataTable({
  shiny::validate(
    shiny::need(status$results==TRUE , message = "No Analysis run yet."),
    shiny::need(input$essentials_cellline_input , message = "Please select a cell line")
  )
  # Get Data from DESeq2
  data <- tibble("gene" = results()$deseq$data$genes$genes,
                 "fc"= results()$deseq$data$genes$log2FoldChange
  )
  
  Table_essentials(data = data, identifier = annos()$IDnew, mode = "gcrispr", essentials = results()$GenomeCRISPR_essentials, celllines = input$essentials_cellline_input)
  
})



