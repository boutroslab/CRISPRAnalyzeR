# soruced by server.R
# saved as enrichment_server.r


# EnrichR API
source(file.path(config$Fundir, "enrichr_api.r"))


observe({
  if( status$results == TRUE){
    shinyjs::enable("submit_enrichment")
    
    shinyjs::disable("GSE_methods_selected")
    shinyjs::disable("GSE_methods_genes")
    shinyjs::disable("GSE_select_top")
    
  } else {
    shinyjs::disable("submit_enrichment")
    
    shinyjs::disable("GSE_methods_selected")
    shinyjs::disable("GSE_methods_genes")
    shinyjs::disable("GSE_select_top")
  }
})

# gene set enrichment list
GSE_list_selected <- shiny::reactiveVal(0)
  
observe({
  
  # now that pre-selected gene lists are activated, we calculate the selected genes
  if(input$GSE_methods_genes_list != "" && !is.null(input$GSE_methods_genes_list) && !is.null(input$GSE_methods_selected) && input$GSE_methods_selected != "" && input$GSE_select_top != "" && !is.null(input$GSE_select_top) && input$GSE_selectList == TRUE)
  {
    # which method was selected?
    # and do we want enriched or depleted?
    # get number of genes
    top <- (as.numeric(input$GSE_select_top))/100 # to get a decimal value of the percentage
    top <- floor(top*nrow(results()$wilcox$data))
    
    if(input$GSE_methods_selected %in% c("wilcox", "deseq", "mageck", "edger", "rsea","zratio") )
    {
      ret <- NULL
      # enriched or depleted?
      if(input$GSE_methods_genes_list == "enriched")
      {
        # Wilcox
        if(input$GSE_methods_selected == "wilcox")
        {
          data <- results()$wilcox$data
          data$genes <- rownames(results()$wilcox$data)
          # only get enriched
          data <- dplyr::filter(data, foldchange >= 1)
          ret <- dplyr::top_n(data, -top, p.value) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # Deseq2
        if(input$GSE_methods_selected == "deseq")
        {
          data <- as.data.frame(results()$deseq$data$genes, stringsAsFactors = FALSE)
          # only get enriched
          data <- dplyr::filter(data, log2FoldChange >= 0)
          ret <- dplyr::top_n(data, -top, padj) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # Mageck
        if(input$GSE_methods_selected == "mageck")
        {
          data <- results()$mageck$data$genes
          ret <- dplyr::top_n(data, -top, rank.pos) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # edgeR
        if(input$GSE_methods_selected == "edger")
        {
          data <- results()$edger$data$genes
          data$genes <- rownames(results()$edger$data$genes)
          # only get enriched
          data <- dplyr::filter(data, Direction == "Up")
          ret <- dplyr::top_n(data, -top, FDR) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # sgrsea
        if(input$GSE_methods_selected == "rsea")
        {
          data <- as.data.frame(results()$rsea$data$gene.pos, stringsAsfactors=FALSE)
          data$genes <- rownames(results()$edger$data$genes)
          ret <- dplyr::top_n(data, -top, rank.pos) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # zratio
        if(input$GSE_methods_selected == "zratio")
        {
          data <- results()$zratio
          ret <- dplyr::top_n(data, top, zratio) %>% dplyr::select(gene) %>% .[["gene"]]
        }
        
      }
      if(input$GSE_methods_genes_list == "depleted")
      {
        # Wilcox
        if(input$GSE_methods_selected == "wilcox")
        {
          data <- results()$wilcox$data
          data$genes <- rownames(results()$wilcox$data)
          # only get depleted
          data <- dplyr::filter(data,foldchange <= 1)
          ret <- dplyr::top_n(data, -top, p.value) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # Deseq2
        if(input$GSE_methods_selected == "deseq")
        {
          data <- as.data.frame(results()$deseq$data$genes, stringsAsFactors = FALSE)
          # only get depleted
          data <- dplyr::filter(data, log2FoldChange <= 0)
          ret <- dplyr::top_n(data, -top, padj) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # Mageck
        if(input$GSE_methods_selected == "mageck")
        {
          data <- results()$mageck$data$genes
          ret <- dplyr::top_n(data, -top, rank.neg) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # edgeR
        if(input$GSE_methods_selected == "edger")
        {
          data <- results()$edger$data$genes
          data$genes <- rownames(results()$edger$data$genes)
          # only get depleted
          data <- dplyr::filter(data, Direction == "Down")
          ret <- dplyr::top_n(data, -top, FDR) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # sgrsea
        if(input$GSE_methods_selected == "rsea")
        {
          data <- as.data.frame(results()$rsea$data$gene.neg, stringsAsfactors=FALSE)
          data$genes <- rownames(results()$edger$data$genes)
          ret <- dplyr::top_n(data, -top, rank.neg) %>% dplyr::select(genes) %>% .[["genes"]]
        }
        
        # zratio
        if(input$GSE_methods_selected == "zratio")
        {
          data <- results()$zratio
          ret <- dplyr::top_n(data, -top, zratio) %>% dplyr::select(gene) %>% .[["gene"]]
        }
      }
      
    }
    if(input$GSE_methods_selected %in% c("bagel")  )
    {
      if(input$GSE_methods_genes_list == "essential")
      {
        data <- as.data.frame(results()$bagel$data)
        ret <- dplyr::top_n(data, top, BF) %>% dplyr::select(GENE) %>% .[["GENE"]]
      }
      
      if(input$GSE_methods_genes_list == "non-essential")
      {
        data <- as.data.frame(results()$bagel$data)
        ret <- dplyr::top_n(data, -top, BF) %>% dplyr::select(GENE) %>% .[["GENE"]]
      }
    } 
    if( input$GSE_methods_selected %in% c("screenbeam") )
    {
      data <- results()$screenbeam$data
      ret <- dplyr::top_n(data, -top, FDR) %>% dplyr::select(gene) %>% .[["gene"]]
    }
    # return genelist
    GSE_list_selected(ret)
    
  } else {
    GSE_list_selected(NULL)
  }
  
})

observe(
  {
    shinyjs::disable("GSE_methods_selected")
    shinyjs::disable("GSE_methods_genes")
    shinyjs::disable("GSE_select_top")
    
    # depending on the use rselection we create gene list sets or the user can select individual genes
    if(input$GSE_selectList == TRUE)
      {
      shinyjs::enable("GSE_methods_selected")
      shinyjs::enable("GSE_methods_genes")
      shinyjs::enable("GSE_select_top")
      
    } else {
      shinyjs::disable("GSE_methods_selected")
      shinyjs::disable("GSE_methods_genes")
      shinyjs::disable("GSE_select_top")
    }
    
  }
)




output$GSE_top <- renderUI({
  return(sliderInput(inputId = "GSE_select_top",label = "Select the percentage of genes",min = 0.1,max = 10,value = 2,round = FALSE))
})

output$GSE_methods <- renderUI({
  shiny::validate(
    shiny::need(results()$GSE_methodlist, FALSE)
  )
  if( status$results == TRUE  ){
    return(selectInput(inputId = "GSE_methods_selected",label = "Select the analysis method", choices = results()$GSE_methodlist, multiple = FALSE))
  } else {
    return(HTML(config$messages$statusanalysis$String))
  }
})

output$GSE_methods_genes <- renderUI({
  shiny::validate(
    shiny::need(input$GSE_methods_selected, FALSE)
  )
  if( status$results == TRUE  ){
    # radio buttons with ENRICHED / DEPLETED for all methods BUT BAGEL and ScreenBEAM
    if(input$GSE_methods_selected %in% c("wilcox", "deseq", "mageck", "edger", "rsea","zratio"))
    {
      # choices
      choice <- list("TOP X enriched" = "enriched", "TOP X depleted" = "depleted")
      return(radioButtons(inputId = "GSE_methods_genes_list",label = "Select a gene list",choices = choice))
    }
    
    # radio buttons for BAGEL stating TOP Essential or TOP non-essential
    if(input$GSE_methods_selected == "bagel")
    {
      # choices
      choice <- list("TOP X essentials" = "essential", "TOP X non-essential" = "non-essential")
      return(radioButtons(inputId = "GSE_methods_genes_list",label = "Select a gene list",choices = choice))
    }
    # radio buttons for ScreenBEAM TOP scored
    if(input$GSE_methods_selected == "screenbeam")
    {
      # choices
      choice <- list("TOP X FDR" = "fdr")
      return(radioButtons(inputId = "GSE_methods_genes_list",label = "Select a gene list", choices = choice))
    }
    
    
  } else {
    return(HTML(config$messages$statusanalysis$String))
  }
  
})



output$enrichmentGene <- renderUI({
  if( status$results == TRUE  ){
    
    # user can select GENES or top X / lowest X of any analysis
    return(selectizeInput("enrichmentselectgene", label = "Please select (multiple) genes", 
                   choices = results()$aggregatedReadcount$design,
                   multiple = TRUE, options = list(maxItems = 400), selected = GSE_list_selected()))
  } else {
    return(HTML(config$messages$statusanalysis$String))
  }
})

# Observe Button

enrichment <- reactiveValues(data=NULL, status=FALSE, enrichr=NULL, stringdbplot = NULL)

observeEvent(input$submit_enrichment, {
  
  withProgress(message = 'Gene Set Analysis', value = 0, {
    
    #### check if everything is ready
    # get gene identifiers
    shiny::validate(
      shiny::need(input$enrichmentselectgene, "Please select a gene")
    )
    genes <- input$enrichmentselectgene
    
    write(paste(userID, ": Gene Set Analysis started", paste(genes, collapse = ", ")), logFile, append = TRUE)
    
    # Allow stringDB to be passed
    enrichment$status <- TRUE
    
    
    
    # get enrichR data via API
    if(is.null(input$enrichmentFDR) || input$enrichmentFDR == "" || input$enrichmentFDR == " " )
    {
      fdr.cutoff = 0.05
    } else
    {fdr.cutoff <- as.numeric(input$enrichmentFDR)}
    
    gene.list <- genes
    db.list <- c("WikiPathways_2016",
                 "KEGG_2016",
                 "Biocarta_2016",
                 "Reactome_2016",
                 "NCI-Nature_2016",
                 "Panther_2016",
                 "ChEA_2015",
                 "TRANSFAC_and_JASPAR_PWMs",
                 "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X",
                 "TargetScan_microRNA",
                 "Transcription_Factor_PPIs",
                 "GO_Biological_Process_2015",
                 "GO_Cellular_Component_2015",
                 "GO_Molecular_Function_2015",
                 "OMIM_Disease",
                 "Cancer_Cell_Line_Encyclopedia",
                 "NCI-60_Cancer_Cell_Lines"
                 )
    
    enrichment$enrichr <-  getenrichr(gene.list, dataset=annos()$dataset, database = config$car.bm.database, host="www.ensembl.org", identifier = annos()$IDnew,  db.list, fdr.cutoff = 0.05, proxurl = config$car.proxy.url, proxport = config$car.proxy.port)
      
      #enrichGeneList(gene.list, db.list, fdr.cutoff, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port)
    
    
  })

  
  
})


# # StringDB network output
# 
#   
#   shiny::withProgress(message = 'Generating Protein Interactions', value = 0,{
#   
#       # Make stringDB stuff
#   shiny::incProgress(amount = 0.1, detail = "Get StringDB information") 
#       
#   stringdbstringdb <- STRINGdb::STRINGdb$new( version="10", species=9606 , score_threshold=400, input_directory=config$database_path )
#   print("Initialized Stingdb")
#   shiny::incProgress(amount = 0.1, detail = "Get graph information") 
#   if(config$car.proxy != ""){
#     stringdbgraphplot <-  httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), stringdbstringdb$get_graph())
#   } else {
#     stringdbgraphplot <- stringdbstringdb$get_graph()
#   }
#   
#   shiny::incProgress(amount = 0.2, detail = "Get gene information") 
#   print("Initialized graphplot")
#   stringdbgraphplot <- igraph::simplify(stringdbgraphplot, remove.multiple=TRUE, remove.loops=TRUE)
#   
#   print("simplified graphplot")
#   shiny::incProgress(amount = 0.2, detail = "Map genes") 
#   # map genes provided
#   genes <- stringdbstringdb$mp(unique(genes))
#   print("got genes")
#   shiny::incProgress(amount = 0.1, detail = "Get neighbours") 
#   # get neighbors and the interactions
#   neighbors <- stringdbstringdb$get_neighbors( c(genes) )
#   print("get neighbours")
#   shiny::incProgress(amount = 0.1, detail = "Get Protein Interactions")
#   stringdbinteractions <- stringdbstringdb$get_interactions( c(neighbors, genes) )
#   print("now jump to next")
#   
#   # set reactive to go
#   stringDB$ready <- TRUE
#     })
#     


output$stringDBnetwork <- renderPlot(res = 300,execOnResize = TRUE, pointsize = 4,{
  shiny::validate(
    shiny::need(input$stringDBthreshold, message = FALSE),
    shiny::need(input$enrichmentselectgene, message = "Please select genes."),
    shiny::need(identical(enrichment$status, TRUE), message = "Please perform a Gene Set Analysis first.")
  )

  shiny::withProgress(message = "Retrieving StringDB Information",{
    
    if(config$car.proxy != ""){
      return(try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), protein_interactions(genes = unlist(input$enrichmentselectgene),   new.identifier = annos()$IDnew, deseq2 = as.data.frame(results()$deseq$data$genes), dataset = annos()$dataset, title="Protein Interactions", subtitle = paste(input$enrichmentselectgene, collapse = ","), database_path = config$database_path, interactive=FALSE, cutscore = input$stringDBthreshold, filename = paste("Protein_Interactions_",paste(input$enrichmentselectgene, collapse = "-"), sep="")  ))))
      
    } else
    {
      return(try(protein_interactions(genes = unlist(input$enrichmentselectgene), new.identifier = annos()$IDnew, deseq2 = as.data.frame(results()$deseq$data$genes), dataset = annos()$dataset, title="Protein Interactions", subtitle = paste(input$enrichmentselectgene, collapse = ","), database_path = config$database_path, interactive=FALSE,  cutscore = input$stringDBthreshold, filename = paste("Protein_Interactions_",paste(input$enrichmentselectgene, collapse = "-"), sep="") )))
    }
    
  })
  
 
  
  #return(enrichment$stringdbplot)
  
})

# observeEvent(input$startstringdb, {
#   
#   shiny::validate(
#     #shiny::need(input$stringDBthreshold, message = FALSE),
#     shiny::need(input$enrichmentselectgene, message = "Please select genes."),
#     shiny::need(identical(enrichment$status, TRUE), message = "Please perform a Gene Set Analysis first.")
#     
#   )
#   shiny::withProgress(message = 'Generating Protein Interactions', value = 0,{
#     
#     
#     if(config$car.proxy != ""){
#       
#       plot <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), protein_interactions(genes = unlist(input$enrichmentselectgene),   new.identifier = annos()$IDnew, deseq2 = as.data.frame(results()$deseq$data$genes), dataset = annos()$dataset, title="Protein Interactions", subtitle = paste(input$enrichmentselectgene, collapse = ","), database_path = config$database_path, interactive=TRUE, filename = paste("Protein_Interactions_",paste(input$enrichmentselectgene, collapse = "-"), sep="")  )))
# 
#       if(class(plot) == "try-error" || plot == "threshold")
#       {
#         plot <- Plot_blank( msg = "No protein interactions found ")
#       }
#     } else
#     {
#       plot <- try(protein_interactions(genes = unlist(input$enrichmentselectgene), new.identifier = annos()$IDnew, deseq2 = as.data.frame(results()$deseq$data$genes), dataset = annos()$dataset, title="Protein Interactions", subtitle = paste(input$enrichmentselectgene, collapse = ","), database_path = config$database_path, interactive=TRUE, filename = paste("Protein_Interactions_",paste(input$enrichmentselectgene, collapse = "-"), sep="") ))
#       if(class(plot) == "try-error" || plot == "threshold")
#       {
#         plot <- Plot_blank( msg = "No protein interactions found")
#       }
#       
#     }
#     
#     # give back plot
#     enrichment$stringdbplot <- plot
#     
#   })
# })
#   
  


# Gene Set Enrichment information using EnrichR

## Transcription Enrichment
# ChEA_2015
# TRANSFAC_and_JASPAR_PWMs
# ENCODE and ChEA Consensus TFs from ChIP-X
# TargetScan microRNA
# Transcription Factor PPIs

output$ChEA_2015 <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  

  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "ChEA_2015")

  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
     Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "ChEA 2015",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GeneSetEnrichment_ChEA_2015" )
  }
  
  
})

output$ChEA_2015_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "ChEA_2015")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
          dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
          formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
          pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_ChEA_2015" )
  
})

# TRANSFAC

output$TRANSFAC <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "TRANSFAC_and_JASPAR_PWMs")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "TRANSFAC and JASPAR",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GeneSetEnrichment_TRANSFAC_and_JASPAR_PWMs" )
  }
  
  
})

output$TRANSFAC_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "TRANSFAC_and_JASPAR_PWMs")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_TRANSFAC_and_JASPAR_PWMs" )
  
})


# ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X


output$ENCODE <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "ENCODE and ChEA Transcription Factor ChIP-X",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GeneSetEnrichment_ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X" )
  }
  
  
})

output$ENCODE_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X" )
  
})

## TargetScan_microRNA
output$targetscan <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "TargetScan_microRNA")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "TargetScan microRNA",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "TargetScan_microRNA" )
  }
  
  
})

output$targetscan_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "TargetScan_microRNA")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_TargetScan_microRNA" )
  
})

## Transcription_Factor_PPIs
output$ppi <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  print(str(enrichment$enrichr))
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Transcription_Factor_PPIs")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Transcription Factor Interactions",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "Transcription_Factor_PPIs" )
  }
  
  
})

output$ppi_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Transcription_Factor_PPIs")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_Transcription_Factor_PPIs" )
  
})
#############################
####### Gene Ontology
#"GO_Biological_Process_2015"
#"GO_Cellular_Component_2015"
#"GO_Molecular_Function_2015"

## go biological process
output$go_biologicalprocess <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Biological_Process_2015")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "GO Biological Process 2015",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GO_Biological_Process_2015" )
  }
  
  
})

output$go_biologicalprocess_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Biological_Process_2015")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_GO_Biological_Process_2015" )
  
})

## GO_Molecular_Function_2015
output$go_molfunction <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Molecular_Function_2015")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "GO Molecular Function 2015",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GO_Molecular_Function_2015" )
  }
  
  
})

output$go_molfunction_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Molecular_Function_2015")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_GO_Molecular_Function_2015" )
  
})

## GO_Molecular_Function_2015
output$go_cellular <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Cellular_Component_2015")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "GO Cellular Component 2015",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "GO_Cellular_Component_2015" )
  }
  
  
})

output$go_cellular_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "GO_Cellular_Component_2015")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_GO_Cellular_Component_2015" )
  
})

###############
####### OMIM_Disease
output$omimdisease <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "OMIM_Disease")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "OMIM Disease",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "OMIM_Disease" )
  }
  
  
})

output$omimdisease_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "OMIM_Disease")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_OMIM_Disease" )
  
})


################
######### Cell Types
#"Cancer_Cell_Line_Encyclopedia",
#"NCI-60_Cancer_Cell_Lines"

output$ccle <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Cancer_Cell_Line_Encyclopedia")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Cancer Cell Line Encyclopedia",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "Cancer_Cell_Line_Encyclopedia" )
  }
  
  
})

output$ccle_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Cancer_Cell_Line_Encyclopedia")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_Cancer_Cell_Line_Encyclopedia" )
  
})

### NCI-60

output$nci60 <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "NCI-60_Cancer_Cell_Lines")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "NCI-60 Cancer Cell Lines",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "NCI-60_Cancer_Cell_Lines" )
  }
  
  
})

output$nci60_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "NCI-60_Cancer_Cell_Lines")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_NCI-60_Cancer_Cell_Lines" )
  
})

###############
### PATHWAYS
#WikiPathways_2016",
#"KEGG_2016",
#"Biocarta_2016",
#"Reactome_2016",
#"NCI-Nature_2016",
#"Panther_2016",



## WikiPathways 2016
output$wikipathways <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "WikiPathways_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Wiki Pathways",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "WikiPathways_2016" )
  }
  
  
})

output$wikipathways_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "WikiPathways_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_WikiPathways_2016" )
})


### KEGG_2016
output$kegg <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "KEGG_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "KEGG",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "KEGG_2016" )
  }
  
  
})

output$kegg_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "KEGG_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_KEGG_2016" )
})




### Biocarta_2016


output$biocarta <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Biocarta_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Biocarta",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "Biocarta_2016" )
  }
  
  
})

output$biocarta_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Biocarta_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_Biocarta_2016" )
})


### Reactome_2016

output$reactome <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Reactome_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Reactome",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "Reactome_2016" )
  }
  
  
})

output$reactome_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Reactome_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_Reactome_2016" )
})


#####NCI-Nature_2016

output$ncinature <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "NCI-Nature_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "NCI Nature",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "NCI-Nature_2016" )
  }
  
  
})

output$ncinature_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "NCI-Nature_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_NCI-Nature_2016" )
})





###### Panther_2016

output$panther <- renderHighchart({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Panther_2016")
  
  tt <- tagList(
    shiny::tags$strong("{point.name}"), shiny::tags$br(),
    shiny::tags$div("Score : {point.y:.5f}", shiny::tags$br(),
                    shiny::tags$div("Adjusted P-Value : {point.qval:.5f}"), shiny::tags$br(),
                    shiny::tags$div("Genes associated: {point.genes}")
    )
  ) %>% as.character()
  
  if(nrow(filtered) == 0)
  {
    Plot_blank(device="hc", msg = "No data available")
  } else {
    # Create Highcharter plot
    Plot_bar( seriesNames = c("score"),
              catName = "category",
              data = filtered, 
              tooltip = tt,
              title = "Panther",
              subtitle = "Gene Set Enrichment",
              xLab = "Score",
              yLab = "Annotation",
              zoom = "x",
              crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = "Panther_2016" )
  }
  
  
})

output$panther_DT <- DT::renderDataTable({
  
  shiny::validate(
    shiny::need(input$enrichmentselectgene, "Please select a gene"),
    shiny::need(enrichment$enrichr, "No Enrichment Analysis has been performed yet.")
  )
  
  # Take data from enrichr
  filtered <-  dplyr::filter(enrichment$enrichr, database == "Panther_2016")
  
  Table_DT(filtered, colNames = c("Database", "Term", "P-Value", "Adjusted P-Value", "Z-Score", "EnrichR Score", "Involved Genes"), bRownames = FALSE, style = "default", class = "display", 
           dom = "flrtip", alignment = list(centre = NULL, justify = NULL, left = NULL), 
           formatCurr = NULL, formatPerc = NULL, formatRoun = list("cols" = c(3,4,5,6), "digits" = 4), buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
           pageLen = 15, bScroll = TRUE, filename = "GeneSetEnrichment_NCI-Panther_2016" )
})













































