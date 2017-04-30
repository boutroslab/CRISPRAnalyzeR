# sourced by 'server.r'
# save as 'inDepth_server.r'
# rendering plots for In-Depth Analysis Tab

#set working dir as backup
setwd(config$wd)

# Source violine plot
source(file.path(config[["Fundir"]], "violin.R"))
source(file.path(config[["Fundir"]], "gene.overview.R"))


######################
#### Gene Overview ###
######################

# REQUIRED INPUT:
# Gene to show: input$indepthOverviewGene
# Dataset/Organism: annos()$dataset
# filter (new gene identifier): annos()$IDnew
# Proxy: config[["car.proxy"]]



output$indepthOverviewGene <- renderUI({
  if( status$results == TRUE && status$info == TRUE ){
    selectizeInput("indepthOverviewGene", label = "Select a gene", 
                   choices = results()$aggregatedReadcount$design, width = "300px",
                   multiple = TRUE, options = list(maxItems = 1))
  } else {
    HTML(config$messages$getinfo2$String)
  }
})

#### create selectize for sgRNA
# select one gene from gene list
# one ui for genomic view, one for sgRNAs
output$hit_select <- renderUI({
  if( status$results == TRUE && status$info == TRUE ){
    selectizeInput("hit_select", label = "Select a gene",
                   choices = results()$aggregatedReadcount$design, width = "300px",
                   multiple = TRUE, options = list(maxItems = 1))
  } else {
    HTML(config$messages$getinfo2$String)
  }
})

############# Section1: General Gene information
indepth_geneOverview <- eventReactive(input$indepthOverviewGene, ignoreNULL = FALSE, {
  
  
  #system2("echo", args = c("gene overview start"), stdout = "/tmp/id_start")
  if(!is.null(input$indepthOverviewGene) && input$indepthOverviewGene != "")
  {
    
    #print("Gene Overview - Start")
    # set attributes for general data retrieval
    shiny::withProgress(message = 'Query Ensembl biomaRt', value = 0,{
      
      attributes <- c("ensembl_gene_id","hgnc_symbol","description","uniprotswissprot", "entrezgene")
      db.list <- c("Transcription_Factor_PPIs",
                   "TRANSFAC_and_JASPAR_PWMs",
                   "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X",
                   "TargetScan_microRNA",
                   "ChEA_2015"
      )
      
     # print("Gene Overview - Get Data from Ensembl")
      out <- NULL
      data <- NA
      datacosmic <- NA
      enrichr <- NA
      keggdata <- NA
      indepth_GO <- NA
      gecrispr <- NA
    
      # GET ENSEMBL INFO
      data <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = config$car.proxy.port), gene.annotation(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, attributes = attributes, host="www.ensembl.org", genomecrispruri = NULL, GRCh = NULL, progresslog = NULL, workdir = config$wd, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port)))  
      #data <- try(gene.annotation(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, attributes = attributes, host="www.ensembl.org", genomecrispruri = NULL, GRCh = NULL, progresslog = NULL, workdir = config$wd, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port))
        if(class(data) == "try-error")
        {
          data <- NA
        }
      
        if(config[["EnrichR"]] && !is.na(data))
        {
          shiny::incProgress(amount = 0.1, message = "Retrieve EnrichR information")
          enrichr <-  try(enrichGeneList(data$hgnc_symbol, db.list, 0.2, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port))
        } else {enrichr <- NA}
        
        if(class(enrichr) == "try-error")
        {
          enrichr <- NA
        }
        
        # LOAD COSMIC INFO if available
        
        if(!is.null(results()$COSMICDB) && !is.na(data))
        {
          datacosmic <- try(cosmicdb(genes = data$hgnc_symbol, database=config$car.bm.database, dataset=annos()$dataset, host="www.ensembl.org", new.identifier = annos()$IDnew, COSMICdb = results()$COSMICDB, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port))
          
          if(class(datacosmic) == "try-error")
          {
            datacosmic <- NA
          }
        } else
        {
          datacosmic <- NA
        }
      #}
      
      # get KEGG information directly
      if(!is.na(data))
      {
        if(is.null(data$entrezgene))
        {
          keggdata <- NA
        } else 
          {
            #print("Gene Overview - Get Data from KEGG using Proxy")
            keggdata <- try(getkegg(unique(data$entrezgene), proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port) )
            if(class(keggdata) == "try-error")
            {
              keggdata <- NA
            }
          }
        
      } else {keggdata <- NA}
      
    })
      # GO
      shiny::withProgress(message = 'Generating Gene Ontology Information', value = 0,{
        res <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = config$car.proxy.port), goterms(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", userdir = userDir)))
        
        if(class(res) == "try-error")
        {
          indepth_GO <- NA
        } else {
          
          indepth_GO <- res
        }
      })

      
      # Genomecrispr
      shiny::withProgress(message = 'Retrieve Screening Information', value = 0,{
        # if(config$car.proxy != ""){
          #print("Gene Overview - Get Data from GenomeCRISPR using Proxy")
          gecrispr <- try(genomecrispr(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, host="www.ensembl.org", new.identifier =  annos()$IDnew, readcount=results()$readcount, ecrisp = info()$ecrisp , proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port))
          #gecrispr <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), genomecrispr(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, host="www.ensembl.org", new.identifier =  annos()$IDnew, readcount=results()$readcount, ecrisp = info()$ecrisp  )))
        # } else
        # {
        #   gecrispr <- try(genomecrispr(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, host="www.ensembl.org", new.identifier =  annos()$IDnew, readcount=results()$readcount, ecrisp = info()$ecrisp, proxyurl = config$car.proxy.url, proxyport = config$car.proxy.port ))
        # }
        
        if(class(gecrispr) == "try-error")
        {
          gecrispr <- NA
        }
        
      })
      
      ## Additional log2FC data
      sgrna.d <- as.data.frame(results()$deseq$data$sgRNA[results()$deseq$data$sgRNA$genes == input$indepthOverviewGene,c("padj","sgRNA")]) #log2FoldChange if DESeq2 log2FC should be shown 
      sgrna.d <- merge.data.frame(x=sgrna.d, y=info()$rawGenes[,c("designs","log2foldchange")], by.x = "sgRNA", by.y = "designs", all.x=TRUE, all.y = FALSE)
      sgrna.d$log2foldchange <- round(as.numeric(sgrna.d$log2foldchange), digits = 4)
      
      sgrna.d <- merge.data.frame(x=sgrna.d, y=results()$libFILE, by.x = "sgRNA", by.y = "design", all.x=TRUE)
      sgrna.d$sequence <- toupper(sgrna.d$sequence)
      
      
      # return list  
      out <- list("data" = data,
                  "cosmic" = datacosmic,
                  "enrichr" = enrichr,
                  "kegg" = keggdata,
                  "GO" = indepth_GO,
                  "genomecrispr" = gecrispr,
                  "sgrna.dt" =  as.data.frame(sgrna.d)
      )
      names(out) <- c("data","cosmic","enrichr","kegg","GO","genomecrispr","sgrna.dt")
      
      #system2("echo", args = c(str(out)), stdout = "/tmp/id_all_str")
      #print(out)
      
      # give back list of data
      return(out)

  } else
  {
    return(NA)
  }
  
})
  


  # HGNC
  output$HGNC_SYMBOL <- renderUI({
    shiny::validate(
      shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
      shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    )
    if(!is.na(indepth_geneOverview()["data"])) {
      HTML(paste("<i class='fa fa-external-link fa-fw'></i>&nbsp;<strong><a href='http://www.genecards.org/cgi-bin/carddisp.pl?gene=",unique(indepth_geneOverview()$data$hgnc_symbol),"' target='_blank'>",unique(indepth_geneOverview()$data$hgnc_symbol),"</a></strong>", sep=""))
    } else {
      HTML(config$messages$biomart1$String)
    }
    
  })
  
  # ENSEMBL Gene Id
  output$ENSEMBL_GENE_ID <- renderUI({
    shiny::validate(
      shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
      shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    )
    if(!is.na(indepth_geneOverview()["data"])) {
      
      if(annos()$dataset == "homo_sapiens") {organism <- "Homo_sapiens"}
      if(annos()$dataset == "mus_musculus") {organism <- "Mus_musculus"}
      if(annos()$dataset == "dario_rerio") {organism <- "Dario_rerio"}
      
      HTML(paste("<i class='fa fa-external-link fa-fw'></i>&nbsp;<strong><a href='http://www.ensembl.org/", organism,"/Gene/Summary?db=core;g=", unique(indepth_geneOverview()$data$ensembl_gene_id),"' target='_blank'>",unique(indepth_geneOverview()$data$ensembl_gene_id),"</a></strong>", sep="", collapse=""))
      
    } else {
      HTML(config$messages$biomart1$String)
    }
    
  })
  # Gene Description
  output$GENE_DESCRIPTION <- renderUI({
    shiny::validate(
      shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
      shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    )
    if(!is.na(indepth_geneOverview()["data"])) {
      HTML(as.character(unique(indepth_geneOverview()$data$description)))
    } else {
      HTML(config$messages$biomart1$String)
    }
    #print(indepth_geneOverview()["data"])
    
  })
  
  # Uniprot ID
  output$UNIPROT_ID <- renderUI({
    shiny::validate(
      shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
      shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    )
    
    if(!is.na(indepth_geneOverview()["data"])) {
      uniqueuniprot <- unique(indepth_geneOverview()$data$uniprotswissprot)
      uniqueuniprot <- uniqueuniprot[uniqueuniprot != ""]
    
      HTML(as.character(paste("<i class='fa fa-external-link fa-fw'></i>&nbsp;<strong><a href='http://www.uniprot.org/uniprot/",uniqueuniprot,"' target='_blank'>", uniqueuniprot,"</a></strong>", sep="", collapse = "<br/>")))
    } else {
      HTML(config$messages$biomart1$String)
    }
    
     })
  # Entrez Gene / NCBI ID
  
  output$ENTREZGENE <- renderUI({
    shiny::validate(
      shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
      shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    )
    
    if(!is.na(indepth_geneOverview()["data"])) {
      HTML(as.character(paste(paste("<i class='fa fa-external-link fa-fw'></i>&nbsp;<strong><a href='http://www.ncbi.nlm.nih.gov/gene/?term=",unique(indepth_geneOverview()$data$entrezgene),"' target='_blank'>", unique(indepth_geneOverview()$data$entrezgene),"</a></strong>", sep="", collapse = "<br/>"), collapse = "; ")))
    } else {
      HTML(config$messages$biomart1$String)
    }
    
  })
  
###### KEGG Information

output$kegg_name <- renderUI({
  
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
    return(HTML(indepth_geneOverview()$kegg$name))
  } else {
    HTML(config$messages$biomart1$String)
  }
  

})  

output$kegg_pathway <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  ## make link to KEGG pathway
  ## http://www.genome.jp/dbget-bin/www_bget?TERM
  ## TERM is names(KEGG.pathway) and name is KEGG.pathway
  ## needs iteration
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
      output <- ""
    for(i in 1:length(indepth_geneOverview()$kegg$pathway))
    {
      output <- paste(output, '<i class="fa fa-external-link fa-fw"></i>&nbsp;<a href="http://www.genome.jp/dbget-bin/www_bget?', names(indepth_geneOverview()$kegg$pathway)[[i]] ,'" target="_blank">',indepth_geneOverview()$kegg$pathway[[i]],'</a> </br> ', sep="")
    }
    
    return(HTML(output))
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  

})  


output$kegg_disease <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  ## make link to http://www.genome.jp/dbget-bin/www_bget?ds:TERM
  ## TERM is names(KEGG.disease) and name of diease is just KEGG.disease
  ## needs iteration
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
    output <- ""
    for(i in 1:length(indepth_geneOverview()$kegg$disease))
    {
      output <- paste(output, '<i class="fa fa-external-link fa-fw"></i>&nbsp;<a href="http://www.genome.jp/dbget-bin/www_bget?ds:', names(indepth_geneOverview()$kegg$disease)[[i]] ,'" target="_blank">',indepth_geneOverview()$kegg$disease[[i]],'</a> <br/> ', sep="")
    }
    return(HTML(output))
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
  
})
  
output$kegg_motif <- renderText({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
    
  )
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
    return(indepth_geneOverview()$kegg$motif)
  } else {
    HTML(config$messages$biomart1$String)
  }
  
})

output$kegg_linkdb <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  if(!is.na(indepth_geneOverview()["kegg"])) {
    output <- paste(indepth_geneOverview()$kegg$dblinks, collapse="</br>")
  
  # make HTML
  return(HTML(output))
  } else {
    HTML(config$messages$biomart1$String)
  }
  

})

output$kegg_structure <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  # Protein structures
  ## remove PDB and empty strings
  ## make URL to 
  # http://www.rcsb.org/pdb/explore/explore.do?structureId=TERM
  ## TERM is KEGG.structure
  # 
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
    output <- ""
  
    for(i in 1:length(indepth_geneOverview()$kegg$structure))
    {
      output <- paste('<i class="fa fa-external-link fa-fw"></i>&nbsp;<a href="http://www.rcsb.org/pdb/explore/explore.do?structureId=' ,indepth_geneOverview()$kegg$structure[[i]], ' target="_blank">', indepth_geneOverview()$kegg$structure[[i]], '</a>' , sep="")
    }
    
    return(HTML(output))
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})


output$kegg_aa <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["kegg"])) {
    returndata <- gsub("(.{50})", "\\1</br>", as.character(indepth_geneOverview()$kegg$aa))
    return(HTML(paste("<pre>",returndata, "</pre>", sep="") )) 
  } else {
    HTML(config$messages$biomart1$String)
  }
  
})


output$keggpathwaylist <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  if(!is.na(indepth_geneOverview()["kegg"])) {
    selectizeInput("keggpathwayinput", label = "Please select one of the pathway",
                   choices = indepth_geneOverview()$kegg$pathway, width = "400px",
                   multiple = FALSE)
  } else {
    HTML(config$messages$biomart1$String)
  }
    
  
})



# Chip Seq data from ENCODE and ChEA Consensus TFs from ChIP-X 
output$chipseqbinding <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["enrichr"])) {
  
      data <- dplyr::filter(indepth_geneOverview()$enrichr, database == c("ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "ChEA_2015")) %>% dplyr::select(category)
    if(nrow(data)  >= 1 )
    {
      # get it different by separating each into a new line and put gene name from where it is derived
      ret <- sapply(data$category, function(x) {
        return <- paste("<strong>" , sub("^(.+?)_(.+)$","\\1", x), "</strong>" , " found in ", sub("^(.+?)_(.+)$","\\2", x), sep="")
        return(return)
      })
      ret <- paste(ret, collapse = "</br>")
    } else {
      ret <- "Not available"
    }
    return(HTML(ret))

  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})

# TargetScan microRNAs
output$targetscanmirna <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["enrichr"])) {
    
    data <- dplyr::filter(indepth_geneOverview()$enrichr, database == "TargetScan_microRNA") %>% dplyr::select(category)
  
  if(nrow(data)  >= 1 )
  {
    ret <- paste(data$category, collapse = "</br>")
  } else {
    ret <- "Not available"
  }
  return(HTML(ret))
  
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})
# Transcription_Factor_PPIs
output$TFppi <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  #View(indepth_geneOverview()$enrichr)
  
  if(!is.na(indepth_geneOverview()["enrichr"])) {
    
      data <- dplyr::filter(indepth_geneOverview()$enrichr, database == c("Transcription_Factor_PPIs","TRANSFAC_and_JASPAR_PWMs")) %>% dplyr::select(category)
    
    if(nrow(data)  >= 1 )
    {
      ret <- paste(data$category, collapse = "</br>")
    } else {
      ret <- "Not available"
    }
    return(HTML(ret))
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})


  
  


######### Section 2: GVIZ Gene model and sgRNA information from cp$ecrisp

indepth_GVIZ_gene_data <- eventReactive(input$indepthOverviewGene, {
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  if( status$results == TRUE && status$info == TRUE && input$indepthOverviewGene != "" ){
    
    shiny::withProgress(message = 'Generating Gene Model', value = 0,{
      ## Plot 1: Gene with surrounding genes and indication whether this is a hit / log2FC / Z-Ratio / pval
      if(!is.null(config$car.proxy.url) && !is.null(config$car.proxy.port)){
        res <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), gene.gviz(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", progresslog = NULL, region = 10000, data.only = TRUE, deseq=results()$deseq)))
      } else
      {
        res <- try(gene.gviz(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", progresslog = NULL, region = 10000, data.only = TRUE, deseq=results()$deseq))
      }
      
      if( class(res) == "try-error" ){
        return(NA)
      } else {
        
        return(res)
      }
    })
  }
})
  
output$indepth_GVIZ_gene <- renderPlot(res = 72, height = 1000, {
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(!is.na(indepth_GVIZ_gene_data()), "No Data Available")
    
  )
  
  if( status$results == FALSE ){
    return(Plot_blank("base", msg = config$messages$noanalysisrunyet$String))
  } else if( status$info == FALSE ){
    return(Plot_blank("base", msg = config$messages$statusinfo$String))
  }
  res <- indepth_GVIZ_gene_data()
  #if(!is.null(res[["itrack"]]))
  try(Gviz::plotTracks(c(res[["itrack"]], res[["refTrack"]], res[["generegionTrack"]], res[["biomTrack"]], res[["dTrack"]]),  
                       groupAnnotation = "id", red="darkred", green="grey"))
})


indepth_GVIZ_sgrna_data <- eventReactive(input$indepthOverviewGene, {
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if( status$results == TRUE && status$info == TRUE && input$indepthOverviewGene != "" ){
    shiny::withProgress(message = 'Generating sgRNA Model', value = 0, {
      ## Plot2: sgRNA plot with information about individual sgRNAs
      
      if(!is.null(config$car.proxy.url) && !is.null(config$car.proxy.port)){
        res <- try(httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)), sgrna.gviz(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", region = 1000, data.only=TRUE, deseq=results()$deseq, readcount = results()$readcount, ecrisp = info()$ecrisp)))
      } else
      {
        res <- try(sgrna.gviz(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", region = 1000, data.only=TRUE, deseq=results()$deseq, readcount = results()$readcount, ecrisp = info()$ecrisp))
      }
      
      #res <- try(sgrna.gviz(genes = input$indepthOverviewGene, database=config$car.bm.database, dataset=annos()$dataset, filter = annos()$IDnew, host="www.ensembl.org", region = 1000, data.only=TRUE, deseq=results()$deseq, readcount = results()$readcount, ecrisp = info()$ecrisp))
      if( class(res) == "try-error" ){
        
        return(NA)
      } else {
        return(res)
      }
    })
  }
})

output$indepth_GVIZ_sgrna <- renderPlot(res = 72, height = 1000, {
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(!is.na(indepth_GVIZ_sgrna_data()), 'No Data Available.')
  )
  if( status$results == FALSE ){
    return(Plot_blank("base", msg = config$messages$noanalysisrunyet$String))
  }
  if( status$info == FALSE ){
    return(Plot_blank("base", msg = config$messages$statusinfo$String))
  }
  res <- indepth_GVIZ_sgrna_data()
  if( is.null(res[["sgrnaTrack"]]) || is.null(res[["dTrack"]]) || is.null(res[["featureTrack"]]) || is.null(res[["motifTrack"]]) ) {
    return(Gviz::plotTracks(c(res[["itrack"]], res[["generegionTrack"]], res[["refTrack"]], res[["biomTrack"]]), from = res[["from"]] , to = res[["to"]] ))
  } else {
    return(Gviz::plotTracks(c(res[["itrack"]], res[["generegionTrack"]], res[["refTrack"]], res[["biomTrack"]], res[["sgrnaTrack"]], res[["dTrack"]], res[["featureTrack"]], res[["motifTrack"]]), from = res[["from"]] , to = res[["to"]], chromosome = res[["chromosome"]] ))
  }
})



######### Section3: GenomCRISPR information
# DT with screens in which genes has been used


output$indepth_DT_overview <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"]) ) {
    if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas) && !is.null(indepth_geneOverview()$genomecrispr$sgrnas))
    {
      return(Table_DT(indepth_geneOverview()$genomecrispr$sgrnas, colNames = c("Pubmed ID", "Cell Line", "Screening Condition", "Hit?", "sgRNA Sequence"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_PublishedCRISPRScreens", sep="")))
    } else {
      return(HTML("No data found in GenomeCRISPR."))
    }
     
  } else {
    return(HTML("Could not connect to GenomeCRISPR."))
  }
  
 
  
})

# output$indepth_HC_overview <- renderDataTable({
#   shiny::validate(
#     shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
#     shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
#   )
#   
#   if(!is.na(indepth_geneOverview()["genomecrispr"]) ) {
#     if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas))
#     { 
#       df <- indepth_geneOverview()$genomecrispr$sgrnas
#     } else {
#       HTML("No data found in GenomeCRISPR.")
#     }
#     
#   } else {
#     HTML("Could not connect to GenomeCRISPR.")
#   }
#   
#   
#   
# })

## 2
# make information for other screens where this gene was used, will be put as HTML
output$indepth_screens <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"]) ) {
    if(!is.na(indepth_geneOverview()$genomecrispr$genes) && !is.null(indepth_geneOverview()$genomecrispr$genes))
    {
      gcrispr_gene_header <- '<table class="table">
  <thead>
      <tr>
      <th style="width:10%;">Pubmed ID</th>
      <th style="width:20%;">Title</th>
      <th style="width:70%;">Abstract</th>
      </tr>
      </thead>
      <tbody>'
      
      gcrispr_gene_footer <- '</tbody>
      </table>'
      
      gcrispr_gene_body <- ""
      # make table
      for(i in 1:length(indepth_geneOverview()$genomecrispr$genes))
      {
        
        gcrispr_gene_body2 <- paste( '<tr>
                                     <td style="width:10%;">
                                     <a href="http://www.ncbi.nlm.nih.gov/pubmed/', indepth_geneOverview()$genomecrispr$genes[[i]]$pubmed, '" target="_blank">', indepth_geneOverview()$genomecrispr$genes[[i]]$pubmed, '</a>
                                     </td>
                                     <td style="width:20%;">', indepth_geneOverview()$genomecrispr$genes[[i]]$title, '</td>
                                     <td style="width:70%;">', indepth_geneOverview()$genomecrispr$genes[[i]]$abstract,'</td>
                                     </tr>', sep="")
        
        gcrispr_gene_body <- paste0(gcrispr_gene_body, gcrispr_gene_body2)
        
      }
      
      # output to be used with HTMLOUTPUT
      HTML(paste0(gcrispr_gene_header, gcrispr_gene_body, gcrispr_gene_footer))
      
    } else {
      return(HTML("No data found in GenomeCRISPR."))
    }
     
    
  } else {
    return(HTML("Could not connect to GenomeCRISPR."))
  }
  
 
})
## 3
# make table with sgRNAs used by the user and the information whether it was used in other screens

output$indepth_DT_sgrna <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"])) {
    
    if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas2)  && !is.null(indepth_geneOverview()$genomecrispr$sgrnas2) && !(class(indepth_geneOverview()$genomecrispr$sgrnas2) %in% c("html")) )
    {
      return(Table_DT(indepth_geneOverview()$genomecrispr$sgrnas2, colNames = c("Pubmed ID", "Cell Line", "Screening Condition", "Hit?", "sgRNA Sequence"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_", input$indepthOverviewGene , "_sgRNAs",sep="")) )
    } else {
      return(HTML("No data found in GenomeCRISPR."))
    }
    
  } else {
    return(HTML("No data found in GenomeCRISPR."))
  }
  
  
})


# GenomeCRISPR Plots

output$indepth_GC_plot_screencond <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"])) {
    
    if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas) && !is.null(indepth_geneOverview()$genomecrispr$sgrnas))
    {
      
      # replace anything with viability and resistance to just viability and resistance
      data <- indepth_geneOverview()$genomecrispr$sgrnas
      data <- mutate(data, condition=replace(condition, grepl(pattern = ".*resistance.*",x = condition), "resistance")) %>% mutate(condition=replace(condition, grepl(x = condition, pattern = ".*viability.*"), "viability"))
      
      screencond <- data %>% select(pubmed, condition,cellline) %>% distinct() %>% group_by(condition) %>% summarise(N = length(condition))
      
      gene <- input$indepthOverviewGene
      title <- "Screening Conditions"
      subtitle <- paste(gene, "was screened in the following conditions according to GenomeCRISPR", sep= " ")
      export = TRUE
      plotx = "condition"
      
      plot <- try(genomecrispr_pie(data = screencond, gene, title, subtitle, export = TRUE, plotx))
      
      if(class(plot) == "try-error")
      {
        return(Plot_blank("hc", msg = "No Data available."))
      } else
      {return(plot)}
      
    } else {
      return(Plot_blank("hc", msg = "No Data available."))
    }
    
  } else {
    return(Plot_blank("hc", msg = "Connection to GenomeCRISPR failed."))
  }
  
})

output$indepth_GC_plot_hit <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"])) {
    
    if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas) && !is.null(indepth_geneOverview()$genomecrispr$sgrnas))
    {
      
      hits <- indepth_geneOverview()$genomecrispr$sgrnas %>% select(pubmed, condition,cellline, hit) %>% filter(hit == "true") %>% distinct() %>% group_by(condition) %>% summarise(N = length(condition))
      # replace values
      #hits <- mutate(hits, hit=replace(hit, is.na(hit), "no information provided")) %>% mutate(hit=replace(hit, hit == "false", "no phenotype observed")) %>% mutate(hit=replace(hit, hit == "true", "phenotype observed"))
      
      if(nrow(hits) == 0)
      {
        # nothing showed up as a hit
        return(Plot_blank("hc", msg = "No phenotypes observed."))
      }
      
      gene <- input$indepthOverviewGene
      title <- "Phenotype Observation"
      subtitle <- paste(gene, "showed phenotypic effects according to GenomeCRISPR", sep= " ")
      export <- TRUE
      plotx <- "condition"
      
      plot <- try(genomecrispr_pie(data = hits, gene, title, subtitle, export = TRUE, plotx))
      
      if(class(plot) == "try-error")
      {
        return(Plot_blank("hc", msg = "No Data available."))
      } else
      {return(plot)}
      
    } else {
      return(Plot_blank("hc", msg = "No Data available."))
    }
    
  } else {
    return(Plot_blank("hc", msg = "Connection to GenomeCRISPR failed."))
  }
  
})

output$indepth_GC_plot_lines <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["genomecrispr"])) {
    
    if(!is.na(indepth_geneOverview()$genomecrispr$sgrnas) && !is.null(indepth_geneOverview()$genomecrispr$sgrnas))
    {
      
      cells <- indepth_geneOverview()$genomecrispr$sgrnas %>% select(pubmed, condition,cellline, hit) %>% distinct()  %>% group_by(cellline) %>% summarise(N = length(cellline))
      
      gene <- input$indepthOverviewGene 
      title <- "Screened Cell Lines"
      subtitle <- paste(gene, "was used in the following cell lines according to GenomeCRISPR", sep= " ")
      export <- TRUE
      plotx <- "cellline"
      
      plot <- try(genomecrispr_pie(data = cells, gene, title, subtitle, export = TRUE, plotx))
      
      if(class(plot) == "try-error")
      {
        return(Plot_blank("hc", msg = "No Data available."))
      } else
      {return(plot)}
      
    } else {
      return(Plot_blank("hc", msg = "No Data available."))
    }
    
  } else {
    return(Plot_blank("hc", msg = "Connection to GenomeCRISPR failed."))
  }
  
})




###### Section 4: ScreenAnalysis specific information

### Part1: Table with Gene Analysis results (pvals and fold changes)


output$indepth_gene.log2fc <- renderText({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  round(as.numeric(results()$deseq$data$genes[results()$deseq$data$genes$genes == input$indepthOverviewGene, "log2FoldChange"]), digits = 5)})

output$indepth_gene.wilcoxpval <- renderText({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  round(as.numeric(results()$wilcox$data[input$indepthOverviewGene , "p.value"]), digits = 5)})

output$indepth_gene.deseq2pval <- renderText({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  round(as.numeric(results()$deseq$data$genes[results()$deseq$data$genes$genes == input$indepthOverviewGene, "padj"]), digits = 5)})

output$indepth_gene.mageckpval <- renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  HTML(paste("<i>Enriched:</i>", round(as.numeric(results()$mageck$data$genes[results()$mageck$data$genes$genes == input$indepthOverviewGene, "pos"]), digits = 5) ,"<i>Depleted:</i>", round(as.numeric(results()$mageck$data$genes[results()$mageck$data$genes$genes == input$indepthOverviewGene, "pos"]), digits = 5), sep=" "))})# will be depleted and enriched pval

output$indepth_gene.rseapval <-  renderUI({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  HTML(paste("<i>Enriched:</i>", round(as.numeric(results()$rsea$data$gene.pos[rownames(results()$rsea$data$gene.pos) == input$indepthOverviewGene, "FDR.pos"]), digits = 5), "<i>Depleted:</i>", round(as.numeric(results()$rsea$data$gene.neg[rownames(results()$rsea$data$gene.neg) == input$indepthOverviewGene, "FDR.neg"]), digits = 5), sep=" "))})

output$indepth_gene.edgerpval<- renderText({
  
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  round(as.numeric(results()$edger$data$genes[rownames(results()$edger$data$genes) == input$indepthOverviewGene,"FDR"]), digits = 5)})

output$indepth_gene.zratio <- renderText({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene')
  )
  round(as.numeric(results()$zratio[results()$zratio$gene == input$indepthOverviewGene,"zratio"]), digits=5)
  })

# output$indepth_gene.screenbeampval <- renderText({
#   shiny::validate(
#     shiny::need(input$indepthOverviewGene, 'Not Available')
#   )
#   round(as.numeric(results()$screenbeam$data$genes[results()$deseq$data$genes$genes == input$indepthOverviewGene, "padj"]), digits = 5)})

### Part2: highcharts plot with log2FC/Zratio for all genes and highlighted BAR for selected gene, log2FC/Zratio will be TABBED View

output$indepth_hc_gene_log2fc <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(results()$deseq$data$genes, 'No analysis run yet.')
  )
  shiny::withProgress(message = 'Creating log2-foldchange View', value = 0,{
  gene.plotdistribution(data = results()$deseq$data$genes, gene = input$indepthOverviewGene , type = "log2fc", sgrna=FALSE, # or "zratio"
                        tooltip = paste("Log2 FoldChange:", "{point.log2FoldChange}", sep=" "), title = "Log2 Foldchanges", subtitle = "for all genes", xLab = "Genes", yLab = "Log2 FoldChange", zoom = "x",
                        crosshair = TRUE, legend = FALSE, export = TRUE, cols = NULL, filename = paste("GeneOverview_",input$indepthOverviewGene , "LOG2FC", sep="") )
  })
})

output$indepth_hc_gene_zratio <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene, 'Please select a gene.')
  )
  shiny::withProgress(message = 'Creating Z-Ratio View', value = 0,{
  gene.plotdistribution(data = results()$zratio, gene = input$indepthOverviewGene , type = "zratio", sgrna=FALSE, # or "zratio"
                        tooltip = paste("Z-Ratio:", "{point.zratio}", sep=" "), title = "Z-Ratios", subtitle = "for all genes", xLab = "Genes", yLab = "Z-Ratio between Treated and Untreated", zoom = "x",
                        crosshair = TRUE, legend = FALSE, export = TRUE, cols = NULL , filename = paste("GeneOverview_",input$indepthOverviewGene , "Z-Ratio", sep=""))
  })
})


# Part3: Data Table with sgRNA Information and Foldchanges for this particular gene


output$indepth_DT_sgrna2 <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["sgrna.dt"]) ) {
    
    return(Table_DT(indepth_geneOverview()$sgrna.dt, colNames = c("sgRNA", "DESeq2 Adjusted P-Value",  "Log2 FoldChange", "Sequence", "Gene"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_",input$indepthOverviewGene , "Log2FC_sgRNAs", sep="")))
  } else {
    return(HTML("No data found."))
  }
  
  
  
})


# Part4: Highcharter Plot with sgRNA log2FC
output$indepth_hc_sgrna_log2fc <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["sgrna.dt"]) ) {
    
    shiny::withProgress(message = 'Creating log2-foldchange View', value = 0,{
  gene.plotdistribution(data = indepth_geneOverview()$sgrna.dt, gene = input$indepthOverviewGene , type = "log2fc", sgrna=TRUE, # or "zratio"
                        tooltip = paste("Log2 FoldChange:", "{point.y}", sep=" "), title = "Log2 FoldChange", subtitle = "for all sgRNA", xLab = "sgRNAs", yLab = "Log2 FoldChange", zoom = "x",
                        crosshair = TRUE, legend = FALSE, export = TRUE, cols = NULL, filename = paste("GeneOverview_",input$indepthOverviewGene , "LOG2FC_sgRNAs", sep="") )
  })
  } else {
    return(HTML("No data found."))
  }
  
})



###### Part5 : Gene Ontology


output$indepth_GO_table1 <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
    if(!is.null(indepth_geneOverview()$GO$table$cellular_component))
    {
      return(Table_DT(indepth_geneOverview()$GO$table$cellular_component, colNames = c("Type", "GO IDs", "Evidence"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_",input$indepthOverviewGene , "GeneOntology_CellularComponents", sep="")))
    } else {
      return("No data found.")
    }
    
   
  } else {
    return(HTML(config$messages$biomart1$String))
  }
  
  
  
})

output$indepth_GO_table2 <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
    if(!is.null(indepth_geneOverview()$GO$table$biological_process))
    {
      return(Table_DT(indepth_geneOverview()$GO$table$biological_process, colNames = c("Type", "GO IDs", "Evidence"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_",input$indepthOverviewGene , "GeneOntology_BiologicalProcess", sep="")))
      
    } else {
      return("No data found.")
    }
    
    
  } else {
    return(HTML(config$messages$biomart1$String))
  }
  
  
  
  
})

output$indepth_GO_table3 <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
    if(!is.null(indepth_geneOverview()$GO$table$cellular_component))
    {
      return(Table_DT(indepth_geneOverview()$GO$table$molecular_function, colNames = c("Type", "GO IDs", "Evidence"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_",input$indepthOverviewGene , "GeneOntology_MolecularFunction", sep="")))
    } else {
      return("No data found.")
    }
    
    
  } else {
    return(HTML(config$messages$biomart1$String))
  }
  
  
  
  
})

output$indepth_GO_plot1 <- renderImage({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
      # get plot
    shiny::withProgress(message = 'Generating Gene Ontology Plot', value = 0,{
    plotGO <- goview(golist = indepth_geneOverview()$GO, term = "cellular_component", userdir = userDir)

    if(is.na(plotGO))
    {
      return(HTML("No data available."))
    } else {
    list(src = plotGO$plot$cellular_component,
         alt = "Gene Ontology for cellular components",
         class="img-responsive")
    }
    })
    
  } else {
    return(HTML(config$messages$biomart1$String))
  }
  
  
  
})
output$indepth_GO_plot2 <- renderImage({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
       # get plot
    shiny::withProgress(message = 'Generating Gene Ontology Plot', value = 0,{
      plotGO <- goview(golist = indepth_geneOverview()$GO, term = "biological_process", userdir = userDir)
    
      
      if(is.na(plotGO))
      {
        return(HTML("No data available."))
      } else {
          list(src = plotGO$plot$biological_process,
               alt = "Gene Ontology for cellular components",
               class="img-responsive")
      }
      })
      
    
  } else {
    return(HTML(config$messages$biomart1$String))
  }
  
  
 
})
output$indepth_GO_plot3 <- renderImage({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String)
  )
  
  if(!is.na(indepth_geneOverview()["GO"])) {
    
    # get plot
  shiny::withProgress(message = 'Generating Gene Ontology Plot', value = 0,{
    plotGO <- goview(golist = indepth_geneOverview()$GO, term = "molecular_function", userdir = userDir)
    
    if(is.na(plotGO))
    {
      return(HTML("No data available."))
    } else {
      return(
        list(src = plotGO$plot$molecular_function,
                   alt = "Gene Ontology for cellular components",
                   class="img-responsive")
              )
    }
 
  })
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})

# # Downloadbuttons for images
# output$download_GO_plot1 <- downloadHandler(
#   shiny::validate(
#     shiny::need(input$indepthOverviewGene, message="Download Image"),
#     shiny::need(indepth_geneOverview$GO, message=FALSE),
#     shiny::need(indepth_geneOverview$GO$table$cellular_component, 'Data is not available')
#   ),
#   filename = function(file) {
#     paste(indepth_geneOverview$GO$plot$cellular_component,"", sep="")
#   },
#   content = function(con) {
#     file.copy( indepth_geneOverview$GO$plot$cellular_component, con)
#   }
# )
# 
# output$download_GO_plot2 <- downloadHandler(
#   shiny::validate(
#     shiny::need(input$indepthOverviewGene, message="Download Image"),
#     shiny::need(indepth_geneOverview$GO, message=FALSE),
#     shiny::need(indepth_geneOverview$GO$table$biological_process, 'Data is not available')
#   ),
#   filename = function(file) {
#     paste(indepth_geneOverview$GO$plot$biological_process,"", sep="")
#   },
#   content = function(con) {
#     file.copy( indepth_geneOverview$GO$plot$biological_process, con)
#   }
# )
# 
# output$download_GO_plot3 <- downloadHandler(
#   # shiny::validate(
#   #   shiny::need(input$indepthOverviewGene, message="Download Image"),
#   #   shiny::need(indepth_geneOverview$GO, message=FALSE),
#   #   shiny::need(indepth_geneOverview$GO$table$molecular_function, 'Data is not available')
#   # ),
#   filename = function(file) {
#     paste(indepth_geneOverview$GO$plot$molecular_function,"", sep="")
#   },
#   content = function(con) {
#     file.copy( indepth_geneOverview$GO$plot$molecular_function, con)
#   }
# )


######################
### COSMIC ###########
#################

output$indepth_DT_cosmic <- renderDataTable({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4) == 0, "No COSMIC Database defined.")
  )
  
  if(!is.na(indepth_geneOverview()["cosmic"])) {
    
    if(!is.null(indepth_geneOverview()$cosmic$mutation))
    {
      return(try(Table_DT(indepth_geneOverview()$cosmic$mutation, colNames = c("Gene", "Sample name", "Primary site", "Mutation AA", "Mutation Description", "Mutation position", "Resistance Mutation"), bRownames = FALSE, class = "stripe hover", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_COSMIC_Mutations", sep=""))))
    } else {
      return("No data found.")
    }
    
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
})

output$indepth_Type_cosmic <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4) == 0, "No COSMIC Database defined.")
  )
  
  if(!is.na(indepth_geneOverview()["cosmic"])) {
    
    if(!is.null(indepth_geneOverview()$cosmic$hcmutation$`Mutation Description`))
    {
       return(try(plot_cosmic(indepth_geneOverview()$cosmic$hcmutation, plottype = "Mutation Description", title = "Type of Mutation", subtitle = paste("in ", input$indepthOverviewGene ,sep=""), ylab ="Occurence in COSMIC database", xlab =" Type of Mutation", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_COSMIC_TypeMutations", sep=""))))
      
    } else {
      return("No data found.")
    }
    
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
 
})

output$indepth_Tumor_cosmic <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4) == 0, "No COSMIC Database defined.")
  )
  
  if(!is.na(indepth_geneOverview()["cosmic"])) {
    
    if(!is.null(indepth_geneOverview()$cosmic$hcmutation$`Tumour origin`))
    {
      return(try(plot_cosmic(indepth_geneOverview()$cosmic$hcmutation, plottype = "Tumour origin", title = "Tumor Origin", subtitle = paste("for ", input$indepthOverviewGene ,sep=""), ylab ="Occurence in COSMIC database", xlab =" Type of Tumor", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_COSMIC_TumorOrigin", sep=""))))
    } else {
      return("No data found.")
    }
    
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
  
  
})

output$indepth_Primary_cosmic <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4) == 0, "No COSMIC Database defined.")
  )
  if(!is.na(indepth_geneOverview()["cosmic"])) {
    
    if(!is.null(indepth_geneOverview()$cosmic$hcmutation$`Primary site`))
    {
      return(try(plot_cosmic(indepth_geneOverview()$cosmic$hcmutation, plottype="Primary site" , title = "Primary Site of Tissue", subtitle = paste("for ", input$indepthOverviewGene ,sep=""), ylab ="Occurence in COSMIC database", xlab = "Primary Site", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_COSMIC_TumorPrimarySite", sep=""))))
    } else {
      return("No data found.")
    }
    
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
})

output$indepth_Sample_cosmic <- renderHighchart({
  shiny::validate(
    shiny::need(input$indepthOverviewGene != "", 'Please select a gene'),
    shiny::need(indepth_geneOverview(), config$messages$getinfo2$String),
    shiny::need(file.access(names = file.path(config$database_path, config$COSMIC_database), mode = 4) == 0, "No COSMIC Database defined.")
  )
  if(!is.na(indepth_geneOverview()["cosmic"])) {
    
    if(!is.null(indepth_geneOverview()$cosmic$hcmutation$`Sample name`))
    {
      return(try(plot_cosmic(indepth_geneOverview()$cosmic$hcmutation, plottype="Sample name", title = "Samples", subtitle = paste("in which mutations for ", input$indepthOverviewGene , " occured",sep=""), ylab ="Number of mutations", xlab = "Primary Site", filename = paste("GeneOverview_", input$indepthOverviewGene ,"_COSMIC_Samples", sep=""))))
    } else {
      return("No data found.")
    }
    
    
  } else {
    HTML(config$messages$biomart1$String)
  }
  
})





###################
#### Readcount ####
###################

output$idReadcount_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      datasets <- c(results()$compare[[1]], results()$compare[[2]])
      gene <- input$hit_select
      normData <- results()$normalizedReadcount
      rawData <- results()$readcount
      pol <- input$idReadcount_pol
      norm <- input$idReadcount_norm
      Plot_sgRNA_readcount( gene, datasets, normData, rawData, bPolar = pol, bNorm = norm , filename = paste( "InDepth_",input$hit_select ,"_Readcount" ,sep=""))
    } else {
      Plot_blank("hc", msg = "You did not define any gene yet")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})









################
#### Effect ####
################

output$idEffect_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      groups <- names(results()$compare)
      gene <- input$hit_select
      df <- info()$rawGenes
      #ratio <- (input$idEffect_radio == "zr")
      sorted <- input$idEffect_sort
      Plot_sgRNA_foldchanges( gene, groups, df, bSort = sorted , filename = paste( "InDepth_",input$hit_select ,"_Effect" ,sep=""))
    } else {
      Plot_blank("hc", msg = "no gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$status$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})










####################
#### Offtargets ####
####################

output$idOfftarget_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      gene <- input$hit_select
      df <- info()$rawGenes
      pol <- input$idOfftarget_pol
      sorted <- input$idOfftarget_sort
      Plot_sgRNA_offtarget( gene, "offtargets", df, bSort = sorted, bPolar = pol , filename = paste( "InDepth_",input$hit_select ,"_PredictedBindingSites" ,sep=""))
    } else {
      Plot_blank("hc", msg = "no gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})











#################
#### Z Score ####
#################

output$idZscore_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      groups <- names(results()$compare)
      gene <- input$hit_select
      df <- info()$rawGenes
      sorted <- input$idZscore_sort
      Plot_sgRNA_zScores( gene, groups, df, bSort = sorted , filename = paste( "InDepth_",input$hit_select ,"_Z-Score" ,sep=""))
    } else {
      Plot_blank("hc", msg = "no gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})








###########################
#### Efficiency Scores ####
###########################

output$idEscores_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      scores <- c("seed_GC", "doench_score", "xu_score")
      gene <- input$hit_select
      df <- info()$rawGenes
      pol <- input$idEscores_pol
      Plot_sgRNA_scores( gene, scores, df, type = "effic", bPolar = pol , filename = paste( "InDepth_",input$hit_select ,"_EfficiencyScores" ,sep=""))
    } else {
      Plot_blank("hc", msg = "no gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})









#######################
#### E-CRISP Scores ####
#######################

output$idCscores_plot <- renderHighchart({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select) != 0 ){
      scores <- c("Spec.Score", "Anno.Score", "Eff.Score", "CDS_score", "exon_score")
      gene <- input$hit_select
      df <- info()$rawGenes
      pol <- input$idCscores_pol
      Plot_sgRNA_scores( gene, scores, df, type = "ecrisp", bPolar = pol , filename = paste( "InDepth_",input$hit_select ,"_ECRISPScores" ,sep=""))
    } else {
      Plot_blank("hc", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("hc", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("hc", msg = config$messages$noanalysisrunyet$String)
  }
})



########## Datatable for sgRNA sequences
# Takes dataframe as all above from raw.genes

output$idSGRNAsequence <- DT::renderDataTable({
  if(  status$results == TRUE && status$info == FALSE ){
    Table_blank(msg = config$messages$statusinfo$String)
  } else {
    df <- info()$rawGenes
    gene <- input$hit_select
    df <- df[ df$genes == gene, ]
    df <- df[, c("designs", "log2foldchange","z.score.foldchange","offtargets", "sequence")]
    df$log2foldchange <- round(as.numeric(df$log2foldchange), digits=2)
    df$z.score.foldchange <- round(as.numeric(df$z.score.foldchange), digits=2)
    df$offtargets <- round(as.numeric(df$offtargets), digits=0)
    
    return(Table_DT(df, colNames = c("sgRNAs", "Foldchange (log2)", "Z-Score (Foldchange)","Targets", "Target Sequence"), bRownames = FALSE, class = "stripe hover", 
              ordering = list(1, 'asc'), filename = paste( "InDepth_",input$hit_select ,"_sgRNAs" ,sep="") ))
  }
})

######### Datatable for sgRNA binding sites


output$idSGRNAtargets <- DT::renderDataTable({
  if(  status$results == TRUE && status$info == FALSE ){
    Table_blank(msg = config$messages$statusinfo$String)
  } else {
    df <- info()$rawGenes
    gene <- input$hit_select
    df <- df[ df$genes == gene, ]
    df <- df[, c("designs","offtargets", "target")]
    df$offtargets <- round(as.numeric(df$offtargets), digits=0)
    
    
    return(Table_DT(df, colNames = c("sgRNAs", "Targets", "Predicted Binding Sites"), bRownames = FALSE, class = "stripe hover", 
                    ordering = list(1, 'asc'), bScroll = TRUE, filename = paste( "InDepth_",input$hit_select ,"_sgRNA_PredictedBindingSites" ,sep="")) )
  }
})


######################
#### Violin Plot ####
######################
# Creates a violin plot of sgRNA distributions
# arguments df  raw.genes from raw.genes as generated for indepth
#           genes         list of gene identifier
#           target        list of positive controls
#           nontarget     list of nontarget controls

# Output for violine plots effects
output$hit_select_violine <- renderUI({
  if( status$results == TRUE && status$info == TRUE ){
    selectizeInput("hit_select_violine", label = "Select (multiple) genes",
                   choices = results()$aggregatedReadcount$design, width = "400px",
                   multiple = TRUE, options = list(maxItems = 20))
  } else {
    HTML(config$messages$statusinfo$String)
  }
})


# Render texts
output$readcount_group1 <- renderText(
  if( status$results == TRUE)
  {
    return(compare()$group1)
  } else {return("")}
  
)
output$readcount_group2 <- renderText(
  if( status$results == TRUE){
    return(compare()$group2)
  }else {return("")}
  
)


#Untreated Readcount
output$idvioline_readcountuntreated <- renderPlot({
  # Source CRISPRAnalyzeRpools function
  
  
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select_violine) != 0 ){
      
      # call violine plot
      violin(df = info()$rawGenes, target = compare()$pos, nontarget = compare()$neg, genes = input$hit_select_violine, type = "untreated", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                       border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                       drawRect=TRUE, smdensity=TRUE)
    } else {
      Plot_blank("base", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("base", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("base", msg = config$messages$noanalysisrunyet$String)
  }
  
},  res=150)

# Treated Readcount
output$idvioline_readcounttreated <- renderPlot({

  
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select_violine) != 0 ){
      
      # call violine plot
      violin(df = info()$rawGenes, target = compare()$pos, nontarget = compare()$neg, genes = input$hit_select_violine, type = "treated", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                       border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                       drawRect=TRUE, smdensity=TRUE)
    } else {
      Plot_blank("base", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("base", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("base", msg = config$messages$noanalysisrunyet$String)
  }
  
},  res=150)



output$idvioline_foldchange <- renderPlot({
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select_violine) != 0 ){
      
      # call violine plot
      violin(df = info()$rawGenes, target = compare()$pos, nontarget = compare()$neg, genes = input$hit_select_violine, type = "log2foldchange", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                       border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                       drawRect=TRUE, smdensity=TRUE)
    } else {
      Plot_blank("base", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("base",msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("base", msg = config$messages$noanalysisrunyet$String)
  }
  
},  res=150)

# Z-Score on log2Foldchange
output$idvioline_zscorefoldchange <- renderPlot({
  
  
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select_violine) != 0 ){
      
      # call violine plot
      violin(df = info()$rawGenes, target = compare()$pos, nontarget = compare()$neg, genes = input$hit_select_violine, type = "z.score.foldchange", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                       border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                       drawRect=TRUE, smdensity=TRUE)
    } else {
      Plot_blank("base", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("base", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("base", msg = config$messages$noanalysisrunyet$String)
  }
  
},  res=150)



# Binding Sites
output$idvioline_bindingsites <- renderPlot({
  
  if( status$results == TRUE && status$info == TRUE ){
    if( length(input$hit_select_violine) != 0 ){
      
      # call violine plot
      violin(df = info()$rawGenes, target = compare()$pos, nontarget = compare()$neg, genes = input$hit_select_violine, type = "offtargets", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                       border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                       drawRect=TRUE, smdensity=TRUE)
    } else {
      Plot_blank("base", msg = "No gene selected")
    }
  } else if( status$results == TRUE && status$info == FALSE ){
    Plot_blank("base", msg = config$messages$statusinfo$String)
  } else {
    Plot_blank("base", msg = config$messages$noanalysisrunyet$String)
  }
  
},  res=150)






