generate.hits = function(methods = NULL, type="enriched", cutoff.deseq = cp$miaccs$sig.pval.deseq, cutoff.wilcox = cp$miaccs$sig.pval.wilcox, cutoff.mageck = cp$miaccs$sig.pval.mageck, cutoff.edger = cp$miaccs$sig.pval.edger, cutoff.rsea = cp$miaccs$sig.pval.rsea, cutoff.override=cp$miaccs$cutoff.override, cutoff.hits=cp$miaccs$compare.cutoff, plot.genes="overlapping")
{
  #methods = NULL -> will automatically detect!
  # other: wilcox,deseq,mageck,rsea,edger,doench,screenbeam
  # Generate hits will automatically load all necessary data frames from cp environment and store the information
  # within cp$overlap.candidates
  
  # Checking for presence of analysis tools
  load.analysis = NULL
  if(is.null(methods)) {
    if(exists("wilcox", envir=cp))
    {
      load.analysis = c(load.analysis,"wilcox")
    }
    if(exists("deseq", envir=cp))
    {
      load.analysis = c(load.analysis,"deseq")
    }
    if(exists("mageck", envir=cp))
    {
      load.analysis = c(load.analysis,"mageck")
    }
    if(exists("edger", envir=cp))
    {
      load.analysis = c(load.analysis,"edger")
    }
    if(exists("rsea", envir=cp))
    {
      load.analysis = c(load.analysis,"rsea")
    }
    if(exists("doench", envir=cp))
    {
      load.analysis = c(load.analysis,"doench")
    }
   
  } else {
    for (method in methods){
      if(exists(method, envir=cp))
      {
        load.analysis = c(load.analysis,method)
      }
    }
  }
  
  
  # create data frame we need for calculations
  
  if(type=="enriched")
  {
    # Go through the list to assemble the dataset
    
    # separate by enrichment 
    if("wilcox" %in% load.analysis)
    {
      # wilcox
      df.wilcox = cp$wilcox[cp$wilcox$foldchange >= 1 ,c("foldchange","p.value")]
      
      df.wilcox = df.wilcox[order(
        df.wilcox$p.value,
        decreasing = FALSE 
      ),]
    }
    
    if("deseq" %in% load.analysis)
    {
      
      #DESeq2
      # Deseq make sure it is -1 * foldchange, as deseq compare UNTREATED vs TREATED< all other treated vs untreated!
      df.deseq = as.data.frame(cp$deseq$genes[cp$deseq$genes$log2FoldChange >= 0 ,c("log2FoldChange","padj")])
      
      df.deseq = df.deseq[order(
        df.deseq$padj,
        decreasing = FALSE
      ),]
    }
    
    
    if("mageck" %in% load.analysis)
    {
      # MAGeCK
      df.mageck = cp$mageck$genes[, c("pos","rank.pos","sgrna.pos.good")]
      
      colnames(df.mageck) = c("fdr","rank","sgrna")
      df.mageck = df.mageck[order(
        df.mageck$rank,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("rsea" %in% load.analysis)
    {
      # sgRSEA
      df.rsea = as.data.frame(cp$rsea$gene.pos[, c("p.value.pos","FDR.pos","NScore")])
      rownames(df.rsea) <- rownames(cp$rsea$gene.pos)
      colnames(df.rsea) = c("pval","fdr","score")
      df.rsea = df.rsea[order(
        df.rsea$fdr,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("edger" %in% load.analysis)
    {
      # EdgeR
      df.edger = as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Up", c("Direction","FDR")])
      rownames(df.edger) <- rownames(as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Up",]))
      colnames(df.edger) = c("Direction","fdr")
      df.edger = df.edger[order(
        df.edger[,"fdr"],
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    # Loading of data complete
    
    # Combine datasets using the cutoff
    # put in the rank of that gene in the data analysis table, if not present, put NA
    
    
    
  } else if(type=="depleted")
  {
    
    if("wilcox" %in% load.analysis)
    {
      
      # wilcox
      df.wilcox = cp$wilcox[cp$wilcox$foldchange < 1 , c("foldchange","p.value")]
      df.wilcox = df.wilcox[order(
        df.wilcox$p.value,
        decreasing = FALSE
      ),]
    }
    # print(df.wilcox)
    if("deseq" %in% load.analysis)
    {
      
      #DESeq2
      df.deseq = as.data.frame(cp$deseq$genes[cp$deseq$genes$log2FoldChange < 0 ,c("log2FoldChange","padj")])
      
      df.deseq = df.deseq[order(
        df.deseq$padj,
        decreasing = FALSE
      ),]
    }
    
    
    if("mageck" %in% load.analysis)
    {
      # MAGeCK
      df.mageck = cp$mageck$genes[, c("neg","rank.neg","sgrna.neg.good")]
      
      colnames(df.mageck) = c("fdr","rank","sgrna")
      
      df.mageck = df.mageck[order(
        df.mageck$rank,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("rsea" %in% load.analysis)
    {
      # sgRSEA
      df.rsea = as.data.frame(cp$rsea$gene.neg[, c("p.value.neg","FDR.neg","NScore")])
      rownames(df.rsea) <- rownames(cp$rsea$gene.neg)
      colnames(df.rsea) = c("pval","fdr","score")
      df.rsea = df.rsea[order(
        df.rsea$fdr,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    if("edger" %in% load.analysis)
    {
      # EdgeR
      
      df.edger = as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Down", c("Direction","FDR")])
      rownames(df.edger) <- rownames(as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Down",]))
      colnames(df.edger) = c("Direction","fdr")
      
      df.edger = df.edger[order(
        df.edger[,"fdr"],
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
  } else {
    stop("No type selected. Please select enriched or depleted.")
  }
  #### Data loading complete
  
  # add ranking just by length of dataset
  #if(!is.null(riger)) {df.riger$rank = c(1:nrow(df.riger))}
  # if(!is.null(wilcox)) {df.wilcox$rank = c(1:nrow(df.wilcox))}
  # if(!is.null(deseq)) {df.deseq$rank = c(1:nrow(df.deseq))}
  # 
  # # create joining data.frame that has all gene names, use any of the above dataset
  # 
  # if(!is.null(wilcox)) { v.join = rownames(wilcox)
  #                          
  # } else if(!is.null(deseq)) { v.join = rownames(deseq) 
  #                              
  # } else if(!is.null(mageck)) { v.join = rownames(mageck) 
  #                               
  # } else {stop("No datasets provided")}
  
  # Create output data.frame
  # rownames = gene names
  # then
  # EITHER p.value, foldchange of RIGER, wilcox, DESEQ + AUC of DSS
  # OR rank of RIGER, wilcox, DESEQ, DSS that are within cutoff
  
  # remove all genes that have NA on all of them
  # remove columns that only have NAs (means not data was provided)
  
  
  ## Prepare output of a list used for Readcount scatter with an output of gene names only of ALL analysis methods
  
  
  # introduce output variable
  df.output = NA
  
  # wilcox
  if("wilcox" %in% load.analysis) {
    
    if(identical(cutoff.override,TRUE) && is.numeric(cutoff.hits))
    {
      df.wilcox = df.wilcox[1:cutoff.hits,]
    }
    else if(identical(cutoff.override,TRUE) && is.null(cutoff.hits))
    {
      df.wilcox = df.wilcox
    }
    else
    {
      df.wilcox = df.wilcox[df.wilcox$p.value <= cutoff.wilcox,]
    }
    
    #print(df.wilcox)
    
    #print(length(row(df.wilcox)))
    if(length(row(df.wilcox)) >=1)
    {
      for(i in 1:nrow(df.wilcox))
      {
        if(match(rownames(df.wilcox[i,]), df.output, nomatch = 1) == 1)
        {
          df.output = c(df.output,rownames(df.wilcox[i,]))
        }
        
      }
    }
    
  }
  
  
  
  # DESEQ2
  if("deseq" %in% load.analysis) {
    
    if(identical(cutoff.override,TRUE) && is.numeric(cutoff.hits))
    {
      df.deseq = df.deseq[1:cutoff.hits,]
    }
    else if(identical(cutoff.override,TRUE) && is.null(cutoff.hits))
    {
      df.deseq = df.deseq
    }
    else
    {
      df.deseq = df.deseq[df.deseq$padj <= cutoff.deseq,]
    }
    
    #print(df.deseq)
    if(length(row(df.deseq) >= 1))
    {
      for(i in 1:nrow(df.deseq))
      {
        if(match(rownames(df.deseq[i,]), df.output, nomatch = 1) == 1)
        {
          df.output = c(df.output,rownames(df.deseq[i,]))
        }
        
      }
    }
    
  }
  
  # MAGeCK
  if("mageck" %in% load.analysis) {
    
    if(identical(cutoff.override,TRUE) && is.numeric(cutoff.hits))
    {
      df.mageck = df.mageck[1:cutoff.hits,]
    }
    else if(identical(cutoff.override,TRUE) && is.null(cutoff.hits))
    {
      df.mageck = df.mageck
    }
    else
    {
      df.mageck = df.mageck[df.mageck$fdr <= cutoff.mageck,]
    }
    
    
    #print(df.mageck)
    if(length(row(df.mageck)) >= 1)
    {
      for(i in 1:nrow(df.mageck))
      {
        if(match(rownames(df.mageck[i,]), df.output, nomatch = 1) == 1)
        {
          df.output = c(df.output,rownames(df.mageck[i,]))
        }
        
      }
    }
  }
  
  #EdgeR
  if("edger" %in% load.analysis) {
    
    if(identical(cutoff.override,TRUE) && is.numeric(cutoff.hits))
    {
      df.edger = df.edger[1:cutoff.hits,]
    }
    else if(identical(cutoff.override,TRUE) && is.null(cutoff.hits))
    {
      df.edger = df.edger
    }
    else
    {
      
      df.edger = df.edger[df.edger[,"fdr"] <= cutoff.edger,]
    }
    
    
    #print(df.mageck)
    if(length(row(df.edger)) >= 1)
    {
      for(i in 1:nrow(df.edger))
      {
        if(match(rownames(df.edger[i,]), df.output, nomatch = 1) == 1)
        {
          df.output = c(df.output,rownames(df.edger[i,]))
        }
        
      }
    }
    
  }
  
  # sgRSEA
  if("rsea" %in% load.analysis) {
    
    if(identical(cutoff.override,TRUE) && is.numeric(cutoff.hits))
    {
      df.rsea = df.rsea[1:cutoff.hits,]
    }
    else if(identical(cutoff.override,TRUE) && is.null(cutoff.hits))
    {
      df.rsea = df.rsea
    }
    else
    {
     
      df.rsea = df.rsea[df.rsea$fdr <= cutoff.rsea,]
    }
    
    
    #print(df.mageck)
    if(length(row(df.rsea)) >= 1)
    {
      for(i in 1:nrow(df.rsea))
      {
        if(match(rownames(df.rsea[i,]), df.output, nomatch = 1) == 1)
        {
          df.output = c(df.output,rownames(df.rsea[i,]))
        }
        
      }
    }
  }
  
  
  
  # by default: only overlapping hits will be plotted
  if(plot.genes=="overlapping") 
  {
    # Remove genes that are not present in all methods, so that only overlapping ones are plotted
    df.check = NULL
    for (method in load.analysis)
    {
      df.check = c(df.check, list(rownames(eval(parse(text = paste("df",method, sep="."))))))
    }
    
    df.output = Reduce(intersect, df.check)
    if(length(df.output) < 1)
    {df.output <- NULL}
    # wilcox
    #if(length(row(df.mageck)) >= 1 && length(row(df.wilcox)) >= 1 && length(row(df.deseq)) >= 1 && length(row(df.edger)) >= 1 && length(row(df.rsea)) >= 1)
    #{
      # check if hits are in other datasets
     # for (i in 1:nrow(df.mageck))
      #{
        
        #if(match(rownames(df.mageck[i,]), rownames(df.wilcox), nomatch = 0) == 1 && match(rownames(df.mageck[i,]), rownames(df.deseq), nomatch = 0) == 1)
       # if(rownames(df.mageck[i,]) %in% rownames(df.deseq) && rownames(df.mageck[i,]) %in% rownames(df.wilcox) && rownames(df.edger[i,]) %in% rownames(df.edger) && rownames(df.rsea[i,]) %in% rownames(df.rsea))
        #{
          
     #     df.output = c(df.output,rownames(df.mageck[i,]))
     #   }
     # }
    #}
    #else if(length(row(df.mageck)) >= 1) # Only plot Mageck
    #{
    #  df.output = rownames(df.mageck)
    #}
    
  }
  
  
  
  
  return(df.output)
  
  
}