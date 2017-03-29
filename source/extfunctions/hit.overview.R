hit.overview = function( cutoff.deseq = cp$miaccs$sig.pval.deseq, cutoff.wilcox = cp$miaccs$sig.pval.wilcox, cutoff.mageck = cp$miaccs$sig.pval.mageck, cutoff.edger = cp$miaccs$sig.pval.edger, cutoff.rsea = cp$miaccs$sig.pval.rsea, cutoff.override=cp$miaccs$cutoff.override, cutoff.hits=cp$miaccs$compare.cutoff,methods=NULL, dataframe=FALSE)
{
  ## The idea is a plot combining all hits in a single plot.
  # Either all overlapping hits are plotted for enriched and depleted dependent on log2FoldChange on Y-Axis and P-value on X-axis or MAGeCK Rank.
  # By default, the FDR corrected P-value of MAGeCK is used for plotting on the Y-Axis
  
  # Enriched hits are RED, depleted are BLUE, all other GREY
  # Moreover, the P-value cutoff of mageck is plotted as a line
  load.analysis = NULL
  if(is.null(methods)) {
    if(exists("wilcox", envir=cp))
    {
      load.analysis = c(load.analysis,"wilcox")
      wilcox = cp$wilcox
    }
    if(exists("deseq", envir=cp))
    {
      load.analysis = c(load.analysis,"deseq")
      deseq = cp$deseq
    }
    if(exists("mageck", envir=cp))
    {
      load.analysis = c(load.analysis,"mageck")
      mageck <- cp$mageck
    }
    if(exists("edger", envir=cp))
    {
      load.analysis = c(load.analysis,"edger")
      edger <- cp$edger
    }
    if(exists("rsea", envir=cp))
    {
      load.analysis = c(load.analysis,"rsea")
      rsea <- cp$rsea
    }
    
    
  } else {
    for (method in methods){
      if(exists(method, envir=cp))
      {
        load.analysis = c(load.analysis,method)
      }
    }
  }
  
  # Call function to gene hit list to get OVERLAPPING genes or all individual genes (if plot.genes != overlapping)
  df.output.enriched = generate.hits(type="enriched", cutoff.deseq = cutoff.deseq, cutoff.wilcox = cutoff.wilcox, cutoff.mageck = cutoff.mageck, cutoff.override=cutoff.override, cutoff.hits=cutoff.hits, plot.genes="overlapping")
  df.output.depleted = generate.hits(type="depleted", cutoff.deseq = cutoff.deseq, cutoff.wilcox = cutoff.wilcox, cutoff.mageck = cutoff.mageck, cutoff.override=cutoff.override, cutoff.hits=cutoff.hits, plot.genes="overlapping")
  
  # Plotting parameter
  par(mfrow=c(1,1))
  plot.method <- list()
  
  # Use DESeq2 as provider of the log2Foldchange
  if("deseq" %in% load.analysis)
  {
    df.deseq=as.data.frame(deseq$genes)
    colnames(df.deseq) = c("base","log2FoldChange","lfcSE","stat","pvalue","deseq.pval","genes")
    df.deseq = df.deseq[,c("log2FoldChange","deseq.pval","genes")]
    plot.method <- c(plot.method, list("DESeq2" = c("deseq.pval","log2FoldChange","DESeq2", cutoff.deseq,"-log10 Adjusted DESeq2 pval", "DESeq2 log2 Foldchange")))
    
  }
  #Wilcox
  if("wilcox"  %in% load.analysis)
  {
    df.wilcox=data.frame(p.value = wilcox[,"p.value"], log2fc = log2(wilcox[,"foldchange"]), stringsAsFactors=FALSE)
    df.wilcox$genes = rownames(wilcox)
    colnames(df.wilcox) = c("wilcox.pval","wilcox.log2fc","genes")
    plot.method <- c(plot.method, list("Wilcox" = c("wilcox.pval","wilcox.log2fc","Wilcox", cutoff.wilcox,"-log10 Adjusted Wilcox pval", "Wilcox log2 Foldchange")))
  }
  #Mageck
  if("mageck" %in% load.analysis )
  {
     df.mageck = as.data.frame(mageck$genes[,c("genes","pos","neg")])
     
     colnames(df.mageck) = c("genes","mageck.pval.enriched", "mageck.pval.depleted")
     plot.method <- c(plot.method, list("MAGeCK enriched" = c("mageck.pval.enriched","log2FoldChange", "MAGeCK enriched", cutoff.mageck,"-log10 Adjusted MAGeCK FDR Enriched", "DESeq2 log2 Foldchange")))
     plot.method <- c(plot.method, list("MAGeCK depleted" = c("mageck.pval.depleted","log2FoldChange", "MAGeCK depleted", cutoff.mageck,"-log10 Adjusted MAGeCK FDR Depleted", "DESeq2 log2 Foldchange")))
  }
  
  
  if("edger"  %in% load.analysis)
  {
    df.edger=data.frame(p.value = edger$genes[,"FDR"], Direction = edger$genes[,"Direction"], stringsAsFactors=FALSE)
    df.edger$genes = rownames(edger$genes)
    colnames(df.edger) = c("edger.pval","edger.direction","genes")
    plot.method <- c(plot.method,list("edgeR" = c("edger.pval","log2FoldChange","edgeR", cutoff.edger,"-log10 Adjusted EdgeR FDR", "DESeq2 log2 Foldchange")))
  }
  
  #EdgeR
  if("rsea" %in% load.analysis)
  {
    df.rsea.enriched=data.frame(p.value = rsea$gene.pos[,"FDR.pos"], rank = rsea$gene.pos[,"rank.pos"], stringsAsFactors=FALSE)
    df.rsea.enriched$genes = rownames(rsea$gene.pos)
    colnames(df.rsea.enriched) = c("rsea.enriched.pval","rsea.enriched.rank","genes")

    df.rsea.depleted=data.frame(p.value = rsea$gene.neg[,"FDR.neg"], rank = rsea$gene.neg[,"rank.neg"], stringsAsFactors=FALSE)
    df.rsea.depleted$genes = rownames(rsea$gene.neg)
    colnames(df.rsea.depleted) = c("rsea.depleted.pval","rsea.depleted.rank","genes")
    
    plot.method <- c(plot.method, list("sgRSEA enriched" = c("rsea.enriched.pval","log2FoldChange", "sgRSEA enriched", cutoff.rsea,"-log10 Adjusted sgRSEA FDR Enriched", "DESeq2 log2 Foldchange")))
    plot.method <- c(plot.method, list("sgRSEA depleted" = c("rsea.depleted.pval","log2FoldChange", "sgRSEA depleted", cutoff.rsea,"-log10 Adjusted sgRSEA FDR Depleted", "DESeq2 log2 Foldchange")))
  }

  # Now we get the information and create a new dataset using all data
  df.plot = data.frame(
    genes = unique(cp$readcount$gene),
    stringsAsFactors=FALSE)
  
  if("wilcox" %in% load.analysis)
  {
    df.plot = merge(df.plot, df.wilcox, by.x="genes", by.y="genes", all.x=TRUE)
  }
  
  if(!is.null(df.deseq))
  {
    df.plot = merge(df.plot, df.deseq, by.x="genes", by.y="genes", all.x=TRUE)
  }
  if(!is.null(df.mageck))
  {
    df.plot = merge(df.plot, df.mageck, by.x="genes", by.y="genes", all.x=TRUE)
  }
  if(!is.null(df.edger))
  {
    df.plot = merge(df.plot, df.edger, by.x="genes", by.y="genes", all.x=TRUE)
  }
  if(!is.null(df.rsea.enriched))
  {
    df.plot = merge(df.plot, df.rsea.enriched, by.x="genes", by.y="genes", all.x=TRUE)
  }
  if(!is.null(df.rsea.depleted))
  {
    df.plot = merge(df.plot, df.rsea.depleted, by.x="genes", by.y="genes", all.x=TRUE)
  }
#     wilcox.pval = as.numeric(df.wilcox$p.value),
#     mageck.pval.enriched = as.numeric(df.mageck$pos),
#     mageck.pval.depleted = as.numeric(df.mageck$neg),
#     deseq2.pval = as.numeric(df.deseq$padj),
#     edger.pval = as.numeric(df.edger$p.value),
#     rsea.pval.enriched = as.numeric(df.rsea.enriched$p.value),
#     rsea.pval.depleted = as.numeric(df.rsea.depleted$p.value),
#     stringsAsFactors=FALSE)
  
  # Remove NA
  df.plot$edger.direction <- sapply(df.plot$edger.direction, function(x) {
    if(is.na(x))
    {
      return(0)
    } else {return(x)}
  })
  df.plot$edger.pval <- sapply(df.plot$edger.pval, function(x) {
    if(is.na(x))
    {
      return(1)
    } else {return(x)}
  })
  
  
  #View(df.plot)
  ## Add color
    df.plot$color = apply(df.plot, 1, function(x){
      
      ret.col = rgb(211,211,211, 255, maxColorValue=255)
      
        # Set color for enriched
        if(x["genes"] %in% df.output.enriched && length(df.output.enriched) >= 1)
        {ret.col = rgb(217,35,35, 255, maxColorValue=255)}
        # Set color for depleted
        else if(x["genes"] %in% df.output.depleted && length(df.output.depleted) >= 1)
        {ret.col = ret.col = rgb(46,98,166, 255, maxColorValue=255)}
        # Set color for overlapping
        else
        {
          if(as.numeric(x["deseq.pval"]) < cutoff.deseq )
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["wilcox.pval"]) < cutoff.wilcox )
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["edger.pval"]) < cutoff.edger)
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["mageck.pval.enriched"]) < cutoff.mageck)
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["mageck.pval.depleted"]) < cutoff.mageck)
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["rsea.enriched.pval"]) < cutoff.rsea)
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
          if(as.numeric(x["rsea.depleted.pval"]) < cutoff.rsea)
          {
            ret.col = rgb(255,165,0, 255, maxColorValue=255)
          }
        }
      return(ret.col)
    })
  
    # Set par
    par(mfrow=c(1,1))
    #### Idea:
    # Independent plots for each analysis method that indicates which hits are the overlaping and which hits are not
    # Plot all methods
    options(bphost="localhost")
    if(identical(dataframe,FALSE))
    {
      BiocParallel::bplapply(plot.method,function(x) {
        
        #calculate min and max
        pval <- abs(-log10(df.plot[,x[[1]]]))
        # remove INF/NA
        pval <- sapply(pval, function(x){
          if(is.finite(x))
          { return(x)}
          else {return(0)}
        })
        min.x <- min(pval, na.rm=TRUE)
        max.x <- max(pval, na.rm=TRUE)
        
        if(max.x < abs(-log10(as.numeric(x[[4]]))))
        {
          max.x <- abs(-log10(as.numeric(x[[4]])))
        }
        if(min.x > abs(-log10(as.numeric(x[[4]]))))
        {
          min.x <- abs(-log10(as.numeric(x[[4]])))
        }
        
        #y axis
        fc <- df.plot[,x[[2]]]
        min.y <- min(fc)
        max.y <- max(fc)
        ## ggplot
        
        
        # plot
        plot(pval,
             fc,
             col=as.character(df.plot$color),
             pch=16,
             ylab=as.character(x[[6]]),
             xlab=as.character(x[[5]]),
             main=as.character(x[[3]]),
             xlim=c(min.x,max.x),
             ylim = c(min.y,max.y),
             yaxt = "l",
             cex=1.1)
        axis(side=2, at=c(-4,-2,-1,0,1,2,4),
             col.axis="black", las=2, cex.axis=0.7, tck=-.01)
        # Generate p-value line
        abline(v=abs(-log10(as.numeric(x[[4]]))))
        
        # Generate Lebel for overlapping hits
        if(length(df.output.enriched) >= 1)
        {
          # Enriched
          enriched.genes=c(which(df.plot$genes %in% df.output.enriched))
          text(abs(-log10(df.plot[,x[[1]]]))[enriched.genes],df.plot[,x[[2]]][enriched.genes],df.plot$genes[enriched.genes],cex=1,pos=2,offset = 1)
        }
        if(length(df.output.depleted) >= 1)
        {
          # Enriched
          depleted.genes=c(which(df.plot$genes %in% df.output.depleted))
          text(abs(-log10(df.plot[,x[[1]]]))[depleted.genes],df.plot[,x[[2]]][depleted.genes],df.plot$genes[depleted.genes],cex=1,pos=2,offset = 1)
        }
        # legend
        legend("topleft",bty = "n", cex = 0.9, pt.bg = c(rgb(217,35,35, 255, maxColorValue=255),rgb(46,98,166, 255, maxColorValue=255),rgb(255,165,0, 255, maxColorValue=255),rgb(211,211,211, 255, maxColorValue=255) ), col = c(rgb(217,35,35, 255, maxColorValue=255),rgb(46,98,166, 255, maxColorValue=255),rgb(255,165,0, 255, maxColorValue=255),rgb(211,211,211, 255, maxColorValue=255) ), pch = c(16,16,16,16), legend=c("Overlapping Hit Enriched","Overlapping Hit Depleted", "Non-Overlapping Hit", "Not Enriched/Depleted") )
        
        
      })
    } else
    {
      return(df.plot)
    }
    

  # set par back to normal
  par(mfrow=c(1,1))
  
}