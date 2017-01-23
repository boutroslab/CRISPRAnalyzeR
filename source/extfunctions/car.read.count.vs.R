car.read.count.vs=function(nontargeting.controls = NULL, pos.controls=NULL)
{
  # check for presence of cp$readcount
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    
    if(!exists("aggregated.readcount", envir=cp) )
    {
      # aggregate data since we need it if it has not been done yet
     stop("Aggregated Read Count data could not be found.")
    }
    
    # Get number of datasets available
    dataset.number <- length(cp$dataset.names)
    
   # Construct data using normalized data
    #sgRNA first
    ## Generate large dataframe where all calculations are done
    df.sgrna = cp$normalized.readcount[,1:(dataset.number+1)]
    df.sgrna$gene = cp$normalized.readcount[,"gene"]
    df.sgrna$labelgene <- ""
    
   # gene readcount is used
    ## Generate large dataframe where all calculations are done
    df.gene = cp$normalized.aggregated.readcount[,1:(dataset.number+1)]
    df.gene$gene = cp$normalized.aggregated.readcount[,"design"]
    df.gene$labelgene <- ""
      
    # Add colors to highlight for non-targeting, positive or no controls
    df.sgrna$labelgene <- sapply(df.sgrna$gene, function(x) {
      if(x %in% nontargeting.controls)
      {
        return("neg")
      } else if (x %in% pos.controls)
      {
        return("pos")
      } else
      {
        return("")
      }
    })
    df.gene$labelgene <- sapply(df.gene$gene, function(x) {
      if(x %in% nontargeting.controls)
      {
        return("neg")
      } else if (x %in% pos.controls)
      {
        return("pos")
      } else
      {
        return("")
      }
    })
      
    # Make LOG?
    df.sgrna.log <- df.sgrna
    df.gene.log <- df.gene
    
    # make log2 transform of normalized data
    for(i in 2:(ncol(df.sgrna)-2))
    {
      df.gene.log[ ,i] <- log2(df.gene.log[ ,i])
      df.sgrna.log[,i] <- log2(df.sgrna.log[ ,i])
    }
    
    # check for NA values and set them to ZERO
    for(i in 2:(ncol(df.sgrna)-2))
    {
      df.gene[!is.finite(df.gene[,i]),i] <- 0
      df.sgrna[!is.finite(df.sgrna[,i]),i] <- 0
      df.gene.log[!is.finite(df.gene.log[,i]),i] <- 0
      df.sgrna.log[!is.finite(df.sgrna.log[,i]),i] <- 0
    }
    

    # Data frames will be packed into a list and return
    # each list is a DF containing the same information
    # controls will be picked by labelgene
    
    out <- list(
      "sgRNA" = df.sgrna,
      "gene" = df.gene,
      "sgRNA_log2" = df.sgrna.log,
      "gene_log2" = df.gene.log
    )
    
    return(out)
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  


}