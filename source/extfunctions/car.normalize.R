car.normalize = function(norm.function = "deseq", extractpattern = cp$miaccs$g.extractpattern # deseq, median, sum
                              )
{
  
  if(!exists("cp", mode="environment") || !exists("readcount", envir=cp) )
  {
    stop("Please load your data with load.file() first.")
  }
  
  if(!exists("aggregated.readcount", envir=cp) )
  {
    aggregatetogenes(extractpattern = extractpattern)
  }
  # prepare data for normalization
  
  
  ##### DESeq2
  if(norm.function == "deseq")
  {
    ### sgRNAs
    
    # load the experimental design
    coldata = data.frame(rep("counts",(length(unlist(strsplit(cp$miaccs$files,","))))) )
    
    countdata = data.frame(cp$readcount[,2:(length(unlist(strsplit(cp$miaccs$files,",")))+1)])
    rownames(countdata) <- cp$readcount$design
    colnames(coldata) <- "condition"
    
    # create an object of class DESeqDataSet, which is the data container used by DESeq2
    dds=DESeq2::DESeqDataSetFromMatrix(
      countData = countdata,
      colData = coldata,
      design = ~ 1)
    
    
    dds=DESeq2::estimateSizeFactors(dds)
    #DESeq2::sizeFactors(dds)
    
    deseq_ncounts=as.data.frame(DESeq2::counts(dds, normalized=TRUE))
    deseq_ncounts$design <- rownames(deseq_ncounts)
    
    # Variance stabilization
    # dds2 <- DESeq2::estimateDispersions(dds)
    # dds2 <- DESeq2::varianceStabilizingTransformation(dds2, blind = TRUE,
                                                     # fitType = "parametric")
    # dds2_counts <- assay(dds2)
    # dds2_counts$design <- rownames(dds2_counts)
    
    # Store in new data frame cp$normalized
    design.old = data.frame(
      design = cp$readcount$design,
      design.old = rownames(cp$readcount)
    )
    
    cp$normalized.readcount <- data.frame(
      "design" = as.character(cp$readcount$design),
      stringsAsFactors = FALSE)
    
    cp$normalized.readcount <- merge(
      cp$normalized.readcount,
      deseq_ncounts, by="design", all.x = TRUE)
    
    cp$normalized.readcount <- merge(
      cp$normalized.readcount,
      cp$readcount[,c("design","gene")], by="design", all.x = TRUE)
    
    # set colnames
    colnames(cp$normalized.readcount) <- c("design",colnames(cp$readcount)[2:(length(unlist(strsplit(cp$miaccs$files,",")))+1)],"gene")
    
    # also store stabilized data in cp$stabilized.readcount
    
    # cp$stabilized.readcount <- data.frame(
    #   "design" = as.character(cp$readcount$design),
    #   stringsAsFactors = FALSE)
    # 
    # cp$stabilized.readcount <- merge(
    #   cp$stabilized.readcount,
    #   dds2_counts, by="design", all.x = TRUE)
    # 
    # cp$stabilized.readcount <- merge(
    #   cp$stabilized.readcount,
    #   cp$readcount[,c("design","gene")], by="design", all.x = TRUE)
    # 
    # # set colnames
    # colnames(cp$stabilized.readcount) <- c("design",colnames(cp$readcount)[2:(length(unlist(strsplit(cp$miaccs$files,",")))+1)],"gene")
    # 
    
    ## Genes
    coldata = data.frame(rep("counts",(length(unlist(strsplit(cp$miaccs$files,","))))) )
    
    countdata = data.frame(cp$aggregated.readcount[,2:(length(unlist(strsplit(cp$miaccs$files,",")))+1)])
    colnames(coldata) <- "condition"
    rownames(countdata) <- cp$aggregated.readcount$design
    # create an object of class DESeqDataSet, which is the data container used by DESeq2
    dds=DESeq2::DESeqDataSetFromMatrix(
      countData = countdata,
      colData = coldata,
      design = ~ 1)
    
    
    dds=DESeq2::estimateSizeFactors(dds)
    #DESeq2::sizeFactors(dds)
    
    deseq_ncounts=as.data.frame(DESeq2::counts(dds, normalized=TRUE))
    deseq_ncounts$design <- rownames(deseq_ncounts)
    
    # Store in new data frame cp$normalized
    
    cp$normalized.aggregated.readcount <- data.frame(
      "design" = as.character(cp$aggregated.readcount$design),
      stringsAsFactors = FALSE)
    cp$normalized.aggregated.readcount <- merge(
      cp$normalized.aggregated.readcount,
      deseq_ncounts, by="design", all.x = TRUE)
    cp$normalized.aggregated.readcount <- merge(
      cp$normalized.aggregated.readcount,
      cp$aggregated.readcount[,c("design","gene")], by="design", all.x = TRUE)
    # set colnames
    colnames(cp$normalized.aggregated.readcount) <- c("design",colnames(cp$aggregated.readcount)[2:(length(unlist(strsplit(cp$miaccs$files,",")))+1)],"gene")
    
    #final
    cp$normalized.readcount <- merge(
      cp$normalized.readcount,
      design.old[,c("design","design.old")], by="design", all.x = TRUE)
    
    rownames(cp$normalized.readcount) <- as.character(cp$normalized.readcount$design.old)
    
  } else if(norm.function == "median")
  {
    # copy information
    cp$normalized.readcount <- cp$readcount
    cp$normalized.aggregated.readcount <- cp$aggregated.readcount
    
    # do normalization
    for(i in 2:(length(unlist(strsplit(cp$miaccs$files,",")))+1))
    {
      # sgRNA
      cp$normalized.readcount[,i] <- (as.numeric(cp$normalized.readcount[,i]) / median(as.numeric(cp$normalized.readcount[,i])) )+1
      
      #genes
      cp$normalized.aggregated.readcount[,i] <- (as.numeric(cp$normalized.aggregated.readcount[,i]) / median(as.numeric(cp$normalized.aggregated.readcount[,i])) )+1
    }
    
  }else if(norm.function == "sum")
  {
    # copy information
    cp$normalized.readcount <- cp$readcount
    cp$normalized.aggregated.readcount <- cp$aggregated.readcount
    
    # do normalization
    for(i in 2:(length(unlist(strsplit(cp$miaccs$files,",")))+1))
    {
      # sgRNA
      cp$normalized.readcount[,i] <- (as.numeric(cp$normalized.readcount[,i]) / sum(as.numeric(cp$normalized.readcount[,i])) )+1
      
      #genes
      cp$normalized.aggregated.readcount[,i] <- (as.numeric(cp$normalized.aggregated.readcount[,i]) / sum(as.numeric(cp$normalized.aggregated.readcount[,i])) )+1
    }
    
  }

  
}


