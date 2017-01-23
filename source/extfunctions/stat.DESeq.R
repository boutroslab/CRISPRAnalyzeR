stat.DESeq=function(agg.function=sum, extractpattern=expression("^(.+?)(_.+)"), sorting=FALSE, groups=cp$groups.compare, sgRNA.pval = 0.01, filename.deseq="data", fitType="parametric", p.adjust="holm"){
# old arguments
  # untreated.list,treated.list,namecolumn=1, fullmatchcolumn=2,
  #requireNamespace(DESeq2)
  

  # FIRST run for sgRNA enrichment/depletion
  # THEN run for gene effect
  # write table with sgRNA infos that will be loaded later for in-depth analysis
  # add $sgRNAs to res for number of significant sgRNAs
  
  # Treatment groups are taken from cp$treatmentgroup!
  
  # set cp$deseq = NULL
  cp$deseq <- NULL
  
  # data structure already exists?
  if(!exists("cp", mode="environment") || !exists("readcount", envir=cp) )
  {
    stop("Please load your data with load.file() first.")
  }
  
  if(!exists("aggregated.readcount", envir=cp) )
  {
    aggregatetogenes(extractpattern = extractpattern)
  }
  
  
  gene.names = cp$aggregated.readcount[,"design"]
  designs=cp$readcount[,"design"]
  
  # get treatment groups
  treatment.groups <- unlist(cp$treatmentgroup)
  
  # Make untreated df
  data.list <- data.frame(
    "designs" = cp$readcount[,"design"]
  )
  # add data according to treatment groups
  for(i in 1:length(treatment.groups))
  {
    # unlist groups from miaccs file
    this.treatment <- as.numeric(treatment.groups[[i]])
    treat.df.sgRNA <- data.frame(
      "designs" = cp$readcount[,"design"],
      stringsAsFactors = FALSE
    )
    
    #genes
    treat.df.genes <- data.frame(
      "designs" = cp$aggregated.readcount[,"design"],
      stringsAsFactors = FALSE
    )
    # add to treatment group for sgRNA
    treat.df.sgRNA <- cbind.data.frame(treat.df.sgRNA, cp$readcount[,(this.treatment)+1])
    treatname.sgrna <- colnames(cp$readcount)[(this.treatment)+1]
    colnames(treat.df.sgRNA) = c("designs",treatname.sgrna)
    treat.df.sgRNA$designs <- NULL
    
    # for genes
    treat.df.genes <- cbind.data.frame(treat.df.genes, cp$aggregated.readcount[,(this.treatment)+1])
    treatname.gene <- colnames(cp$aggregated.readcount)[(this.treatment)+1]
    colnames(treat.df.genes) = c("designs",treatname.gene)
    treat.df.genes$designs <- NULL
    
    if(i==1)
    {
      #sgRNA
      data.list.sgRNA <- list(treat.df.sgRNA)
      names(data.list.sgRNA) <- treatname.sgrna
      #genes
      data.list.genes <- list(treat.df.genes)
      names(data.list.genes) <- treatname.gene
    }
    else
    {
      #sgRNA
      name.old <- names(data.list.sgRNA)
      data.list.sgRNA <- c(data.list.sgRNA, list(treat.df.sgRNA) )
      # set new name
      names(data.list.sgRNA) <- c(name.old,treatname.sgrna)
      
      #genes
      name.old <- names(data.list.genes)
      data.list.genes <- c(data.list.genes, list(treat.df.genes) )
      # set new name
      names(data.list.genes) <- c(name.old,treatname.gene)
    }
  }
  
#   if(is.numeric(groups))
#   {
#     
#     print(treatment.groups[groups[1]])
#     # Get groups and set up the data frame for use with deseq2
#     #sgRNA
#     untreated.length = length(data.list.sgRNA[[treatment.groups[groups[1]]]])
#     treated.length = length(data.list.sgRNA[[treatment.groups[groups[2]]]])  
#     
#     print(treated.length)
#     print(untreated.length)
#     
#     countdata.sgRNA = data.frame(
#       "CTRL" = data.list.sgRNA[[groups[1]]][2:(ncol(data.list.sgRNA[[groups[1]]]))],
#       "TREAT" = data.list.sgRNA[[groups[2]]][2:(ncol(data.list.sgRNA[[groups[2]]]))]
#     )
#     #genes
#     countdata.genes = data.frame(
#       "CTRL" = data.list.genes[[groups[1]]][2:(ncol(data.list.genes[[groups[1]]]))],
#       "TREAT" = data.list.genes[[groups[2]]][2:(ncol(data.list.genes[[groups[2]]]))]
#     )
#   }
  
    # get the group columns
    for(i in 1:length(groups))
    {
      if(i==1)
      {
        groups.compare = list("CTRL" =  colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
      }
      else
      {
        groups.compare = c(groups.compare,list("TREAT" = colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1]))
      }
    }

    # now get the data frame
    untreated.length = length(data.list.sgRNA[groups.compare[[1]]])
    treated.length = length(data.list.sgRNA[groups.compare[[2]]])  
    
    #sgRNA
    countdata.sgRNA = data.frame(
      "CTRL" = data.list.sgRNA[groups.compare[[1]]],
      "TREAT" = data.list.sgRNA[groups.compare[[2]]]
    )
    #Genes
    countdata.genes = data.frame(
      "CTRL" = data.list.genes[groups.compare[[1]]],
      "TREAT" = data.list.genes[groups.compare[[2]]]
    )
    
  # Run for testing sgRNA effects
  row.names(countdata.sgRNA)=designs
  row.names(countdata.genes)=gene.names
  
  
  if(length(untreated.length) != length(treated.length) || (length(untreated.length) == 1 && length(treated.length) ==1 ) )
  {
    colnames(countdata.sgRNA) <- NULL
    colnames(countdata.genes) <- NULL
  }
  else
  {
    colnames(countdata.sgRNA)=c(rep("CTRL",untreated.length),rep("TREAT",treated.length))
    colnames(countdata.genes)=c(rep("CTRL",untreated.length),rep("TREAT",treated.length))
  }
  
  coldata=data.frame(condition = c(rep(groups[1],untreated.length),rep(groups[2],treated.length)), stringsAsFactors = TRUE)
  coldata$condition <- factor(coldata$condition, levels = c(groups[1],groups[2]))
 
  row.names(coldata) = c(groups.compare[[1]],groups.compare[[2]])
  
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = countdata.sgRNA,
                                     colData = coldata,
                                design =  ~ condition)
  sgrna.DESeq2set <- dds
  
  dds <- DESeq2::estimateSizeFactors(dds)
  dds <- DESeq2::estimateDispersions(dds, fitType="parametric")
  des.sgRNA <- DESeq2::nbinomWaldTest(dds)
  
  #plotDispEsts(dds)

  res.sgRNA <- DESeq2::results(des.sgRNA)
  
  # now run for gene enrichment


  dds.genes <- DESeq2::DESeqDataSetFromMatrix(countData = countdata.genes,
                                colData = coldata,
                                design =  ~ condition)
  gene.DESeq2set <- dds.genes  
  
  # TRY CLEAN UP FOR OUTLIERS
  dds.genes <- DESeq2::estimateSizeFactors(dds.genes)
  dds.genes <- DESeq2::estimateDispersions(dds.genes, fitType=fitType)
  des.genes <- DESeq2::nbinomWaldTest(dds.genes)
  
  #plotDispEsts(des.genes)
  res.genes <- DESeq2::results(des.genes, pAdjustMethod = p.adjust)
  
  # Combine
  res.sgRNA$genes = cp$readcount[,"gene"]
  res.sgRNA$sgRNA = row.names(res.sgRNA)
  
  # count number of sgRNAs for a given gene
  sgRNA.number = aggregate.data.frame(res.sgRNA$padj,list(res.sgRNA$genes),function(x) length(x[as.character(x) <= sgRNA.pval]))
  
  res.genes$genes = row.names(res.genes)
  #res.genes$sgRNA = apply(as.data.frame(res.genes), 1, function(x) {
  #                 return(sgRNA.number[sgRNA.number[,1] == x["genes"],2])
  #})
  
  # check for NAs in padjusted and pvalue
  res.genes[is.na(res.genes[,"pvalue"]), "pvalue"] <- 1
  res.genes[is.na(res.genes[,"padj"]), "padj"] <- 1
  res.genes[is.na(res.genes[,"baseMean"]), "baseMean"] <- 0
  res.genes[is.na(res.genes[,"log2FoldChange"]), "log2FoldChange"] <- 0
  res.genes[is.na(res.genes[,"stat"]), "stat"] <- 0
  ### sgRNAs
  res.sgRNA[is.na(res.sgRNA[,"pvalue"]), "pvalue"] <- 1
  res.sgRNA[is.na(res.sgRNA[,"padj"]), "padj"] <- 1
  res.sgRNA[is.na(res.sgRNA[,"baseMean"]), "baseMean"] <- 0
  res.sgRNA[is.na(res.sgRNA[,"log2FoldChange"]), "log2FoldChange"] <- 0
  res.sgRNA[is.na(res.sgRNA[,"stat"]), "stat"] <- 0
  
  # Write sgRNA table
  #write.table(res.sgRNA, file=paste(filename.deseq, "DESeq2_sgRNA.tab", sep="_"), col.names=TRUE, quote=FALSE, sep="\t", row.names=TRUE)
  
  if(sorting==TRUE){
    res.genes = res.genes[order(res.genes$padj, na.last=TRUE),]
    res.sgRNA = res.sgRNA[order(res.sgRNA$padj, na.last=TRUE),]
  }
  
cp$deseq <- list("genes" = res.genes, "sgRNA" = res.sgRNA, "deseq.genes" = des.genes, "deseq.sgRNA" = des.sgRNA)
  
# Return list
return(list("genes" = res.genes, "sgRNA" = res.sgRNA, "deseq.genes" = des.genes, "deseq.sgRNA" = des.sgRNA, "dds.genes" = gene.DESeq2set, "dds.sgrna" = sgrna.DESeq2set))

  
}
