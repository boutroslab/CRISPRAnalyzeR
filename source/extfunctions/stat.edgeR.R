stat.edgeR <- function( sorting=FALSE, groups=cp$groups.compare, filename.edger="data", type="standard", fdr=0.001)
{
  # In this emethod,
  # edgeR is used accoring to http://f1000research.com/articles/3-95/v2, http://bioinf.wehi.edu.au/shRNAseq/
  # to analyse screens as a fifth method of analysis.
  # Readcount is passed to edgeR in an UNNORMALIZED fashion.
  
  cp$edger <- NULL
  
  write(paste("edger ","1"), file.path(getwd(),"edger.txt"), append = TRUE)
  
  # data structure already exists?
  if(!exists("cp", mode="environment") || !exists("readcount", envir=cp) )
  {
    stop("Please load your data with load.file() first.")
  }
  
  gene.names = cp$normalized.readcount[,"gene"]
  designs=cp$normalized.readcount[,"design"]
  
  
  # get treatment groups
  treatment.groups <- unlist(cp$treatmentgroup)
  
  # Make untreated df
  data.list <- data.frame(
    "designs" = cp$normalized.readcount[,"design"]
  )
  # add data according to treatment groups
  for(i in 1:length(treatment.groups))
  {
    # unlist groups from miaccs file
    #this.treatment <- as.numeric(unlist(strsplit(treatment.groups[[i]],",")))
    this.treatment <- as.numeric(treatment.groups[[i]])
    treat.df.sgRNA <- data.frame(
      "designs" = cp$normalized.readcount[,"design"],
      stringsAsFactors = FALSE
    )
    
    # add to treatment group for sgRNA
    treat.df.sgRNA <- cbind.data.frame(treat.df.sgRNA, cp$normalized.readcount[,(this.treatment)+1])
    treatname.sgrna <- colnames(cp$normalized.readcount)[(this.treatment)+1]
    colnames(treat.df.sgRNA) = c("designs",treatname.sgrna)
    treat.df.sgRNA$designs <- NULL
    
    
#     # also add 1 after normalization to get rid of zeroes
#     treat.df.sgRNA[,1] <- (treat.df.sgRNA[,1] / norm.function(treat.df.sgRNA[,1]))+1
    
    #remove NAN etc and set to 1
    treat.df.sgRNA[,1] = sapply(treat.df.sgRNA[,1], function(x)
    {
      if(is.na(x) || is.infinite(x) || is.nan(x))
      {return(as.numeric(0))}
      else {return(as.numeric(x))}
    })
    
    # add count of 1
    treat.df.sgRNA[,1] = sapply(treat.df.sgRNA[,1], function(x)
    {
      return(as.numeric(x)+1 )
    })
    
    if(i==1)
    {
      #sgRNA
      data.list.sgRNA <- list(treat.df.sgRNA)
      names(data.list.sgRNA) <- treatname.sgrna
    }
    else
    {
      #sgRNA
      name.old <- names(data.list.sgRNA)
      data.list.sgRNA <- c(data.list.sgRNA, list(treat.df.sgRNA) )
      # set new name
      names(data.list.sgRNA) <- c(name.old,treatname.sgrna)
    }
  }
  
  
  # get the group columns
  for(i in 1:length(groups))
  {
    if(i==1)
    {
      groups.compare = list(colnames(cp$normalized.readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
    }
    else
    {
      groups.compare = c(groups.compare,list(colnames(cp$normalized.readcount)[cp$treatmentgroup[groups[i]][[1]]+1]))
    }
  }
  
  # now get the data frame
  untreated.length = length(data.list.sgRNA[groups.compare[[1]]])
  treated.length = length(data.list.sgRNA[groups.compare[[2]]]) 
  
  # all data is now in data.list and can be accessed to construct the necessary data frame
  # Data frame needs to look like
  # sgRNA Gene  Readcount.Untreated Readcount.treated
  
  # construct data frame dataset.combined
  
  countdata.sgRNA = data.frame(
    "designs" = designs,
    "genes" = gene.names,
    "CTRL" = data.list.sgRNA[groups.compare[[1]]],
    "TREAT" = data.list.sgRNA[groups.compare[[2]]]
  )
  
  # normalize read counts
  
  
  # Add treated group, which is ALWAYS the second treatment group
  #dataset.combined$treated <- data.list.sgRNA[groups.compare[[2]]]
  
  # Add untreated group, which is ALWAYS the second treatment group
  #dataset.combined$untreated <- data.list.sgRNA[groups.compare[[1]]]
  
  #str(countdata.sgRNA)
  
  # counts for DGELIST
  #DGE <- new("DGEList")
  #DGE$counts <- countdata.sgRNA[,3:ncol(countdata.sgRNA)]
  #colnames(DGE$counts) <- c(names(data.list.sgRNA[groups.compare[[1]]]),names(data.list.sgRNA[groups.compare[[2]]]))
  #rownames(DGE$counts) <- countdata.sgRNA$designs
  #str(DGE$counts)
  #print(dim(DGE$counts))
  
  #print(groups[1])
  # Sample for DGELIST
  
  #DGE$samples <- data.frame("SampleID" = colnames(DGE$counts),
  #                      "group" = c(rep(groups[1],untreated.length),rep(groups[2],treated.length)),
  #                      "lib.size" = colSums(DGE$counts),
  #                      "norm.factors" = 1
  #                      )
  #str(DGE$samples)
  
  # Set gene samples
  #DGE$genes <- data.frame(countdata.sgRNA$genes)
  #colnames(DGE$genes) <- "genes"
  #rownames(DGE$genes) <- countdata.sgRNA$designs
  
  counts=countdata.sgRNA[,3:ncol(countdata.sgRNA)]
  rownames(counts) <- countdata.sgRNA$designs
  counts <- as.matrix(counts)
  
  lib.size = colSums(counts)
  edger.groups <- as.factor(c(rep(groups[1],untreated.length),rep(groups[2],treated.length)))
  edger.groups <- factor(edger.groups, levels = c(groups[1],groups[2]))
  DGE2 <- edgeR::DGEList(counts=counts, lib.size = lib.size,
                  norm.factors = rep(1,ncol(counts)), group = edger.groups,
                  genes = data.frame(countdata.sgRNA$genes),
                  remove.zeros = FALSE)
  
  colnames(DGE2$genes) <- "genes"
  rownames(DGE2$genes) <- countdata.sgRNA$designs
  
  #str(DGE)
  #str(DGE2)
  # Set matrix for comparison
  des = stats::model.matrix(~0+DGE2$samples$group, data=DGE2$samples)
  colnames(des)[1:2] <- groups[1:2]
  
  
  
  # # if dispersion is NA, set to 0.3
  # if(is.na(xglm$common.dispersion))
  # {
  #   xglm$common.dispersion <- 0.4
  # }
  # get plot for output
  #plotting=list()
  #plotting$BCV <- edgeR::plotBCV(xglm, main=paste(groups[2],"BCV Plot",sep=":"))
  #View(xglm)
  ## Fit negative bionomial GLM IF more than a single replicate is present!


  if(type=="standard")
  {
    # Estimate Dispersion
    xglm = edgeR::estimateDisp(DGE2, des)
    fit = edgeR::glmFit(xglm, des)
    #fit = edgeR::glmFit(des)
    treated.test <- edgeR::glmLRT(fit, contrast=c(-1,1))
  } 
  if(type == "noreplicate")
  {
    #estimateGLMCommonDisp
    #with method="deviance", robust=TRUE and subset=NULL
    # no replicates, will use exactTest!
    # Estimate Dispersion
    xglm = edgeR::estimateGLMCommonDisp(DGE2, des, subset=NULL)
    # Set dispersion as suggested
    xglm$common.dispersion <- 0.2
    treated.test <- edgeR::exactTest(xglm)
    
    
  }
  
  #View(treated.test)
  #top sgRNAs
  #View(edgeR::topTags(fit))
  sgRNAs.top <- treated.test$table[treated.test$table[,"PValue"] <= fdr,]
  sgRNAs.top$design <- rownames(treated.test$table[treated.test$table[,"PValue"] <= fdr,])
  sgRNAs.top$genes <- treated.test$genes[rownames(treated.test$genes) %in% sgRNAs.top$design,"genes"]
  
  # Carry out GENE analysis ONLY on those genes which had significant DE in sgRNAs
  #genesymbols = as.list(countdata.sgRNA$genes)

  genesymbollist = list()
  unq = unique(countdata.sgRNA$genes)
  unq = unq[!is.na(unq)]
  for(i in unq) {
    sel = countdata.sgRNA$genes==i & !is.na(countdata.sgRNA$genes)
    if(sum(sel)>=3)
      genesymbollist[[i]] = which(sel)
  }
  ##
 
  ## Run mroast for all genes
  set.seed(3042014)
  #test <- limma::camera(xglm, index=genesymbollist, design = des, contrast=2)
   
  genes.top <- limma::mroast(xglm, index=genesymbollist,
                            des, contrast=c(-1,1), adjust.method="holm" , nrot = 9999, set.statistic="mean")#adjust.method = "BH"
   
  ## Display sgRNA and GENE results for output similar to DESeq2
  
  cp$edger <- list(genes = genes.top, sgRNA = sgRNAs.top)
  return(list(genes = genes.top, sgRNA = sgRNAs.top))
  

}
