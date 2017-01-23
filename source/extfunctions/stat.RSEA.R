stat.RSEA <- function(extractpattern=expression("^(.+?)(_.+)"), sorting=FALSE, groups=cp$groups.compare, normalize = TRUE, filename.rsea="data", rsea.multiplier = 50, rsea.seed = NULL, type="standard")
{
  ##### In this method, the statistical method RSEA of the sgRSEA package is used to evaluate the enrichment/depletion (essentiality) if screens
  ## see https://cran.r-project.org/web/packages/sgRSEA/sgRSEA.pdf
  ## Maintainer of sgRSEA Jungsik Noh <Jungsik.Noh@UTSouthwestern.edu>
  
  
  ## Prepare data for sgRSEA (similar to deseq,mageck)
  
  cp$rsea <- NULL
  
  # data structure already exists?
  if(!exists("cp", mode="environment") || !exists("readcount", envir=cp) )
  {
    stop("Please load your data with load.file() first.")
  }
  
  if(!exists("aggregated.readcount", envir=cp) )
  {
    aggregatetogenes(extractpattern = extractpattern)
  }
  
  
  if(identical(normalize,TRUE))
  {
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
      
      
      #remove NAN etc and set to 1
      treat.df.sgRNA[!is.finite(treat.df.sgRNA[,1]),1] <- as.numeric(0)
      
      #treat.df.sgRNA[,1] = sapply(treat.df.sgRNA[,1], function(x)
      #{
      #  if(is.na(x) || is.infinite(x) || is.nan(x))
      #  {return(as.numeric(0))}
      #  else {return(as.numeric(x))}
      #})
      
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
    
    
  }
  else
  {
    gene.names = cp$readcount[,"gene"]
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
      #this.treatment <- as.numeric(unlist(strsplit(treatment.groups[[i]],",")))
      this.treatment <- as.numeric(treatment.groups[[i]])
      treat.df.sgRNA <- data.frame(
        "designs" = cp$readcount[,"design"],
        stringsAsFactors = FALSE
      )
      
      # add to treatment group for sgRNA
      treat.df.sgRNA <- cbind.data.frame(treat.df.sgRNA, cp$readcount[,(this.treatment)+1])
      treatname.sgrna <- colnames(cp$readcount)[(this.treatment)+1]
      colnames(treat.df.sgRNA) = c("designs",treatname.sgrna)
      treat.df.sgRNA$designs <- NULL
      
      
      # also add 1 after normalization to get rid of zeroes
      treat.df.sgRNA[,1] <- (treat.df.sgRNA[,1] / norm.function(treat.df.sgRNA[,1]))+1
      
      #remove NAN etc and set to 1
      treat.df.sgRNA[!is.finite(treat.df.sgRNA[,1]),1] <- as.numeric(0)
      # treat.df.sgRNA[,1] = sapply(treat.df.sgRNA[,1], function(x)
      # {
      #   if(is.na(x) || is.infinite(x) || is.nan(x))
      #   {return(as.numeric(0))}
      #   else {return(as.numeric(x))}
      # })
      
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
  }
  
  
  
  
  # get the group columns
  for(i in 1:length(groups))
  {
    if(i==1)
    {
      groups.compare = list(colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
    }
    else
    {
      groups.compare = c(groups.compare,list(colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1]))
    }
  }
  
  # now get the data frame
  untreated.length = length(data.list.sgRNA[groups.compare[[1]]])
  treated.length = length(data.list.sgRNA[groups.compare[[2]]]) 
  
  # all data is now in data.list and can be accessed to construct the necessary data frame
  # Data frame needs to look like
  # sgRNA Gene  Readcount.Untreated Readcount.treated
  
  # construct data frame dataset.combined
  
  dataset.combined <- data.frame(
    "designs" = designs,
    "genes" = gene.names,
    stringsAsFactors=FALSE
  )
  
  # normalize read counts
  
  
  # Add treated group, which is ALWAYS the second treatment group
  dataset.combined$treated <- apply(as.data.frame(data.list.sgRNA[groups.compare[[2]]]),1, function(x){
    return(mean(x)) })
  
  # Add untreated group, which is ALWAYS the second treatment group
  dataset.combined$untreated <- apply(as.data.frame(data.list.sgRNA[groups.compare[[1]]]),1, function(x){
    return(mean(x)) })
  
  #str(dataset.combined)
  
  # Remove 0 as readcount by adding 1 to every count file as this is required by sgRSEA
  dataset.combined$treated <- sapply(dataset.combined$treated, function(x){
    return(as.numeric(x)+1) })
  
  dataset.combined$untreated <- sapply(dataset.combined$untreated, function(x){
    return(as.numeric(x)+1) })
  
  #### Call the RSEA method
  # Result will be
  # $gene.pos
  # $gene.neg
  # $stdTmat Normalized observed gene scores.
  # $stdNullTmat Normalized permutation gene scores.
  # $Tmat Observed gene scores.
  # $NullTmat Permutation gene scores.
  # $sgRNA.stat The input data.frame added with the sgRNA/gene scores and the numbers of
  # $sgRNAs within genes.
  if(type=="mean"){
    data.rsea <- sgRSEA::sgRSEA.mean(dataset.combined, multiplier = rsea.multiplier, r.seed = rsea.seed)
  }
  else
  {
    data.rsea <- sgRSEA::sgRSEA(dataset.combined, multiplier = rsea.multiplier, r.seed = rsea.seed)
  }
  
  #sgRSEA::plotScores(data.rsea, m=8)
  
  # Return as list and store in cp environment
  cp$rsea <- list( gene.pos = data.rsea$gene.pos, gene.neg = data.rsea$gene.neg,
                   stdTmat=data.rsea$stdTmat,
                   stdNullTmat = data.rsea$stdNullTmat,
                   Tmat = data.rsea$Tmat,
                   NullTmat = data.rsea$NullTmat,
                   sgRNA.stat = data.rsea$sgRNA.stat)
  
  return(cp$rsea)
  
}