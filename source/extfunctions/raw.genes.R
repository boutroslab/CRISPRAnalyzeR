raw.genes=function(genes=NULL, extractpattern=expression("^(.+?)(_.+)"), dataframe=FALSE, log=FALSE, put.names=TRUE, type="foldchange", controls.target= NULL, controls.nontarget=NULL, sort=TRUE, groups=cp$groups.compare)
  {
 
  
  if(is.null(genes))
  {
    genes <- unique(cp$readcount$gene)
  }
  
  if(!is.null(controls.target))
  {
    if(!controls.target %in% cp$readcount$gene)
    {controls.target <- NULL}
  }
  if(!is.null(controls.nontarget))
  {
    if(!controls.nontarget %in% cp$readcount$gene)
    {controls.nontarget <- NULL}
  }
  
  
    gene.names = cp$normalized.readcount[,"gene"]
    designs=cp$normalized.readcount[,"design"]
    designs.row <- as.character(cp$normalized.readcount$design.old)
    
    # get treatment groups
    treatment.groups <- unlist(cp$treatmentgroup)
    
    # Make untreated df
    data.list <- data.frame(
      "designs" = cp$normalized.readcount[,"design"]
    )
    # add data acording to treatment groups
    for(i in 1:length(treatment.groups))
    {
      # unlist groups from miaccs file
      #this.treatment <- as.numeric(unlist(strsplit(treatment.groups[[i]],",")))
      this.treatment <- as.numeric(treatment.groups[[i]])
      treat.df <- data.frame(
        "designs" = cp$normalized.readcount[,"design"],
        stringsAsFactors = FALSE
      )
      # add to treatment group
      treat.df <- cbind.data.frame(treat.df, cp$normalized.readcount[,(this.treatment)+1])
      treatname <- colnames(cp$readcount)[(this.treatment)+1]
      
      colnames(treat.df) = c("designs",treatname)
      treat.df$designs <- NULL
      
      # add 1 to get rid of zeroes
      treat.df[,1] <- sapply(treat.df[,1], function(x) {
        return(as.numeric(x)+1)
      })
    
      
      #     # Normalize
      #     for(n in 2:ncol(treat.df))
      #     {
      #       # also add 1 after normalization to get rid of zeroes
      #       treat.df[,n] <- (treat.df[,n] / norm.function(treat.df[,n]))+1
      #     }
      
      if(i==1)
      {
        data.list <- list(treat.df)
        names(data.list) <- treatname
      }
      else
      {
        name.old <- names(data.list)
        data.list <- c(data.list, list(treat.df) )
        names(data.list) <- c(name.old,treatname)
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
  
  
  untreated <- apply(as.data.frame(data.list[groups.compare[[1]]]),1, function(x){
    return(mean(x))
  } )
  #print(untreated)
  #untreated <- as.data.frame(data.list[groups.compare[[1]]])
  treated <- apply(as.data.frame(data.list[groups.compare[[2]]]),1, function(x){
    return(mean(x))
  } )
  untreated.sd <- apply(as.data.frame(data.list[groups.compare[[1]]]),1, function(x){
    return(sd(x))
  } )
  treated.sd <- apply(as.data.frame(data.list[groups.compare[[2]]]),1, function(x){
    return(sd(x))
  } )
  
  
  ##############################################
  ##############################################
  # Generate new data frame
  # 1.  UNTREATED meaned
  # 2.  TREATED meaned
  # 3.  designs
  # 4.  gene name
  # 5.  foldchange
  # 6.  wilcox p-value
  # get gene names for the dataframe
  
  dataset.combined <- data.frame( 
    designs = as.character(designs),
    untreated = as.numeric(untreated),
    untreated.log10 = as.numeric(log10(untreated)),
    untreated.sd = as.numeric(untreated.sd),
    untreated.log10.sd = as.numeric(sd(log10(untreated), na.rm = TRUE)),
    treated = as.numeric(treated),
    treated.log10 = as.numeric(log10(treated)),
    treated.sd = as.numeric(treated.sd),
    treated.log10.sd = as.numeric(sd(log10(treated), na.rm = TRUE)),
    genes=gene.names,
    log2foldchange=round(log2(as.numeric(treated)/as.numeric(untreated)), digits=2),
    row.names = designs, 
    stringsAsFactors=FALSE)
  
  # remove infinites and set them to 0
  
  #Add rownames for cp$libFILE usage
  row.names(dataset.combined) <- designs.row 
  
  par(mar=c(5,4,4,2))
  par(oma=c(2,2,2,2))
  
  
  
  # Get number of off-targets
  if(type == "offtarget" && exists("ecrisp", envir=cp)  && exists("libFILE", envir=cp))
  {
    if(identical(cp$miaccs$g.convert,TRUE))
    {
      gene.identifier <- cp$miaccs$g.identifier.new
    } else
    {
      gene.identifier <- cp$miaccs$g.identifier
    }
    # Take scores calculated by e-crisp
    # use dataset.combined$designs
    #print("start!")
    # get all other stuff since we do it for everything
    
    # Z-Score
    dataset.combined$z.score.untreated = (dataset.combined$untreated.log10-mean(dataset.combined$untreated.log10))/sd(dataset.combined$untreated.log10)
    dataset.combined$z.score.treated = (dataset.combined$treated.log10-mean(dataset.combined$treated.log10))/sd(dataset.combined$treated.log10)
    dataset.combined$z.score.untreated = (dataset.combined$untreated.log10-mean(dataset.combined$untreated.log10))/sd(dataset.combined$untreated.log10)
    dataset.combined$z.score.treated = (dataset.combined$treated.log10-mean(dataset.combined$treated.log10))/sd(dataset.combined$treated.log10)
    dataset.combined$z.score.foldchange = (dataset.combined$log2foldchange-mean(dataset.combined$log2foldchange))/sd(dataset.combined$log2foldchange)
    
    # Z-Ratio
    #zratio.initial = dataset.combined$z.score.treated-dataset.combined$z.score.untreated
    #dataset.combined$z.ratio=round((dataset.combined$z.score.treated - dataset.combined$z.score.untreated) / (sd(dataset.combined$z.score.treated - dataset.combined$z.score.untreated, na.rm = TRUE)), digits=4)
    
    ## add sgRNA sequences from libFile
    dataset.combined$olddesign <- row.names(dataset.combined)
    
    dataset.combined <- merge(x=dataset.combined, y=cp$libFILE, by.x="designs", by.y="design", all.x=TRUE, all.y=FALSE)
    dataset.combined$sequence <- toupper(dataset.combined$sequence)
    dataset.combined$olddesign <- NULL
    
    #print("copy data for gene")
    # get sgRNAs for genes
    ecrisp.data <- cp$ecrisp[cp$ecrisp$gene %in% genes,]
  #print("get non-full matches")
    # get NON-full match sgRNAs
    ecrisp.data.offtarget <- ecrisp.data[grep(pattern = "(^[n,m,M,x,X]+$)", x = ecrisp.data$Matchstring),c("design", "Spec.Score" , "Anno.Score" , "Eff.Score","Start")]
    # Make unique offtarget sites
    ecrisp.data.offtarget <- ecrisp.data.offtarget[!duplicated(ecrisp.data.offtarget$Start),]
    ecrisp.data.ontarget <- ecrisp.data[grep(pattern = "(^[n,m,M]+$)", x = ecrisp.data$Matchstring),]
    # Combine
    #print("combine")
    
    dataset.combined <- merge(dataset.combined, ecrisp.data.ontarget[,c("design", "Spec.Score" , "Anno.Score" , "Eff.Score" , "CDS_score", "exon_score" , "seed_GC", "doench_score", "xu_score")], by.x="designs", by.y="design", all.x=TRUE)
    
    ecrisp.data.offtarget <- merge(dataset.combined, ecrisp.data.offtarget, by.x = "designs", by.y = "design", all.x=TRUE, all.y=FALSE)
    
    #print("aggregate for uniqueness")
    # Aggregate for unique sgRNAs and count!
    sgrna.offtargets <- tapply(X = ecrisp.data.offtarget$design, INDEX = list(ecrisp.data.offtarget$design), function(x) {
      return(length(x))
    })
    sgrna.offtargets <- as.data.frame(sgrna.offtargets)
    colnames(sgrna.offtargets) <- c("offtargets")
    sgrna.offtargets$design <- rownames(sgrna.offtargets)
  
    #print("get sequence and targets")
    # get sequence + targets
    # This takes a lot of time, need to be made faster
    # sgrna.offtargets$targets <- NA
    
    # make list of gene targets by E-CRISP
    offtargets <- as.list(cp$ecrisp$Gene.targets)
    # Set name of design to it
    names(offtargets) <- cp$ecrisp$design

    options(bphost="localhost")

    offtargets.all <- BiocParallel::bplapply(offtargets, function(x) {
      if(is.na(x))
      {return("intergenic")}
      else {return(as.character(x))}
    })
    
    dataset.combined$target <- sapply(dataset.combined$designs, function(x) {
      return(paste(unique(offtargets.all[names(offtargets.all) == as.character(x)]), collapse = ","))
    })
    
    #sgrna.offtargets$targets <- sapply(sgrna.offtargets$design, function(x){
    #    return(paste(cp$ecrisp[cp$ecrisp$design == as.character(x) ,"Gene.targets"], collapse = ","))
    # })
    
    #print("add to dataset.combined via merge")
    # add to dataset.combined
    dataset.combined <- merge.data.frame(x = dataset.combined, y=sgrna.offtargets, by.x = "designs", by.y = "design", all.x = TRUE)
    
    # Make unique for designs
    dataset.combined <- dataset.combined[!duplicated(dataset.combined$designs),]
    rownames(dataset.combined) <- dataset.combined$designs
    # give back only uniques
    
    #dataset.combined <- dataset.combined[unique(dataset.combined$designs),]
      #cp$ecrisp[cp$ecrisp$design ,"Gene.targets"]
    
    if(identical(dataframe,FALSE))
    {
      
    }
    else
    {
      return(dataset.combined)
    }
  }

  # set back to standard pars
par(mar=c(5,4,4,2))
par(oma=c(2,2,2,2))


}
