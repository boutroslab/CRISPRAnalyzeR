stat.wilcox=function(normalize=TRUE, controls=NULL, control.picks=300, sorting=TRUE, groups=cp$groups.compare, logfile = NULL){
  # OLD arguments
  # untreated.list=list(NULL, NULL),treated.list=list(NULL, NULL), namecolumn=1, fullmatchcolumn=2,
  
  # NEW:
  # Treatment groups are taken from cp$treatmentgroup!
 
  cp$wilcox <- NULL
  
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
    
    # gene.names = cp$stabilized.readcount[,"gene"]
    # designs=cp$stabilized.readcount[,"design"]
    # # get treatment groups
    # treatment.groups <- unlist(cp$treatmentgroup)
    # 
    # # Make untreated df
    # data.list <- data.frame(
    #   "designs" = cp$stabilized.readcount[,"design"]
    # )
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
  }

  if(!is.null(logfile))
  {
    write(paste(": wilcox 1"), logfile, append = TRUE)
  }
  
  
  # add data acording to treatment groups
  for(i in 1:length(treatment.groups))
  {
    if(identical(normalize,TRUE))
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
      treatname <- colnames(cp$normalized.readcount)[(this.treatment)+1]
      # treat.df <- cbind.data.frame(treat.df, cp$stabilized.readcount[,(this.treatment)+1])
      # treatname <- colnames(cp$stabilized.readcount)[(this.treatment)+1]
      colnames(treat.df) = c("designs",treatname)
      treat.df$designs <- NULL
    
      #remove NAN etc and set to 1
      treat.df[,1] = sapply(treat.df[,1], function(x)
      {
        if(is.na(x) || is.infinite(x) || is.nan(x))
        {return(as.numeric(0))}
        else {return(as.numeric(x))}
      })
      
    } else
    {
      # unlist groups from miaccs file
      #this.treatment <- as.numeric(unlist(strsplit(treatment.groups[[i]],",")))
      this.treatment <- as.numeric(treatment.groups[[i]])
      treat.df <- data.frame(
        "designs" = cp$readcount[,"design"],
        stringsAsFactors = FALSE
      )
      # add to treatment group
      treat.df <- cbind.data.frame(treat.df, cp$readcount[,(this.treatment)+1])
      treatname <- colnames(cp$readcount)[(this.treatment)+1]
      colnames(treat.df) = c("designs",treatname)
      treat.df$designs <- NULL
      
    }
    

    if(i==1)
    {
      data.list <- list(treat.df)
      names(data.list) <- treatname
    }
    else
    {
      name.old <- names(data.list)
      data.list <- c(data.list, list(treat.df) )
      # set new name
      names(data.list) <- c(name.old,treatname)
    }
  }
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 2"), logfile, append = TRUE)
  }
  
  # get groups to be compared
  for(i in 1:length(groups))
  {
    if(i==1)
    {
      groups.compare = list("CTRL" = colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
    }
    else
    {
      groups.compare = c(groups.compare,list("TREAT" = colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1]))
    }
  }
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 3"), logfile, append = TRUE)
  }
  
  countdata.group1 = data.frame(
    "CTRL" = data.list[groups.compare[[1]]]
  )
  countdata.group2 = data.frame(
    "CTRL" = data.list[groups.compare[[2]]]
  )
  
  # set groups AND MAKE MEAN of all datasets belonging to one group
  untreated <- apply(countdata.group1,1, function(x){
    return(mean(x))
  } )
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 4"), logfile, append = TRUE)
  }
  
  treated <- apply(countdata.group2,1, function(x){
    return(mean(x))
  } )
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 5"), logfile, append = TRUE)
  }
  
  # Generate new data frame
  # 1.  UNTREATED meaned
  # 2.  TREATED meaned
  # 3.  designs
  # 4.  gene name
  # 5.  foldchange
  # 6.  wilcox p-value
  # get gene names for the dataframe

  dataset.combined <- data.frame( 
    untreated = as.numeric(untreated),
    treated = as.numeric(treated),
    designs = designs,
    genes=gene.names,
    foldchange=as.numeric(treated)/as.numeric(untreated),
    stringsAsFactors=FALSE)
  
  rownames(dataset.combined) = designs

  if(!is.null(logfile))
  {
    write(paste(": wilcox 6"), logfile, append = TRUE)
    #write(rownames(dataset.combined), logfile, append = TRUE)
  }
  
  # remove data where entry is NaN, -Inf or +Inf
  # this will lead to NaN in future calculations
  #print("remove unwanted chars")
  
  dataset.combined <- dataset.combined %>% dplyr::filter(is.finite(treated) & is.finite(untreated) & is.finite(foldchange) )
  dataset.combined <- as.data.frame(dataset.combined, stringsAsFactors = FALSE)
  rownames(dataset.combined) = dataset.combined$designs
  # for(i in 1: nrow(dataset.combined))
  # {
  #   # check if either TREATED or UNTREATED Readcount is either NaN, NA, +Inf or -Inf
  #   # if this is the case, delete the line
  #   if(!is.finite(dataset.combined[i,4])){
  #     # set fold change to 1
  #     dataset.combined[i,4] = 1
  #   }else if(dataset.combined[i,4] ==0 ){
  #     dataset.combined[i,4] = 1
  #   }
  # }
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 7"), logfile, append = TRUE)
  }
  
  ### Wilcoxon Rank sum test 
  ## compare distribution of all sgRNAs for a gene to the group of negative controls (wilcox approach)
  
  # IF no non-targeting controls are set (controls = NULL), all sgRNAs will be used as control reference
  
  # make log2 foldchange
 # print("Get Controls")
  # get neg controls
  if(!is.null(controls))
  {
    # we take only the non-targeting sgrnas
    control.test = dataset.combined$foldchange[dataset.combined$genes==controls]

  }
  else
  {
    # non-targeting controls are NULL, so we take all sgRNA data in the dataset OR control.picks number of sgRNAs
    random.picked = dataset.combined[sample(nrow(dataset.combined), control.picks),"foldchange"]
    control.test = random.picked
    #control.test = dataset.combined$foldchange
  }
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 8"), logfile, append = TRUE)
  }
  
  #print("Do p-val calculation")
  # still a bit too slow with large libraries
  options(bphost="localhost")
    # pvals=do.call(
    #   "rbind.data.frame",
      pvals <- BiocParallel::bplapply(
        split(dataset.combined ,
               f = dataset.combined$genes ),
        FUN = function(x) 
        {
          
          ret <- c(unique(x$genes),
            mean(x$untreated),
            mean(x$treated),
            mean(x$foldchange),
            wilcox.test(x$foldchange,
                        control.test,alternative = "t")$p.value
          )
          
          names(ret) <- c(
            "genes",
            "untreated",
            "treated",
            "foldchange",
            "p.value"
          )
          
          return(ret)
          
        }
          
      )
    #)
      
    # Now we need to make the rows
    # Make tibble
    df.wilcox <- tibble::tibble(
      "genes",
      "untreated",
      "treated",
      "foldchange",
      "p.value"
    )
    
    colnames(df.wilcox) <- c(
      "genes",
      "untreated",
      "treated",
      "foldchange",
      "p.value"
    )
    
    # Add everything
    for(i in 1:length(pvals))
    {
      df.wilcox <- df.wilcox %>% dplyr::bind_rows(pvals[[i]])
    }
    
    # remove first row
    df.wilcox <- df.wilcox[-1,]
    
    if(!is.null(logfile))
    {
      write(paste(": wilcox 9"), logfile, append = TRUE)
      #write(print(str(pvals)), logfile, append = TRUE)
    }
  
  #pvals <- as.data.frame(pvals,
  #                       stringsAsFactors = FALSE)
  # names(pvals)=c(
  #               "genes",
  #               "untreated",
  #               "treated",
  #               "foldchange",
  #               "p.value"
  #                )
  df.wilcox <- as.data.frame(df.wilcox,
                             stringsAsFactors = FALSE)
  rownames(df.wilcox) = df.wilcox$genes
  df.wilcox$genes = NULL
  #print("correct for multiple testing")
  # correct for multiple testing
  df.wilcox$p.value.unadjusted = as.numeric(df.wilcox$p.value)
  df.wilcox$p.value = as.numeric(p.adjust(df.wilcox$p.value, method = "BH", n = length(df.wilcox$p.value)))
  
  if(!is.null(logfile))
  {
    write(paste(": wilcox 10"), logfile, append = TRUE)
    write(row.names(df.wilcox), logfile, append = TRUE)
  }
  
 # print("Sorting")
  # Sort data frame according to pValue
  if(sorting)
  {
    # Return
    cp$wilcox <- df.wilcox[order(df.wilcox$p.value),]
  }
  else
  {
    cp$wilcox <- df.wilcox
  }
  
  # Return
  # make numeric
  cp$wilcox$untreated <- as.numeric(cp$wilcox$untreated)
  cp$wilcox$treated <- as.numeric(cp$wilcox$treated)
  cp$wilcox$foldchange <- as.numeric(cp$wilcox$foldchange)
  cp$wilcox$foldchange <- as.numeric(round(cp$wilcox$foldchange, digits=2))
  cp$wilcox$treated <- as.numeric(round(cp$wilcox$treated, digits=2))
  cp$wilcox$untreated <- as.numeric(round(cp$wilcox$untreated, digits=2))
  
  
  return(cp$wilcox)
  
}