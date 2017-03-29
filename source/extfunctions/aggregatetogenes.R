aggregatetogenes=function(agg.function=sum,extractpattern=expression("^(.+?)_.+")){
  
  # aggregate to genes will be performed on the cp$readcount and will also generate a new data.frame cp$annotated that can be used for annotation of genes
  # the only information we now need is the agg.function to be used and the extractionpattern we get from cp$miaccs["g.extractpattern"]
  
  
  # check for presence of cp$readcount
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    # aggregate data of cp$readcount data frame
    
    # now make aggregated data and store it as cp$aggregated.readcount
    # use aggregate function
    aggregated.readcount <- try(aggregate(cp$readcount, list(cp$readcount[,"gene"]),
                                         FUN=function(x){
                                           if(is.numeric(x)){
                                             sum(x)
                                           }else{
                                             x[1]
                                           }
                                         }))
    if(class(aggregated.readcount) == "try-error")
    {
      stop()
    }
    
    cp$aggregated.readcount <- aggregated.readcount
    
    # check for uniqueness
    if(length(unique(cp$aggregated.readcount$Group.1)) != length(cp$aggregated.readcount$Group.1))
    {
      # Remove double genes
      
      cp$aggregated.readcount <- subset(cp$aggregated.readcount, !duplicated(Group.1))
      row.names(cp$aggregated.readcount) <- cp$aggregated.readcount$Group.1
      
    } else {
      row.names(cp$aggregated.readcount) <- cp$aggregated.readcount$Group.1
    }
    
    cp$aggregated.readcount[,"design"] <- cp$aggregated.readcount$Group.1
    cp$aggregated.readcount$Group.1 <- NULL
    colnames(cp$aggregated.readcount) <- colnames(cp$readcount)
    
    
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  
  
  return(TRUE)
}