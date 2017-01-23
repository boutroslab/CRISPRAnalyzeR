aggregatetogenes=function(agg.function=sum,extractpattern=expression("^(.+?)_.+")){
  
  # aggregate to genes will be performed on the cp$readcount and will also generate a new data.frame cp$annotated that can be used for annotation of genes
  # the only information we now need is the agg.function to be used and the extractionpattern we get from cp$miaccs["g.extractpattern"]
  
  
  # check for presence of cp$readcount
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    # aggregate data of cp$readcount data frame
    
    # get gene name using extractpattern
    if(is.null(extractpattern))
    {
      extractpattern = cp$miaccs$g.extractpattern
    }
    
    # get gene name and make it as character
    cp$readcount[,"gene"] <- as.character(sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE))
    cp$readcount[,"gene"] <- sapply(cp$readcount[,"gene"], FUN=function(x){as.character(x)})
    
    # now make aggregated data and store it as cp$aggregated.readcount
    # use aggregate function
    cp$aggregated.readcount <- aggregate(cp$readcount, list(cp$readcount[,"gene"]),
                                         FUN=function(x){
                                           if(is.numeric(x)){
                                             sum(x)
                                           }else{
                                             x[1]
                                           }
                                         })
    row.names(cp$aggregated.readcount) <- cp$aggregated.readcount$Group.1
    cp$aggregated.readcount$Group.1 <- NULL
    cp$aggregated.readcount[,"design"] <- row.names(cp$aggregated.readcount)
    colnames(cp$aggregated.readcount) <- colnames(cp$readcount)
    #str(cp$aggregated.readcount)
    
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  
  
  return(TRUE)
}