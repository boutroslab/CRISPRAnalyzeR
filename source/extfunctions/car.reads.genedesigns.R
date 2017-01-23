car.reads.genedesigns = function(extractpattern=expression("^(.+?)(_.+)"), col = rgb(0, 0, 0, alpha = 0.65), dataframe=FALSE)
{
  # OLD arguments
  # dataset, namecolumn=1, fullmatchcolumn=2, title="Read Count", xlab="Percentage of sgRNAs present", ylab="Number of Genes", 
  # plot fullmatchreads per gene on X axis
  # plot number of fullmatchdesigns on Y axis

  # check for presence of cp$readcount
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    
    if(!exists("aggregated.readcount", envir=cp) )
    {
      # aggregate data since we need it if it has not been done yet
      patt = extractpattern
      aggregatetogenes(extractpattern=patt)
    }
    
    # get gene names
    gene.names = cp$readcount[,"gene"]
    
    # get number of sgRNAs per gene
    sgRNA.counts.total = tapply(cp$readcount[,"design"],cp$readcount[,"gene"],function(x) length(x))
    
    # Get number of datasets available
    number.files <- length(cp$dataset.names)
    
    # loop for all data sets present in cp$readcount
    for(i in 2:(number.files+1) )
    {
      sgRNA.counts.present = tapply(cp$readcount[,i],cp$readcount[,"gene"],function(x) length(x[x>0]))
      
      plot.data <- data.frame(
        gene <- unique(cp$readcount[,"gene"]),
        stringsAsFactors=FALSE
      )
      
      colnames(plot.data) <- c("gene")
      # add sgRNA countings
      plot.data$sgrna.total <- apply(plot.data,1, function(z) {
        # get the number of sgRNAs from sgRNA.counts
        
        return.info <- sgRNA.counts.total[attr(sgRNA.counts.total,"name") == as.character(z["gene"])][[1]]
        return(return.info)
      })
      
      plot.data$sgrna.present <- apply(plot.data,1, function(z) {
        # get the number of sgRNAs from sgRNA.counts
        
        return.info <- sgRNA.counts.present[attr(sgRNA.counts.present,"name") == as.character(z["gene"])][[1]]
        return(return.info)
      })
      
      colnames(plot.data) <- c("gene","total","present")
      
      plot.data$ratio <- as.numeric((plot.data[,"present"]/plot.data[,"total"])*100)
      
      # prepare plot
      # add read counts
      if(identical(dataframe,FALSE))
      {
        hist(plot.data$ratio, breaks=50, xlab="Percentage of sgRNAs present", ylab="# of Genes", main=paste(colnames(cp$readcount[i]),"sgRNA Coverage",sep=":"), border=FALSE, col=col)   
      } else {
        # Prepare dataframe for return
        if(i==2)
        {
          data.return <- list(plot.data)
          names(data.return) <- colnames(cp$readcount[i])
          
        }
        else
        {
          data.return <- c(data.return,list(plot.data))
          names(data.return) <- colnames(cp$readcount)[2:i]
        }
      }
      
      
      ## ggplot2
      #print(ggplot(plot.data, aes(x=ratio)) + geom_histogram(colour="white", fill="darkgrey", binwidth=1, weight=2) + ggtitle(paste(colnames(cp$readcount[i]),"sgRNA Coverage",sep=" : ")) +
      #        theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x=element_text(size=15, vjust=0.5), axis.text.y=element_text(size=15, vjust=0.5),
      #              panel.background = element_rect(fill = 'white')))
      
    } # end FOR
    if(identical(dataframe,TRUE))
    {
      return(data.return)
    }
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  
  # get gene names
  #rownames(dataset) = dataset[,namecolumn]
  #gene.names = sub(extractpattern,"\\1",dataset[,namecolumn],perl=TRUE)
  #dataset[,namecolumn] = gene.names
  
  #dataset$genes = gene.names
  
  # get readcount
#   dataset.genes = aggregate(dataset, list(dataset$genes),
#                         FUN=function(x){
#                           if(is.numeric(x)){
#                             agg.function(x)
#                           }else{
#                             x[1]
#                           }
#                         }) 
#   
#   # get number of max designs per gene
#   design.max = aggregate(dataset[,fullmatchcolumn], by=list(dataset$genes), function(x) return(length(x)))
# 
#   design.present.all = apply(dataset, 1, function(x) if(as.numeric(x[fullmatchcolumn])>0) {return(as.numeric(1))} else { return(as.numeric(0)) })
#   
#   design.present = aggregate(design.present.all, by=list(dataset$genes), function(x) return(length(x[x==1])))
# 
#   design.present.final = data.frame(gene = design.max$Group.1,
#                                     design.all = design.max$x,
#                                     design.present = design.present$x,
#                                     design.fraction = (design.present$x / design.max$x)*100,
#                                     stringsAsFactors=FALSE)
#   
#   # add read counts
#   hist(design.present.final$design.fraction, breaks=50, xlab=xlab, ylab=ylab, main=title, border=FALSE, col=col)
#   
  
}

