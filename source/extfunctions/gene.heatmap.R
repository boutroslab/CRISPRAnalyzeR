gene.heatmap <- function(extractpattern=expression("^(.+?)(_.+)"), write=TRUE, allplot="threshold", threshold=0.75, type="enriched", cluster=FALSE, centers=10, pdfratio=4, groups=NULL, base = "genes", dataframe=FALSE, log=FALSE)
{
 
  # data structure already exists?
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    
#     if(!exists("unmapped.all", envir=cp))
#     {
#       stop("Please run unmapped.genes first.")
#     }
    
    # Calculate unmapped sgRNA / Gene infromation
    if(!exists("aggregated.readcount", envir=cp))
    {
      aggregatetogenes(agg.function=sum,extractpattern = extractpattern)
    }
    
    # AIM:
    # Plot heatmap showing the percentage OR absolut number of sgRNAs per Gene which showed a readcount
    # higher than the MEAN, MEDIAN or QUANTILE of the respective dataset
    
    # get threshold for each dataset
    number.files <- length(cp$dataset.names)
  
      
    if(allplot == "threshold")
    {
      for(i in 2:(number.files+1))
      {
        if(i==2)
        {
          threshold.cut <- quantile(cp$readcount[,i], probs = threshold)[[1]]
        }
        else
        {
          threshold.cut <- c(threshold.cut, quantile(cp$readcount[,i],probs = threshold)[[1]])
        }
      }
      
      # copy readcount df
      df.threshold <- cp$readcount
      
      # now we apply the threshold and count how many sgRNAs meet that threshold
      for(i in 2:(number.files+1) )
      {
        
        df.threshold[,colnames(cp$readcount)[i]] <- sapply(cp$readcount[,i], function(x){
          
          # return 1 if read count is 0 and the other way round
          if(type=="depleted")
          {
            if(as.numeric(x[1]) <= threshold.cut[(i-1)])
            {data.return <- 1}
            else
            {data.return <- 0}
          }
          else
          {
            if(as.numeric(x[1]) >= threshold.cut[(i-1)])
            {data.return <- 1}
            else
            {data.return <- 0}
          }
          
          return(data.return)
          
        })
        
      }
      
      rownames(df.threshold) <- df.threshold[,"design"]
      df.threshold$design = NULL
      df.threshold <- aggregate.data.frame(df.threshold, by=list(df.threshold$gene), function(x){
        return(length(x[x == 1]))
      })
      
      df.threshold$gene = NULL
      rownames(df.threshold) <- df.threshold$Group.1
      
      
      for(i in 2:(number.files+1) )
      {
        
        # get number of sgRNAs for each gene
        sgRNA.counts.total = tapply(cp$readcount[,i],cp$readcount[,"gene"],function(x) length(x[x>=0]))
        # walk through df and replace number with the ratio!
        
        # now we take gene name and look what kind of total sgRNAs there is for that
        df.threshold[,i] <- apply(df.threshold,1,function(u){
          
          ratio <- (as.numeric(u[i]) / sgRNA.counts.total[names(sgRNA.counts.total) == as.character(u["Group.1"])])*100 
          return(ceiling(ratio))
          
        })
      }
      
      ## set title
      if(type=="depleted")
      {
        title.plot = paste("% of sgRNAs with read count", "\n" ,"lower than",( threshold*100),"% of all sgRNAs",sep=" ")
      }
      else
      {
        title.plot = paste("% of sgRNAs with read count", "\n" ,"higher than",( threshold*100),"% of all sgRNAs",sep=" ")
      }
      df.threshold$Group.1 = NULL
    }
    else if(allplot=="normfraction")
    {
      ##############
      ######## Now we want a heatmap showing how much readcount that gene makes up of the dataset
      
      # copy readcount df
      df.threshold <- cp$aggregated.readcount
      
      for(i in 2:(number.files+1) )
      {
        # get number of sgRNAs for each gene
        sgRNA.counts.total = tapply(cp$readcount[,i],cp$readcount[,"gene"],function(x) length(x[x>=0]))
        
        #get sum of all read counts for this dataset divided by all sgRNAs
        dataset.sum <- as.numeric(sum(df.threshold[,i], na.rm=TRUE) / sum(sgRNA.counts.total, na.rm=TRUE))
        # now we take each gene and get the readcount divided by the number of sgRNAs in comparison to the total readcount divided by the number of total sgRNAs
        df.threshold[,i] <- apply(df.threshold,1,function(u){
          
          fraction <- (as.numeric(u[i]) / dataset.sum)
          #print(fraction)
          fraction <- (fraction / sgRNA.counts.total[names(sgRNA.counts.total) == as.character(u["design"])])*100 
          if(identical(log,TRUE))
          {
            # get rid of NA,INF etc
            fraction = sapply(fraction, function(x){
              if(as.numeric(x) <= 0.01) { return(0.01)} else return(x)
              
            })
            fraction = log2(fraction)
          }
          return(fraction)
          
        })
      }
      
      ## set title
      title.plot = paste("% of all reads","\n","normalized per sgRNA",sep=" ")
      df.threshold$gene <- NULL
      rownames(df.threshold) <- df.threshold$design
      df.threshold$design <- NULL
      
      
      
    }
    else if(allplot=="fraction")
    {
      ##############
      ######## Now we want a heatmap showing how much readcount that gene makes up of the dataset
      
      # copy readcount df
      if(base=="genes")
      {
        df.threshold <- cp$aggregated.readcount
      }
      else if (base=="sgrna")
      {
        df.threshold <- cp$readcount
        
      }else if(base=="normgenes")
      {
        df.threshold <- cp$normalized.aggregated.readcount
        
      }else if(base=="normsgrna")
      {
        df.threshold <- cp$normalized.readcount
        df.threshold$design.old <- NULL
      }else
      {
        df.threshold <- cp$aggregated.readcount
      }
      
      
      for(i in 2:(number.files+1) )
      {
        # get number of sgRNAs for each gene
        #sgRNA.counts.total = tapply(cp$readcount[,i],cp$readcount[,"gene"],function(x) length(x[x>0]))
        
        #get sum of all read counts for this dataset divided by all sgRNAs
        dataset.sum <- as.numeric(sum(df.threshold[,i], na.rm=TRUE)) #/sum(sgRNA.counts.total, na.rm=TRUE))reads
        df.threshold[,i] <- apply(df.threshold,1,function(u){
          
          fraction <- (as.numeric(u[i]) / dataset.sum)*100
          
          #print(fraction)
          #fraction <- (fraction / sgRNA.counts.total[names(sgRNA.counts.total) == as.character(u["design"])])*100 
          if(identical(log,TRUE))
          {
            # get rid of NA,INF etc
            fraction = sapply(fraction, function(x){
              if(as.numeric(x) <= 0.01) { return(0.01)} else return(x)
                 
            })
            fraction = log2(fraction)
          }
          return(fraction)
          
        })
        
        
      }
      
      ## set title
      title.plot = paste("% of all reads",sep=" ")
      df.threshold$gene <- NULL
      rownames(df.threshold) <- df.threshold$design
      df.threshold$design <- NULL
    }
    else if(allplot=="readcount")
    {
      ##############
      ######## Now we want a heatmap showing how much readcount that gene makes up of the dataset
      
      # copy readcount df
      if(base=="genes")
      {
        df.threshold <- cp$aggregated.readcount
      }
      else if (base=="sgrna")
      {
        df.threshold <- cp$readcount
        
      }else if(base=="normgenes")
      {
        df.threshold <- cp$normalized.aggregated.readcount
      }else if(base=="normsgrna")
      {
        df.threshold <- cp$normalized.readcount
        df.threshold$design.old <- NULL
      }else
      {
        df.threshold <- cp$aggregated.readcount
      }
      
      
      for(i in 2:(number.files+1) )
      {
        df.threshold[,i] <- apply(df.threshold,1,function(u){
        
          readcount <- as.numeric(u[i])
          
          if(identical(log,TRUE))
          {
            # get rid of NA,INF etc
            readcount = sapply(readcount, function(x){
              if(as.numeric(x) <= 0.01) { return(0.01)} else return(x)
              
            })
            readcount = log2(readcount)
          }
          return(readcount)
          
        })
        
        
      }
      
      
      ## set title
      title.plot = paste("Read Count",sep=" ")
      df.threshold$gene <- NULL
      rownames(df.threshold) <- df.threshold$design
      df.threshold$design <- NULL
      
      #str(df.threshold)
    }
    
    if(allplot == "binary")
    {
      # copy readcount df
      if(type=="sgRNA")
      {
        # make binary plot for sgRNA
        df.threshold <- cp$readcount
      }
      else
      {
        df.threshold <- cp$aggregated.readcount
      }
      
      # now we walk through the data and return a binary value of 1 if present and 0 if no readcount is present
      for(i in 2:(number.files+1) )
      {
        # now we take gene name and look what kind of total sgRNAs there is for that
        df.threshold[,i] <- apply(df.threshold,1,function(u){
          #if no read count present, set to 0, otherwise to 1 (binary!)
          if(as.numeric(u[i]) == 0)
          {
            binary <- 0
          }
          else
          {
            binary <- 1
          }
          return(binary)
        })
      }
      ## set title
      title.plot = paste("Binary Mapping Presence",sep=" ")
      df.threshold$gene <- NULL
      rownames(df.threshold) <- df.threshold$design
      df.threshold$design <- NULL
    }
    # remove NAs / infinites by first setting all infinites to 0 and then all NAs to zero
    df.threshold <- data.table::data.table(df.threshold, keep.rownames=TRUE)
    options(bphost="localhost")
    invisible(BiocParallel::bplapply(names(df.threshold),function(.name) data.table::set(df.threshold, which(is.infinite(df.threshold[[.name]])), j = .name,value =0)))
    invisible(BiocParallel::bplapply(names(df.threshold),function(.name) data.table::set(df.threshold, which(is.na(df.threshold[[.name]])), j = .name,value =0)))
    # for(i in 1:ncol(df.threshold))
    # {
    #   df.threshold[,i] <- sapply(df.threshold[,i], function(x){
    #     
    #     if(class(x) == "numeric" || class(x) == "integer")
    #     {
    #       if(is.infinite(x))
    #       {
    #         if(type=="depleted" && allplot == "threshold")
    #         {return (100)}
    #         else
    #         {
    #           return(0) 
    #         }
    #       }
    #       else
    #       {return(x)}
    #     }
    #     else
    #     { return(x)}
    #     
    #   })
    # 
    # }
    # #str(df.threshold)
    # # remove NAs
    # df.threshold[is.na(df.threshold)] <- 0
    df.threshold <- as.data.frame(df.threshold)
    rownames(df.threshold) <- df.threshold$rn
    df.threshold$rn <- NULL
    
    
    ## Compare only groups?
    # groups must be a vector with the group names to be compared
    if(!is.null(groups))
    {
      # so we modify df.threshold an remove ALL but the groups which are to be compared!
      # get the group columns
      for(i in 1:length(groups))
      {
        if(i==1)
        {
          groups.compare = colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1]
        }
        else
        {
          groups.compare = c(groups.compare,colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
        }
      }
      
      # now that we have the groups, we will remove all but the groups
      df.threshold <- df.threshold[,groups.compare]
    }
    #View(df.threshold)
    
    # Generate interactive heatmap
    #interactive.heatmap <- d3heatmap::d3heatmap(df.threshold, Colv=FALSE, k_row=4,scale="column", showGrid=TRUE,cellnote=df.threshold, cellnote_scale = TRUE, yaxis_font_size="2pt", na.rm=TRUE)
    
    # CLUSTER?
    if(identical(cluster,TRUE))
    {
      # Perform clustering
      
      k <- kmeans(df.threshold, centers)
      k$gene <- attr(k$cluster,"names")
      # Append id and cluster
      df.threshold <- cbind(df.threshold, id=seq(nrow(df.threshold)), cluster=k$cluster, gene=k$gene)
      
      # Add idsort, the id number ordered by cluster 
      df.threshold$idsort <- df.threshold$id[order(df.threshold$cluster)]
      df.threshold$idsort <- order(df.threshold$idsort)
      df.threshold$cluster <- NULL
      # use reshape2::melt to create data.frame in long format
      df.threshold <- reshape2::melt(df.threshold, id.vars=c("id", "idsort","gene"))
      
      #df.threshold <- as.matrix(df.threshold)
      #df.threshold <- reshape2::melt(df.threshold)
      
      customlabel <- aggregate.data.frame(df.threshold,by=list(df.threshold$idsort), function(x) {
        return(x)
      })
      
      customlabel$Group.1 <- NULL
      customlabel$id <- NULL
      customlabel$variable <- NULL
      customlabel$value <- NULL
      customlabel$idsort <- NULL
      
      df.threshold <- plyr::ddply(df.threshold, plyr::.(variable), transform,
                         rescale = scale(value))
     
      #prepare ggplot
      p <- ggplot2::ggplot(df.threshold, ggplot2::aes(y=idsort,x=variable)) +  ggplot2::scale_fill_gradient(low="white", high="darkblue", name=title.plot) + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_tile(ggplot2::aes(fill=value)) +
        ggplot2::ggtitle(title.plot)  + ggplot2::scale_y_discrete(labels = c("",customlabel[1:(nrow(as.data.frame(customlabel$gene))),"gene"]) ) +
        ggplot2::theme(legend.position="bottom", plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=4),
              axis.title=ggplot2::element_text(size=15, vjust=0.5), legend.text=ggplot2::element_text(size=10, vjust=0.5,lineheight = 2), axis.ticks=ggplot2::element_blank(), plot.background = ggplot2::element_blank(), panel.background = ggplot2::element_blank() )
      
      
    }
    else
    {
      
      df.threshold <- as.matrix(df.threshold)
      df.threshold <- reshape2::melt(df.threshold)
      #str(df.threshold)
      #prepare ggplot
      p <- ggplot2::ggplot(df.threshold, ggplot2::aes(y=Var1,x=Var2)) +  ggplot2::scale_fill_gradient(low="white", high="darkblue", name=title.plot) + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_tile(ggplot2::aes(fill=value)) +
        ggplot2::ggtitle(title.plot) + #ggplot2::scale_y_discrete(labels = c("",customlabel[1:(nrow(customlabel$gene)),"gene"]) ) +
        ggplot2::theme(legend.position="bottom", plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=2, lineheight=2),
              axis.title=ggplot2::element_text(size=15, vjust=0.5), legend.text=ggplot2::element_text(size=10, vjust=0.5,lineheight = 1), axis.ticks=ggplot2::element_blank(), plot.background = ggplot2::element_blank(), panel.background = ggplot2::element_blank() )
        
      
      
    }
    
    p + ggplot2::coord_fixed(ratio=10)
    
    # Writing to file
    if(identical(write,TRUE) && identical(dataframe,FALSE))
    {
      if(is.null(groups))
      {
        name <- ""
      }
      else
      {
        name <- paste(groups, collapse="-")
      }
      
      pdf(paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste(base,"HEATMAP", allplot, type,"cluster",cluster, sep="-" ),"_", name ,".pdf",sep=""), sep="_"), sep="/"), width=14, height=14*pdfratio)
      print(p)
      dev.off()
      #xlsx::write.xlsx(as.data.frame(df.threshold), file=paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste(base,"HEATMAP", allplot, type, sep="-" ),"_",name,".xls",sep=""), sep="_"), sep="/"), sheetName="Data", append=FALSE ,row.names=FALSE)
    }
    # Print heatmap
    if(identical(dataframe,TRUE))
    {
      #plotly
      # test d3Heatmap
      # color=colorRampPalette(brewer.pal(9,"Blues"))(1000),
      
      if(identical(cluster,TRUE))
      {
        
        df.threshold.return <- df.threshold[,c("variable","idsort","value","gene")]
        df.threshold.return$value <- round(df.threshold.return$value,digits = 2)
        colnames(df.threshold.return) <- c("Var1","y","value","Var2")
        
        df.levels <- data.frame(levels = unique(df.threshold.return$Var1), number = seq.int(from=1, to=length(levels(df.threshold.return$Var1)), by=1), stringsAsFactors=FALSE)
        df.threshold.return$x<- sapply(df.threshold.return$Var1, function(x){
          return(as.numeric(df.levels[df.levels$levels == as.character(x),"number"]))
        })
        
        # get reordered gene names
        #Var2
        
        df.levels <- data.frame(levels = df.threshold.return$Var2, number = df.threshold.return$y, stringsAsFactors=FALSE)
        df.levels <- unique(df.levels)
        
        df.threshold.return$Var2<- sapply(df.threshold.return$y, function(x){
          return(as.character(df.levels[df.levels$number == as.numeric(x),"levels"]))
        })
        
        df.threshold.return$Var2 <- as.character(df.threshold.return$Var2)
        
        df.threshold.return$Var1 <- as.character(df.threshold.return$Var1)
        
        # Order data frame according to Y for categorized output of genes
        df.threshold.return <- df.threshold.return[order(df.threshold.return$y),]
        
        
      }else {
        df.threshold.return <- df.threshold
        df.threshold.return$Var1 <- df.threshold$Var2
        df.threshold.return$Var2 <- df.threshold$Var1
        df.threshold.return$value <- round(df.threshold$value,digits = 2)
        
        #convert factors to numbers for highcharts
        ## VAR1
        
        df.levels <- data.frame(levels = unique(df.threshold.return$Var1), number = seq.int(from=1, to=length(levels(df.threshold.return$Var1)), by=1), stringsAsFactors=FALSE)
        
        df.levels <- unique(df.levels)
        
        df.threshold.return$x<- sapply(df.threshold.return$Var1, function(x){
          return(as.numeric(df.levels[as.character(df.levels$levels) == as.character(x),"number"]))
        })
        
        #Var2
        
        df.levels <- data.frame(levels = unique(df.threshold.return$Var2), number = seq.int(from=1, to=length(levels(df.threshold.return$Var2)), by=1), stringsAsFactors=FALSE)
        
        # df.levels <- unique(df.levels)
        df.threshold.return$y<- sapply(df.threshold.return$Var2, function(x){
          return(as.numeric(df.levels[df.levels$levels == as.character(x),"number"]))
        })
        
        df.threshold.return$Var1 <- as.character(df.threshold.return$Var1)
        #df.threshold.return$Var2 <- as.character(levels(df.threshold.return$Var2))
        
        # Order data frame according to Y for categorized output of genes
        df.threshold.return <- df.threshold.return[order(df.threshold.return$y),]
        
      }
     
      return(df.threshold.return)

    }
    else
    {
      print(p)
    }
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  
  
   
}