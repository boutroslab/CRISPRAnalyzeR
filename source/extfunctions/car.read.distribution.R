car.read.distribution=function(breaks=200, xlab="log2 Readcount", ylab="# sgRNAs",statistics=TRUE, col=rgb(0, 0, 0, alpha = 0.65),extractpattern=expression("^(.+?)(_.+)"), plotgene=NULL, type="distribution", logscale=TRUE, dataframe=FALSE){   
  # OLD arguments
  # dataset,namecolumn=1, fullmatchcolumn=2,
  # dataset = read.table frame
  # fullmatchcolumn = integer, in which column is the matchcount
  # xlim = integer, which row should be the last row to display (e.g. if random genes make up most, you can exzlude them)
  # TYPE - distribution | whisker
  
  
  if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
  {
    # Plotting the READ distribution for all datasets at once, scaled to the highest read counts of all datasets
    
    # already aggregated?
    if(!exists("aggregated.readcount", envir=cp))
    {
      aggregatetogenes(extractpattern = extractpattern)
    }
    
    # No gene is plotted in detail, so we plot read count for genes instead of sgRNA
    if(is.null(plotgene))
    {
      # number of genes for BREAK calculation
      if (breaks=="")
      {breaks = nrow(cp$readcount)/100}
      
      # Walk through each dataset!
      for(i in 2:(length(cp$dataset.names)+1))
      {
        # # compute general statistics
        meancount=as.integer(mean(cp$readcount[,i], na.rm = TRUE))
        mediancount= as.integer(median(cp$readcount[,i], na.rm = TRUE))
        maxcount=max(cp$readcount[,i], na.rm = TRUE)
        
        # copy of read count for manipulation
        data.readcount = data.frame(
          design <- cp$readcount[,"design"],
          reads <- cp$readcount[,i])
        colnames(data.readcount) <- c("design","reads")
        
        if(logscale==TRUE)
        {
          #data.readcount = log2(data.readcount)
          meancount = log2(meancount)
          mediancount = log2(mediancount)  
          # remove NAs ? infinites and set them to 0
          data.readcount[,"reads"] = sapply(cp$readcount[,i], function(x) 
          {
            if(is.finite(log2(as.numeric(x[1])))) {return(log2(as.numeric(x[1])))}
                                            else {return(as.numeric(0))}
          })
          #data.readcount <- as.data.frame(data.readcount)
        }
        
        
        
        # make plot TEXTS changeable
        # make plot working for genes AND designs
        if(type=="distribution") {
          #plot without LIMIT
          xlim=max(data.readcount[,2])
          
          
          
          ## GGPLOT2 TEST
          #print(ggplot(data.readcount, aes(x=reads)) + geom_histogram(binwidth=0.5))
          if(identical(dataframe,FALSE))
          {
            histinfo = hist(data.readcount[,2],breaks=breaks,xlim=c(0,xlim), col=col, xlab=xlab, ylab=ylab, main=colnames(cp$readcount)[i], border=FALSE)  
          }
          
          
          #print(histinfo)
          if(statistics)
          {
          
            if(identical(dataframe,FALSE))
            {
              legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","orange"))
              
              lines(x=rep(meancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="red", lwd=2, lty=2)
              lines(x=rep(mediancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="orange", lwd=2, lty=2)
              
              # add density function as a fit
              par(new=TRUE)
              hist(data.readcount[,2],breaks=breaks,xlim=c(0,xlim), col=rgb(1, 1, 1, 0), prob=TRUE, axes=F, xlab="", ylab="", main="", border=FALSE)
              lines(density(data.readcount[,2]), col="navy", lwd=3)
            }
            
            
          }
          #END TYPE
        } else if(type=="whisker")
          
        {
         if(identical(dataframe,FALSE))
         {
           boxplot(data.readcount[,2],
                   col=col,
                   pch=16,
                   main=colnames(cp$readcount)[i],
                   horizontal=TRUE,
                   xlab=xlab,
                   #range=IQR(dataset[,fullmatchcolumn]),
                   las=2)
           
           legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","red","orange"))
           
         } 
           
        }# end type whisker
        
        #### DATA FRAME TO RETURN?
        if(identical(dataframe,TRUE))
        {
          if(i==2)
          {
            if(type=="whisker")
            {
              data.return = list(list("plot" = data.readcount, "mean" = meancount, "median" = mediancount))
              attr(data.return, which="name") <- colnames(cp$readcount)[i]
            }else
            {
              data.return = list(list("plot" = hist(data.readcount$reads, plot=FALSE, breaks="FD"), "mean" = meancount, "median" = mediancount))
              attr(data.return, which="name") <- colnames(cp$readcount)[i]
            }
            
          } else
          {
            if(type=="whisker"){
              attr.old <- attr(data.return, which="name")
              data.return <- c(data.return, list(list("plot" = data.readcount, "mean" = meancount, "median" = mediancount)))
              attr(data.return, which="name") <- c(attr.old, colnames(cp$readcount)[i])
            }else {
              attr.old <- attr(data.return, which="name")
              data.return <- c(data.return, list(list("plot" = hist(data.readcount$reads, plot=FALSE, breaks="FD"), "mean" = meancount, "median" = mediancount)))
              attr(data.return, which="name") <- c(attr.old, colnames(cp$readcount)[i])
            }
            
          }
          
          
        }
      }
      
 ##############
    }
    else
    {
      # a gene is passed to the function, so we plot the sgRNA read count instead
      # get gene names
      gene.names = cp$readcount[,"gene"]
      
      # number of genes
      numberdata = nrow(cp$readcount[cp$readcount[,"gene"] %in% plotgene,])
      if(is.null(numberdata) | numberdata <1)
      {
        plotgene = gene.names
        numberdata = nrow(cp$readcount[cp$readcount[,"gene"] %in% plotgene,])
      }
      if (breaks=="" || is.null(breaks))
      { breaks = numberdata/2 }
      
      
      # Walk through each dataset!
      for(i in 2:(length(cp$dataset.names)+1))
      {
        # # compute general statistics
        meancount=as.integer(mean(cp$readcount[cp$readcount[,"gene"] %in% plotgene,i], na.rm = TRUE))
        mediancount= as.integer(median(cp$readcount[cp$readcount[,"gene"] %in% plotgene,i], na.rm = TRUE))
        maxcount=max(cp$readcount[cp$readcount[,"gene"] %in% plotgene,i], na.rm = TRUE)
        
        # copy of read count for manipulation
        data.readcount = data.frame(
          design <- cp$readcount[cp$readcount[,"gene"] %in% plotgene,"design"],
          reads <- cp$readcount[cp$readcount[,"gene"] %in% plotgene,i])
        colnames(data.readcount) <- c("design","reads")
        
        if(logscale==TRUE)
        {
          #data.readcount = log2(data.readcount)
          
          # remove NAs ? infinites and set them to 0
          data.readcount[,"reads"] = sapply(cp$readcount[cp$readcount[,"gene"] %in% plotgene,i], function(x) 
          {
            if(is.finite(log2(as.numeric(x[1])))) {return(log2(as.numeric(x[1])))}
            else {return(as.numeric(0))}
          })
          #data.readcount <- as.data.frame(data.readcount)
        }
        
        if(logscale==TRUE)
        {
          meancount = log2(meancount)
          mediancount = log2(mediancount)  
        }
        
        # make plot TEXTS changeable
        # make plot working for genes AND designs
        if(type=="distribution") {
          #plot without LIMIT
          xlim=max(data.readcount[,2])
          
          ## GGPLOT2 TEST
          #print(ggplot(data.readcount, aes(x=reads)) + geom_histogram(binwidth=0.5))
          if(identical(dataframe,FALSE))
          {
            histinfo = hist(data.readcount[,2],breaks=breaks,xlim=c(0,xlim), col=col, xlab=xlab, ylab=ylab, main=colnames(cp$readcount)[i], border=FALSE)
          }
          
          #print(histinfo)
          if(identical(statistics,TRUE) && identical(dataframe,TRUE))
          {
            legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","orange"))
            
            lines(x=rep(meancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="red", lwd=2, lty=2)
            lines(x=rep(mediancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="orange", lwd=2, lty=2)
            
            # add density function as a fit
            par(new=TRUE)
            hist(data.readcount[,2],breaks=breaks,xlim=c(0,xlim), col=rgb(1, 1, 1, 0), prob=TRUE, axes=F, xlab="", ylab="", main="", border=FALSE)
            lines(density(data.readcount[,2]), col="navy", lwd=3)
            
          }
          #END TYPE
        } else if(type=="whisker")
          
        {
          if(identical(dataframe,FALSE))
          {
            boxplot(data.readcount[,2],
                    col=col,
                    pch=16,
                    main=colnames(cp$readcount)[i],
                    horizontal=TRUE,
                    xlab=xlab,
                    #range=IQR(dataset[,fullmatchcolumn]),
                    las=2)
            
            legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","red","orange"))
            
          }
          
        }# end type whisker
      
        #### DATA FRAME TO RETURN?
        #### DATA FRAME TO RETURN?
        if(identical(dataframe,TRUE))
        {
          if(i==2)
          {
            data.return = list(list("plot" = data.readcount, "mean" = meancount, "median" = mediancount))
            attr(data.return, which="name") <- colnames(cp$readcount)[i]
          } else
          {
            attr.old <- attr(data.return, which="name")
            data.return <- c(data.return, list(list("plot" = data.readcount, "mean" = meancount, "median" = mediancount)))
            attr(data.return, which="name") <- c(attr.old, colnames(cp$readcount)[i])
          }
        }
      } # end of FOR
      
      
#       # compute statistics
#       meancount=as.integer(mean(data.readcount[,fullmatchcolumn]))
#       mediancount= as.integer(median(dataset2[,fullmatchcolumn]))
#       
#       # make log2
#       if(logscale==TRUE)
#       {
#         dataset2[,fullmatchcolumn] = log2(dataset2[,fullmatchcolumn])
#         
#         # remove NAs ? infinites and set them to 0
#         dataset2[,fullmatchcolumn] = apply(dataset2, 1, function(x) if(is.finite(as.numeric(x[fullmatchcolumn]))) {return(as.numeric(x[fullmatchcolumn]))}
#                                            else {return(as.numeric(0))})
#       }
#       
#       maxcount=max(dataset2[,fullmatchcolumn])
#       
#       xlim=max(dataset2[,fullmatchcolumn])
#       
#       if(type=="distribution")
#       {
#         #plot without LIMIT
#         histinfo = hist(dataset2[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=col, xlab=xlab, ylab=ylab, main=title, border=FALSE)
#         
#         if(statistics)
#         {
#           legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","orange"))
#           
#           if(logscale==TRUE)
#           {
#             meancount = log2(meancount)
#             mediancount = log2(mediancount)  
#           }
#           
#           lines(x=rep(meancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="red", lwd=2, lty=2)
#           lines(x=rep(mediancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="orange", lwd=2, lty=2)
#           
#           # add density function as a fit
#           par(new=TRUE)
#           hist(dataset2[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=rgb(1, 1, 1, 0),prob=TRUE, axes=F, xlab="", ylab="", main="", border=FALSE)
#           lines(density(dataset2[,fullmatchcolumn]), col="navy", lwd=3)
#           
#         }
#       } # end type
   }
    
  }
  else
  {
    stop("Please load your data with load.file() first.")
  }
  if(identical(dataframe,TRUE))
  {
    # Return data
    return(data.return)
  }
  #if(!is.null(plotgene))
  #{
#     gene.names = sub(extractpattern,"\\1",dataset[,namecolumn],perl=TRUE)
#     dataset$genes = as.character(gene.names)
#     # number of genes
#     numberdata = nrow(dataset[dataset$genes %in% plotgene,])
#     if(is.null(numberdata) | numberdata <1)
#     {
#       plotgene = gene.names
#       numberdata = nrow(dataset[dataset$genes %in% plotgene,])
#     }
#     if (breaks=="" || is.null(breaks))
#     { breaks = numberdata/2 }
#     
#     dataset2 = dataset[dataset$genes %in% plotgene,]
# 
#     
#     
#     # compute statistics
#     meancount=as.integer(mean(dataset2[,fullmatchcolumn]))
#     mediancount= as.integer(median(dataset2[,fullmatchcolumn]))
#     
#     # make log2
#     if(logscale==TRUE)
#     {
#       dataset2[,fullmatchcolumn] = log2(dataset2[,fullmatchcolumn])
#       
#       # remove NAs ? infinites and set them to 0
#       dataset2[,fullmatchcolumn] = apply(dataset2, 1, function(x) if(is.finite(as.numeric(x[fullmatchcolumn]))) {return(as.numeric(x[fullmatchcolumn]))}
#                                                                               else {return(as.numeric(0))})
#     }
#     
#     maxcount=max(dataset2[,fullmatchcolumn])
#     
#     xlim=max(dataset2[,fullmatchcolumn])
#     
    # make plot TEXTS changeable
    # make plot working for genes AND designs
#     if(type=="distribution")
#     {
#     #plot without LIMIT
#     histinfo = hist(dataset2[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=col, xlab=xlab, ylab=ylab, main=title, border=FALSE)
#     
#     if(statistics)
#     {
#       legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","orange"))
#       
#       if(logscale==TRUE)
#       {
#         meancount = log2(meancount)
#         mediancount = log2(mediancount)  
#       }
#       
#       lines(x=rep(meancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="red", lwd=2, lty=2)
#       lines(x=rep(mediancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="orange", lwd=2, lty=2)
#       
#       # add density function as a fit
#       par(new=TRUE)
#       hist(dataset2[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=rgb(1, 1, 1, 0),prob=TRUE, axes=F, xlab="", ylab="", main="", border=FALSE)
#       lines(density(dataset2[,fullmatchcolumn]), col="navy", lwd=3)
#       
#       }
#     } # end type

  #}
  #else # some special sgRNAs will be plotted
  #{
    
#     # number of genes
#     numberdata=nrow(dataset)
#     if (breaks=="")
#     {breaks = numberdata/2}
#     
#     # compute statistics
#     meancount=as.integer(mean(dataset[,fullmatchcolumn]))
#     mediancount= as.integer(median(dataset[,fullmatchcolumn]))
#     maxcount=max(dataset[,fullmatchcolumn])
#     
#     if(logscale==TRUE)
#     {
#       dataset[,fullmatchcolumn] = log2(dataset[,fullmatchcolumn])
#       
#       # remove NAs ? infinites and set them to 0
#       dataset[,fullmatchcolumn] = apply(dataset, 1, function(x) if(is.finite(as.numeric(x[fullmatchcolumn]))) {return(as.numeric(x[fullmatchcolumn]))}
#                                          else {return(as.numeric(0))})
#     }
#     
#     # make plot TEXTS changeable
#     # make plot working for genes AND designs
#     if(type=="distribution") {
#     #plot without LIMIT
#     xlim=max(dataset[,fullmatchcolumn])
#       
#     histinfo = hist(dataset[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=col, xlab=xlab, ylab=ylab, main=title, border=FALSE)
#     #print(histinfo)
#     if(statistics)
#     {
#       legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","orange"))
#       
#       if(logscale==TRUE)
#       {
#         meancount = log2(meancount)
#         mediancount = log2(mediancount)  
#       }
#       
#       lines(x=rep(meancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="red", lwd=2, lty=2)
#       lines(x=rep(mediancount, times=max(histinfo$counts)+1), y=c(0:max(histinfo$counts)), col="orange", lwd=2, lty=2)
#       
#       
#       # add density function as a fit
#       par(new=TRUE)
#       hist(dataset[,fullmatchcolumn],breaks=breaks,xlim=c(0,xlim), col=rgb(1, 1, 1, 0), prob=TRUE, axes=F, xlab="", ylab="", main="", border=FALSE)
#       lines(density(dataset[,fullmatchcolumn]), col="navy", lwd=3)
#       
#     }
#     #END TYPE
#     } else if(type=="whisker")
#       
#     {
#       boxplot(dataset[,fullmatchcolumn],
#                   col=col,
#                   pch=16,
#                   main=title,
#                   horizontal=TRUE,
#                   xlab=xlab,
#               #range=IQR(dataset[,fullmatchcolumn]),
#                   las=2)
#     
#       legend("topright",c(paste("MEAN:",meancount),paste("MEDIAN:",mediancount)),cex=0.8, bty="n", text.col=c("red","red","orange"))
#       
#     }# end type whisker
#     
#   }
    #}
  
}

