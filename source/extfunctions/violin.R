violin <- function(df, target, nontarget, genes, type = "log2foldchange", range=1.5,h=NULL,ylim=NULL, horizontal=FALSE, 
                             border="black", lty=1, lwd=1, rectCol=rgb(0,0,0,150, maxColorValue=255), colMed="white", pchMed=16, add=FALSE, wex=1, 
                             drawRect=TRUE, smdensity=TRUE)
{
  #type can be any column of raw.genes when called with offtarget:
  # 
  # "untreated"          "untreated.sd"       "treated"            "treated.sd"         "genes"              "log2foldchange"     "z.score.untreated" 
  # "z.score.treated"    "z.score.foldchange" "z.ratio"            "sequence"           "gene"               "Spec.Score"         "Anno.Score"         "Eff.Score"         
  #"CDS_score"          "exon_score"         "seed_GC"            "doench_score"       "xu_score"           "target"             "offtargets"    
  
  #requireNamespace("sm")
  # check for length
  #if(length(col)==1) col <- rep(col,n)
  
  # process multiple datas
  datas <- as.list(genes)
  
  col <- rep("grey", times = length(genes))
  
  # add target / nontarget control
  if(!is.null(target))
  {
    datas <- c(datas,as.list(target))
    col <- c(col, rep("red", times = length(target)) )
  }
  if(!is.null(nontarget))
  {
    datas <- c(datas,as.list(nontarget))
    col <- c(col, rep("blue", times = length(nontarget)) )
  }
  
  
  
  
  # get total length of genes to plot
  n <- length(datas)
  #print(n)
  at <- 1:n
  
  # pass 1
  #
  # - calculate base range
  # - estimate density
  #
  
  # setup parameters for density estimation
  upper  <- vector(mode="numeric",length=n)
  lower  <- vector(mode="numeric",length=n) 
  q1     <- vector(mode="numeric",length=n)
  q3     <- vector(mode="numeric",length=n)
  med    <- vector(mode="numeric",length=n)
  #data.scatter    <- vector(mode="numeric",length=n)
  base   <- vector(mode="list",length=n)
  height <- vector(mode="list",length=n)
  baserange <- c(Inf,-Inf)
  
  # global args for sm.density function-call   
  args <- list(display="none")
  
  if (!(is.null(h)))
    args <- c(args, h=h)
  
  
  for(i in 1:n) {
    
    #get gene identifier
    gene <- datas[[i]]
    
    # get data for this gene out of raw.genes DF
    data <- df[df$genes == gene,type]
    
    # calculate plot parameters
    #   1- and 3-quantile, median, IQR, upper- and lower-adjacent
    
    data.min <- min(data, na.rm = TRUE)
    data.max <- max(data, na.rm = TRUE)
    
    q1[i]<-quantile(data,0.25, na.rm=TRUE)
    q3[i]<-quantile(data,0.75, na.rm=TRUE)
    med[i]<-median(data, na.rm = TRUE)
    iqd <- q3[i]-q1[i]
    upper[i] <- min( q3[i] + range*iqd, data.max , na.rm = TRUE)
    lower[i] <- max( q1[i] - range*iqd, data.min , na.rm = TRUE)
    
    
    #   strategy:
    #       xmin = min(lower, data.min))
    #       ymax = max(upper, data.max))
    #
    
    est.xlim <- c( min(lower[i], data.min), max(upper[i], data.max) ) 
    
    # estimate density curve
    
    #smout <- do.call("sm.density", c( list(data, xlim=est.xlim), args ) )
    if(smdensity==TRUE){
      #library(sm)
      smout <- do.call(sm::sm.density, c(list(data, xlim = est.xlim), 
                                         args))
    } else {
      smout <- do.call("density",list(data))
      smout[["estimate"]] <- smout[["y"]]
      smout[["eval.points"]] <- smout[["x"]]
    }
    
    # calculate stretch factor
    #
    #  the plots density heights is defined in range 0.0 ... 0.5 
    #  we scale maximum estimated point to 0.4 per data
    #
    
    hscale <- 0.4/max(smout$estimate) * wex
    
    
    # add density curve x,y pair to lists
    
    base[[i]]   <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    
    
    # calculate min,max base ranges
    
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1],t[1])
    baserange[2] <- max(baserange[2],t[2])
    
  }
  
  # pass 2
  #
  # - plot graphics
  
  # setup parameters for plot
  
  if(!add){
    xlim <- if(n==1) 
      at + c(-.5, .5)
    else 
      range(at) + min(diff(at))/2 * c(-1,1)
    
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  
  # make X-Axis sample names
  
  makelab <- as.vector(datas)
  
  
  boxwidth <- 0.05 * wex
  
  
  # setup plot
  
  if(!add)
    plot.new()
  if(!horizontal) {
    if(!add){
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1,at = at, labels=makelab , las=2)
    }  
    
    box()
    
    for(i in 1:n) {
      
      #get gene identifier
      gene <-datas[[i]]
      
      # get data for this gene out of raw.genes DF
      data <- df[df$genes == gene,type]
      
      # plot left/right density curve
      
      polygon( c(at[i]-height[[i]], rev(at[i]+height[[i]])), 
               c(base[[i]], rev(base[[i]])),
               col = col[i], border=border, lty=lty, lwd=lwd, yaxt="l")
      
      # col = col[i], border=border, lty=lty, lwd=lwd)
      
      
      if(drawRect){
        # plot IQR
        
        points( jitter(rep(at[i], length(data))
                       #,factor=2
                       , amount = 0.1
        )
        , data, pch=pchMed, col="lightgrey", cex=0.7, yaxt="l" )
        
        lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)
        
        lines(0:(n+1), y = rep(0, length(0:(n+1))), lwd=1, lty=2)
        # plot 50% KI box
        
        rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)
        
        # plot median point
        #str(datas[[i]])
        points( at[i], med[i], pch=pchMed, col=colMed )
        
        
        #stripchart(rep(at[i], length(datas[[i]])), datas[[i]],
        #             vertical = TRUE, method = "jitter", 
        #            pch = 16, col = "maroon", bg = "bisque", 
        #           add = TRUE) 
      }
    }
    
  }
  else {
    if(!add){
      plot.window(xlim = ylim, ylim = xlim, yaxt="s")
      axis(1)
      axis(2,at = at, labels=makelab )
    }
    
    box()
    for(i in 1:n) {
      
      # plot left/right density curve
      
      polygon( c(base[[i]], rev(base[[i]])),
               c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               col = col[i], border=border, lty=lty, lwd=lwd, yaxt="s")
      
      
      if(drawRect){
        # plot IQR
        lines( c(lower[i], upper[i]), at[c(i,i)] ,lwd=lwd, lty=lty, yaxt="s")
        
        # plot 50% KI box
        
        rect( q1[i], at[i]-boxwidth/2, q3[i], at[i]+boxwidth/2,  col=rectCol)
        
        # plot median point
        points( med[i], at[i], pch=pchMed, col=colMed, yaxt="s")
      }
    }
  }
  
  invisible (list( upper=upper, lower=lower, median=med, q1=q1, q3=q3))
  
  # Add title
  title(paste(type, "sgRNA Distribution\n" ,sep = " "),
        ylab = type) 
  
  # Add Sample Names
  # already done above automatically
}