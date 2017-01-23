unmapped.genes = function(extractpattern=expression("^(.+?)(_.+)"), write=TRUE, plotgenes=NULL,type=NULL, allplot="ratio",arrange="horizontal", pdfratio=4, dataframe=FALSE) 
{
  # old arguments
  # data, namecolumn=1, fullmatchcolumn=2,
  # input
  #   data = dataset sgRNA with readcount
  #   namecolumn
  #   fullmatchcolumn
  #   genes = NULL -> will list genes with unmapped sgRNAs
  #   genes = vector with gene names -> will list unmapped sgRNAs by name for the provided gene names

  
if(exists("cp", mode="environment") && exists("readcount", envir=cp) && exists("aggregated.readcount", envir=cp) )
{
    
  if(is.null(plotgenes) && is.null(type))
  {
    
    # number of datasets
    number.files <- length(cp$dataset.names)
    
    
    # create huge data.frame for whole analysis of sgRNA dropouts
    # in this df for each dataset (column) we will add a 0 if sgRNA is not missing and 1 if it is missing
    # later this can be used for plotting
    cp$unmapped.all <- cp$readcount[,1:(number.files+1)]
    
    # add old design names
    
    cp$unmapped.all$old.design <- cp$readcount$design
    # Now we add all necessary information to it or transform it
    # transformation is done by setting all to ZERO, wehre there is read count, and to 1 where there is one!
    
    for(i in 2:(number.files+1) )
    {
      
      cp$unmapped.all[cp$unmapped.all[,colnames(cp$readcount)[i]]==0,colnames(cp$readcount)[i]] <- 0
      cp$unmapped.all[cp$unmapped.all[,colnames(cp$readcount)[i]]>0,colnames(cp$readcount)[i]] <- 1
      
#       cp$unmapped.all[,colnames(cp$readcount)[i]] <- sapply(cp$readcount[,i], function(x){
#         
#         # return 1 if read count is 0 and the other way round
#         if(as.numeric(x[1]) == 0)
#         {data.return <- 1}
#         else
#         {data.return <- 0}
#         return(data.return)
#         
#       })
      
      # Calculate unmapped sgRNA / Gene infromation
      df.unmapped.gene = tapply(cp$readcount[,i], sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE), function(z)
        {
          return(length(z[z==0]))
        })
      
      # now we have a df with all gene names and number of missing sgRNAs
      # so we also go and pick the missing sgRNAs for each single gene!
      #& cp$readcount[,"gene"] %in% as.character(attr(df.unmapped.gene,"name")[1])
      
      df.unmapped.sgRNA <- data.frame(
        sgRNA = cp$readcount[cp$readcount[,i] == 0 ,"design"],
        gene = cp$readcount[cp$readcount[,i] == 0 ,"gene"],
        stringsAsFactors = FALSE
      )
      
      # if nothing is missing, set 0
      if(nrow(df.unmapped.sgRNA) < 1)
      {
        df.unmapped.sgRNA <- NULL
      }
      
      #df.unmapped.gene$gene <- unlist(attr(df.unmapped.gene,"name") )
      #print(str(df.unmapped.sgRNA))
      #print(str(df.unmapped.gene))
      
      # Prepare for output
      df.unmapped <- list("Genes" <- df.unmapped.gene, "sgRNA" <- df.unmapped.sgRNA)
      
      if(i == 2)
      {
        # add to data frame that give statistic how many where not mapped!
        cp$unmapped <- data.frame(
          "dataset" <- as.character(colnames(cp$readcount)[i]),
          "gene" <- as.numeric(nrow(cp$aggregated.readcount[cp$aggregated.readcount[,i] == 0,])),
          "sgRNA" <- as.numeric(nrow(cp$readcount[cp$readcount[,i] == 0,])),
          stringsAsFactors=FALSE
        )
        colnames(cp$unmapped) <- c("dataset","gene","sgRNA")
        rownames(cp$unmapped) <- colnames(cp$readcount)[i]
        
        df.unmapped.output <- list(df.unmapped)
        
        # plot for number of missing sgRNAs of all datasets
        
        plot.unmapped.genes <- as.data.frame(
          gene <- attr(df.unmapped.gene,"name"),
          stringsAsFactors=FALSE
        )
        
        plot.unmapped.genes[,colnames(cp$readcount)[i]] <- df.unmapped.gene
        
        
        # Writing to file
        if(identical(write,TRUE) && identical(dataframe,FALSE))
        {
          # Open workbook to write
          
            wb1 <- openxlsx::createWorkbook()
          ## Output as XLSX file
          openxlsx::addWorksheet(wb = wb1, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
          openxlsx::writeDataTable(wb = wb1, sheet = colnames(cp$readcount)[i],  x = as.data.frame(df.unmapped.gene),  tableStyle = "TableStyleLight2")
          # WRITE XLSX
          openxlsx::saveWorkbook(wb1, paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_GENE.xlsx", sep="_"), sep="/"), overwrite = TRUE)
          
          if(!is.null(df.unmapped.sgRNA))
          {
             wb1 <- openxlsx::createWorkbook()
            ## Output as XLSX file
            openxlsx::addWorksheet(wb = wb1, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
            openxlsx::writeDataTable(wb = wb1, sheet = colnames(cp$readcount)[i],  x = df.unmapped.sgRNA,  tableStyle = "TableStyleLight2")
            # WRITE XLSX
            openxlsx::saveWorkbook(wb1, paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_sgRNA.xlsx", sep="_"), sep="/"), overwrite = TRUE)

          }
          
        }
        
      }
      else
      {
        # add statistics to unmapped DF
        new.add <- data.frame(
          dataset = as.character(colnames(cp$readcount)[i]),
          gene = as.numeric(nrow(cp$aggregated.readcount[cp$aggregated.readcount[,i] == 0,])),
          sgRNA = as.numeric(nrow(cp$readcount[cp$readcount[,i] == 0,])),
          stringsAsFactors=FALSE
        )
        colnames(new.add) <- c("dataset","gene","sgRNA")
        cp$unmapped <- rbind.data.frame(cp$unmapped,new.add)
        rownames(cp$unmapped) <- colnames(cp$readcount)[2:i]
        
        colnames(cp$unmapped) <- c("dataset","gene","sgRNA")

        
        df.unmapped.output <- c(df.unmapped.output, list(df.unmapped))
        
        # for plotting
        plot.unmapped.genes[,colnames(cp$readcount)[i]] <- df.unmapped.gene
        
        # Writing to file
        if(identical(write,TRUE) && identical(dataframe,FALSE))
        {
          # Open workbook to write
          if(file.exists(paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_GENE.xlsx", sep="_"), sep="/")) && file.exists(paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_sgRNA.xlsx", sep="_"), sep="/")))
          {
            wb1 <- openxlsx::loadWorkbook(file = paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_GENE.xlsx", sep="_"), sep="/"))
            wb2 <- openxlsx::loadWorkbook(file = paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_sgRNA.xlsx", sep="_"), sep="/"))
          } else {
            wb1 <- openxlsx::createWorkbook()
            wb2 <- openxlsx::createWorkbook()}
          
          ## Output as XLSX file
          openxlsx::addWorksheet(wb = wb1, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
          openxlsx::addWorksheet(wb = wb2, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
          
          openxlsx::writeDataTable(wb = wb1, sheet = colnames(cp$readcount)[i],  x = as.data.frame(df.unmapped.gene),  tableStyle = "TableStyleLight2")
          openxlsx::writeDataTable(wb = wb2, sheet = colnames(cp$readcount)[i],  x = df.unmapped.sgRNA,  tableStyle = "TableStyleLight2")
          
          # WRITE XLSX
          openxlsx::saveWorkbook(wb1, paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_GENE.xlsx", sep="_"), sep="/"), overwrite = TRUE)
          openxlsx::saveWorkbook(wb2, paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "UNMAPPED_sgRNA.xlsx", sep="_"), sep="/"), overwrite = TRUE)
          
        }
      }

      #df.unmapped.sgRNA = data.frame(
      ##    sgRNA = data[ data[,fullmatchcolumn] == 0 & sub(extractpattern,"\\1",data[, namecolumn],perl=TRUE) %in% genes, namecolumn],
      #    stringsAsFactors=FALSE)
      #df.unmapped.sgRNA$gene = sub(extractpattern,"\\1",df.unmapped.sgRNA[, "sgRNA"],perl=TRUE) 
        
    }# end FOR LOOP

    
    
   # plot data
    # First: Missing sgRNAs and Genes total from cp$unmapped!
    # Barplot
    if(identical(dataframe,FALSE))
    {
      g1 <- ggplot2::ggplot(cp$unmapped, ggplot2::aes(x=dataset, y=gene), fill=dataset) + ggplot2::geom_bar( width=.8, stat="identity") + 
        ggplot2::guides(fill=FALSE) +
        ggplot2::scale_colour_brewer(palette="Spectral") +
        ggplot2::ylab("Missing Genes") +
        ggplot2::ggtitle("Missing Genes") + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=15, vjust=0.5) )
      
      
      
      g2 <- ggplot2::ggplot(cp$unmapped, ggplot2::aes(x=dataset, y=sgRNA), fill=dataset) + ggplot2::geom_bar( width=.8, stat="identity") + 
        ggplot2::guides(fill=FALSE) +
        ggplot2::scale_colour_brewer(palette="Spectral") +
        ggplot2::ylab("Missing sgRNA") +
        ggplot2::ggtitle("Missing sgRNAs") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=15, vjust=0.5) )
      
      # arrange as grid
      if(arrange=="horizontal") {gridExtra::grid.arrange(g1, g2, ncol=2, nrow =1)} else {gridExtra::grid.arrange(g1, g2, ncol=1, nrow =2)}
    }
    
  
    # add gene info to large dataset
    cp$unmapped.all[,"gene"] <- cp$readcount[,"gene"]
    
    return(cp$unmapped)
   
  } # plot gene information
  else if(is.null(plotgenes) && type=="all")
  {
    if(!exists("unmapped.all", envir=cp))
    {
         df <- unmapped.genes()
    }
    # annotate data with GO term
    # Then count how many sgRNAs are missing
    
    # plot heatmap like using a clustering
    rownames(cp$unmapped.all) <- cp$unmapped.all[,"design"]
    df.all <- cp$unmapped.all
    df.all$design = NULL
    df.all <- aggregate.data.frame(df.all, by=list(df.all$gene), function(x){
      return(length(x[x == 0]))
    })
    
    df.all$gene = NULL
    rownames(df.all) <- df.all$Group.1
    
    # title of plot
    title.plot = "Number of missing sgRNAs"
    
    # if ratio of missing sgRNA to all sgRNA of a gene is asked
    if(!is.null(allplot) && allplot=="ratio")
    {
      sgRNA.counts.total = tapply(cp$unmapped.all[,"design"],cp$unmapped.all[,"gene"],function(x) length(x))
      title.plot = "% of sgRNAs missing"
      # walk through df and replace number with the ratio!
      
      
      for(i in 2:(length(cp$dataset.names)+1) )
      {
        # now we take gene name and look what kind of total sgRNAs there is for that
        df.all[,i] <- apply(df.all,1,function(u){
          
          ratio <- (as.numeric(u[i]) / sgRNA.counts.total[names(sgRNA.counts.total) == as.character(u["Group.1"])])*100 
          
          return(floor(ratio))
          
        })
      }
      
    }
    if("old.design" %in% colnames(df.all))
    {
      df.all[,"old.design"] <- NULL
    }
    
    df.all$Group.1 = NULL
    
    df.all <- as.matrix(df.all)
    df.all <- reshape2::melt(df.all)
    
    #prepare ggplot
    p <- ggplot2::ggplot(df.all, ggplot2::aes(y=Var1,x=Var2)) +  ggplot2::scale_fill_gradient(low="white", high="darkblue", name=title.plot) + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_tile(ggplot2::aes(fill=value)) +
      ggplot2::ggtitle("Missing sgRNAs") + 
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=6, vjust=0.5,lineheight = 1),
            axis.title=ggplot2::element_text(size=15, vjust=0.5), legend.text=ggplot2::element_text(size=10, vjust=0.5,lineheight = 2))
    
    p + ggplot2::coord_fixed(ratio=10)
    
    # Writing to file
    if(identical(write,TRUE) && dataframe==FALSE)
    {
      pdf(paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED-HEATMAP", allplot, sep="-" ),".pdf",sep=""), sep="_"), sep="/"), width=14, height=14*pdfratio)
      print(p)
      dev.off()
      # Open workbook to write
      wb1 <- openxlsx::createWorkbook()
      ## Output as XLSX file
      openxlsx::addWorksheet(wb = wb1, sheetName = "data", gridLines = FALSE)
      
      openxlsx::writeDataTable(wb = wb1, sheet = "data",  x = as.data.frame(df.all),  tableStyle = "TableStyleLight2")
      # WRITE XLSX
      openxlsx::saveWorkbook(wb1, paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED-HEATMAP", allplot, sep="-" ),".xlsx",sep=""), sep="_"), sep="/"), overwrite = TRUE)
      
      
      #xlsx::write.xlsx(as.data.frame(df.all), file=paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED-HEATMAP", allplot, sep="-" ),".xls",sep=""), sep="_"), sep="/"), sheetName="Data", append=FALSE ,row.names=FALSE)
    }
    
    # Print Plot
    if(identical(dataframe,FALSE))
    {
      print(p)
    }else {
      # Prepare data frame
      
      df.return <- df.all
      df.return$Var1 <- df.all$Var2
      df.return$Var2 <- df.all$Var1
      df.return$value <- round(df.return$value,digits = 2)
      #convert factors to numbers for highcharts
      ## VAR1
      #View(df.threshold)
      df.levels <- data.frame(levels = unique(df.return$Var1), number = seq.int(from=1, to=length(levels(df.return$Var1)), by=1), stringsAsFactors=FALSE)
      df.levels <- unique(df.levels)
      df.return$x<- sapply(df.return$Var1, function(x){
        return(as.numeric(df.levels[df.levels$levels == as.character(x),"number"]))
      })
      
      #Var2
      df.levels <- data.frame(levels = unique(df.return$Var2), number = seq.int(from=1, to=length(levels(df.return$Var2)), by=1), stringsAsFactors=FALSE)
      df.levels <- unique(df.levels)
      df.return$y<- sapply(df.return$Var2, function(x){
        return(as.numeric(df.levels[df.levels$levels == as.character(x),"number"]))
      })
      
      df.return$Var1 <- as.character(levels(df.return$Var1))
      df.return$Var2 <- as.character(levels(df.return$Var2))
      
      return(df.return)
    }

    # p + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")
  }
  else if(!is.null(plotgenes))
  {
    ## make plot for missing sgRNAs of single gene over all samples
    if(type == "single")
    {
      # plotting only single gene, but all sgRNAs for it!
      # we plot % ofmissing sgRNAs vs. the dataset for that particular gene
      
      # get number of datasets
      number.files <- length(cp$dataset.names)
      
      if(!exists("aggregated.readcount", envir=cp))
      {
        aggregatetogenes(agg.function=sum,extractpattern = extractpattern)
      }
      
      # add data for it
      for(i in 2:(number.files+1))
      {
        
        if(i==2)
        {
          # make data.frame for plotting as line plot
          plot.data <- data.frame(
            gene <- plotgenes,
            dataset <- colnames(cp$readcount)[i],
            stringsAsFactors = FALSE
          )
          colnames(plot.data) <- c("gene","dataset")
          rownames(plot.data) <- plotgenes
          # add missing sgRNA number for that gene
          plot.data[,"missing"] <- apply(plot.data,1, function(x) {
            
            counter <- cp$readcount[cp$readcount[,"gene"] == as.character(x[1]),i]
            ratio <- (table(counter == 0)["TRUE"][[1]] / length(counter))*100
            return(ceiling(ratio))
            
          })
          colnames(plot.data) <- c("gene","dataset","missing")
          
        }
        else
        {
          # also add other stuff where readcount is 0
          # add missing sgRNA number for that gene
          for(gene in plotgenes)
          {
            
            counter <- cp$readcount[cp$readcount[,"gene"] == as.character(gene),i]
            ratio <- ceiling((table(counter == 0)["TRUE"][[1]] / length(counter))*100)
            ratio[is.na(ratio)] <- 0
            
            # add row
            df.add = data.frame(
              gene <- gene,
              dataset <- colnames(cp$readcount)[i],
              missing <- ratio,
              stringsAsFactors = FALSE
            )
            colnames(df.add) <- c("gene","dataset","missing")
            
            
            plot.data <- rbind.data.frame(plot.data, df.add)
            
          }
          
        }
        
      } # 
      #rownames(plot.data) <- plot.data$dataset
      #View(plot.data)
      # Remove NA and set to 0
      plot.data$missing <- sapply(plot.data$missing, function(x) {
        if(is.na(x)) { return(0)}
        else {return(x)}
      })
      
      # THIS DOESnT WORK, HOW CAN WE PLOT IT, THOUGH DATA IS DISTRIBUTED IN COLUMNS
      g <- ggplot2::ggplot(plot.data, ggplot2::aes(x=gene, y=missing, fill=dataset))  + ggplot2::geom_bar(colour="black",width=.8, stat="identity",position=ggplot2::position_dodge()) + ggplot2::ylab("Percentage of sgRNAs not present") +
        ggplot2::xlab("Genes") +
        ggplot2::ggtitle("Missing sgRNAs") + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size=20, face="bold", vjust=2), axis.text.x=ggplot2::element_text(size=15, vjust=0.5, angle=45), axis.text.y=ggplot2::element_text(size=15, vjust=0.5),
              axis.title=ggplot2::element_text(size=15, vjust=0.5)) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "Dataset"))
      
      
      # Writing to file
      if(identical(write,TRUE) && dataframe==FALSE)
      {
        pdf(paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED", paste(plotgenes, sep="_", collapse="_"), sep="-"),".pdf",sep=""), sep="_"), sep="/"), width=14, height=14*pdfratio)
        print(g)
        dev.off()
        
        # Open workbook to write
        wb1 <- openxlsx::createWorkbook()
        ## Output as XLSX file
        openxlsx::addWorksheet(wb = wb1, sheetName = "data", gridLines = FALSE)
        
        openxlsx::writeDataTable(wb = wb1, sheet = "data",  x = as.data.frame(plot.data),  tableStyle = "TableStyleLight2")
        # WRITE XLSX
        openxlsx::saveWorkbook(wb1, paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED", paste(plotgenes, sep="_", collapse="_"), sep="-"),".xlsx",sep=""), sep="_"), sep="/"), overwrite = TRUE)
        
        #xlsx::write.xlsx(as.data.frame(plot.data), file=paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, paste(paste("UNMAPPED-HEATMAP", type, sep="-" ),".xls",sep=""), sep="_"), sep="/"), sheetName="Data", append=FALSE ,row.names=FALSE)
      }
      
      if(identical(dataframe,FALSE))
      {
        # Print the plot
        print(g)
      } else {
        plot.data$Var1
        return(plot.data)
      }

      
    }
    
  } 
    
}

  


}