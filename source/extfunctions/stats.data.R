stats.data = function(extractpattern=expression("^(.+?)_.+"),  controls.target = NULL, controls.nontarget = NULL, type="stats", write=TRUE, output=TRUE)
{
    # type can be:
  #   mapping | for mapping information
  #   stats | for general statistics of the dataset
  #   dataset | for indepth dataset statistics for each gene
  #   controls | for indepth statistics per control (target and non-targeting)
 
# check for presence of cp$readcount
if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
{
    ## Load data, extract gene names
  gene.names = sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE)
  # }
  
  # Get number of datasets available
  number.files <- length(cp$dataset.names)
    
    
  ## General Statistics
  if(type=="stats")
  {
    

    for(i in 2:(number.files+1) )
    {
      # Calculate averages per dataset
      df.add <- data.frame(
      readcount.mean = round(mean(cp$readcount[,i])),
      readcount.median = round(median(cp$readcount[,i])),
      readcount.min = round(min(cp$readcount[,i])),
      readcount.max = round(max(cp$readcount[,i])),
      readcount.sd = round(sd(cp$readcount[,i], na.rm=TRUE)),
      unmapped.sgRNA = length(cp$readcount[cp$readcount[,i] == 0,i]),
      stringsAsFactors = FALSE)
      colnames(df.add) <- c("Mean", "Median", "Min", "Max","SD", "# sgRNA not present")
      rownames(df.add) <- colnames(cp$readcount)[i]
      
      # add to data.freame using rbind
      if(i ==2)
      {
        # construct data frame for output
        data.sgRNA <- df.add
      }
      else
      {
        # add columns
        data.sgRNA <- rbind.data.frame(data.sgRNA, df.add)
      }
      
    }
    
    ## Output as table
    data.return = list( "sgRNA" = data.sgRNA)
    
    if(identical(write,TRUE))
    {
      # Open workbook to write
      wb <- openxlsx::createWorkbook()
      ## Output as XLSX file
      openxlsx::addWorksheet(wb = wb, sheetName = "sgRNA stats", gridLines = FALSE)
      openxlsx::writeDataTable(wb = wb, sheet = "sgRNA stats",  x = data.sgRNA,  tableStyle = "TableStyleLight2")
      #xlsx::write.xlsx(data.sgRNA, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS.xls", sep="_"), sep="/"), sheetName="sgRNA stats", row.names=TRUE)
      
    }
   
    
    ## aggregated data already there!
    # get gene information
    for(i in 2:(number.files+1) )
    {
      # Calculate averages per dataset
      df.add.gene <- data.frame(
        readcount.mean = round(mean(cp$aggregated.readcount[,i])),
        readcount.median = round(median(cp$aggregated.readcount[,i])),
        readcount.min = round(min(cp$aggregated.readcount[,i])),
        readcount.max = round(max(cp$aggregated.readcount[,i])),
        readcount.sd = round(sd(cp$aggregated.readcount[,i], na.rm=TRUE)),
        unmapped.sgRNA = length(cp$aggregated.readcount[cp$aggregated.readcount[,i] == 0,i]),
        stringsAsFactors = FALSE)
      colnames(df.add.gene) <- c("Mean", "Median", "Min", "Max", "SD", "# Genes not present")
      rownames(df.add.gene) <- colnames(cp$aggregated.readcount)[i]
      
      # add to data.freame using rbind
      if(i ==2)
      {
        # construct data frame for output
        data.gene <- df.add.gene
      }
      else
      {
        # add columns
        data.gene <- rbind.data.frame(data.gene, df.add.gene)
      }
      
    }
    
    ## Output as table
    data.return <- c(data.return, list("Gene" = data.gene) )
    setNames(data.return, c("sgRNA","Gene"))
    
    if(identical(write,TRUE))
    {
      ## Output as XLSX file
      openxlsx::addWorksheet(wb = wb, sheetName = "Gene stats", gridLines = FALSE)
      openxlsx::writeDataTable(wb = wb, sheet = "Gene stats",  x = data.gene,  tableStyle = "TableStyleLight2")
      #xlsx::write.xlsx(data.gene, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS.xls", sep="_"), sep="/"), sheetName="Gene stats", row.names=TRUE, append=TRUE)
      # WRITE XLSX
      openxlsx::saveWorkbook(wb, paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS.xlsx", sep="_"), sep="/"), overwrite = TRUE)
    }
    options(bphost="localhost")
    # make output for return
    if(identical(output,TRUE))
    {
      # use knitr to make table output
      BiocParallel::bplapply(data.return, function(x){
        print(knitr::kable(x))
        
      })
      
    }
    
    
   return(data.return) 
    
#     dataset.g = aggregatetogenes(dataset, namecolumn=namecolumn, countcolumn=fullmatchcolumn, extractpattern=extractpattern, agg.function=sum)
#     
#     readcount.gene.mean = round(mean(dataset.g[,fullmatchcolumn]))
#     readcount.gene.median = round(median(dataset.g[,fullmatchcolumn]))
#     readcount.gene.min = round(min(dataset.g[,fullmatchcolumn]))
#     readcount.gene.max = round(max(dataset.g[,fullmatchcolumn]))
#     readcount.gene.sd = round(sd(dataset.g[,fullmatchcolumn], na.rm=TRUE))
#     unmapped.gene = length(dataset[dataset.g[,fullmatchcolumn] == 0,namecolumn])
#     
#     data.return$gene = c(readcount.gene.mean,readcount.gene.median,readcount.gene.sd,readcount.gene.min,readcount.gene.max, unmapped.gene)
#     
#     return(data.return)
  }
  
  ## GET DATASET STATISTICS en detail
  
  if(type=="dataset" || type=="controls")
  {
    # get number of sgRNAs per gene
    # therefore we just count the different number of gene rows
    # Get number of datasets available
    number.files <- length(cp$dataset.names)
    
    
    if(exists("aggregated.readcount", envir=cp))
    {
      sgRNA.counts = tapply(cp$readcount[,"design"],cp$readcount[,"gene"],function(x) length(x))
    }
    else
    {
      sgRNA.counts = tapply(cp$readcount[,"design"], sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE),function(x) length(x))
    }
    
    
    for(i in 2:(number.files+1) )
    {
        # Calculate GENE information for each dataset
        if(exists("aggregated.readcount", envir=cp))
        {
          df.gene = tapply(cp$readcount[,i], cp$readcount[,"gene"], function(z)
          {
            return(c(round(mean(z)),round(median(z)),round(sd(z)),round(min(z)),round(max(z))))
          })
          df.sgrna =  tapply(cp$readcount[,i], cp$readcount[,"design"], function(z)
          {
            return(c(round(mean(z)),round(median(z)),round(sd(z)),round(min(z)),round(max(z))))
          })
          
        }
        else
        {
          df.gene = tapply(cp$readcount[,i], sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE), function(z)
          {
            return(c(round(mean(z)),round(median(z)),round(sd(z)),round(min(z)),round(max(z))))
          })
          df.sgrna = tapply(cp$readcount[,i], cp$readcount[,"design"], function(z)
          {
            return(c(round(mean(z)),round(median(z)),round(sd(z)),round(min(z)),round(max(z))))
          })
        }
        
        # make data.frame for each dataset
        df.attributes = attributes(df.gene)
        df.gene = do.call(rbind.data.frame, df.gene)
        df.gene$Name = unlist(df.attributes[[2]])
        colnames(df.gene) = c("MEAN","MEDIAN","SD","MIN","MAX","Name")
        
        df.attributes.sgrna = attributes(df.sgrna)
        df.sgrna = do.call(rbind.data.frame, df.sgrna)
        df.sgrna$Name = unlist(df.attributes.sgrna[[2]])
        colnames(df.sgrna) = c("MEAN","MEDIAN","SD","MIN","MAX","Name")
        
        if(type == "dataset")
        {
          # write to file
          
          if(i==2)
          {
            if(identical(write,TRUE))
            {
              # Open workbook to write
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb = wb, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = colnames(cp$readcount)[i],  x = df.gene,  tableStyle = "TableStyleLight2")
              
              openxlsx::addWorksheet(wb = wb, sheetName = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"),  x = df.sgrna,  tableStyle = "TableStyleLight2")
              
              
            #xlsx::write.xlsx(df.gene, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_DATASET.xls", sep="_"), sep="/"), sheetName=colnames(cp$readcount)[i], append=FALSE ,row.names=FALSE)
            #xlsx::write.xlsx(df.sgrna, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_DATASET.xls", sep="_"), sep="/"), sheetName=paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), append=TRUE ,row.names=FALSE)
            }
            # make for return as list
            data.return <- list(df.gene)
          }
          else
          {
            if(identical(write,TRUE))
            {
              openxlsx::addWorksheet(wb = wb, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = colnames(cp$readcount)[i],  x = df.gene,  tableStyle = "TableStyleLight2")
              
              openxlsx::addWorksheet(wb = wb, sheetName = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"),  x = df.sgrna,  tableStyle = "TableStyleLight2")
              
              #xlsx::write.xlsx(df.gene, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_DATASET.xls", sep="_"), sep="/"), sheetName=colnames(cp$readcount)[i], append=TRUE ,row.names=FALSE)
            #xlsx::write.xlsx(df.sgrna, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_DATASET.xls", sep="_"), sep="/"), sheetName=paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), append=TRUE ,row.names=FALSE)
            }
            # add to list for return
            data.return <- c(data.return,list(df.gene))
          }
          

        }
        else if(type=="controls")
        {
          # get gene data
          df.gene = as.data.frame(df.gene[as.character(df.gene[,"Name"]) %in% c(controls.target,controls.nontarget),"Name"])
          
          colnames(df.gene) = "Name"
          df.gene[,"MEAN"] = apply(df.gene, 1, function(u) {
            value = cp$readcount[gene.names == u["Name"] , i]
            return(round(mean(value)))
          })
          df.gene[,"MEDIAN"] = apply(df.gene, 1, function(u) {
            value = cp$readcount[gene.names == u["Name"] , i]
            return(round(median(value)))
          })
          df.gene[,"SD"] = apply(df.gene, 1, function(u) {
            value = cp$readcount[gene.names == u["Name"] , i]
            return(round(sd(value)))
          })
          df.gene[,"MIN"] = apply(df.gene, 1, function(u) {
            value = cp$readcount[gene.names == u["Name"] , i]
            return(round(min(value)))
          })
          df.gene[,"MAX"] = apply(df.gene, 1, function(u) {
            value = cp$readcount[gene.names == u["Name"] , i]
            return(round(max(value)))
          })
          
          
          if(i==2)
          {
            # write to file
            if(identical(write,TRUE))
            {  
              # Open workbook to write
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb = wb, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = colnames(cp$readcount)[i],  x = df.gene,  tableStyle = "TableStyleLight2")
              
              openxlsx::addWorksheet(wb = wb, sheetName = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"),  x = df.sgrna,  tableStyle = "TableStyleLight2")
              
              #xlsx::write.xlsx(df.gene, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_CONTROLS.xls", sep="_"), sep="/"), sheetName=colnames(cp$readcount)[i], append=FALSE ,row.names=FALSE)
            } 
              # make for return as list
              data.return <- list(df.gene)
          }
          else
          {
            # write to file
            if(identical(write,TRUE))
            {
               #xlsx::write.xlsx(df.gene, file=paste(cp$miaccs["datapath"][[1]], paste(cp$miaccs["analysis.name"][[1]], "STATS_CONTROLS.xls", sep="_"), sep="/"), sheetName=colnames(cp$readcount)[i], append=TRUE ,row.names=FALSE)
              openxlsx::addWorksheet(wb = wb, sheetName = colnames(cp$readcount)[i], gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = colnames(cp$readcount)[i],  x = df.gene,  tableStyle = "TableStyleLight2")
              
              openxlsx::addWorksheet(wb = wb, sheetName = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"), gridLines = FALSE)
              openxlsx::writeDataTable(wb = wb, sheet = paste(colnames(cp$readcount)[i],"sgRNAs", sep="_"),  x = df.sgrna,  tableStyle = "TableStyleLight2")
              
            }
            
              # add to list for return
              data.return <- c(data.return,list(df.gene) )
            
          }
        }

        
    } # end of for loop
    
    if(identical(write,TRUE))
    {
      # Write XLSX
      # WRITE XLSX
      openxlsx::saveWorkbook(wb, paste(cp$miaccs$datapath, paste(cp$miaccs$analysis.name, "STATS.xlsx", sep="_"), sep="/"), overwrite = TRUE)
    }
    
    names(data.return) <- colnames(cp$readcount)[2:(number.files+1)]
    
    # make output for return
    if(identical(output,TRUE))
    {
      # use knitr to make table output
      for(i in 1:length(data.return))
      {
        cat("\n")
        print(names(data.return)[[i]])
        cat("\n")
        print(knitr::kable(data.return[[i]]))
        cat("\n")
      }
#       lapply(data.return, function(x){
#         print(names(x))
#         print(knitr::kable(x))
#         return(TRUE)
#       })
      
    }
    
    return(data.return)
    
    
    
  }
  
}
else
{
  stop("Please load your data with load.file() first.")
}
  

}