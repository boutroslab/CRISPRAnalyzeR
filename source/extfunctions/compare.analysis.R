compare.analysis = function(methods=NULL, type="enriched", cutoff.deseq = cp$miaccs$sig.pval.deseq, cutoff.wilcox = cp$miaccs$sig.pval.wilcox, cutoff.mageck = cp$miaccs$sig.pval.mageck, cutoff.edger = cp$miaccs$sig.pval.edger, cutoff.rsea = cp$miaccs$sig.pval.rsea, cutoff.override=cp$miaccs$cutoff.override, cutoff.hits=cp$miaccs$compare.cutoff, output="list",plot.method=c("wilcox","mageck", "deseq2"), plot.feature=c("pval","fdr","pval"), pch=16)
{
  #type = enriched | depleted
  #output = list | rank | venn
  #sort.by[1] =  wilcox | deseq 
  #sort.by[3] = pval | fc OR if sorting.output.method=DSS AUC | sgRNA
  #methods = NULL -> will automatically detect!
  # other: wilcox,deseq,mageck,rsea,edger,doench,screenbeam
  # Generate hits will automatically load all necessary data frames from cp environment and store the information
  # within cp$overlap.candidates
  if(!exists("aggregated.readcount", envir=cp))
  {
    aggregatetogenes(extractpattern = cp$miaccs$g.extractpattern)
  }
  # Checking for presence of analysis tools
  load.analysis = NULL
  if(is.null(methods)) {
    if(exists("wilcox", envir=cp))
    {
      load.analysis = c(load.analysis,"wilcox")
    }
    if(exists("deseq", envir=cp))
    {
      load.analysis = c(load.analysis,"deseq2")
    }
    if(exists("mageck", envir=cp))
    {
      load.analysis = c(load.analysis,"mageck")
    }
    if(exists("edger", envir=cp))
    {
      load.analysis = c(load.analysis,"edger")
    }
    if(exists("rsea", envir=cp))
    {
      load.analysis = c(load.analysis,"rsea")
    }
    if(exists("doench", envir=cp))
    {
      load.analysis = c(load.analysis,"doench")
    }
    # if(exists("screenbeam", envir=cp))
    # {
    #   load.analysis = c(load.analysis,"screenbeam")
    # }
  } else {
    for (method in methods){
      if(exists(method, envir=cp))
      {
        load.analysis = c(load.analysis,method)
      }
    }
  }

# input:
#   data: data from at least 2 data analysis -> check how many!
#   sortby: pval | fc -> to determine sorting for any of those
#   separate: TRUE | FALSE -> whether comparison is determined for enriched and depleted separately
#   cutoff: NUMERIC -> how many top candidates to use

# final returned output shall look like this

# status                gene        riger.rank  riger.pval  wilcox.rank   wilcox.pval   deseq.rank  deseq.pval    
# enriched/depleted     gene name   rank        pval        rank            pval            rank        pval        
#
# rank means at which position from most enriched/depleted did it occur in the screening set
# will be NA if not present within cutoff


###################################################

# first check which data sets are provided and put them into a new df,
# also make separate if desired before sorting
  
  # create data frame we need for calculations
  
if(type=="enriched")
{
    # Go through the list to assemble the dataset
    
    # separate by enrichment 
    if("wilcox" %in% load.analysis)
    {
      # wilcox
      df.wilcox = cp$wilcox[cp$wilcox$foldchange >= 1 ,c("foldchange","p.value")]
      
      df.wilcox = df.wilcox[order(
        df.wilcox$p.value,
        decreasing = FALSE 
      ),]
    }
    
    if("deseq2" %in% load.analysis)
    {
      
      #DESeq2
      # Deseq make sure it is -1 * foldchange, as deseq compare UNTREATED vs TREATED< all other treated vs untreated!
      df.deseq = as.data.frame(cp$deseq$genes[cp$deseq$genes$log2FoldChange >= 0 ,c("log2FoldChange","padj")])
      
      df.deseq = df.deseq[order(
        df.deseq$padj,
        decreasing = FALSE
      ),]
    }
    
    
    if("mageck" %in% load.analysis)
    {
      # MAGeCK
      df.mageck = cp$mageck$genes[, c("pos","rank.pos","sgrna.pos.good")]
      
      colnames(df.mageck) = c("fdr","rank","sgrna")
      df.mageck = df.mageck[order(
        df.mageck$rank,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("rsea" %in% load.analysis)
    {
      # sgRSEA
      df.rsea = as.data.frame(cp$rsea$gene.pos[, c("p.value.pos","FDR.pos","NScore")])
      rownames(df.rsea) <- rownames(cp$rsea$gene.pos)
      colnames(df.rsea) = c("pval","fdr","score")
      df.rsea = df.rsea[order(
        df.rsea$fdr,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("edger" %in% load.analysis)
    {
      # EdgeR
      df.edger = as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Up", c("Direction","FDR")])
      rownames(df.edger) <- rownames(as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Up",]))
      colnames(df.edger) = c("Direction","fdr")
      df.edger = df.edger[order(
        df.edger[,"fdr"],
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    # Loading of data complete
    
    # Combine datasets using the cutoff
    # put in the rank of that gene in the data analysis table, if not present, put NA
    
    
    
  } else if(type=="depleted")
  {
    
    if("wilcox" %in% load.analysis)
    {
      
      # wilcox
      df.wilcox = cp$wilcox[cp$wilcox$foldchange < 1 , c("foldchange","p.value")]
      df.wilcox = df.wilcox[order(
        df.wilcox$p.value,
        decreasing = FALSE
      ),]
    }
    # print(df.wilcox)
    if("deseq2" %in% load.analysis)
    {
      
      #DESeq2
      df.deseq = as.data.frame(cp$deseq$genes[cp$deseq$genes$log2FoldChange < 0 ,c("log2FoldChange","padj")])
      
      df.deseq = df.deseq[order(
        df.deseq$padj,
        decreasing = FALSE
      ),]
    }
    
    
    if("mageck" %in% load.analysis)
    {
      # MAGeCK
      df.mageck = cp$mageck$genes[, c("neg","rank.neg","sgrna.neg.good")]
      
      colnames(df.mageck) = c("fdr","rank","sgrna")
      
      df.mageck = df.mageck[order(
        df.mageck$rank,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
    if("rsea" %in% load.analysis)
    {
      # sgRSEA
      df.rsea = as.data.frame(cp$rsea$gene.neg[, c("p.value.neg","FDR.neg","NScore")])
      rownames(df.rsea) <- rownames(cp$rsea$gene.neg)
      colnames(df.rsea) = c("pval","fdr","score")
      df.rsea = df.rsea[order(
        df.rsea$fdr,
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    if("edger" %in% load.analysis)
    {
      # EdgeR
      
      df.edger = as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Down", c("Direction","FDR")])
      rownames(df.edger) <- rownames(as.data.frame(cp$edger$genes[cp$edger$genes[,"Direction"] == "Down",]))
      colnames(df.edger) = c("Direction","fdr")
      
      df.edger = df.edger[order(
        df.edger[,"fdr"],
        decreasing=FALSE,
        na.last=TRUE
      ),]
      
    }
    
  } else {
    stop("No type selected. Please select enriched or depleted.")
  }
#### Data loading complete

# # add ranking just by length of dataset
# #if(!is.null(riger)) {df.riger$rank = c(1:nrow(df.riger))}
#   if(!is.null(wilcox)) {df.wilcox$rank = c(1:nrow(df.wilcox))}
#   if(!is.null(deseq)) {df.deseq$rank = c(1:nrow(df.deseq))}
#   if(!is.null(edger)) {df.edger$rank = c(1:nrow(df.edger))}
#   
# # create joining data.frame that has all gene names, use any of the above dataset
#                     
# if(!is.null(wilcox)) { v.join = rownames(wilcox)
#                  
# } else if(!is.null(deseq)) { v.join = rownames(deseq) 
# 
#                            
# } else if(!is.null(mageck)) { v.join = rownames(mageck) 
#                     
# } else if(!is.null(rsea)) { v.join = rownames(rsea) 
# 
# } else if(!is.null(edger)) { v.join = rownames(edger) 
# 
# } else {stop("No datasets provided")}

# Create output data.frame
# rownames = gene names
# then
# EITHER p.value, foldchange of RIGER, wilcox, DESEQ + AUC of DSS
# OR rank of RIGER, wilcox, DESEQ, DSS that are within cutoff

# remove all genes that have NA on all of them
# remove columns that only have NAs (means not data was provided)

if(output=="list")
  {
    df.output = data.frame(
      genes = unique(cp$aggregated.readcount$gene),
      stringsAsFactors=FALSE)
    
    rownames(df.output) = df.output$genes


      # wilcox
      if("wilcox" %in% load.analysis) {
        
        df.output$wilcox.log2fc=apply(df.output,1,function(i) return(log2(df.wilcox[i["genes"],"foldchange"])))
        df.output$wilcox.pval=apply(df.output,1,function(i) return(df.wilcox[i["genes"],"p.value"]))
        
      }

      # DESEQ2
      if("deseq2" %in% load.analysis) {
        
        df.output$deseq.log2fc=apply(df.output,1,function(i) return(df.deseq[i["genes"],"log2FoldChange"]))
        df.output$deseq.pval=apply(df.output,1,function(i) return(df.deseq[i["genes"],"padj"]))
        
      }
    
      # MAGeCK
      if("mageck" %in% load.analysis) {
        
        df.output$mageck.fdr=apply(df.output,1,function(i) return(df.mageck[i["genes"],"fdr"]))
        df.output$mageck.rank=apply(df.output,1,function(i) return(df.mageck[i["genes"],"rank"]))
        
      }
    
    # sgRSEA
    if("rsea" %in% load.analysis) {
      
      df.output$rsea.fdr=apply(df.output,1,function(i) return(df.rsea[i["genes"],"fdr"]))
      df.output$rsea.rank=apply(df.output,1,function(i) return(df.rsea[i["genes"],"rank"]))
      
    }
    
    # EdgeR
    if("edger" %in% load.analysis) {
      
      df.output$edger.fdr=apply(df.output,1,function(i) return(df.edger[i["genes"],"fdr"]))
      
    }

    #df.output = df.output[-todelete,]
      
      # remove gene name column, we have rownames
    
  
    # Cutoff
    #if(!is.null(cutoff.hits))
    #{
    #  df.output = df.output[1:cutoff.hits,]
    #}
    
    return(df.output)
    #return data

} else if(output == "rank") 
{

  df.output = data.frame(
    genes = unique(cp$aggregated.readcount$gene),
    stringsAsFactors=FALSE)
  
  rownames(df.output) = df.output$genes

  # wilcox
  if("wilcox" %in% load.analysis) {
    
    df.output$wilcox=apply(df.output,1,function(i) return(df.wilcox[i["genes"],"rank"]))
    #df.output$wilcox.pval=apply(df.output,1,function(i) return(df.wilcox[i["genes"],"p.value"]))
    
  }
  
  # DESEQ2
  if("deseq2" %in% load.analysis) {
    df.output$deseq=apply(df.output,1,function(i) return(df.deseq[i["genes"],"rank"]))
    #df.output$deseq.pval=apply(df.output,1,function(i) return(df.deseq[i["genes"],"padj"]))
  }
  
  # MAGeCK
  if("mageck" %in% load.analysis) {
    df.output$mageck=apply(df.output,1,function(i) return(df.mageck[i["genes"],"rank"]))
  }
  
  # sgRSEA
  if("rsea" %in% load.analysis) {
    df.output$rsea=apply(df.output,1,function(i) return(df.rsea[i["genes"],"rank"]))
  }
  
  # edgeR
  if("edger" %in% load.analysis) {
    df.output$edger=apply(df.output,1,function(i) return(df.edger[i["genes"],"rank"]))
  }
  
  

  # remove gene name column, we have rownames
  df.output$genes=NULL

  # Cutoff
  if(!is.null(cutoff.hits))
  {
    df.output = df.output[1:cutoff.hits,]
    
  }
  #return data
  return(df.output)
  
#### FOR VENN DIAGRAMM  

}  else if(output == "venn")
  # CREATE VENN DIAGRAM
{  
  
  if(identical(cutoff.override,TRUE))
  {
    cutoff.wilcox = cutoff.hits
    cutoff.deseq = cutoff.hits
    cutoff.mageck = cutoff.hits
    cutoff.rsea = cutoff.hits
    cutoff.edger = cutoff.hits
  }
  
  list.return=list()
 
  # wilcox
  if("wilcox" %in% load.analysis) {
    
    venn.wilcox=df.wilcox[order(df.wilcox[,"p.value"], na.last=TRUE, decreasing=FALSE),]
    #df.output$wilcox.pval=apply(df.output,1,function(i) return(df.wilcox[i["genes"],"p.value"]))
  
    # Cutoff
    if(!is.null(cutoff.wilcox))
    {
      if(identical(cutoff.override,TRUE))
      {
        venn.wilcox = rownames(venn.wilcox[1:cutoff.hits,])
      }
      else
      {
        venn.wilcox = rownames(venn.wilcox[venn.wilcox$p.value <= cutoff.wilcox,])
      }
      
    }
    # return list
    list.return=c(list.return,list("Wilcox" = venn.wilcox))
  }
  
  # DESEQ2
  if("deseq2" %in% load.analysis) {
    
    venn.deseq=df.deseq[order(df.deseq[,"padj"], na.last=TRUE, decreasing=FALSE),]
    #df.output$deseq.pval=apply(df.output,1,function(i) return(df.deseq[i["genes"],"padj"]))
    # Cutoff
    if(!is.null(cutoff.deseq))
    {
      if(identical(cutoff.override,TRUE))
      {
        venn.deseq = rownames(venn.deseq[1:cutoff.hits,])
      }
      else
      {
        venn.deseq = rownames(venn.deseq[venn.deseq$padj <= cutoff.deseq,])
      }
    }
    # return list
    list.return=c(list.return,list("DESeq2" = venn.deseq))
  }
  
  
  # MAGeCK
  if("mageck" %in% load.analysis) {
    
    venn.mageck=df.mageck[order(df.mageck[,"rank"], na.last=TRUE,decreasing=FALSE),]

    
    # Cutoff
    if(!is.null(cutoff.mageck))
    {
      if(identical(cutoff.override,TRUE))
      {
        venn.mageck = rownames(venn.mageck[1:cutoff.hits,])
      }
      else
      {
        venn.mageck = rownames(venn.mageck[venn.mageck$fdr <= cutoff.mageck,])
      }
    }
    # return list
    list.return=c(list.return,list("MAGeCK" = venn.mageck))
  }
  
  # sgrsea
  if("rsea" %in% load.analysis) {
    venn.rsea=df.rsea[order(df.rsea[,"fdr"], na.last=TRUE,decreasing=FALSE),]
    
    
    # Cutoff
    if(!is.null(cutoff.rsea))
    {
      if(identical(cutoff.override,TRUE))
      {
        venn.rsea = rownames(venn.rsea[1:cutoff.hits,])
      }
      else
      {
        venn.rsea = rownames(venn.rsea[venn.rsea$fdr <= cutoff.rsea,])
      }
    }
    # return list
    list.return=c(list.return,list("sgRSEA" = venn.rsea))
  }
  
  # edgeR
  if("edger" %in% load.analysis) {
    
    venn.edger=df.edger[order(df.edger[,"fdr"], na.last=TRUE,decreasing=FALSE),]
    
    
    # Cutoff
    if(!is.null(cutoff.edger))
    {
      if(identical(cutoff.override,TRUE))
      {
        venn.edger = rownames(venn.edger[1:cutoff.hits,])
      }
      else
      {
        venn.edger = rownames(venn.edger[venn.edger$fdr <= cutoff.edger,])
      }
    }
    # return list
    list.return=c(list.return,list("EdgeR" = venn.edger))
  }
  
  ## Now we have each gene name in the list, so we need to count
 
  #return data
  return(list.return)
  
}
  


}