get.gene.info = function(gene = NULL, extractpattern=expression("^(.+?)(_.+)"), database="ENSEMBL_MART_ENSEMBL", dataset=NULL, filter="ensembl_gene_id", new.identifier = cp$miaccs$g.identifier.new, attribute = c("hgnc_symbol"), controls=FALSE, annotate = FALSE, convert.identifier = cp$miaccs$g.convert, host="www.ensembl.org")
{

  if(dataset == "homo_sapiens") {dataset <- "hsapiens_gene_ensembl"}
  if(dataset == "danio_rerio") {dataset <- "drerio_gene_ensembl"}
  if(dataset == "mus_musculus") {dataset <- "mmusculus_gene_ensembl"}
  ## annotate all data
if(identical(annotate,FALSE) && identical(convert.identifier,TRUE) && is.null(gene))
{
  
  gene.enrichment = c(filter, new.identifier)
  # get rid of duplicates
  gene.enrichment = unique(gene.enrichment)

  
  
# check for presence of cp$readcount
if(exists("cp", mode="environment") || exists("readcount", envir=cp) )
{
  
  # check for attributes and filters to be only one, attributes must be also the filter
  if (length(filter)!=1) stop("None or more than one single filter selected")
  
  gene.names = sub(extractpattern,"\\1", cp$readcount[,"design"], perl=TRUE, fixed=FALSE)
  
  # Add annotation information to cp$annotation
  #start biomaRt interface and check if biomaRt is available
  
  # Only use biomaRt if identifier is to be converted
  if(!(filter == new.identifier))
  {
    handling <- biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host)
    
    if(!exists("handling"))
    {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible. \n
          You can skip any data annotation by setting it to FALSE in the MIACCS file.")}
    
    #handling <- biomaRt::useDataset(dataset,mart=handling)
    gene.info <- biomaRt::getBM(
      filters = filter,
      attributes = gene.enrichment,
      values = unique(gene.names),
      mart = handling)
    
    # Set original gene identifier as backupinformation
    cp$readcount[,"replace"] = as.character(gene.names)
 
    #View(cp$readcount)
    #View(gene.info)
    #str(gene.info)
    # check if we were able to retrieve data and check if conversion makes sense, not needed if both are the same
    if(nrow(gene.info) >= 1)
    {
      # Gene info has - original identifier - new identifier
      # data has - namecolumn, .... - original gene name
      # we want to replace the namecolumn set with the new identifier, if there is no new one, we keep the old one
      
      # First duplicate check on gene info to remove duplicates in NEW gene identifier
      # output to file for additional reference
      # Remove duplicates
      
      gene.info <- gene.info[!duplicated(gene.info[,1]),]
      # Then apply on data with namecolumn and put in there, what gene info 2 is giving for those where gene.info 1 is equal to extractd namecolumn, replace namecolumn 
      
        cp$readcount[,"design"] = apply(cp$readcount,1, function(z){
          # compare name in gene.info[,1] with data namecolumn and set replace = gene.info[,2]
          #return(gene.info[gene.info[,1] == z["replace"] ,2]) 
          sgRNAident = as.character(sub(extractpattern,"\\2",as.character(z["design"]),perl=TRUE))
          
          testgeneid <- NA
          replacement = FALSE
          # check if gene identifier of the actual sgRNA could been converted, which means we hav an entry in gene.info
          # if this is the case, mark it for replacment, otherwise the original identifier will be kept
          if(as.character(z["replace"]) %in% gene.info[,1])
          {replacement = TRUE
          genereplace <- as.character(gene.info[gene.info[,1] == z["replace"] ,2])
          if(genereplace != "")
          {
            testgeneid <- genereplace
          }
          } 
          
          
          
          # in case of controls, treatment is different
          if(identical(controls,TRUE))
          {
            if(identical(replacement, TRUE) && !is.na(testgeneid))
            { return(gene.info[gene.info[,1] == z["replace"] ,2])
            } else {
              return(z["replace"])
            }
            #return(gene.info[gene.info[,1] == z["replace"] ,2])
          } 
          # Regular dataset, so we check if for the current sgRNA the gene identifier could be replaced by the one that is wanted for conversion
          else {
            
            # only if replacement is TRUE, we are going to change it
            if(identical(replacement, TRUE) && !is.na(testgeneid))
            { 
              # also overwrite gene name
              return(as.factor(paste(as.character(gene.info[gene.info[,1] == z["replace"] ,2][1]),sgRNAident,sep="")))
            } else {
              # we do not replace it, so we keep the old one
              return(as.factor(paste(as.character(z["replace"]), sgRNAident, sep="")))
            }
            #return(as.factor(paste(as.character(gene.info[gene.info[,1] == z["replace"] ,2]),sub(extractpattern,"\\2",as.character(z[namecolumn]),perl=TRUE),sep="")) )
          }
        })
        
        
        # also create an annotation data.frame from which all data is retrieved later on
        
        
        # aggregate data for later easier use with new gene names
        #aggregatetogenes(agg.function=sum,extractpattern = extractpattern)
        
        cp$readcount[,"gene"] = sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE)
        
        cp$annotation = data.frame(
          "gene" <- cp$readcount[,"gene"],
          "gene.old" <- cp$readcount[,"replace"],
          stringsAsFactors = FALSE)
        colnames(cp$annotation) <- c("gene", "gene.old")
        cp$annotation = cp$annotation[!duplicated(cp$annotation[,"gene.old"]),]
        
        
        # merge annotated features to gene names
        cp$annotation = merge(cp$annotation, gene.info, by.x = c("gene.old"), by.y = c(colnames(gene.info)[1]) )
        
        ### Change information in cp$libFILE
        if(exists("libFILE", envir=cp))
        {
          
          rownames(cp$libFILE) <- cp$libFILE$design
          cp$libFILE$ROWS <- rownames(cp$libFILE)
          # remove design do we do not have two
          colnames(cp$libFILE) <- c("design.old","sequence","ROWS")
          
          cp$libFILE$gene <- NULL
          cp$readcount$ROWS <- rownames(cp$readcount)
          # Change Gene information based on cp$readcount as both should have the same orientation!
          cp$libFILE <- merge(cp$libFILE, cp$readcount[,c("design","ROWS", "gene")], by.x="ROWS", by.y="ROWS", all.x = TRUE)
          cp$libFILE$design.old <- cp$libFILE$design
          cp$libFILE$design <- NULL
          cp$libFILE$ROWS <- NULL
          cp$readcount$ROWS <- NULL
          colnames(cp$libFILE) <- c("design", "sequence", "gene")
         # cp$libFILE[,"gene"] <- sapply(rownames(cp$readcount), FUN=function(x){
            # old: sapply(cp$readcount[,"gene"], return as charcter(x))
            #get gene depending on ROW name
          #  cp$readcount[as.character(x),"gene"]
          #  })
        }
      
      # output to file for additional reference
      cp$readcount["replace"]=NULL
  
    }
    else
    {
      # since no data retrieval worked (maybe wrong gene identifier), we just keep what we have.
      # Just put in the original gene identifier
      cp$readcount[,"gene"] = sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE)
      
      cp$annotation = data.frame(
        "gene" <- cp$readcount[,"gene"],
        "gene.old" <- cp$readcount[,"replace"],
        stringsAsFactors = FALSE)
      colnames(cp$annotation) <- c("gene", "gene.old")
      cp$annotation = cp$annotation[!duplicated(cp$annotation[,"gene.old"]),]
      
      #stop("Gene annotation failed. Please check the used dataset, database, filter and attributes for biomaRt.")
    }
  } else {
    # imitate
    
    cp$readcount[,"gene"] = sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE)
    
    cp$annotation = data.frame(
      "gene" <- cp$readcount[,"gene"],
      "gene.old" <- cp$readcount[,"gene"],
      stringsAsFactors = FALSE)
    colnames(cp$annotation) <- c("gene", "gene.old")
    cp$annotation = cp$annotation[!duplicated(cp$annotation[,"gene.old"]),]
    
    ### Change information in cp$libFILE
    if(exists("libFILE", envir=cp))
    {
      rownames(cp$libFILE) <- cp$libFILE$design
      cp$libFILE$ROWS <- rownames(cp$libFILE)
      # remove design do we do not have two
      colnames(cp$libFILE) <- c("design.old","sequence","ROWS")
      cp$libFILE$gene <- NULL
      cp$readcount$ROWS <- rownames(cp$readcount)
      # Change Gene information based on cp$readcount as both should have the same orientation!
      cp$libFILE <- merge(cp$libFILE, cp$readcount[,c("design","ROWS", "gene")], by.x="ROWS", by.y="ROWS", all.x = TRUE)
      cp$libFILE$design.old <- cp$libFILE$design
      cp$libFILE$design <- NULL
      cp$libFILE$ROWS <- NULL
      cp$readcount$ROWS <- NULL
      colnames(cp$libFILE) <- c("design", "sequence", "gene")
    }
    
  }
  
}
else
{
  stop("Please load your data with load.file() first.")
}

}  

  
if(identical(annotate,TRUE) && identical(convert.identifier,FALSE) && !is.null(filter) && !is.null(attribute) && !is.null(gene))
{
  ##### Handling to annotate certain genes that are passed on to this function
  ## Return: a data frame consisting of the query gene + all information as used for attributes
  # Used parameters are:
  # filter -> input used, is the geneidentifier
  # attirbutes -> what we would like to retrieve as information
  # Add annotation information to cp$annotation
  #start biomaRt interface and check if biomaRt is available
 
  gene.enrichment = c(filter,attribute)
  
  gene.enrichment = unique(gene.enrichment)
  
  handling = biomaRt::useMart(database, host=host)
  
  if(!exists("handling"))
  {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible. \n
        You can skip any data annotation by setting it to FALSE in the MIACCS file.")}
  handling = biomaRt::useDataset(dataset,mart=handling)
  gene.info <- biomaRt::getBM(
    filters = filter,
    attributes = gene.enrichment,
    values = unique(gene),
    mart = handling)
  
  # no conversion only annotation
  # aggregate data for later easier use with new gene names
  #aggregatetogenes(agg.function=sum,extractpattern = extractpattern)
  
  #cp$readcount["gene"] = sub(extractpattern,"\\1",cp$readcount[,"design"],perl=TRUE)
  if(!exists("cp", mode="environment") || !exists("aggregated.readcount", envir=cp) )
  {
    stop("Data have not been aggregated yet.")
  }
  
  cp$annotation = data.frame(
    "gene" <- unique(cp$readcount["gene"]),
    "gene2" <- unique(cp$readcount["gene"]),
    stringsAsFactors = FALSE)
  colnames(cp$annotation) <- c("gene","gene.2")
  #cp$annotation = cp$annotation[!duplicated(cp$annotation[,"gene"]),]
  
  # merge annotated features to gene names
  cp$annotation = merge(cp$annotation, gene.info, by.x = c("gene"), by.y = c(colnames(gene.info)[1]) )
  cp$annotation[,"gene.2"] = NULL
  
  
  return(as.data.frame(gene.info))
  
}
  ############ OLD
  
#   
# if(!is.null(data) && nrow(data)>=1)
# {
#     
#   
#   # we apply this to an sgRNA dataset, not single genes
#   if(return.val=="dataset")
#   {
#     # check for attributes and filters to be only one, attributes must be also the filter
#     if (length(filters)!=1) stop("None or more than one single filter selected")
#     
#     # if dataset is control
#     if(identical(controls, TRUE))
#     {
#       # get gene names from controls
#       
#       gene.names = data[,namecolumn]
#     }
#     else
#     {
#       # get gene names from design file
#       gene.names = sub(extractpattern,"\\1",data[,namecolumn],perl=TRUE)
#     }
#     
#     
#     #start biomaRt interface and check if biomaRt is available
#     handling = biomaRt::useMart(database)
#     if(!exists("handling"))
#     {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible. \n
#           You can skip any data annotation by setting it to FALSE in the MIACCS file.")}
#     handling = biomaRt::useDataset(dataset,mart=handling)
#     gene.info = biomaRt::getBM(
#       filters=filters,
#       attributes= c(filters,attributes),
#       values= gene.names,
#       mart = handling)
#     
#     #str(gene.info)
#     if(nrow(gene.info) >= 1)
#     {
#       data$replace = as.character(gene.names)
#       
#       #print(data$replace)
#       #gene.info.ext <-gene.info
#       #print(gene.info)
#       
#       
#       # Gene info has - original identifier - new identifier
#       # data has - namecolumn, .... - original gene name
#       # we want to replace the namecolumn set with the new identifier, if there is no new one, we keep the old one
#       
#       # First duplicate check on gene info to remove duplicates in NEW gene identifier
#       # output to file for additional reference
#       # Remove duplicates
#       gene.info <- gene.info[!duplicated(gene.info[,2]),]
#       # Then apply on data with namecolumn and put in there, what gene info 2 is giving for those where gene.info 1 is equal to extractd namecolumn, replace namecolumn 
#       data[,namecolumn] = apply(data,1, function(z){
#         # compare name in gene.info[,1] with data namecolumn and set replace = gene.info[,2]
#           #return(gene.info[gene.info[,1] == z["replace"] ,2]) 
#         sgRNAident = sub(extractpattern,"\\2",as.character(z[namecolumn]),perl=TRUE)
#         
#         if(z["replace"] %in% gene.info[,1])
#         {replacement = TRUE
#         } else {
#           replacement = FALSE
#         }
#         if(identical(controls,TRUE))
#         {
#           if(identical(replacement, TRUE))
#           { return(gene.info[gene.info[,1] == z["replace"] ,2])
#           } else {
#             return(z["replace"])
#           }
#           #return(gene.info[gene.info[,1] == z["replace"] ,2])
#         } else {
#           
#           if(identical(replacement, TRUE))
#           { 
#             return(as.factor(paste(as.character(gene.info[gene.info[,1] == z["replace"] ,2][1]),sgRNAident,sep="")))
#           } else {
#             return(as.factor(paste(as.character(z["replace"]),sgRNAident,sep="")))
#           }
#           #return(as.factor(paste(as.character(gene.info[gene.info[,1] == z["replace"] ,2]),sub(extractpattern,"\\2",as.character(z[namecolumn]),perl=TRUE),sep="")) )
#         }
#       })
# 
#       # output to file for additional reference
#       data$replace=NULL
#       
#       # check for duplicates and use original identifier instead which is stored in gene names
#       
#       
#       
#       return(data)
#       
#     }
#     else
#     {
#       return(as.data.frame(data))
#     }
    
#      
#   }
#   if(return.val=="info")
#   {
#     # We can enrich gene information based on what is provided for biomaRt
#     # get gene names from design file
#     # if dataset is control
#     if(identical(controls, TRUE))
#     {
#       # get gene names from controls
#       gene.names = data[,namecolumn]
#     }
#     else
#     {
#       # get gene names from design file
#       gene.names = sub(extractpattern,"\\1",data[,namecolumn],perl=TRUE)
#     }
#     #start biomaRt interface
#     handling = biomaRt::useMart(database)
#     if(!exists("handling"))
#     {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible. \n
#           You can skip any data annotation by setting it to FALSE in the MIACCS file.")}
#     handling = biomaRt::useDataset(dataset,mart=handling)
#     gene.info = biomaRt::getBM(
#       filters=filters,
#       attributes= c(filters,attributes),
#       values= gene.names,
#       mart= handling)
#     
#     cols = ncol(gene.info)
#     gene = aggregate.data.frame(gene.names,by=list(gene.names), function(x) return(x[1]))
#     gene$Group.1=NULL
#     data.return=data.frame(
#       gene = as.character(gene$x),
#       stringsAsFactors=FALSE)
#     
#     
#     data.return[,colnames(gene.info)] = NA
#     for(m in 1:cols)
#     {
#       for(i in 1:nrow(gene.info))
#       {
#         data.return[data.return[,namecolumn]== gene.info[i,1],m] = gene.info[i,m]
#       }
#       #data.return = cbind.data.frame(data.return, gene.info[,1:cols])
#     }
#   
#     colnames(data.return) = colnames(gene.info)
#     data.return[,is.na(colnames(data.return))] = NULL
#     return(data.return)
#     
#   }
#   
# }

  
}