stat.mageck=function(norm.fun="median", extractpattern=expression("^(.+?)(_.+)"), groups=cp$groups.compare, mageckfolder=NULL, sort.criteria="neg", adjust.method="fdr", filename=NULL, fdr.pval=0.05, logfile = NULL){
  # OLD ARGUMENTS
  # untreated.list, treated.list, namecolumn=1, fullmatchcolumn=2,
  # mageckfolder: where to store analysis files
  # by default: getwd

  # Dataset will be passed on to Mageck Python Script

  # check for identity
  # take NAME element of each dataset and check whether it is identical in the others
  
  # Treatment groups are taken from cp$treatmentgroup!
  
  cp$mageck <- NULL
  
  # data structure already exists?
  if(!exists("cp", mode="environment") || !exists("readcount", envir=cp) )
  {
    stop("Please load your data with load.file() first.")
  }
  
  if(!exists("aggregated.readcount", envir=cp) )
  {
    aggregatetogenes(extractpattern = extractpattern)
  }
  if(!exists("normalized.readcount", envir=cp) && norm.fun == "none" )
  {
    norm.fun="median"
  }
  
  
  # get treatment groups
  treatment.groups <- unlist(cp$treatmentgroup)
  
  # Make untreated df
  if(norm.fun=="none")
  {
    data.list <- data.frame(
      "designs" = cp$normalized.readcount[,"design"])
    
    gene.names = cp$normalized.readcount[,"gene"]
    designs=cp$normalized.readcount[,"design"]
  }
  else
  {
    data.list <- data.frame(
      "designs" = cp$readcount[,"design"])
    
    gene.names = cp$readcount[,"gene"]
    designs=cp$readcount[,"design"]
  }
  
  
  # add data acording to treatment groups
  for(i in 1:length(treatment.groups))
  {
    # unlist groups from miaccs file
    this.treatment <- as.numeric(treatment.groups[[i]])
    if(norm.fun=="none")
    {
      treat.df.sgRNA <- data.frame(
        "designs" = cp$normalized.readcount[,"design"],
        stringsAsFactors = FALSE
      )
      # add to treatment group for sgRNA
      treat.df.sgRNA <- cbind.data.frame(treat.df.sgRNA, cp$normalized.readcount[,(this.treatment)+1])
      treatname.sgrna <- colnames(cp$normalized.readcount)[(this.treatment)+1]
      colnames(treat.df.sgRNA) = c("designs",treatname.sgrna)
      treat.df.sgRNA$designs <- NULL
    }
    else
    {
      treat.df.sgRNA <- data.frame(
        "designs" = cp$readcount[,"design"],
        stringsAsFactors = FALSE
      )
      # add to treatment group for sgRNA
      treat.df.sgRNA <- cbind.data.frame(treat.df.sgRNA, cp$readcount[,(this.treatment)+1])
      treatname.sgrna <- colnames(cp$readcount)[(this.treatment)+1]
      colnames(treat.df.sgRNA) = c("designs",treatname.sgrna)
      treat.df.sgRNA$designs <- NULL
    }
    
    if(!is.null(logfile))
    {
      write(paste(": MAGeCK 1"), logfile, append = TRUE)
    }
    
    # add to treatment group
    #treat.df <- cbind.data.frame(treat.df, cp$readcount[,(this.treatment)+1])
    
    
    if(i==1)
    {
      #data.list <- list(treat.df)
      #sgRNA
      data.list.sgRNA <- list(treat.df.sgRNA)
      names(data.list.sgRNA) <- treatname.sgrna
    }
    else
    {
      #sgRNA
      name.old <- names(data.list.sgRNA)
      data.list.sgRNA <- c(data.list.sgRNA, list(treat.df.sgRNA) )
      # set new name
      names(data.list.sgRNA) <- c(name.old,treatname.sgrna)
    }
  }
  
  # provide the necessary dataset combination used for mageck
  # sgRNA name  sample.treated.reads (replicate)  sample.untreated.reads (replicate)

    # get the group columns
    for(i in 1:length(groups))
    {
      if(i==1)
      {
        groups.compare = list(colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1])
      }
      else
      {
        groups.compare = c(groups.compare,list(colnames(cp$readcount)[cp$treatmentgroup[groups[i]][[1]]+1]))
      }
    }
    
    # now get the data frame
    untreated.length = length(data.list.sgRNA[groups.compare[[1]]])
    treated.length = length(data.list.sgRNA[groups.compare[[2]]])  
    
    #sgRNA
    dataset.combined = data.frame(
      "designs" = designs,
      "genes" = gene.names,
      "CTRL" = data.list.sgRNA[groups.compare[[1]]],
      "TREAT" = data.list.sgRNA[groups.compare[[2]]],
      stringsAsFactors=FALSE
    )
  
  # Set names etc.
  
  row.names(dataset.combined)=designs
  
  colnames(dataset.combined)=c("designs", "genes",groups.compare[[1]],groups.compare[[2]])
  

  
  # get the number of replicates
  #untreated.length = length(data.list[[groups[1]]][2:(ncol(data.list[[groups[1]]]))])
  #treated.length = length(data.list[[groups[1]]][2:(ncol(data.list[[groups[1]]]))])
  

  # create data.frame for use
  #dataset.combined = data.frame( 
  #  designs = designs,
  #  genes = gene.names,
  #  stringsAsFactors=FALSE)
  
  #add treated FIRST
  #dataset.combined <- cbind.data.frame(dataset.combined, data.list[[groups[2]]][2:(ncol(data.list[[groups[2]]]))])
  
  ## add untreated via cbind
  #dataset.combined <- cbind.data.frame(dataset.combined, data.list[[groups[1]]][2:(ncol(data.list[[groups[1]]]))])
  
  if(!is.null(logfile))
  {
    write(paste(": MAGeCK 2"), logfile, append = TRUE)
  }
  # Write file to pass on to Mageck
  if(is.null(filename)) { filename="mageckanalysisfile"}
  if(is.null(mageckfolder)) {dirstore = getwd()} else {dirstore = mageckfolder}
  
  dataset.combined.file=file.path(dirstore,paste(filename, "_MAGeCK_sgRNA.tab", sep="") )
  write.table(dataset.combined, file=dataset.combined.file, row.names=FALSE,quote=FALSE)
  
  if(!is.null(logfile))
  {
    write(paste(": MAGeCK 3"), logfile, append = TRUE)
  }
  #print(dataset.combined$designs)
  
  # pass on the dataset to Mageck via rPython
#   
#   usage: mageck test [-h] -k COUNT_TABLE -t TREATMENT_ID [-c CONTROL_ID]
#   [-n OUTPUT_PREFIX] [--norm-method {none,median,total}]
#   [--normcounts-to-file]
#   [--gene-test-fdr-threshold GENE_TEST_FDR_THRESHOLD]
#   [--adjust-method {fdr,holm}] [--variance-from-all-samples]
#   [--sort-criteria {neg,pos}] [--keep-tmp]
  
  
# unTreated samples start in 3rd row
  #untreated.samples = 0+treated.length
  untreated.samples = 0
  if(untreated.length >1)
  {
    #for(i in seq(from = (0+treated.length+1), to = (0+treated.length+untreated.length-1), by = 1))
    for(i in seq(from = 1, to = (0+untreated.length-1), by = 1))
    {
      untreated.samples = paste(untreated.samples, i, sep=",")
    }
  } 

 
# treated samples start at 3rd + ncol.treated +1
  treated.samples = 0+untreated.length
  if(treated.samples > 1)
  {
    
    #for(i in seq(from = 1, to = (0 + treated.length-1), by = 1))
    if(treated.length >1)
    {
      
      for(i in seq(from = (0+untreated.length+1), to = (0 + untreated.length + treated.length -1), by = 1))
      {
        treated.samples = paste(treated.samples, i, sep=",")
      }
    }
    else
    {
      treated.samples = treated.samples
    }
    
  }

  
#   str(dataset.combined)
#   str(dataset.combined.file)
#   print(treated.samples)
# # Create String
#  if(ncol.treated== 2)
#  {
#    treated.samples = paste(0, 1, sep=",")
#    if(ncol.untreated== 2)
#    {
#      
#      untreated.samples = paste(ncol.treated, ncol.treated+1, sep=",")
#    }
#    else {
#      untreated.samples = ncol.treated
#    }
#  }
#  else {
#    treated.samples = 0 
#    
#    if(ncol.untreated== 2)
#    {
#      
#      untreated.samples = paste(1, 2, sep=",")
#    }
#    else {
#      untreated.samples = 1
#    }
#    
#  }



# str(dataset.combined.file)
# str(treated.samples)
# str(untreated.samples)
# str(norm.fun)
# str(sort.criteria)
# str(adjust.method)
# str(filename)

#mageckstring = paste("mageck test", "--count-table", dataset.combined.file, "--treatment-id", treated.samples, "--control-id", untreated.samples, "--norm-method", norm.fun, "--sort-criteria", sort.criteria, "--gene-test-fdr-threshold ", fdr.pval, "--adjust-method", adjust.method, "--output-prefix", file.path(dirstore,filename), sep=" ")
# Pass on to Mageck
#write(mageckstring, "string.txt")
#system(mageckstring)

# system2 replaces system, only ABSOLUTE PATHS

mageck.args <- c("test", paste("--count-table", dataset.combined.file, sep=" "), paste("--treatment-id", treated.samples , sep=" "), paste("--control-id", untreated.samples,sep=" "), paste("--norm-method", norm.fun,sep=" "), paste("--sort-criteria", sort.criteria,sep=" "), paste( "--gene-test-fdr-threshold ", fdr.pval,sep=" "), paste("--adjust-method", adjust.method,sep=" "), paste("--output-prefix", file.path(dirstore,filename),sep=" "))
system2(command = "mageck", args = mageck.args)
#test <- system(mageckstring, intern=TRUE)
if(!is.null(logfile))
{
  write(paste(": MAGeCK 4"), logfile, append = TRUE)
  write(paste(dirstore ,": MAGeCK 4"), logfile, append = TRUE)
  write(paste(filename ,": MAGeCK 4"), logfile, append = TRUE)
  write(paste(file.path(dirstore, paste(filename, "gene_summary.txt", sep="." )) ,": MAGeCK 4"), logfile, append = TRUE)
  
}
# load files created by Mageck
# Filenames:
# sample1.gene_summary.txt
# sample1.sgrna_summary.txt


data.mageck.genes = read.table(file.path(dirstore, paste(filename, "gene_summary.txt", sep="." )), header= TRUE, sep="\t", comment.char="")


if(!is.null(logfile))
{
  write(paste(": MAGeCK 5"), logfile, append = TRUE)
}
data.mageck.sgrna = read.table(file.path(dirstore, paste(filename, "sgrna_summary.txt", sep="." )), header= TRUE, sep="\t", comment.char="")

#str(data.mageck.genes)
#str(data.mageck.sgrna)

# Layout gene file:
#id  num	neg|score	neg|p-value	neg|fdr	neg|rank	neg|goodsgrna	pos|score	pos|p-value	pos|fdr	pos|rank	pos|goodsgrna
#ENSG00000171552	30	1.0203e-05	0.00020577	0.084158	1	18	0.33353	0.70983	0.998632	254	2

# Layout sgRNA file:
#sgrna   Gene   control_count   treatment_count control_mean    treat_mean      control_var     adj_var score   p.low   p.high  p.twosided      FDR     high_in_treatment
#INO80B_m74682554   INO80B        0.0/0.0 1220.1598778/1476.14096301      0.810860655738  1348.15042041   0.0     19.0767988005   308.478081895   1.0     1.11022302463e-16       2.22044604925e-16       1.57651669497e-14       True

dataset.return = data.frame( 
  row.names = data.mageck.genes$id,
  genes = data.mageck.genes$id,
  pos = data.mageck.genes[,paste("pos.", adjust.method, sep="")],
  rank.pos = data.mageck.genes[,"pos.rank"],
  neg = data.mageck.genes[,paste("neg.", adjust.method, sep="")],
  rank.neg = as.numeric(data.mageck.genes[,"neg.rank"]),
  #sgrna.neg = as.numeric(sgRNA.sig.neg$x),
 # sgrna.pos = as.numeric(sgRNA.sig.pos$x),
  sgrna.neg.good = as.numeric(data.mageck.genes[,"neg.goodsgrna"]),
  sgrna.pos.good = as.numeric(data.mageck.genes[,"pos.goodsgrna"]),
  stringsAsFactors=FALSE)

# now add the correct number of significant sgRNAs
#dataset.return$sgrna.neg = apply(dataset.return, 1, function(u) return(sgRNA.sig.neg[sgRNA.sig.neg$Group.1==u["genes"],"x"]))
#dataset.return$sgrna.pos = apply(dataset.return, 1, function(u) return(sgRNA.sig.pos[sgRNA.sig.pos$Group.1==u["genes"],"x"]))

if(!is.null(logfile))
{
  write(paste(": MAGeCK 6"), logfile, append = TRUE)
}

# Return data
cp$mageck <- list(genes = dataset.return, sgRNA = data.mageck.sgrna)
  
return(list(genes = dataset.return, sgRNA = data.mageck.sgrna))
  
  
}