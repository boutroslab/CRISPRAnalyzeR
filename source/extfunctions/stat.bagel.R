stat.bagel=function(numiter=1000, scriptpath = cp$miaccs$scriptpath, outputfolder=NULL, groups=NULL, lowercutoff = -50, highercutoff = 100){
  # OLD ARGUMENTS
  
  # Dataste is taken from DESeq, or any other data.frame that provides fold changes
  if(is.null(groups))
  {
    stop("Please define the groups you would like to compare. groups must be a character vector with two groups you would like to compare, e.g. c('CTRL','TREATMENT'). This will compare treatment with CTRL.")
  }
  # reset cp$bagel
  cp$bagel <- NULL
  
  
  # User can provide their own data OR deseq2/wilcox data frames for log2 foldchanges
  # Input for BAGEL is:
  #SEQID   GENE    Log2foldchange of compared group   
  #Seq1    A1BG    1.006  
  #Seq2    A1BG    0.163  
  
  #### TESTING
  #load(file.path("cp.rds"))
  
  #numiter=1000
  #scriptpath = getwd()
  #outputfolder=getwd()
  #groups=cp$groups.compare

    if(!exists("normalized.readcount", envir = cp) )
    {
      # perform normalizations
      stop("No cp environment available")
    }
    # Caculcate log2 Fold Change based on the groups
    df.bagel <- data.frame(
      "SEQID" = cp$normalized.readcount[,"design"],
      "GENE" = cp$normalized.readcount[,"gene"],
      stringsAsFactors=FALSE
    )
    
    # Always take replicates of treated group and compare it to mean of untreated - most people will also have only one day0 file
    num.replicates = length(cp$treatmentgroup[[groups[2]]])
    compare.cols <- paste(seq(1:length(cp$treatmentgroup[[groups[2]]])), collapse = "," )
    for(i in 1:num.replicates)
    {
      df.bagel[,paste("Replicate","-",i, sep="")] <- log2(cp$normalized.readcount[,(cp$treatmentgroup[[groups[2]]][i]+1)]/mean(cp$normalized.readcount[,(cp$treatmentgroup[[groups[1]]])]))
      df.bagel[!is.finite(df.bagel[,paste("Replicate","-",i, sep="")]),paste("Replicate","-",i, sep="")] <- as.numeric(0) #sapply(df.bagel[,paste("Replicate","-",i, sep="")], function(x) { if(is.infinite(x) || is.na(x)) { return(0)} else { return(x)}})
    }
   
  
  
  
  # Write FC file
  
  if(is.null(outputfolder))
  {
    dirstore = getwd()
  } else
  {
    dirstore <- outputfolder
  }
 
  df.bagel.file=file.path(dirstore,"BAGEL_FC.tab")
  # write bagel TAB
  write.table(df.bagel, file=df.bagel.file, row.names=FALSE,quote=FALSE, sep = "\t")
  
  df.bagel.outputfile=file.path(dirstore,"BAGEL_output.tab")
  
  # Call bagel.py
  # BAGEL.py -i [fold change file] -o [output file] -e [reference essentials] -n [reference nonessentials] -c [columns to test]
  training.essentials <- file.path(scriptpath,"training_essentials.txt")
  training.nonessentials <- file.path(scriptpath,"training_nonessential.txt")
  
  
  bagel.args <- c(file.path(scriptpath, "BAGEL.py"), "-i", df.bagel.file, "-o", df.bagel.outputfile, "-e", training.essentials, "-n", training.nonessentials, "-c", compare.cols)
  
  
  res <- system2(command = "python2", args = bagel.args, stderr = file.path(dirstore, "stderr_bagel.log"), stdout = file.path(dirstore, "stdout_bagel.log"))
  
  
  # read back BAGEL outputfile into df
  bagel_df <- readr::read_tsv(df.bagel.outputfile)
  
  # set cutoff value
  ref_essential <- read_tsv(file.path(scriptpath, 'training_essentials.txt'))
  ref_nonessential <- read_tsv(file.path(scriptpath,'training_nonessential.txt'))
  distanceOptimalFDR <- function(cutoff, bagel_df){
    tp <- bagel_df %>% filter(BF > cutoff & GENE %in% ref_essential$Gene) %>% nrow
    fp <- bagel_df %>% filter(BF > cutoff & GENE %in% ref_nonessential$Gene) %>% nrow
    
    ## add tiny amount so division by 0 doesn't happen
    fdr <- 1 - tp/(tp+fp+0.00001)
    return(abs(fdr - 0.05))
  }
  # get Bayes Factor cutoff dependent on training datasets
  cutoff <- DEoptim::DEoptim(distanceOptimalFDR, lower=lowercutoff, upper=highercutoff, bagel_df=bagel_df)$optim$bestmem
  
  out <- list("info" = list("cutoff" = cutoff),
              "data" = bagel_df)
  
  return(out)
}