stat.screenbeam=function(groups=cp$groups.compare, folder = NULL, iterations = 15000, burnin=5000, pval=0.05){
  ## SCREENBEAM Method according to
  # https://github.com/jyyu/ScreenBEAM 
  # http://www.ncbi.nlm.nih.gov/pubmed/26415723
  
  # Treatment groups are taken from cp$treatmentgroup!
  
  ############### Screenbeam function utulities.R

  cp$screenbeam <- NULL
  
  
  # get treatment groups
  treatment.groups <- unlist(cp$treatmentgroup)
  
  # # Make untreated df
  # data.list <- data.frame(
  #   "designs" = cp$normalized.readcount[,"design"])
  #   
  # gene.names = cp$normalized.readcount[,"gene"]
  # designs=cp$normalized.readcount[,"design"]
  
  
  sb_infile <- file.path(folder,'ScreenBEAM_infile.tsv')
  sb_df <- cp$normalized.readcount[,c('design', 'gene', names(cp$dataset.names)[treatment.groups])]
 
  
  readr::write_tsv(sb_df, sb_infile)
  
  sb_result <- ScreenBEAM(
    ###input format
    input.file = sb_infile,
    control.samples = names(cp$dataset.names)[cp$treatmentgroup[[1]]],#column names of control samples
    case.samples = names(cp$dataset.names)[cp$treatmentgroup[[2]]],#column names of case/treated samples
    control.groupname = names(cp$treatmentgroup)[1],#name your control group
    case.groupname = names(cp$treatmentgroup)[2],#name your case group
    ###data pre-processing
    data.type='NGS',#data type
    do.normalization=FALSE,
    filterLowCount=FALSE, ## you might want to change this?
    filterBy = 'control',
    count.cutoff=0,
    
    ###Bayesian computing
    nitt=as.numeric(iterations), #number of MCMC iterations, use small number here for testing, please use larger number in real data, 15000 is default
    burnin=as.numeric(burnin) #number of burnin in MCMC sampling, 5000 is default
  )
  
  # subset
  data.analysis <- sb_result[,c(1,3,4,5,6,7)]
  colnames(data.analysis) <- c("gene","n.sgRNAs.passed","B","Z","pval","FDR")
  #data.analysis[,c(5,6)] <- as.numeric(data.analysis[,c(5,6)])
 
  
  return(data.analysis)
  
  
}
